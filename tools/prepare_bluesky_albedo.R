# prepare_bluesky_albedo.R: monthly blue-sky albedo, 2000-2009, over the land part of each
# 1-degree cell, built from the MODIS daily albedo. Run once, from the repository root,
# after tools/download_era5_radiation.R:
#
#   Rscript tools/prepare_bluesky_albedo.R            # everything (restartable)
#   Rscript tools/prepare_bluesky_albedo.R test       # one tile and one month, as a check
#
# It writes data/albedo_modis_cells.csv (committed), which script 2 reads when config.R
# chooses the rebuilt albedo; Andria's raster data/blue_sky_monthly_2000-2009.tif stays the
# legacy setting. Decided by Chris on 2026-10-15 (questions B2c, B5), each setting following
# published practice:
#
#   Product      MCD43A3 and MCD43A2 v6.1, daily, 500 m (Schaaf et al. 2002,
#                doi:10.1016/S0034-4257(02)00091-3; Wang et al. 2018,
#                doi:10.1016/j.rse.2018.02.001), shortwave broadband (0.3-5 um), from 5 March
#                2000, the start of the validated record (MCD43 user guide).
#   Blue sky     (1 - f) x black-sky + f x white-sky (Lucht et al. 2000, doi:10.1109/36.841980;
#                Roman et al. 2010, doi:10.1016/j.rse.2009.11.014; MCD43A3 user guide), with f
#                the all-sky diffuse fraction from ERA5, (ssrd - fdir) / ssrd, in the hour
#                containing local solar noon (the black-sky albedo is for local solar noon).
#   Quality      full BRDF inversions only (mandatory quality 0, 2, 4, 6), as the product team
#                recommends (MCD43A3 user guide); all processed retrievals (0-7) are kept as
#                the sensitivity (Wang et al. 2018 validate all qualities).
#   Low sun      solar zenith at local noon at most 70 degrees (MCD43 user guide; NASA MODIS
#                land validation page).
#   Water        only pixels the product's own land/water class (MCD43A2
#                BRDF_Albedo_LandWaterType) labels land (1), as NASA's processing does.
#   Snow         three versions from the MCD43A2 day-of-interest snow flag (Wang et al. 2018):
#                all days, snow-free days, snowy days.
#   Averaging    days to a monthly mean in each year; years averaged with equal weight; no gap
#                filling and no minimum number of days, the numbers kept beside every value
#                (as the published climatologies do: Jia et al. 2022,
#                doi:10.1029/2021JD035987; Duveiller et al. 2018, doi:10.1038/s41467-017-02810-8).
#   Cells        the mean over the land pixels of each 1-degree cell (500 m sinusoidal pixels
#                have equal area, so a plain mean is the area mean; Oleson et al. 2003,
#                doi:10.1029/2002GL016749, aggregate MODIS albedo by area).
#
# Needs a NASA Earthdata login in ~/.netrc ("machine urs.earthdata.nasa.gov login ...
# password ..."). Each tile and month is downloaded into data/modis_albedo/staging/,
# reduced to one monthly file in data/modis_albedo/monthly/, and the downloaded day files
# are then deleted, so the disk never holds more than a few tile-months of raw data. A
# tile-month whose monthly file exists is skipped, so the script can be rerun after an
# interruption. Nothing under data/modis_albedo/ or data/era5/ is committed.

library(terra)
library(curl)
library(jsonlite)

# --- settings ------------------------------------------------------------------------------
FIRST_DAY   = as.Date('2000-03-05')
LAST_DAY    = as.Date('2009-12-31')
CELLS_FILE  = 'data/elev_interp_cells.RDS'      # the 1-degree cell centres the pipeline uses
ERA5_DIR    = 'data/era5'
WORK_DIR    = 'data/modis_albedo'
OUT_FILE    = 'data/albedo_modis_cells.csv'
WORKERS     = 8                                  # tile-months processed at once
FULL_QA     = c(0, 2, 4, 6)                      # full inversions (odd codes are magnitude inversions)
MAX_ZENITH  = 70
LAND_CLASS  = 1
STREAMS     = c('full_all', 'full_snowfree', 'full_snowy', 'any_all', 'any_snowfree', 'any_snowy')

# --- the sinusoidal grid ------------------------------------------------------------------
# MODIS tiles: 36 x 18 tiles of 2,400 x 2,400 pixels on a sphere of radius 6,371,007.181 m.
SIN_CRS   = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs'
TILE_SIZE = 1111950.5196666666
X0 = -20015109.354; Y0 = 10007554.677

# Longitude and latitude of every pixel centre of a tile, from the sinusoidal formulas
# (y = R lat, x = R lon cos lat).
R_EARTH = 6371007.181
tile_template = function(tile) {
  h = as.integer(substr(tile, 2, 3)); v = as.integer(substr(tile, 5, 6))
  rast(nrows = 2400, ncols = 2400, crs = SIN_CRS,
       extent = ext(X0 + h * TILE_SIZE, X0 + (h + 1) * TILE_SIZE, Y0 - (v + 1) * TILE_SIZE, Y0 - v * TILE_SIZE))
}
pixel_lonlat = function(template) {
  xy = xyFromCell(template, seq_len(ncell(template)))
  lat = xy[, 2] / R_EARTH
  cbind(lon = xy[, 1] / (R_EARTH * cos(lat)) * 180 / pi, lat = lat * 180 / pi)
}

# The tiles that hold any part of the cells: each cell's outline, densified, in sinusoidal.
tiles_for_cells = function(cells) {
  corners = expand.grid(dx = seq(-0.5, 0.5, by = 0.1), dy = seq(-0.5, 0.5, by = 0.1))
  pts = cbind(rep(cells$x, each = nrow(corners)) + corners$dx, rep(cells$y, each = nrow(corners)) + corners$dy)
  xy = crds(project(vect(pts, crs = 'EPSG:4326'), SIN_CRS))
  sort(unique(sprintf('h%02dv%02d', floor((xy[, 1] - X0) / TILE_SIZE), floor((Y0 - xy[, 2]) / TILE_SIZE))))
}

# --- finding and downloading the day files --------------------------------------------------
# NASA's Common Metadata Repository lists every file with its download link; no login needed.
granule_urls = function(product, tile, days) {
  q = sprintf(paste0('https://cmr.earthdata.nasa.gov/search/granules.json?short_name=%s&version=061',
                     '&temporal=%sT00:00:00Z,%sT23:59:59Z&page_size=2000',
                     '&readable_granule_name=%s.*.%s.*&options[readable_granule_name][pattern]=true'),
              product, min(days) - 16, max(days) + 16, product, tile)
  entries = fromJSON(rawToChar(curl_fetch_memory(q)$content), simplifyVector = FALSE)$feed$entry
  want = sprintf('%s.A%s.%s.', product, format(days, '%Y%j'), tile)
  urls = setNames(rep(NA_character_, length(days)), want)
  for (e in entries) {
    hit = which(startsWith(e$producer_granule_id, want))
    if (!length(hit)) next
    href = vapply(e$links, function(l) l$href, '')
    href = href[grepl('^https://.*\\.hdf$', href)]
    if (length(href)) urls[hit] = href[1]
  }
  urls
}

earthdata_handle = function(cookies) {
  new_handle(netrc = 1L, followlocation = 1L, unrestricted_auth = 1L,
             cookiefile = cookies, cookiejar = cookies)
}

download_files = function(urls, dir, cookies) {
  dest = file.path(dir, basename(urls))
  for (i in seq_along(urls)) {
    if (is.na(urls[i]) || file.exists(dest[i])) next
    for (attempt in 1:5) {
      ok = !inherits(try(curl_download(urls[i], dest[i], handle = earthdata_handle(cookies), quiet = TRUE),
                         silent = TRUE), 'try-error')
      if (ok && file.size(dest[i]) > 1e6) break
      if (file.exists(dest[i])) file.remove(dest[i])
      Sys.sleep(10 * attempt)
    }
    if (!file.exists(dest[i])) stop('could not download ', urls[i])
  }
  ifelse(is.na(urls), NA_character_, dest)
}

# --- reading one day ----------------------------------------------------------------------
# A layer of an HDF4-EOS file, as plain numbers: fill values become NA, the scale factor is
# applied here rather than by GDAL so that nothing is applied twice.
read_layer = function(file, layer, scale = 1, fill) {
  r = rast(sprintf('HDF4_EOS:EOS_GRID:"%s":MOD_Grid_BRDF:%s', file, layer))
  scoff(r) = cbind(1, 0)
  v = values(r, mat = FALSE)
  v[v == fill] = NA
  v * scale
}

# --- the diffuse fraction from ERA5 ---------------------------------------------------------
# For each pixel: the nearest ERA5 grid point, and the ERA5 time stamp whose hour contains
# local solar noon. Local solar noon in UTC hours is 12 - longitude / 15 - EoT / 60, with the
# equation of time EoT in minutes (Spencer 1971, as given by NOAA's solar calculator notes).
equation_of_time = function(date) {
  g = 2 * pi / 365 * (as.numeric(format(date, '%j')) - 1)
  229.18 * (0.000075 + 0.001868 * cos(g) - 0.032077 * sin(g) - 0.014615 * cos(2 * g) - 0.040849 * sin(2 * g))
}

era5_month = function(month) {
  f = file.path(ERA5_DIR, sprintf('era5_ssrd_fdir_%s.nc', format(month, '%Y_%m')))
  nc = ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  tname = intersect(c('valid_time', 'time'), names(nc$dim))[1]
  t0 = ncdf4::ncatt_get(nc, tname, 'units')$value
  list(lon = ncdf4::ncvar_get(nc, 'longitude'), lat = ncdf4::ncvar_get(nc, 'latitude'),
       time = as.POSIXct(ncdf4::ncvar_get(nc, tname), origin = sub('.*since ', '', t0), tz = 'UTC'),
       ssrd = ncdf4::ncvar_get(nc, 'ssrd'), fdir = ncdf4::ncvar_get(nc, 'fdir'))
}

diffuse_fraction = function(era5, lon, lat, date) {
  noon = 12 - lon / 15 - equation_of_time(date) / 60          # UTC hours, 15.3 to 23.7 here
  stamp = as.POSIXct(date, tz = 'UTC') + ceiling(noon) * 3600  # the hour ending at the stamp
  it = match(as.numeric(stamp), as.numeric(era5$time))
  ix = match(round(lon * 4) / 4, round(era5$lon * 4) / 4)
  iy = match(round(lat * 4) / 4, round(era5$lat * 4) / 4)
  idx = cbind(ix, iy, it)
  ssrd = era5$ssrd[idx]; fdir = era5$fdir[idx]
  f = (ssrd - fdir) / ssrd
  f[!is.finite(f) | ssrd <= 0] = NA
  pmin(pmax(f, 0), 1)
}

# --- one tile and one month -----------------------------------------------------------------
process_tile_month = function(tile, month, cookies) {
  out = file.path(WORK_DIR, 'monthly', sprintf('%s_%s.tif', tile, format(month, '%Y_%m')))
  if (file.exists(out)) return(out)
  days = seq(month, seq(month, by = 'month', length.out = 2)[2] - 1, by = 'day')
  days = days[days >= FIRST_DAY & days <= LAST_DAY]
  staging = file.path(WORK_DIR, 'staging', paste(tile, format(month, '%Y_%m'), sep = '_'))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  a3 = download_files(granule_urls('MCD43A3', tile, days), staging, cookies)
  a2 = download_files(granule_urls('MCD43A2', tile, days), staging, cookies)

  template = tile_template(tile)
  ll = pixel_lonlat(template)
  era5 = lapply(unique(format(c(days, max(days) + 1), '%Y-%m-01')), function(m) era5_month(as.Date(m)))
  era5 = list(lon = era5[[1]]$lon, lat = era5[[1]]$lat, time = do.call(c, lapply(era5, `[[`, 'time')),
              ssrd = do.call(abind_time, lapply(era5, `[[`, 'ssrd')), fdir = do.call(abind_time, lapply(era5, `[[`, 'fdir')))

  sums = matrix(0, ncell(template), length(STREAMS), dimnames = list(NULL, STREAMS))
  days_n = sums
  land = NULL
  for (d in seq_along(days)) {
    if (is.na(a3[d]) || is.na(a2[d])) next          # a day with no file counts as no data
    bsa  = read_layer(a3[d], 'Albedo_BSA_shortwave', 0.001, 32767)
    wsa  = read_layer(a3[d], 'Albedo_WSA_shortwave', 0.001, 32767)
    qa   = read_layer(a3[d], 'BRDF_Albedo_Band_Mandatory_Quality_shortwave', 1, 255)
    snow = read_layer(a2[d], 'Snow_BRDF_Albedo', 1, 255)
    sza  = read_layer(a2[d], 'BRDF_Albedo_LocalSolarNoon', 1, 255)
    if (is.null(land)) land = read_layer(a2[d], 'BRDF_Albedo_LandWaterType', 1, 255) == LAND_CLASS
    f = diffuse_fraction(era5, ll[, 1], ll[, 2], days[d])
    blue = (1 - f) * bsa + f * wsa
    usable = !is.na(blue) & !is.na(qa) & qa <= 7 & !is.na(sza) & sza <= MAX_ZENITH & land %in% TRUE
    full = usable & qa %in% FULL_QA
    keep = cbind(full_all = full, full_snowfree = full & snow %in% 0, full_snowy = full & snow %in% 1,
                 any_all = usable, any_snowfree = usable & snow %in% 0, any_snowy = usable & snow %in% 1)
    sums = sums + ifelse(keep, blue, 0)
    days_n = days_n + keep
  }
  monthly = sums / days_n                            # NaN where no day qualified
  monthly[!is.finite(monthly)] = NA
  layers = c(lapply(STREAMS, function(s) setValues(template, monthly[, s])),
             lapply(STREAMS, function(s) setValues(template, days_n[, s])),
             list(setValues(template, as.numeric(land %in% TRUE))))
  r = rast(layers)
  names(r) = c(STREAMS, paste0('days_', STREAMS), 'land')
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeRaster(r, out, gdal = 'COMPRESS=DEFLATE', datatype = 'FLT4S')
  unlink(staging, recursive = TRUE)                  # the raw day files are no longer needed
  out
}

abind_time = function(...) {                          # join [lon, lat, time] arrays along time
  a = list(...); d = dim(a[[1]])
  array(unlist(a), dim = c(d[1], d[2], sum(vapply(a, function(x) dim(x)[3], 1))))
}

# --- climatology and cells --------------------------------------------------------------------
# Per pixel and calendar month: the mean over years of the monthly means (each year with data
# weighs the same), the number of such years and the mean number of qualifying days in them.
# Per 1-degree cell: the mean of those values over the cell's land pixels that have one (equal-
# area pixels, so this is the area mean), summed tile by tile and divided at the end because a
# cell can span several tiles.
cell_sums = function(tile, cell_keys) {
  files = list.files(file.path(WORK_DIR, 'monthly'), pattern = paste0('^', tile, '_.*\\.tif$'), full.names = TRUE)
  if (!length(files)) return(NULL)
  month_of = as.integer(sub('.*_(\\d{2})\\.tif$', '\\1', files))
  ll = pixel_lonlat(tile_template(tile))
  cell = paste(floor(ll[, 1]) + 0.5, floor(ll[, 2]) + 0.5)
  land = values(rast(files[1])[['land']], mat = FALSE) == 1
  keep = land & cell %in% cell_keys
  if (!any(keep)) return(NULL)
  out = list()
  for (m in sort(unique(month_of))) {
    yearly = lapply(files[month_of == m], function(f) values(rast(f)[[c(STREAMS, paste0('days_', STREAMS))]])[keep, , drop = FALSE])
    value = simplify2array(lapply(yearly, function(y) y[, STREAMS, drop = FALSE]))          # pixel x stream x year
    days  = simplify2array(lapply(yearly, function(y) y[, paste0('days_', STREAMS), drop = FALSE]))
    years = apply(!is.na(value), c(1, 2), sum)
    clim  = apply(value, c(1, 2), mean, na.rm = TRUE); clim[years == 0] = NA
    mean_days = apply(ifelse(is.na(value), NA, days), c(1, 2), mean, na.rm = TRUE); mean_days[years == 0] = NA
    has = !is.na(clim)
    out[[length(out) + 1]] = data.frame(
      cell = rownames(rowsum(rep(1, sum(keep)), cell[keep])), month = m,
      land_pixels = rowsum(rep(1, sum(keep)), cell[keep])[, 1],
      setNames(as.data.frame(rowsum(ifelse(has, clim, 0), cell[keep])), paste0('sum_', STREAMS)),
      setNames(as.data.frame(rowsum(has * 1, cell[keep])), paste0('pixels_', STREAMS)),
      setNames(as.data.frame(rowsum(ifelse(has, years, 0), cell[keep])), paste0('sumyears_', STREAMS)),
      setNames(as.data.frame(rowsum(ifelse(has, mean_days, 0), cell[keep])), paste0('sumdays_', STREAMS)),
      check.names = FALSE)
  }
  do.call(rbind, out)
}

cell_table = function(tiles, cells) {
  parts = do.call(rbind, lapply(tiles, cell_sums, cell_keys = paste(cells$x, cells$y)))
  num = setdiff(names(parts), c('cell', 'month'))
  totals = aggregate(parts[num], by = parts[c('cell', 'month')], FUN = sum)
  xy = do.call(rbind, strsplit(totals$cell, ' '))
  out = data.frame(x = as.numeric(xy[, 1]), y = as.numeric(xy[, 2]), month = MONTHS_ABB[totals$month],
                   land_pixels = totals$land_pixels)
  for (s in STREAMS) {
    n = totals[[paste0('pixels_', s)]]
    out[[s]]                    = round(ifelse(n > 0, totals[[paste0('sum_', s)]] / n, NA), 5)
    out[[paste0('pixels_', s)]] = n
    out[[paste0('years_', s)]]  = round(ifelse(n > 0, totals[[paste0('sumyears_', s)]] / n, NA), 2)
    out[[paste0('days_', s)]]   = round(ifelse(n > 0, totals[[paste0('sumdays_', s)]] / n, NA), 2)
  }
  out[order(out$y, out$x, match(out$month, MONTHS_ABB)), ]
}
MONTHS_ABB = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

# --- main -------------------------------------------------------------------------------------
if (sys.nframe() == 0) {
  args = commandArgs(TRUE)
  cells = readRDS(CELLS_FILE)[, c('x', 'y')]
  tiles = tiles_for_cells(cells)
  months = seq(as.Date('2000-03-01'), as.Date('2009-12-01'), by = 'month')
  if ('test' %in% args) { tiles = 'h11v04'; months = as.Date('2005-07-01') }
  cat(length(tiles), 'tiles:', paste(tiles, collapse = ' '), '\n')
  cookies = tempfile('earthdata_cookies')
  tasks = expand.grid(tile = tiles, month = months, stringsAsFactors = FALSE)
  started = Sys.time()
  done = parallel::mclapply(seq_len(nrow(tasks)), function(i)
    tryCatch(process_tile_month(tasks$tile[i], tasks$month[i], cookies),
             error = function(e) paste('FAILED', tasks$tile[i], tasks$month[i], conditionMessage(e))),
    mc.cores = WORKERS, mc.preschedule = FALSE)
  failed = grep('^FAILED', unlist(done), value = TRUE)
  if (length(failed)) { writeLines(failed); stop(length(failed), ' tile-months failed; rerun to retry them') }
  cat(sprintf('%d tile-months in %.1f h\n', nrow(tasks), as.numeric(difftime(Sys.time(), started, units = 'hours'))))

  # The cell table: one row per cell and month, every stream with its pixel, year and day
  # numbers. In test mode it goes to a scratch file, not to data/.
  table = cell_table(tiles, cells)
  target = if ('test' %in% args) file.path(WORK_DIR, 'test_cells.csv') else OUT_FILE
  write.csv(table, target, row.names = FALSE)
  cat(target, ':', nrow(table), 'cell-months;', length(unique(paste(table$x, table$y))), 'cells\n')
}
