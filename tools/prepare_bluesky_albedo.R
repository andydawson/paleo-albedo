# prepare_bluesky_albedo.R: monthly blue-sky albedo, 2000-2009, over the land part of each
# 1-degree cell, built from the MODIS daily albedo as delivered by NASA's AppEEARS service.
# Run once, from the repository root, after tools/download_era5_radiation.R, in steps that
# are repeated month by month (see "How the build runs" below):
#
#   Rscript tools/prepare_bluesky_albedo.R requests 2005-08 2005-09   # request files for those months
#   Rscript tools/prepare_bluesky_albedo.R collect                    # check and file the downloads
#   Rscript tools/prepare_bluesky_albedo.R build                      # reduce them to monthly files
#   Rscript tools/prepare_bluesky_albedo.R cells                      # the cell table, at the end
#   Rscript tools/prepare_bluesky_albedo.R test h11v04                # July 2005 check (see below)
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
# How the build runs. AppEEARS cuts the six layers the build uses out of NASA's MODIS files
# for the study area and returns one GeoTIFF per layer and day on the MODIS sinusoidal grid,
# about 11 GB a month. Its scripted access has refused Chris's login since 2026-10-16, so
# requests and downloads go through the AppEEARS website, driven with Claude in Chrome (Chris,
# 2026-10-20); Chris stays signed in, and approves each batch of submissions and downloads:
#   1. "requests" writes one request file per month to data/modis_albedo/requests/.
#   2. On https://appeears.earthdatacloud.nasa.gov: Extract -> Area Sample -> "Upload a request
#      file" (on the options page, before the form), then submit.
#   3. When it has finished: Explore -> the request's download page -> every file, into
#      ~/Downloads.
#   4. "collect" checks every downloaded GeoTIFF against the request's SHA-256 list and moves
#      the month into data/modis_albedo/appeears/<year-month>/.
#   5. "build" reduces each collected month, tile by tile, to data/modis_albedo/monthly/
#      <tile>_<year>_<month>.tif, then deletes that month's day files, so the disk holds only
#      the months waiting to be built (about 11 GB each).
#   6. Once every month is built, "cells" writes the cell table.
# Every step can be rerun: requests are not written for months already collected or built, a
# month already collected is not collected again, and a tile-month whose file exists is skipped.
#
# "test" builds July 2005 from data/modis_albedo/appeears_full_2005-07/ (downloaded 2026-10-19,
# requested before the noon solar zenith layer was added, so the 70-degree rule is not applied;
# the July noon sun is everywhere within 61 degrees of the zenith) into
# data/modis_albedo/test_appeears/, to compare with the tile built from NASA's original files on 2026-10-17 by the first version of this
# script (commit e97ec44), and, with "test all", with Andria's raster. Nothing under
# data/modis_albedo/ or data/era5/ is committed.

library(terra)
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

APPEEARS_DIR = file.path(WORK_DIR, 'appeears')  # collected months, one folder each
DOWNLOADS   = path.expand('~/Downloads')         # where the browser saves the AppEEARS files

# The layers, as AppEEARS names its files (<product>_<layer>), with their scale and fill.
LAYERS = data.frame(
  name  = c('bsa', 'wsa', 'qa', 'snow', 'sza', 'land'),
  file  = c('MCD43A3.061_Albedo_BSA_shortwave', 'MCD43A3.061_Albedo_WSA_shortwave',
            'MCD43A3.061_BRDF_Albedo_Band_Mandatory_Quality_shortwave', 'MCD43A2.061_Snow_BRDF_Albedo',
            'MCD43A2.061_BRDF_Albedo_LocalSolarNoon', 'MCD43A2.061_BRDF_Albedo_LandWaterType'),
  scale = c(0.001, 0.001, 1, 1, 1, 1),
  fill  = c(32767, 32767, 255, 255, 255, 255))

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

# --- AppEEARS requests and downloads --------------------------------------------------------
task_name = function(month) paste0('paleoRF_MCD43_', format(month, '%Y-%m'))

# The request area: the box around the cells' outlines, its edges given every half degree so
# that the box keeps its shape when AppEEARS projects it to the sinusoidal grid.
request_area = function(cells) {
  x = c(min(cells$x) - 0.5, max(cells$x) + 0.5); y = c(min(cells$y) - 0.5, max(cells$y) + 0.5)
  sx = seq(x[1], x[2], by = 0.5); sy = seq(y[1], y[2], by = 0.5)
  ring = rbind(cbind(sx, y[1]), cbind(x[2], sy[-1]), cbind(rev(sx)[-1], y[2]), cbind(x[1], rev(sy)[-1]))
  unname(ring)                                   # closed: the last point is the first
}

# One month's request file, in the form AppEEARS's "Upload a request file" accepts.
write_request = function(month, ring) {
  last = seq(month, by = 'month', length.out = 2)[2] - 1
  request = list(
    task_type = 'area', task_name = task_name(month),
    params = list(
      dates  = list(list(startDate = format(max(month, FIRST_DAY), '%m-%d-%Y'),
                         endDate = format(min(last, LAST_DAY), '%m-%d-%Y'))),
      layers = lapply(LAYERS$file, function(f) list(product = sub('_.*', '', f), layer = sub('^[^_]*_', '', f))),
      output = list(format = list(type = 'geotiff'), projection = 'native'),
      geo = list(type = 'FeatureCollection', features = list(list(
        type = 'Feature', properties = setNames(list(), character(0)),
        geometry = list(type = 'Polygon', coordinates = list(ring)))))))
  file = file.path(WORK_DIR, 'requests', paste0(task_name(month), '.json'))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  write_json(request, file, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  file
}

# Moves each finished month from the browser's download folder into APPEEARS_DIR, once every
# GeoTIFF on the request's SHA-256 list ("<task name without underscores>-sha256sums.txt",
# as AppEEARS names it) is there and matches. A copy the browser saved twice ("name (1).tif")
# is left where it is.
collect_downloads = function() {
  lists = list.files(DOWNLOADS, pattern = '^paleoRFMCD43\\d{4}-\\d{2}-sha256sums\\.txt$', full.names = TRUE)
  if (!length(lists)) cat('no request checksum lists in', DOWNLOADS, '\n')
  for (l in lists) {
    month = sub('^paleoRFMCD43(\\d{4}-\\d{2}).*', '\\1', basename(l))
    dest = file.path(APPEEARS_DIR, month)
    if (dir.exists(dest)) { cat(month, ': already collected\n'); next }
    sums = read.table(l, col.names = c('sha256', 'file'), colClasses = 'character')
    sums = sums[grepl('\\.tif$', sums$file), ]
    here = file.path(DOWNLOADS, sums$file)
    if (!all(file.exists(here))) {
      cat(month, ':', sum(!file.exists(here)), 'of', nrow(sums), 'files not downloaded yet\n'); next
    }
    got = vapply(here, function(f) as.character(openssl::sha256(file(f))), '')
    if (any(got != sums$sha256))
      stop(month, ': ', sum(got != sums$sha256), ' files differ from their SHA-256, e.g. ',
           sums$file[got != sums$sha256][1], '; download them again')
    dir.create(dest, recursive = TRUE)
    if (!all(file.rename(c(here, l), file.path(dest, c(sums$file, basename(l))))))
      stop(month, ': could not move the files into ', dest)
    cat(month, ':', nrow(sums), 'files checked and moved to', dest, '\n')
  }
}

# --- reading one day ----------------------------------------------------------------------
# The file of each layer for each day (rows: days, columns: LAYERS$name), NA where AppEEARS
# sent none: on a day with no MODIS file there is no output file either.
day_files = function(day_dir, days) {
  present = list.files(day_dir, pattern = '\\.tif$')
  do.call(cbind, setNames(lapply(LAYERS$file, function(l) {
    f = sprintf('%s_doy%s000000_aid0001.tif', l, format(days, '%Y%j'))
    ifelse(f %in% present, file.path(day_dir, f), NA_character_)
  }), LAYERS$name))
}

# One layer of one day, cut out of AppEEARS's study-area mosaic for one tile, as plain
# numbers: the mosaic is on the MODIS grid itself, so the tile is a whole-pixel window of it
# (pixels outside the request area are NA). Fill values become NA, and the scale factor is
# applied here rather than by GDAL so that nothing is applied twice.
read_layer = function(file, template, name) {
  r = rast(file)
  if (!grepl('Sinusoidal', crs(r)) || !grepl('6371007.181', crs(r))) stop(file, ' is not on the MODIS sinusoidal grid')
  scoff(r) = cbind(1, 0)
  r = crop(r, ext(template), snap = 'near', extend = TRUE)
  if (nrow(r) != nrow(template) || ncol(r) != ncol(template) ||
      max(abs(as.vector(ext(r)) - as.vector(ext(template)))) > 0.01) stop(file, ': the tile window is not whole pixels')
  v = values(r, mat = FALSE)
  v[v == LAYERS$fill[LAYERS$name == name]] = NA
  v * LAYERS$scale[LAYERS$name == name]
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
# need_zenith = FALSE only for the July 2005 test (see the header).
process_tile_month = function(tile, month, day_dir, out_dir, need_zenith = TRUE) {
  out = file.path(out_dir, sprintf('%s_%s.tif', tile, format(month, '%Y_%m')))
  if (file.exists(out)) return(out)
  days = seq(month, seq(month, by = 'month', length.out = 2)[2] - 1, by = 'day')
  days = days[days >= FIRST_DAY & days <= LAST_DAY]
  files = day_files(day_dir, days)
  needed = if (need_zenith) LAYERS$name else setdiff(LAYERS$name, 'sza')
  if (need_zenith && all(is.na(files[, 'sza']))) stop('no noon solar zenith files in ', day_dir)

  template = tile_template(tile)
  ll = pixel_lonlat(template)
  era5 = lapply(unique(format(c(days, max(days) + 1), '%Y-%m-01')), function(m) era5_month(as.Date(m)))
  era5 = list(lon = era5[[1]]$lon, lat = era5[[1]]$lat, time = do.call(c, lapply(era5, `[[`, 'time')),
              ssrd = do.call(abind_time, lapply(era5, `[[`, 'ssrd')), fdir = do.call(abind_time, lapply(era5, `[[`, 'fdir')))

  sums = matrix(0, ncell(template), length(STREAMS), dimnames = list(NULL, STREAMS))
  days_n = sums
  land = NULL
  for (d in seq_along(days)) {
    if (anyNA(files[d, needed])) next                # a day with no file counts as no data
    get  = function(name) read_layer(files[d, name], template, name)
    bsa  = get('bsa')
    wsa  = get('wsa')
    qa   = get('qa')
    snow = get('snow')
    sza  = if (need_zenith) get('sza') else rep(0, ncell(template))
    if (is.null(land)) land = get('land') == LAND_CLASS
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
  out
}

# Every tile of one month, WORKERS at a time; stops if any failed (rerunning retries them).
build_month = function(tiles, month, day_dir, out_dir, need_zenith = TRUE) {
  done = parallel::mclapply(tiles, function(tile)
    tryCatch(process_tile_month(tile, month, day_dir, out_dir, need_zenith),
             error = function(e) paste('FAILED', tile, conditionMessage(e))),
    mc.cores = WORKERS, mc.preschedule = FALSE)
  failed = grep('^FAILED', unlist(done), value = TRUE)
  if (length(failed)) { writeLines(failed); stop(length(failed), ' tiles of ', format(month, '%Y-%m'), ' failed') }
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
cell_sums = function(tile, cell_keys, monthly_dir) {
  files = list.files(monthly_dir, pattern = paste0('^', tile, '_.*\\.tif$'), full.names = TRUE)
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

cell_table = function(tiles, cells, monthly_dir) {
  parts = do.call(rbind, lapply(tiles, cell_sums, cell_keys = paste(cells$x, cells$y), monthly_dir = monthly_dir))
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
  step = if (length(args)) args[1] else ''
  cells = readRDS(CELLS_FILE)[, c('x', 'y')]
  tiles = tiles_for_cells(cells)
  months = seq(as.Date('2000-03-01'), as.Date('2009-12-01'), by = 'month')
  monthly_dir = file.path(WORK_DIR, 'monthly')
  built = function(month) all(file.exists(file.path(monthly_dir, sprintf('%s_%s.tif', tiles, format(month, '%Y_%m')))))

  if (step == 'requests') {
    wanted = if (length(args) > 1) as.Date(paste0(args[-1], '-01'), optional = TRUE) else months
    if (anyNA(wanted) || !all(wanted %in% months)) stop('months are written like 2005-08, from 2000-03 to 2009-12')
    ring = request_area(cells)
    for (i in seq_along(wanted)) {
      m = wanted[i]
      if (built(m) || dir.exists(file.path(APPEEARS_DIR, format(m, '%Y-%m')))) { cat(format(m, '%Y-%m'), ': already collected or built\n'); next }
      cat(write_request(m, ring), '\n')
    }

  } else if (step == 'collect') {
    collect_downloads()

  } else if (step == 'build') {
    cat(length(tiles), 'tiles:', paste(tiles, collapse = ' '), '\n')
    for (day_dir in list.files(APPEEARS_DIR, pattern = '^\\d{4}-\\d{2}$', full.names = TRUE)) {
      started = Sys.time()
      build_month(tiles, as.Date(paste0(basename(day_dir), '-01')), day_dir, monthly_dir)
      unlink(day_dir, recursive = TRUE)              # the day files are no longer needed
      cat(sprintf('%s: built in %.1f min; its day files deleted\n', basename(day_dir),
                  as.numeric(difftime(Sys.time(), started, units = 'mins'))))
    }

  } else if (step == 'cells') {
    missing = months[!vapply(seq_along(months), function(i) built(months[i]), TRUE)]
    if (length(missing)) stop(length(missing), ' months not built yet, e.g. ', format(missing[1], '%Y-%m'))
    table = cell_table(tiles, cells, monthly_dir)
    write.csv(table, OUT_FILE, row.names = FALSE)
    cat(OUT_FILE, ':', nrow(table), 'cell-months;', length(unique(paste(table$x, table$y))), 'cells\n')

  } else if (step == 'test') {
    # July 2005 into a folder of its own: one tile ("test h11v04"), or every tile and the cell
    # table for that one month ("test all").
    test_tiles = if (identical(args[2], 'all')) tiles else args[-1]
    if (!length(test_tiles) || !all(test_tiles %in% tiles)) stop('test needs "all" or tiles of the study area, e.g. h11v04')
    out_dir = file.path(WORK_DIR, 'test_appeears')
    started = Sys.time()
    build_month(test_tiles, as.Date('2005-07-01'), file.path(WORK_DIR, 'appeears_full_2005-07'), out_dir, need_zenith = FALSE)
    cat(sprintf('%d tiles in %.1f min\n', length(test_tiles), as.numeric(difftime(Sys.time(), started, units = 'mins'))))
    if (identical(args[2], 'all')) {
      table = cell_table(tiles, cells, out_dir)
      write.csv(table, file.path(out_dir, 'cells_2005_07.csv'), row.names = FALSE)
      cat(file.path(out_dir, 'cells_2005_07.csv'), ':', nrow(table), 'cells\n')
    }

  } else {
    cat('usage: Rscript tools/prepare_bluesky_albedo.R requests|collect|build|cells|test ...',
        '(see the top of this file)\n')
    quit(status = 2)
  }
}
