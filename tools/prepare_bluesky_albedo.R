# prepare_bluesky_albedo.R: monthly blue-sky albedo, 2000-2009, over the land part of each
# 1-degree cell, built from the MODIS daily albedo as delivered by NASA's AppEEARS service.
# Run once, from the repository root, after tools/download_era5_radiation.R, in steps that
# are repeated month by month (see "How the build runs" below):
#
#   Rscript tools/prepare_bluesky_albedo.R requests 2005-08 2005-09   # request files for those months
#   Rscript tools/prepare_bluesky_albedo.R collect                    # check and file the downloads
#   Rscript tools/prepare_bluesky_albedo.R build                      # reduce them to monthly files
#   Rscript tools/prepare_bluesky_albedo.R build 2005-03 keep         # one month, keeping its day files
#   Rscript tools/prepare_bluesky_albedo.R cells                      # the cell table, at the end
#   Rscript tools/prepare_bluesky_albedo.R test all                   # July 2005 check (see below)
#
# It writes data/albedo_modis_cells.csv (committed), for script 2 to read when config.R chooses
# the rebuilt albedo (not wired into script 2 yet); Andria's raster
# data/blue_sky_monthly_2000-2009.tif stays the legacy setting. Settings decided by Chris on
# 2026-10-15 (questions B2c, B5) and revised on 2026-10-20 after two independent reviews of the
# build. Where a setting is our own judgement rather than published practice, it says so:
#
#   Product      MCD43A3 and MCD43A2 collection 6.1, daily, 500 m, shortwave broadband (0.3-5 um),
#                from 5 March 2000, the start of the validated record (MCD43 user guide; algorithm:
#                Schaaf et al. 2002, doi:10.1016/S0034-4257(02)00091-3; daily collection 6: Wang
#                et al. 2018, doi:10.1016/j.rse.2018.02.001). Each daily value comes from the
#                observations of a 16-day window centred on that day.
#   Blue sky     (1 - f) x black-sky + f x white-sky, the isotropic approximation of the MCD43A3
#                user guide (Lucht et al. 2000, doi:10.1109/36.841980), which the guide says is
#                weakest at large solar angles over bright surfaces such as snow; Roman et al. 2010
#                (doi:10.1016/j.rse.2009.11.014) found the full anisotropic form improves the
#                biases only slightly. f is the all-sky diffuse fraction from ERA5,
#                (ssrd - fdir) / ssrd (fdir is through a horizontal plane), in the hour containing
#                local solar noon. The black-sky albedo is for local solar noon, so the result is
#                the noon blue-sky albedo, not an albedo weighted by the day's sunlight.
#   Quality      full BRDF inversions only (mandatory quality 0, 2, 4, 6), as the product team
#                recommends (MCD43A3 user guide); any processed retrieval (0-7) is kept for
#                comparison (Wang et al. 2018 validate all qualities).
#   Low sun      noon solar zenith at most 75 degrees, as in the published climatology of Jia et
#                al. 2022 (doi:10.1029/2021JD035987). Kept for comparison: 70 degrees (NASA's MODIS
#                land validation page calls values beyond 70 degrees suspect) and no rule. The
#                product stores the zenith in whole degrees, so a stored 75 passes. The rule
#                removes the north in winter: at 75 degrees, December keeps only cells south of
#                about 53 N. A day whose zenith file is missing counts only for the no-rule version.
#   Land         pixels whose land/water class (MCD43A2 BRDF_Albedo_LandWaterType) is land (1) on
#                more than half of the days it has a class, over the whole record (Chris,
#                2026-10-20). Our choice: the product is also made for coastal and shallow-water
#                areas (MCD43 user guide). A blank class means no retrieval that day and is ignored.
#                Shoreline days (class 2, "ocean coastlines and lake shorelines") are kept per pixel,
#                so LAND_RULE = 'land_or_shore' can count them with land; whether to is still open.
#   Snow         the MCD43A2 snow flag of each day (Wang et al. 2018): every version keeps all,
#                snowy and snow-free days; days with no snow flag count only in all. Each cell has
#                snowy, snow-free and unknown-snow albedos and shares that mix back exactly to the
#                all-days value (snowy and snow-free alone do so only where no day lacks a flag).
#   Averaging    our judgement: within each year, every qualifying land pixel-day of a cell counts
#                once; the years with data then weigh the same; no gap filling and no minimum
#                number of days, the counts kept beside every value. Jia et al. 2022 likewise
#                average each date over the years, but also drop the largest and smallest values;
#                Duveiller et al. 2018 (doi:10.1038/s41467-017-02810-8) take the median of
#                mid-month composites. We take the mean because forcing is linear in albedo.
#   Cells        the mean over the qualifying land pixel-days of each 1-degree cell within each
#                year, then the years weighted equally (see Averaging). The 500 m sinusoidal pixels
#                have equal area (MODIS grid), so every pixel-day stands for the same area, but
#                pixels with more qualifying days weigh more: this is not the plain area mean of
#                the pixels' own means. Oleson et al. 2003 (doi:10.1029/2002GL016749) aggregated
#                MODIS albedo to a model grid "using area weighting".
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
#   4. "collect" checks every downloaded GeoTIFF against the request's SHA-256 list, moves the
#      month into data/modis_albedo/appeears/<year-month>/ (through "<year-month>.partial", so an
#      interrupted move is finished by running collect again, at whatever point it stopped), and
#      keeps the checksum list and AppEEARS's other files (its copy of the request, the granule
#      list, metadata; named with hyphens) in data/modis_albedo/records/<year-month>/.
#   5. "build" checks the month's files against the checksum list again, checks that every layer
#      was delivered, that at least one day has every albedo, quality, snow and land layer and that
#      at least one such day also has the noon solar zenith, and that the request asked for the right
#      days and layers. AppEEARS's copy of the request must be in the records and ask for the same
#      dates, layers and area as our request file; with "keep" a missing or different copy only
#      gives a warning. Any of these failing stops the build before anything is built or deleted.
#      It then reduces the month tile by tile to data/modis_albedo/monthly/<tile>_<year>_<month>.tif
#      (per-pixel sums and counts, tagged with the settings and this script's SHA-256), checks every
#      layer of each new file under a temporary name before naming it, adds a section to
#      records/<year-month>/build.txt (code, settings, inputs, days used and skipped, each tile's
#      origin, retrievals lost), and only then deletes the month's day files. "keep" keeps them for
#      good (a KEEP file in the day folder, written before the month is built so that a failed
#      build leaves it too; later builds without month names skip that month).
#   6. Once every month is built, "cells" decides the land pixels and writes the cell table, with
#      a note beside it of the land rule and the script versions that made the monthly files. It
#      stops if any monthly file was made with other settings, or by a version of this script that
#      is neither this one nor one on the short list ACCEPTED_OLDER_SHAS, or does not record which
#      version made it (Chris, 2026-10-21). After an edit to this script, either every month is rebuilt, from day files
#      downloaded again where they were deleted, or the old version's SHA-256 is added to that list,
#      once it has been checked to compute the same numbers (e.g. a change to comments only, seen
#      in the diff); its entry says what changed and how that was checked. Reuse at build time
#      stays strict: a tile file is reused only if this version made it.
# Every step can be rerun: requests are not written for months already collected or built, a
# month already collected is not collected again, and a tile-month whose file exists is reused,
# unless it was made with other settings or by another version of this script, which stops the
# build. Without this script's own SHA-256, "build" and "test" stop before anything else they do,
# and "cells" before it reads any monthly file.
#
# "test" builds July 2005 from data/modis_albedo/appeears_full_2005-07/ (downloaded 2026-10-19,
# before the noon solar zenith layer was added to the requests, so the zenith is taken as 0:
# valid only for July, when the noon sun is everywhere within 61 degrees of the zenith) into
# data/modis_albedo/test_appeears_v4/, and with "test all" also that month's cell table, to
# compare with Andria's raster. Nothing under data/modis_albedo/ or data/era5/ is committed.

library(terra)
library(jsonlite)

# --- settings ------------------------------------------------------------------------------
FIRST_DAY   = as.Date('2000-03-05')
LAST_DAY    = as.Date('2009-12-31')
CELLS_FILE  = 'data/elev_interp_cells.RDS'      # the 1-degree cell centres the pipeline uses
ERA5_DIR    = 'data/era5'
WORK_DIR    = 'data/modis_albedo'
OUT_FILE    = 'data/albedo_modis_cells.csv'
WORKERS     = 6                                  # tile-months processed at once (several GB each)
FULL_QA     = c(0, 2, 4, 6)                      # full inversions (odd codes are magnitude inversions)

# The versions kept for every pixel and month. Main: full inversions, with the noon sun within
# 75 degrees of the zenith (Jia et al. 2022). For comparison: any processed retrieval; the
# 70-degree rule of NASA's validation page; and no low-sun rule at all.
VERSIONS = data.frame(name       = c('full_75', 'any_75', 'full_70', 'full_nocut'),
                      quality    = c('full',    'any',    'full',    'full'),
                      max_zenith = c(75,        75,       70,        Inf))
# What each version keeps per pixel and month: the number of qualifying days (all, snowy,
# snow-free) and the sums of their blue-sky albedo. A day whose snow flag is missing counts
# only in "all". Sums, not means, so that land and the averaging can be decided at the end.
SUMS = c('n_all', 'n_snow', 'n_free', 's_all', 's_snow', 's_free')
VERSION_LAYERS = as.vector(outer(SUMS, VERSIONS$name, function(s, v) paste(v, s, sep = '_')))
TILE_LAYERS = c(VERSION_LAYERS, 'land_days', 'shore_days', 'read_days')
# Which pixels count as land at the cell step (see is_land). 'land': class 1 (land) on more than
# half of the days with a class, as Chris decided on 2026-10-20. 'land_or_shore': classes 1 or 2
# (ocean coastlines and lake shorelines) together on more than half. The shoreline days are kept
# per pixel so that this choice, still open for Chris and Andria, can be made after the day files
# are deleted.
LAND_RULE   = 'land'
# Written into every monthly file; a file with another tag is never reused, so a changed setting
# cannot silently mix with outputs made before the change. The July 2005 test, which takes the
# noon zenith as 0, has a tag of its own, so its files can never pass for real-zenith ones.
settings_tag = function(need_zenith = TRUE)
  paste0('reduction 3 (2026-10-20); versions ',
         paste(VERSIONS$name, VERSIONS$quality, VERSIONS$max_zenith, collapse = ', '),
         '; full inversions ', paste(FULL_QA, collapse = ' '), '; from ', FIRST_DAY,
         if (need_zenith) '; noon zenith from the data' else '; noon zenith taken as 0 (July 2005 test only)')
SETTINGS_TAG = settings_tag(TRUE)
# The code that makes a file: this script's own SHA-256, written into every monthly file next to
# the settings tag. A file is reused only if both match, so a corrected script never picks up
# tiles made by an earlier version of itself. (Run with Rscript, the script is the --file
# argument; sourced, e.g. by the tests, it is the file source() is reading.)
SCRIPT_FILE = local({
  reading = Filter(function(f) exists('ofile', envir = f, inherits = FALSE), sys.frames())  # inside source()
  if (length(reading)) get('ofile', envir = reading[[length(reading)]])
  else sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
})
SCRIPT_SHA = if (length(SCRIPT_FILE) == 1 && !is.na(SCRIPT_FILE) && file.exists(SCRIPT_FILE))
  as.vector(as.character(openssl::sha256(file(SCRIPT_FILE)))) else 'unknown'   # as.vector: drop the 'hash' class
# Without its SHA-256 the build could not tell its own files from those of another version, so the
# steps that write or combine monthly files stop first (Chris, 2026-10-21).
require_script_sha = function() {
  if (!grepl('^[0-9a-f]{64}$', SCRIPT_SHA))
    stop('cannot tell which version of this script is running (its SHA-256 is "', SCRIPT_SHA,
         '"); run it with Rscript tools/prepare_bluesky_albedo.R from the repository root')
}
# Older versions of this script whose monthly files the cell step still accepts, because each was
# checked to compute the same numbers as this version (Chris, 2026-10-21). Any other version's
# files are refused, so without an entry here even a comment edit would make the cell step refuse
# every month built before it. Add a version only after that check, one line each, saying what
# changed and how the check was made, e.g.
#   'c0ffee...(its 64-character SHA-256)',   # commit 1a2b3c4: comment-only change; verified by ...
# A version's SHA-256 is the one in its monthly files and build records. Empty: this version only.
ACCEPTED_OLDER_SHAS = c(
)

APPEEARS_DIR = file.path(WORK_DIR, 'appeears')  # collected months, one folder each
RECORDS_DIR = file.path(WORK_DIR, 'records')     # kept for good: checksum lists, AppEEARS's files, build records
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
# How AppEEARS names the files it returns for a task: the checksum list drops the underscores of
# the task name ("paleoRFMCD432005-03-sha256sums.txt"), its other files turn them into hyphens
# ("paleoRF-MCD43-2005-03-request.json", "...-granule-list.txt", "...-MCD43A3-061-metadata.xml").
appeears_prefixes = function(month) {
  if (is.character(month)) month = as.Date(paste0(month, '-01'))
  c(gsub('_', '', task_name(month)), gsub('_', '-', task_name(month)))
}

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

# Every GeoTIFF on a request's SHA-256 list ("<task name without underscores>-sha256sums.txt",
# as AppEEARS names it) must be at `where` and match; stops otherwise.
check_checksums = function(list_file, where) {
  sums = read.table(list_file, col.names = c('sha256', 'file'), colClasses = 'character')
  sums = sums[grepl('\\.tif$', sums$file), ]
  if (length(where) == 1) where = file.path(where, sums$file)
  if (!all(file.exists(where))) stop(sum(!file.exists(where)), ' of ', nrow(sums), ' files on ', basename(list_file), ' are missing')
  got = vapply(where, function(f) as.character(openssl::sha256(file(f))), '')
  if (any(got != sums$sha256))
    stop(sum(got != sums$sha256), ' files differ from their SHA-256 on ', basename(list_file), ', e.g. ',
         sums$file[got != sums$sha256][1], '; download them again')
  nrow(sums)
}

# Moves each finished month from the browser's download folder into APPEEARS_DIR, once every
# GeoTIFF on its checksum list is there and matches. The files go first into "<month>.partial",
# which is renamed only when all are in, so an interrupted move is finished by running collect
# again. The checksum list and AppEEARS's other files for the request (granule list, request,
# metadata) go to RECORDS_DIR/<month>/, which is never deleted. A copy the browser saved twice
# ("name (1).tif") is not on the list and is left where it is.
collect_downloads = function() {
  lists = list.files(DOWNLOADS, pattern = '^paleoRFMCD43\\d{4}-\\d{2}-sha256sums\\.txt$', full.names = TRUE)
  # A collect interrupted after the checksum list went to the records but before the final
  # rename: finish it from the list kept there.
  stuck = sub('\\.partial$', '', list.files(APPEEARS_DIR, pattern = '^\\d{4}-\\d{2}\\.partial$'))
  stuck = stuck[!dir.exists(file.path(APPEEARS_DIR, stuck))]
  # (as.character: with no such month, unlist() gives NULL, which basename() refuses)
  kept = as.character(unlist(lapply(stuck, function(m) list.files(file.path(RECORDS_DIR, m), pattern = '-sha256sums\\.txt$', full.names = TRUE))))
  lists = c(lists, kept[!basename(kept) %in% basename(lists)])
  if (!length(lists)) cat('no request checksum lists in', DOWNLOADS, '\n')
  for (l in lists) {
    month = sub('^paleoRFMCD43(\\d{4}-\\d{2}).*', '\\1', basename(l))
    dest = file.path(APPEEARS_DIR, month)
    partial = paste0(dest, '.partial')
    if (dir.exists(dest)) { cat(month, ': already collected\n'); next }
    sums = read.table(l, col.names = c('sha256', 'file'), colClasses = 'character')
    sums = sums[grepl('\\.tif$', sums$file), ]
    moved = file.exists(file.path(partial, sums$file))
    where = ifelse(moved, file.path(partial, sums$file), file.path(DOWNLOADS, sums$file))
    if (!all(file.exists(where))) {
      cat(month, ':', sum(!file.exists(where)), 'of', nrow(sums), 'files not downloaded yet\n'); next
    }
    check_checksums(l, where)
    dir.create(partial, recursive = TRUE, showWarnings = FALSE)
    if (!all(file.rename(where[!moved], file.path(partial, sums$file[!moved]))))
      stop(month, ': could not move every file into ', partial, '; run collect again to finish')
    record = file.path(RECORDS_DIR, month)
    dir.create(record, recursive = TRUE, showWarnings = FALSE)
    side = list.files(DOWNLOADS, full.names = TRUE)
    ours = Reduce(`|`, lapply(paste0(appeears_prefixes(month), '-'), function(p) startsWith(basename(side), p)))
    side = side[ours & !grepl('\\.tif$', side)]
    if (!all(file.rename(side, file.path(record, basename(side))))) stop(month, ': could not move the request files into ', record)
    if (!file.rename(partial, dest)) stop(month, ': could not rename ', partial)
    cat(month, ':', nrow(sums), 'files checked and moved to', dest, '; request files kept in', record, '\n')
  }
}

# --- what AppEEARS delivered for a month ------------------------------------------------------
# The days of a month that the build uses (the record starts on FIRST_DAY).
month_days = function(month) {
  days = seq(month, seq(month, by = 'month', length.out = 2)[2] - 1, by = 'day')
  days[days >= FIRST_DAY & days <= LAST_DAY]
}

# Which layers were delivered on which day (rows: days, columns: LAYERS$name). The checksum list
# proves that what AppEEARS listed arrived intact, not that it processed every layer and day, so
# this stops if any GeoTIFF in the folder is not one of our layers on a day of this month (a
# renamed layer, a wrong month), if a needed layer has no file at all, or if no day has every
# albedo, quality, snow and land layer (Chris, 2026-10-21): in either case every day would be
# skipped and the tiles would come out empty, yet pass their checks as valid zeros. It also stops
# if the noon zenith is needed but no such usable day has it (Chris, 2026-10-21): the versions with
# a low-sun rule, the main one among them, would then be empty in the same way. A day missing only
# the zenith is still used by the version with no low-sun rule.
delivery_table = function(day_dir, days, needed) {
  tifs = list.files(day_dir, pattern = '\\.tif$')
  named = grepl('_doy\\d{7}000000_aid0001\\.tif$', tifs)
  layer = sub('_doy\\d{7}000000_aid0001\\.tif$', '', tifs)
  day = as.Date(sub('.*_doy(\\d{7})000000_aid0001\\.tif$', '\\1', tifs), '%Y%j')
  stray = !named | !(layer %in% LAYERS$file) | !(day %in% days)
  if (any(stray)) stop(day_dir, ': ', sum(stray), ' files are not one of the layers on a day of this month, e.g. ', tifs[stray][1])
  present = do.call(cbind, lapply(LAYERS$file, function(l) days %in% day[layer == l]))
  colnames(present) = LAYERS$name
  none = needed[colSums(present[, needed, drop = FALSE]) == 0]
  if (length(none)) stop(day_dir, ': no file at all for ', paste(none, collapse = ', '), '; the delivery is incomplete')
  if (!any(usable_days(present, needed)))
    stop(day_dir, ': no day has every albedo, quality, snow and land layer (', paste(setdiff(needed, 'sza'), collapse = ', '),
         '); every day would be skipped, so the delivery is incomplete')
  if ('sza' %in% needed && !any(usable_days(present, needed) & present[, 'sza']))
    stop(day_dir, ': no usable day has the noon solar zenith layer (sza); the versions with a low-sun rule, ',
         'the main one among them, would have no data, so the delivery is incomplete')
  data.frame(day = days, present, check.names = FALSE)
}

# The days the build can use: those with every needed layer but the noon zenith (a day missing
# only the zenith is used by the version with no low-sun rule). `present`: days by layer names.
usable_days = function(present, needed) apply(present[, setdiff(needed, 'sza'), drop = FALSE], 1, all)

# How the days are used (the same rule as process_tile_month): a day missing an albedo, quality,
# snow or land layer is skipped; a day missing only the noon zenith is used, but only by the
# version with no low-sun rule. Lines for the build record, so that archive gaps stay visible.
delivery_summary = function(tab, needed) {
  skipped = !usable_days(tab, needed)
  no_zenith = !skipped & 'sza' %in% needed & !tab$sza
  dates = function(k) if (any(k)) paste(format(tab$day[k], '%d'), collapse = ' ') else '-'
  c(sprintf('days in the month: %d; used by every version: %d', nrow(tab), sum(!skipped & !no_zenith)),
    sprintf('days used without the noon zenith (no-low-sun-rule version only): %d (%s)', sum(no_zenith), dates(no_zenith)),
    sprintf('days skipped, an albedo, quality, snow or land layer missing: %d (%s)', sum(skipped), dates(skipped)),
    paste('days with a file, by layer:', paste(sprintf('%s %d', LAYERS$name, colSums(tab[, LAYERS$name])), collapse = ', ')))
}

# What a request file asks for: its dates, layers (<product>_<layer>) and area (the polygon's
# coordinates, in order). NULL if the file is missing or cannot be read.
read_request = function(f) {
  req = tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  r = tryCatch(list(dates  = as.Date(unlist(req$params$dates[1, c('startDate', 'endDate')]), '%m-%d-%Y'),
                    layers = paste(req$params$layers$product, req$params$layers$layer, sep = '_'),
                    area   = as.numeric(unlist(req$params$geo$features$geometry$coordinates))),
               error = function(e) NULL)
  if (is.null(r) || length(r$dates) != 2 || anyNA(r$dates) || !length(r$layers) || !length(r$area)) NULL else r
}

# Where two requests differ in what they ask for: dates, layers or area (empty if nowhere).
request_differences = function(a, b) {
  c(if (!identical(as.numeric(a$dates), as.numeric(b$dates))) 'dates',
    if (!setequal(a$layers, b$layers)) 'layers',
    if (length(a$area) != length(b$area) || max(abs(a$area - b$area)) > 1e-9) 'area')
}

# The request behind a month. AppEEARS's copy, which collect files in the records, shows what
# AppEEARS was actually asked to do; it must be there and ask for the same dates, layers and area
# as our request file in requests/. A build that will delete the day files stops if it is missing
# or differs (Chris, 2026-10-21): once they are gone, nothing else shows what the month was made
# from. A build with "keep" only warns. Either way, what was asked (AppEEARS's copy if it can be
# read, else ours) must be the month's days and our layers, else the build stops.
check_request = function(month, record_dir, deleting = TRUE) {
  theirs = list.files(record_dir, pattern = 'request.*\\.json$', full.names = TRUE)
  ours = file.path(WORK_DIR, 'requests', paste0(task_name(month), '.json'))
  their_request = if (length(theirs) == 1) read_request(theirs)
  our_request = read_request(ours)
  differ = if (!is.null(their_request) && !is.null(our_request)) request_differences(their_request, our_request)
  problem = character(0)
  if (length(theirs) != 1) {
    problem = paste0("AppEEARS's copy of the request ", if (length(theirs)) 'is not unique' else 'is missing', ' in ', record_dir)
  } else if (is.null(their_request)) {
    problem = paste("AppEEARS's copy of the request cannot be read:", theirs)
  } else if (is.null(our_request)) {
    problem = paste("our request file is missing or cannot be read, so AppEEARS's copy cannot be compared with it:", ours)
  } else if (length(differ)) {
    problem = paste0("AppEEARS's copy of the request differs from ours in ", paste(differ, collapse = ', '), ': ', theirs)
  }
  if (length(problem) && deleting)
    stop(format(month, '%Y-%m'), ': ', problem, '; stopped before building or deleting anything')
  if (length(problem)) warning(format(month, '%Y-%m'), ': ', problem, ' (going on: a build with "keep")', call. = FALSE, immediate. = TRUE)

  f = if (!is.null(their_request)) theirs else ours
  r = if (!is.null(their_request)) their_request else our_request
  if (is.null(r)) return(paste('request file: none found or readable; checked against the checksum list only. WARNING', problem))
  days = month_days(month)
  if (!identical(as.numeric(r$dates), as.numeric(range(days))))
    stop(f, ': requested ', paste(r$dates, collapse = ' to '), ', not ', paste(range(days), collapse = ' to '))
  if (!all(LAYERS$file %in% r$layers)) stop(f, ': does not request ', paste(setdiff(LAYERS$file, r$layers), collapse = ', '))
  whose = if (!length(problem)) "AppEEARS's copy, i.e. what it was asked to do; the same dates, layers and area as our request file"
          else if (identical(f, theirs)) paste("AppEEARS's copy, i.e. what it was asked to do. WARNING", problem)
          else paste("our own request file only, i.e. what we meant to ask. WARNING", problem)
  paste('request file:', f, '-', whose, '- dates and layers as expected')
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
  year = as.numeric(format(date, '%Y'))
  days_in_year = ifelse(year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0), 366, 365)  # NOAA: 366 in leap years
  g = 2 * pi / days_in_year * (as.numeric(format(date, '%j')) - 1)
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

# --- the arithmetic, in small functions that tests/test_bluesky_albedo.R checks by hand ------
# Blue-sky albedo: black-sky and white-sky albedo mixed by the diffuse fraction f.
blue_sky = function(bsa, wsa, f) (1 - f) * bsa + f * wsa

# Whether the noon sun is high enough for a version (Inf: no rule, so a missing angle passes).
zenith_ok = function(sza, max_zenith) {
  if (is.infinite(max_zenith)) rep(TRUE, length(sza)) else !is.na(sza) & sza <= max_zenith
}

# One day's contribution to a version's totals, pixel by pixel.
day_totals = function(keep, snow, blue) {
  snowy = keep & snow %in% 1
  free  = keep & snow %in% 0
  cbind(n_all = keep, n_snow = snowy, n_free = free,
        s_all = ifelse(keep, blue, 0), s_snow = ifelse(snowy, blue, 0), s_free = ifelse(free, blue, 0))
}

# Land: a pixel whose land/water class says land (1) on more than half of the days it has one,
# over the whole record (Chris, 2026-10-20); with rule 'land_or_shore', land or shoreline (2)
# together. A blank class means no retrieval that day (checked on the July 2005 download: blank
# class and no albedo coincide on every pixel-day), so blank days are not counted. A tie (exactly
# half) is not land.
is_land = function(land_days, shore_days, read_days, rule = LAND_RULE) {
  yes = switch(rule, land = land_days, land_or_shore = land_days + shore_days, stop('unknown land rule ', rule))
  read_days > 0 & yes > read_days / 2
}

# The climatology of one version for each group (a cell and calendar month), from its totals per
# year. Within a year every qualifying land pixel-day counts once; then each year with data weighs
# the same. The days split into snowy, snow-free and unknown snow status (no snow flag); their
# means and shares use the same weights (1 / days in that year), so
#   share_snowy x snowy + share_snowfree x snowfree + share_unknown x unknown = all
# exactly, as the snow-mixing plan needs. Read this way, the snowy value is the average of each
# year's snowy mean weighted by that year's snow share. A part with share 0 has no mean (NA) and
# contributes nothing.
climatology = function(group, n_all, n_snow, n_free, s_all, s_snow, s_free) {
  w = ifelse(n_all > 0, 1 / n_all, 0)
  n_unknown = n_all - n_snow - n_free
  s_unknown = ifelse(n_unknown > 0, s_all - s_snow - s_free, 0)
  t = rowsum(cbind(years = n_all > 0, all = s_all * w, n_snow = n_snow * w, s_snow = s_snow * w,
                   n_free = n_free * w, s_free = s_free * w, n_unknown = n_unknown * w, s_unknown = s_unknown * w), group)
  per_year = function(x) ifelse(t[, 'years'] > 0, x / t[, 'years'], NA)
  part_mean = function(part) ifelse(t[, paste0('n_', part)] > 0, t[, paste0('s_', part)] / t[, paste0('n_', part)], NA)
  data.frame(group = rownames(t), years = t[, 'years'],
             all = per_year(t[, 'all']),
             snowfree = part_mean('free'), snowy = part_mean('snow'), unknown = part_mean('unknown'),
             share_snowfree = per_year(t[, 'n_free']), share_snowy = per_year(t[, 'n_snow']),
             share_unknown = per_year(t[, 'n_unknown']),
             row.names = NULL)
}

# --- one tile and one month -----------------------------------------------------------------
# Per pixel: every version's totals (VERSION_LAYERS), and the days the land/water class said land
# and the days it had any class. Nothing is filtered for land here; that is decided at the cell
# step. need_zenith = FALSE only for the July 2005 test (see the header).
process_tile_month = function(tile, month, day_dir, out_dir, need_zenith = TRUE) {
  out = file.path(out_dir, sprintf('%s_%s.tif', tile, format(month, '%Y_%m')))
  if (file.exists(out)) {
    if (!identical(build_tag(out), settings_tag(need_zenith))) stop(out, ' was made with other settings; move it away first')
    if (!identical(build_tag(out, 'paleoRF_script_sha256'), SCRIPT_SHA))
      stop(out, ' is stale: it was made by another version of this script (SHA-256 ',
           build_tag(out, 'paleoRF_script_sha256'), ', now ', SCRIPT_SHA, '); move it away first')
    return(out)
  }
  days = month_days(month)
  files = day_files(day_dir, days)
  core = setdiff(LAYERS$name, 'sza')                 # without these a day cannot be used
  if (need_zenith && all(is.na(files[, 'sza']))) stop('no noon solar zenith files in ', day_dir)

  template = tile_template(tile)
  ll = pixel_lonlat(template)
  era5 = lapply(unique(format(c(days, max(days) + 1), '%Y-%m-01')), function(m) era5_month(as.Date(m)))
  era5 = list(lon = era5[[1]]$lon, lat = era5[[1]]$lat, time = do.call(c, lapply(era5, `[[`, 'time')),
              ssrd = do.call(abind_time, lapply(era5, `[[`, 'ssrd')), fdir = do.call(abind_time, lapply(era5, `[[`, 'fdir')))

  totals = matrix(0, ncell(template), length(TILE_LAYERS), dimnames = list(NULL, TILE_LAYERS))
  out_of_range = 0
  with_albedo = 0; no_diffuse = 0                    # pixel-days with an albedo; of those, without a diffuse fraction
  for (d in seq_along(days)) {
    if (anyNA(files[d, core])) next                  # counted in the build record (delivery_summary)
    get  = function(name) read_layer(files[d, name], template, name)
    bsa  = get('bsa')
    wsa  = get('wsa')
    # The product's valid range allows stored values above 1 after scaling; an albedo outside
    # 0-1 is physically impossible, so it is treated as no retrieval and counted.
    bad  = !is.na(bsa) & (bsa < 0 | bsa > 1) | !is.na(wsa) & (wsa < 0 | wsa > 1)
    out_of_range = out_of_range + sum(bad)
    bsa[bad] = NA; wsa[bad] = NA
    qa   = get('qa')
    snow = get('snow')
    # A missing zenith file leaves the zenith unknown: such a day counts only for the version with
    # no low-sun rule. Without need_zenith (July 2005 test) the zenith is taken as 0.
    sza  = if (!need_zenith) rep(0, ncell(template)) else if (is.na(files[d, 'sza'])) rep(NA, ncell(template)) else get('sza')
    land = get('land')
    totals[, 'land_days'] = totals[, 'land_days'] + (land %in% 1)
    totals[, 'shore_days'] = totals[, 'shore_days'] + (land %in% 2)
    totals[, 'read_days'] = totals[, 'read_days'] + !is.na(land)
    f = diffuse_fraction(era5, ll[, 1], ll[, 2], days[d])
    # A retrieval with no diffuse fraction (ERA5 hour or point not found, or no sunlight in that
    # hour) is lost; counted, so that a timing or grid mismatch, which would empty every tile
    # while each still passed its checks as valid zeros, shows up (build_month warns).
    has_albedo = !is.na(bsa) & !is.na(wsa)
    with_albedo = with_albedo + sum(has_albedo); no_diffuse = no_diffuse + sum(has_albedo & is.na(f))
    blue = blue_sky(bsa, wsa, f)
    usable = !is.na(blue) & !is.na(qa) & qa <= 7
    full = usable & qa %in% FULL_QA
    for (i in seq_len(nrow(VERSIONS))) {
      keep = (if (VERSIONS$quality[i] == 'full') full else usable) & zenith_ok(sza, VERSIONS$max_zenith[i])
      cols = paste(VERSIONS$name[i], SUMS, sep = '_')
      totals[, cols] = totals[, cols] + day_totals(keep, snow, blue)
    }
  }
  r = setValues(rast(template, nlyrs = length(TILE_LAYERS)), totals)
  names(r) = TILE_LAYERS
  # The tags travel with the file, so a later build that reuses it can still record how it was made.
  metags(r) = c(paleoRF_build = settings_tag(need_zenith), paleoRF_script_sha256 = SCRIPT_SHA,
                paleoRF_created = format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'),
                paleoRF_out_of_range = as.character(out_of_range),
                paleoRF_with_albedo = as.character(with_albedo), paleoRF_no_diffuse = as.character(no_diffuse))
  publish_tile(r, out, settings_tag(need_zenith), template)
}

# Writes a monthly file under a temporary name, checks it in full (output_problems) and only then
# gives it its real name. A crash cannot leave a partial file under the real name for a rerun to
# reuse, and a file that fails its checks is left as "<name>.tmp.tif" for inspection: the tile
# fails, so its day files are not deleted.
publish_tile = function(r, out, tag, template) {
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp = sub('\\.tif$', '.tmp.tif', out)
  writeRaster(r, tmp, gdal = 'COMPRESS=DEFLATE', datatype = 'FLT4S', overwrite = TRUE)
  problems = output_problems(tmp, tag = tag, template = template)
  if (length(problems)) stop('the new file ', tmp, ' failed its checks (', paste(problems, collapse = '; '), '); left under that name')
  if (!file.rename(tmp, out)) stop('could not rename ', tmp)
  out
}

build_tag = function(file, name = 'paleoRF_build') {
  m = metags(rast(file))
  v = m$value[m$name == name]
  if (length(v)) v else NA_character_
}

# What is wrong with a monthly file, if anything (an empty result means it is usable). It must
# open, have our layers and settings tag, be on the MODIS sinusoidal grid of its tile, and every
# layer must read in full and obey what the reduction guarantees: counts are whole, non-negative
# and at most the days of the month the build uses (max_days); snowy plus snow-free days are at
# most all days; a sum of albedos lies between 0 and its number of days (albedo is in 0-1), and
# so does the rest of the "all" sum once the snowy and snow-free sums are taken out (the days of
# unknown snow status); land and shoreline days are at most the days with a class; and the
# versions nest (a stricter version never keeps more days). A file failing any of this is not
# accepted, so its day files are not deleted.
tile_of = function(file) sub('_.*', '', basename(file))
# The number of days a monthly file can count, from its name (<tile>_<year>_<month>[.tmp].tif):
# the days of that month the build uses (27 in March 2000, which starts on the 5th); 31 otherwise.
days_of_file = function(file) {
  m = regmatches(basename(file), regexec('_(\\d{4})_(\\d{2})(\\.tmp)?\\.tif$', basename(file)))[[1]]
  if (!length(m)) return(31)
  length(month_days(as.Date(sprintf('%s-%s-01', m[2], m[3]))))
}
output_problems = function(file, tag = SETTINGS_TAG, template = tile_template(tile_of(file)), max_days = days_of_file(file),
                           script_sha = SCRIPT_SHA) {
  r = tryCatch(rast(file), error = function(e) NULL)
  if (is.null(r)) return('cannot be opened')
  if (!identical(names(r), TILE_LAYERS)) return('does not have the expected layers')
  if (!identical(build_tag(file), tag)) return('was made with other settings')
  if (!identical(build_tag(file, 'paleoRF_script_sha256'), script_sha)) return('was made by another version of the build script')
  if (!grepl('Sinusoidal', crs(r)) || !grepl('6371007.181', crs(r))) return('is not on the MODIS sinusoidal projection')
  if (nrow(r) != nrow(template) || ncol(r) != ncol(template) ||
      max(abs(as.vector(ext(r)) - as.vector(ext(template)))) > 0.01) return('is not on its tile grid')
  tryCatch({
    get = function(name) values(r[[name]], mat = FALSE)
    whole = function(x) all(is.finite(x) & x >= 0 & x == round(x) & x <= max_days)
    within = function(s, n) all(is.finite(s) & s >= -1e-4 & s <= n * (1 + 1e-6) + 1e-4)  # float32 sums
    problems = character(0)
    n_all = list()
    for (v in VERSIONS$name) {
      x = setNames(lapply(paste(v, SUMS, sep = '_'), get), SUMS)
      if (!whole(x$n_all) || !whole(x$n_snow) || !whole(x$n_free))
        problems = c(problems, paste(v, 'counts not whole, non-negative and at most', max_days, 'days'))
      if (any(x$n_snow + x$n_free > x$n_all)) problems = c(problems, paste(v, 'snowy plus snow-free days exceed all days'))
      if (!within(x$s_all, x$n_all) || !within(x$s_snow, x$n_snow) || !within(x$s_free, x$n_free))
        problems = c(problems, paste(v, 'albedo sums outside 0 to their number of days'))
      if (!within(x$s_all - x$s_snow - x$s_free, x$n_all - x$n_snow - x$n_free))
        problems = c(problems, paste(v, 'unknown-snow sum outside 0 to its number of days'))
      n_all[[v]] = x$n_all
    }
    land = get('land_days'); shore = get('shore_days'); read = get('read_days')
    if (!whole(land) || !whole(shore) || !whole(read))
      problems = c(problems, paste('land, shoreline or class days not whole, non-negative and at most', max_days, 'days'))
    if (any(land + shore > read)) problems = c(problems, 'land plus shoreline days exceed the days with a class')
    if (any(n_all$full_70 > n_all$full_75) || any(n_all$full_75 > n_all$full_nocut) || any(n_all$full_75 > n_all$any_75))
      problems = c(problems, 'the versions do not nest')
    problems
  }, error = function(e) paste('cannot be read in full:', conditionMessage(e)))
}
valid_output = function(file, ...) !length(output_problems(file, ...))

# Every tile of one month, WORKERS at a time. Stops unless every tile's file was written and is
# valid (new files are checked before they are named, reused ones here): a worker that dies (out of memory, killed) returns no message at all, so success is
# checked against the expected files rather than by looking for failure messages. Rerunning
# retries the missing tiles.
build_month = function(tiles, month, day_dir, out_dir, need_zenith = TRUE) {
  require_script_sha()
  expected = file.path(out_dir, sprintf('%s_%s.tif', tiles, format(month, '%Y_%m')))
  reused = file.exists(expected)
  done = parallel::mclapply(tiles, function(tile)
    tryCatch(process_tile_month(tile, month, day_dir, out_dir, need_zenith),
             error = function(e) paste('FAILED', tile, conditionMessage(e))),
    mc.cores = WORKERS, mc.preschedule = FALSE)
  # A new file was checked in full before it got its name (publish_tile); a reused one is checked here.
  problems = lapply(seq_along(tiles), function(i) {
    if (!identical(done[[i]], expected[i])) return(if (is.character(done[[i]])) done[[i]] else 'the worker died')
    if (reused[i]) output_problems(expected[i], tag = settings_tag(need_zenith)) else character(0)
  })
  ok = lengths(problems) == 0
  for (i in which(!ok)) cat('FAILED', tiles[i], paste(problems[[i]], collapse = '; '), '\n')
  if (!all(ok)) stop(sum(!ok), ' tiles of ', format(month, '%Y-%m'), ' failed; the day files are kept')
  # One line per tile for the build record: made now or reused, when, albedo values dropped, and
  # retrievals lost for want of a diffuse fraction; a warning (not a stop) where that share is large.
  with_albedo = as.numeric(vapply(expected, build_tag, '', name = 'paleoRF_with_albedo'))
  no_diffuse = as.numeric(vapply(expected, build_tag, '', name = 'paleoRF_no_diffuse'))
  cat(no_diffuse_warnings(tiles, no_diffuse, with_albedo), sep = '\n')
  sprintf('tile %s: %s, made %s, albedo values outside 0-1 dropped: %s, retrievals without a diffuse fraction: %s of %s', tiles,
          ifelse(reused, 'reused from an earlier run', 'made in this run'),
          vapply(expected, build_tag, '', name = 'paleoRF_created'), vapply(expected, build_tag, '', name = 'paleoRF_out_of_range'),
          no_diffuse, with_albedo)
}

# Tiles where more than NO_DIFFUSE_WARN of the retrievals found no diffuse fraction. A few are
# expected near the edge of the polar night (no sunlight in the noon hour); many would mean the
# ERA5 hours or grid do not match the MODIS days and pixels.
NO_DIFFUSE_WARN = 0.001
no_diffuse_warnings = function(tiles, no_diffuse, with_albedo) {
  share = ifelse(with_albedo > 0, no_diffuse / with_albedo, 0)
  big = which(is.na(share) | share > NO_DIFFUSE_WARN)
  sprintf('WARNING tile %s: %s of %s retrievals (%.2f%%) had no diffuse fraction and were lost', tiles[big],
          no_diffuse[big], with_albedo[big], 100 * share[big])
}

# What made a month's files, added to RECORDS_DIR/<month>/build.txt, which stays after its day
# files are deleted: the code (git commit and the script's own SHA-256), the settings, the files checked
# and the ERA5 files used.
write_build_record = function(month, record_dir, n_files, tiles, delivery, tile_lines) {
  script = SCRIPT_FILE
  commit = tryCatch(system2('git', c('-C', dirname(script), 'rev-parse', 'HEAD'), stdout = TRUE, stderr = FALSE),
                    error = function(e) 'unknown', warning = function(w) 'unknown')
  era5 = file.path(ERA5_DIR, sprintf('era5_ssrd_fdir_%s.nc', format(c(month, seq(month, by = 'month', length.out = 2)[2]), '%Y_%m')))
  era5 = era5[file.exists(era5)]
  # Appended, never overwritten: a month built again (e.g. a kept test month after a fix) adds a
  # section, so the record of the first build survives.
  con = file(file.path(record_dir, 'build.txt'), open = 'a'); on.exit(close(con))
  writeLines(c('', paste('=== built:', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
               paste('code:', script, 'commit', commit[1], 'sha256', SCRIPT_SHA),
               paste('settings:', SETTINGS_TAG),
               paste('day files checked against the SHA-256 list:', n_files),
               delivery,
               paste('tiles:', length(tiles), '-', paste(tiles, collapse = ' ')),
               tile_lines,
               paste('ERA5:', basename(era5), 'sha256', vapply(era5, function(f) as.character(openssl::sha256(file(f))), ''))),
             con)
}

# Builds collected months (all of them, or those named) and deletes their day files, unless "keep"
# is asked for. A month built with "keep" gets a KEEP file in its day folder and keeps its day
# files for good: a later build without names skips it, and a build that names it rebuilds it but
# still keeps its files. Only removing its KEEP file by hand lets a build delete them. The KEEP file
# is written before the month is built (Chris, 2026-10-21), so a build that fails part-way still
# leaves the day files protected.
KEEP_MARKER = 'KEEP'
build_collected = function(tiles, monthly_dir, named = character(0), keep = FALSE) {
  require_script_sha()                               # before the long checksum checks
  day_dirs = list.files(APPEEARS_DIR, pattern = '^\\d{4}-\\d{2}$', full.names = TRUE)
  if (length(named)) {
    if (!all(named %in% basename(day_dirs))) stop('not collected: ', paste(setdiff(named, basename(day_dirs)), collapse = ' '))
    day_dirs = day_dirs[basename(day_dirs) %in% named]
  }
  cat(length(tiles), 'tiles:', paste(tiles, collapse = ' '), '\n')
  for (day_dir in day_dirs) {
    kept_before = file.exists(file.path(day_dir, KEEP_MARKER))
    if (kept_before && !length(named)) {
      cat(basename(day_dir), ': marked "keep" by an earlier build; skipped (name it to build it again)\n'); next
    }
    keep_this = keep || kept_before
    if (keep_this) writeLines('Marked by a build with "keep": this folder\'s day files are kept. Remove this file to let a build delete them.',
                              file.path(day_dir, KEEP_MARKER))
    started = Sys.time()
    record_dir = file.path(RECORDS_DIR, basename(day_dir))
    list_file = list.files(record_dir, pattern = '-sha256sums\\.txt$', full.names = TRUE)
    if (length(list_file) != 1) stop(basename(day_dir), ': expected one checksum list in ', record_dir)
    n_files = check_checksums(list_file, day_dir)   # what AppEEARS listed is there and intact
    month = as.Date(paste0(basename(day_dir), '-01'))
    delivery = c(check_request(month, record_dir, deleting = !keep_this),  # and it is what we asked for, every layer
                 delivery_summary(delivery_table(day_dir, month_days(month), LAYERS$name), LAYERS$name))
    cat(basename(day_dir), ':', delivery, sep = '\n  ')
    tile_lines = build_month(tiles, month, day_dir, monthly_dir)  # stops, keeping the day files, unless all tiles are valid
    write_build_record(month, record_dir, n_files, tiles,
                       c(delivery, if (keep_this) 'day files: kept (built with "keep"; KEEP file in the day folder)'
                                   else 'day files: deleted after this record'), tile_lines)
    if (!keep_this) unlink(day_dir, recursive = TRUE)   # only now are the day files no longer needed
    cat(sprintf('%s: built in %.1f min; its day files %s\n', basename(day_dir),
                as.numeric(difftime(Sys.time(), started, units = 'mins')), if (keep_this) 'kept' else 'deleted'))
  }
}

abind_time = function(...) {                          # join [lon, lat, time] arrays along time
  a = list(...); d = dim(a[[1]])
  array(unlist(a), dim = c(d[1], d[2], sum(vapply(a, function(x) dim(x)[3], 1))))
}

# --- land and cells ---------------------------------------------------------------------------
# The monthly files of a tile, with their year and calendar month.
tile_files = function(tile, monthly_dir) {
  files = list.files(monthly_dir, pattern = paste0('^', tile, '_\\d{4}_\\d{2}\\.tif$'), full.names = TRUE)
  data.frame(file = files, year = as.integer(sub('.*_(\\d{4})_\\d{2}\\.tif$', '\\1', files)),
             month = as.integer(sub('.*_(\\d{2})\\.tif$', '\\1', files)))
}

# Pass 1: a tile's land pixels, from all its monthly files together (see is_land).
tile_land = function(files) {
  land_days = 0; shore_days = 0; read_days = 0
  for (f in files) {
    v = values(rast(f)[[c('land_days', 'shore_days', 'read_days')]])
    land_days = land_days + v[, 1]; shore_days = shore_days + v[, 2]; read_days = read_days + v[, 3]
  }
  is_land(land_days, shore_days, read_days)
}

# Pass 2: for each cell, year and calendar month, the version totals summed over the cell's land
# pixels in this tile, its number of land pixels, and per version the number of land pixels with
# at least one qualifying day. Sums, because a cell can span several tiles.
tile_cell_totals = function(tile, cell_keys, monthly_dir, months, tag, template = tile_template(tile)) {
  tf = tile_files(tile, monthly_dir)
  # Exactly the months asked for, each made with the expected settings; anything else stops.
  have = sprintf('%d-%02d', tf$year, tf$month); want = format(months, '%Y-%m')
  if (!setequal(have, want) || length(have) != length(want))
    stop(tile, ': monthly files for ', length(have), ' months, expected ', length(want),
         if (length(setdiff(want, have))) paste(', missing e.g.', setdiff(want, have)[1]),
         if (length(setdiff(have, want))) paste(', unexpected e.g.', setdiff(have, want)[1]))
  wrong = tf$file[!vapply(tf$file, function(f) identical(build_tag(f), tag), TRUE)]   # a missing tag counts as wrong
  if (length(wrong)) stop(length(wrong), ' monthly files were made with other settings, e.g. ', wrong[1])
  # And made by this version of the script or an older one listed in ACCEPTED_OLDER_SHAS (Chris,
  # 2026-10-21): a file with any other script SHA-256, or with none (made before the files recorded
  # it), stops the cell step, so the cell table never mixes outputs of code not known to agree.
  accepted = c(SCRIPT_SHA, ACCEPTED_OLDER_SHAS)
  stale = tf$file[!vapply(tf$file, function(f) build_tag(f, 'paleoRF_script_sha256') %in% accepted, TRUE)]
  if (length(stale)) {
    stale_sha = build_tag(stale[1], 'paleoRF_script_sha256')
    stop(length(stale), ' monthly files were made by another version of this script, not one listed in ACCEPTED_OLDER_SHAS, ',
         'or do not say which, e.g. ', stale[1], ' (SHA-256 ', if (is.na(stale_sha)) 'none recorded' else stale_sha, ', now ', SCRIPT_SHA, ')')
  }
  ll = pixel_lonlat(template)
  cell = paste(floor(ll[, 1]) + 0.5, floor(ll[, 2]) + 0.5)
  keep = tile_land(tf$file) & cell %in% cell_keys
  if (!any(keep)) return(data.frame())               # a tile with no land in the cells: empty, not missing
  out = vector('list', nrow(tf))
  for (i in seq_len(nrow(tf))) {
    v = values(rast(tf$file[i])[[VERSION_LAYERS]])[keep, , drop = FALSE]
    with_data = v[, paste0(VERSIONS$name, '_n_all'), drop = FALSE] > 0
    colnames(with_data) = paste0(VERSIONS$name, '_pixels')
    t = rowsum(cbind(land_pixels = 1, v, with_data), cell[keep])
    out[[i]] = data.frame(cell = rownames(t), year = tf$year[i], month = tf$month[i], t, check.names = FALSE)
  }
  do.call(rbind, out)
}

# Every tile's totals must have come back as a table (an empty one for a tile with no land in the
# cells). A worker that dies returns nothing (NULL), and an error returns a message: either stops,
# since a lost tile would silently drop cells, or part of a cell spanning two tiles.
combine_parts = function(parts, tiles) {
  bad = which(!vapply(parts, is.data.frame, TRUE))
  if (length(bad)) stop('cell totals failed for ', length(bad), ' tiles: ', paste(vapply(bad, function(i)
    if (is.character(parts[[i]])) parts[[i]][1] else paste(tiles[i], 'worker died'), ''), collapse = '; '))
  parts = parts[vapply(parts, nrow, 1) > 0]
  if (!length(parts)) stop('no land pixels in any cell')
  do.call(rbind, parts)
}

# Which versions of this script made the monthly files, for the note beside the cell table: per
# script SHA-256, the number of files and whether it is this version or an accepted older one
# (ACCEPTED_OLDER_SHAS). The files of any version other than this one are named, so each tile can be
# traced to the code that made it. A file that records no SHA-256 is counted as such, not left out.
script_versions = function(files) {
  sha = vapply(files, build_tag, '', name = 'paleoRF_script_sha256', USE.NAMES = FALSE)
  sha[is.na(sha)] = 'none recorded'
  vapply(unique(sha), function(s) {
    whose = if (s == SCRIPT_SHA) 'this version' else if (s %in% ACCEPTED_OLDER_SHAS) 'an accepted older version' else 'not accepted'
    line = sprintf('monthly files by build script sha256: %s - %d files (%s)', s, sum(sha == s), whose)
    if (s == SCRIPT_SHA) line else paste0(line, ': ', paste(basename(files[sha == s]), collapse = ' '))
  }, '', USE.NAMES = FALSE)
}

# The cell table: per cell and calendar month, for each version, the climatology (see
# climatology: all days, and snowy, snow-free and unknown-snow means with their shares), the years
# with data, the coverage (mean over those years of the share of the
# cell's land pixels with at least one qualifying day) and the qualifying days per land pixel in
# a year with data.
cell_table = function(tiles, cells, monthly_dir, months, tag = SETTINGS_TAG, template_of = tile_template) {
  require_script_sha()
  parts = parallel::mclapply(tiles, function(tile) tryCatch(tile_cell_totals(tile, paste(cells$x, cells$y), monthly_dir, months, tag,
                                                                             template_of(tile)),
                                                            error = function(e) paste('FAILED', tile, conditionMessage(e))),
                             mc.cores = WORKERS, mc.preschedule = FALSE)
  parts = combine_parts(parts, tiles)
  num = setdiff(names(parts), c('cell', 'year', 'month'))
  totals = aggregate(parts[num], by = parts[c('cell', 'year', 'month')], FUN = sum)
  key = paste(totals$cell, totals$month)
  out = data.frame(group = sort(unique(key)))
  out$land_pixels = tapply(totals$land_pixels, key, max)[out$group]
  for (v in VERSIONS$name) {
    col = function(s) totals[[paste(v, s, sep = '_')]]
    cl = climatology(key, col('n_all'), col('n_snow'), col('n_free'), col('s_all'), col('s_snow'), col('s_free'))
    with_data = col('n_all') > 0
    coverage = rowsum(ifelse(with_data, col('pixels') / totals$land_pixels, 0), key)[, 1]
    days = rowsum(ifelse(with_data, col('n_all') / totals$land_pixels, 0), key)[, 1]
    stopifnot(identical(cl$group, out$group), identical(names(coverage), out$group))
    out[[v]]                         = round(cl$all, 6)
    out[[paste0(v, '_snowfree')]]       = round(cl$snowfree, 6)
    out[[paste0(v, '_snowy')]]          = round(cl$snowy, 6)
    out[[paste0(v, '_unknown')]]        = round(cl$unknown, 6)
    out[[paste0(v, '_share_snowfree')]] = round(cl$share_snowfree, 6)
    out[[paste0(v, '_share_snowy')]]    = round(cl$share_snowy, 6)
    out[[paste0(v, '_share_unknown')]]  = round(cl$share_unknown, 6)
    out[[paste0(v, '_years')]]          = cl$years
    out[[paste0(v, '_coverage')]]       = round(ifelse(cl$years > 0, coverage / cl$years, NA), 4)
    out[[paste0(v, '_days')]]           = round(ifelse(cl$years > 0, days / cl$years, NA), 2)
  }
  parts = do.call(rbind, strsplit(out$group, ' '))
  out = data.frame(x = as.numeric(parts[, 1]), y = as.numeric(parts[, 2]), month = MONTHS_ABB[as.integer(parts[, 3])],
                   out[setdiff(names(out), 'group')], check.names = FALSE)
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
      if (built(m) || any(dir.exists(file.path(APPEEARS_DIR, paste0(format(m, '%Y-%m'), c('', '.partial')))))) {
        cat(format(m, '%Y-%m'), ': already collected or built\n'); next
      }
      cat(write_request(m, ring), '\n')
    }

  } else if (step == 'collect') {
    collect_downloads()

  } else if (step == 'build') {
    # "build" builds every collected month and then deletes its day files; months may be named
    # ("build 2005-03 2005-12"), and "keep" keeps the day files (Chris, 2026-10-20: the test
    # months keep theirs, so they can be rebuilt after a later fix and checked against the raw files).
    keep = 'keep' %in% args[-1]
    named = grep('^\\d{4}-\\d{2}$', args[-1], value = TRUE)
    if (length(setdiff(args[-1], c(named, 'keep')))) stop('build takes months like 2005-03 and/or "keep"')
    build_collected(tiles, monthly_dir, named, keep)

  } else if (step == 'cells') {
    missing = months[!vapply(seq_along(months), function(i) built(months[i]), TRUE)]
    if (length(missing)) stop(length(missing), ' months not built yet, e.g. ', format(missing[1], '%Y-%m'))
    table = cell_table(tiles, cells, monthly_dir, months)
    write.csv(table, OUT_FILE, row.names = FALSE)
    cat(OUT_FILE, ':', nrow(table), 'cell-months;', length(unique(paste(table$x, table$y))), 'cells\n')
    # Beside the table: the land rule it used, and which versions of this script made the monthly
    # files (script_versions).
    files = list.files(monthly_dir, pattern = '^h\\d{2}v\\d{2}_\\d{4}_\\d{2}\\.tif$', full.names = TRUE)
    writeLines(c(paste('cell table:', OUT_FILE, 'written', format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')),
                 paste('land rule:', LAND_RULE), paste('settings:', SETTINGS_TAG),
                 paste('cell step script sha256:', SCRIPT_SHA),
                 script_versions(files)),
               sub('\\.csv$', '_provenance.txt', OUT_FILE))

  } else if (step == 'test') {
    require_script_sha()                               # before hashing the July download (about 11 GB)
    # July 2005 into a folder of its own: one tile ("test h11v04"), or every tile and the cell
    # table for that one month ("test all").
    test_tiles = if (identical(args[2], 'all')) tiles else args[-1]
    if (!length(test_tiles) || !all(test_tiles %in% tiles)) stop('test needs "all" or tiles of the study area, e.g. h11v04')
    out_dir = file.path(WORK_DIR, 'test_appeears_v4')
    test_dir = file.path(WORK_DIR, 'appeears_full_2005-07')
    started = Sys.time()
    cat(check_checksums(list.files(test_dir, pattern = '-sha256sums\\.txt$', full.names = TRUE), test_dir),
        'day files match their SHA-256 list\n')
    needed = setdiff(LAYERS$name, 'sza')             # requested before the zenith layer was added
    cat(delivery_summary(delivery_table(test_dir, month_days(as.Date('2005-07-01')), needed), needed), sep = '\n')
    build_month(test_tiles, as.Date('2005-07-01'), test_dir, out_dir, need_zenith = FALSE)
    cat(sprintf('%d tiles in %.1f min\n', length(test_tiles), as.numeric(difftime(Sys.time(), started, units = 'mins'))))
    if (identical(args[2], 'all')) {
      table = cell_table(tiles, cells, out_dir, as.Date('2005-07-01'), settings_tag(FALSE))
      write.csv(table, file.path(out_dir, 'cells_2005_07.csv'), row.names = FALSE)
      cat(file.path(out_dir, 'cells_2005_07.csv'), ':', nrow(table), 'cells\n')
    }

  } else {
    cat('usage: Rscript tools/prepare_bluesky_albedo.R requests|collect|build|cells|test ...',
        '(see the top of this file)\n')
    quit(status = 2)
  }
}
