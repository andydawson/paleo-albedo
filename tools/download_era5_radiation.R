# download_era5_radiation.R: the ERA5 surface radiation needed for the diffuse fraction of
# the blue-sky albedo (tools/prepare_bluesky_albedo.R). Run once, from the repository root:
#
#   Rscript tools/download_era5_radiation.R
#
# What it fetches: ERA5 hourly data on single levels (Hersbach et al. 2020, QJRMS 146:1999,
# doi:10.1002/qj.3803; dataset doi:10.24381/cds.adbb2d47), two variables, both accumulated
# over the hour ending at the time stamp, J m-2:
#   surface_solar_radiation_downwards           (ssrd, all-sky global shortwave)
#   total_sky_direct_solar_radiation_at_surface  (fdir, all-sky direct shortwave)
# so the all-sky diffuse fraction is f = (ssrd - fdir) / ssrd.
# Only the UTC hours that can contain local solar noon over the study area are needed:
# noon falls between about 15:30 and 23:40 UTC from 52 W to 172 W, and an ERA5 value stamped
# T covers the hour before T, so hours 16:00 to 23:00 and 00:00 (the next day) suffice.
# One file per month, data/era5/era5_ssrd_fdir_YYYY_MM.nc (not committed), 2000-02 to
# 2010-01 (one month each side of the 2000-2009 albedo period, for days that need the
# 00:00 value of the next month).
#
# Needs a Copernicus Climate Data Store account: the key in ~/.cdsapirc ("key: ..." line),
# and the dataset's Terms of Use accepted on its Download page. The key is read from that
# file into this R session only; it is never written anywhere else.
# Already downloaded months are skipped, so the script can be rerun after an interruption.

library(ecmwfr)

OUT_DIR = 'data/era5'
MONTHS  = seq(as.Date('2000-02-01'), as.Date('2010-01-01'), by = 'month')
AREA    = c(80, -173, 16, -51)       # north, west, south, east, degrees: the 1-degree cells plus a margin
HOURS   = c('00:00', sprintf('%02d:00', 16:23))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# The key, from ~/.cdsapirc, held for this session only (keyring's "env" backend keeps it in
# an environment variable of this R process instead of a system keyring, which a headless
# machine does not have).
rc = readLines('~/.cdsapirc')
key = trimws(sub('^key:', '', grep('^key:', rc, value = TRUE)))
if (length(key) != 1 || !nzchar(key)) stop('no "key:" line in ~/.cdsapirc')
options(keyring_backend = 'env')
wf_set_key(key = key)

for (m in seq_along(MONTHS)) {
  month = MONTHS[m]
  target = sprintf('era5_ssrd_fdir_%s.nc', format(month, '%Y_%m'))
  if (file.exists(file.path(OUT_DIR, target))) next
  days = format(seq(month, seq(month, by = 'month', length.out = 2)[2] - 1, by = 'day'), '%d')
  request = list(dataset_short_name = 'reanalysis-era5-single-levels',
                 product_type = 'reanalysis',
                 variable = c('surface_solar_radiation_downwards', 'total_sky_direct_solar_radiation_at_surface'),
                 year = format(month, '%Y'), month = format(month, '%m'), day = days, time = HOURS,
                 area = AREA, data_format = 'netcdf', download_format = 'unarchived',
                 target = target)
  cat(format(Sys.time(), '%H:%M'), 'requesting', target, '\n')
  wf_request(request, path = OUT_DIR, transfer = TRUE, time_out = 6 * 3600)
}

# A checksum list of everything fetched, for the provenance record.
files = sort(list.files(OUT_DIR, pattern = '^era5_ssrd_fdir_.*\\.nc$', full.names = TRUE))
writeLines(paste(tools::md5sum(files), basename(files)), file.path(OUT_DIR, 'MD5SUMS.txt'))
cat(length(files), 'monthly files in', OUT_DIR, '\n')
