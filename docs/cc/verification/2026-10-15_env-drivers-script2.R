# Environment check after adding GDAL HDF4/HDF5/netCDF drivers and ecmwfr (2026-10-15):
# script 2 under the legacy configuration must still match its regression anchors.
list(
  name    = 'env-drivers-script2',
  commit  = 'origin/feature/single-config',
  config  = 'configs/legacy_config.R',
  seed    = c('lct_modern'),
  scripts = 'scripts/2_calibration_lct_bluesky.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-allmonths-2026-09-20', '.rundirs/env-drivers-script2')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
