# Environment check after adding GDAL HDF4/HDF5/netCDF drivers and ecmwfr (2026-10-15):
# script 8 under the legacy configuration must still match its regression anchors.
list(
  name    = 'env-drivers-script8',
  commit  = 'origin/feature/single-config',
  config  = 'configs/legacy_config.R',
  seed    = c('albedo_differences'),
  scripts = 'scripts/8_radiative.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/env-drivers-script8')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-23'))
