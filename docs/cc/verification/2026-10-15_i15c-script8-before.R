# I15c script 8 (before): script 8 under the legacy configuration from the frozen albedo differences.
list(
  name    = 'i15c-script8-before',
  commit  = 'origin/feature/single-config',
  config  = 'configs/legacy_config.R',
  seed    = c('albedo_differences'),
  scripts = 'scripts/8_radiative.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/i15c-script8-before')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
