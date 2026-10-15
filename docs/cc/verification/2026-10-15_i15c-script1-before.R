# I15c script 1 (before): script 1 in diagnostic mode under the legacy configuration.
list(
  name     = 'i15c-script1-before',
  commit   = 'origin/feature/single-config',
  config   = 'configs/legacy_config.R',
  settings = list(run = list(diagnostic = TRUE)),
  scripts  = 'scripts/1_veg_lct_prep.R',
  checks   = list(c('tools/check_anchors.R', 'tests/anchors/interp-allmonths-2026-09-20', '.rundirs/i15c-script1-before')),
  rscript  = 'tools/rscript.sh',
  env      = c(THREADS = '8', CORES = '8-15'))
