# Redundant reads removed, after merging the trunk (centre-elevation tool): script 1 (changed by the
# trunk) and script 8 (changed by this branch) under the legacy configuration, with the tests.
list(
  name    = 'simplify-io-merged',
  commit  = 'simplify-redundant-io',
  config  = 'configs/legacy_config.R',
  seed    = c('albedo_differences'),
  scripts = c('scripts/1_veg_lct_prep.R', 'scripts/8_radiative.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-allmonths-2026-09-20', '.rundirs/simplify-io-merged'),
                 c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/simplify-io-merged'),
                 'tests/test_paths.R', 'tests/test_config.R', 'tests/test_tools.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '4', CORES = '16-19'))
