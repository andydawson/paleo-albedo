# I15c script 6: script 6 under the legacy configuration, seed 1, from the frozen selected
# models; the draws and summaries must match the seed-1 anchor, the point predictions the
# all-months anchor.
list(
  name    = 'i15c-script6-after',
  commit  = 'i15c-script6',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_selected', 'lct_paleo'),
  scripts = 'scripts/6_prediction_model.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-seed1-2026-10-14', '.rundirs/i15c-script6-after'),
                 c('tools/check_anchors.R', 'tests/anchors/interp-allmonths-2026-09-20', '.rundirs/i15c-script6-after')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-23'))
