# I16b with the verification tool merged in: scripts 5 and 6 under the legacy configuration
# with seed 1 must reproduce the seed-1 anchor (prediction fits seeded from the frozen model 8),
# and every test file must pass.
months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
list(
  name    = 'i16b-merged-legacy-seed1',
  commit  = 'i16b-calibration',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_table', 'calibration_ladder', 'calibration_spatial',
              'calibration_spatial_elev_cover', 'aic_table', 'lct_paleo'),
  copy    = lapply(months, function(m) c(sprintf('output/calibration/calibration_mod8_interp_%s_bluesky.RDS', m),
                                         sprintf('output/calibration/calibration_prediction_fit_%s_bluesky.RDS', m))),
  scripts = c('scripts/5_calibration_eval.R', 'scripts/6_prediction_model.R'),
  anchors = 'tests/anchors/interp-seed1-2026-10-14',
  checks  = list('tests/test_calendar.R', 'tests/test_check_merge_evidence.R', 'tests/test_config.R', 'tests/test_differences.R', 'tests/test_paths.R', 'tests/test_preparation_ice.R', 'tests/test_preparation.R', 'tests/test_random.R', 'tests/test_run_manifest.R', 'tests/test_tools.R', 'tests/test_verify_change.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
