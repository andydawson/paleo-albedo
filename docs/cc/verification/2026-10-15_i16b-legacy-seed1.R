# I16b, legacy configuration: scripts 5 and 6 with seed 1 must reproduce the seed-1 anchor,
# reading the new prediction-fit file seeded from the frozen model 8 (what script 5 read before).
months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
list(
  name    = 'i16b-legacy-seed1',
  commit  = 'i16b-calibration',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_table', 'calibration_ladder', 'calibration_spatial',
              'calibration_spatial_elev_cover', 'aic_table', 'lct_paleo'),
  copy    = lapply(months, function(m) c(sprintf('output/calibration/calibration_mod8_interp_%s_bluesky.RDS', m),
                                         sprintf('output/calibration/calibration_prediction_fit_%s_bluesky.RDS', m))),
  scripts = c('scripts/5_calibration_eval.R', 'scripts/6_prediction_model.R'),
  anchors = 'tests/anchors/interp-seed1-2026-10-14',
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
