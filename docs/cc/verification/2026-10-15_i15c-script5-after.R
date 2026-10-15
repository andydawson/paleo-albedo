# I15c script 5 (after): script 5 under the legacy configuration, seed 1, from the frozen fits.
months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
list(
  name    = 'i15c-script5-after',
  commit  = 'i15c-script5',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_table', 'calibration_ladder', 'calibration_spatial', 'calibration_spatial_elev_cover'),
  copy    = lapply(months, function(m) c(sprintf('output/calibration/calibration_mod8_interp_%s_bluesky.RDS', m),
                                         sprintf('output/calibration/calibration_prediction_fit_%s_bluesky.RDS', m))),
  scripts = 'scripts/5_calibration_eval.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-seed1-2026-10-14', '.rundirs/i15c-script5-after')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
