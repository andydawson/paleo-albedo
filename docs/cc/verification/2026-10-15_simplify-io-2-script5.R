# Redundant reads/writes removed (after review): script 5 under the legacy configuration from the frozen fits.
months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
list(
  name    = 'simplify-io-2-script5',
  commit  = 'simplify-redundant-io',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_table', 'calibration_ladder', 'calibration_spatial', 'calibration_spatial_elev_cover'),
  copy    = lapply(months, function(m) c(sprintf('output/calibration/calibration_mod8_interp_%s_bluesky.RDS', m),
                                         sprintf('output/calibration/calibration_prediction_fit_%s_bluesky.RDS', m))),
  scripts = 'scripts/5_calibration_eval.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-seed1-2026-10-14', '.rundirs/simplify-io-2-script5')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '0-7'))
