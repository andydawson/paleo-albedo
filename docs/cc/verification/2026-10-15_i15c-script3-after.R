# I15c script 3 (after): script 3 (diagnostic plots, no anchor) under the legacy configuration.
list(
  name    = 'i15c-script3-after',
  commit  = 'i15c-script3',
  config  = 'configs/legacy_config.R',
  seed    = c('calibration_table'),
  scripts = 'scripts/3_plot_cal_lct_albedo.R',
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
