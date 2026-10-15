# I15c scripts 7 and 7a (after): both under the legacy configuration from the frozen predictions;
# the tail anchor must match (ice tables, coarse and full albedo differences), and the
# figures are compared with the other version's.
list(
  name    = 'i15c-scripts7-after',
  commit  = 'i15c-scripts7',
  config  = 'configs/legacy_config.R',
  seed    = c('prediction_point', 'prediction_summary'),
  scripts = c('scripts/7_plot_preds.R', 'scripts/7a_alb_diff_full.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/i15c-scripts7-after')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-23'))
