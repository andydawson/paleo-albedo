# Redundant reads removed: script 7 under the legacy configuration, seeded with the prediction
# summary only (not the point predictions it no longer reads, which proves it does not need them).
list(
  name    = 'simplify-io-script7',
  commit  = 'simplify-redundant-io',
  config  = 'configs/legacy_config.R',
  seed    = c('prediction_summary'),
  scripts = 'scripts/7_plot_preds.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/simplify-io-script7')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
