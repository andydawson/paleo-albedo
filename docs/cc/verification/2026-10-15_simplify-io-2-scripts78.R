# Redundant reads removed (after review): scripts 7, 7a and 8 in sequence under the legacy
# configuration, seeded with the prediction summary only (not the point predictions or anything
# 8 needs from 7a, which 7a must produce here); outputs against the tail anchor.
list(
  name    = 'simplify-io-2-scripts78',
  commit  = 'simplify-redundant-io',
  config  = 'configs/legacy_config.R',
  seed    = c('prediction_summary'),
  scripts = c('scripts/7_plot_preds.R', 'scripts/7a_alb_diff_full.R', 'scripts/8_radiative.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/simplify-io-2-scripts78', '--all')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
