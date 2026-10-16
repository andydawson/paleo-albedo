# Script 10 after review (band edges, exact columns, labels, unit test): script 9's four tables identical to their anchor (--all), script 10 runs (it stops unless its parts add up to script 9), with the config, path and breakdown tests; 8 threads as the regression contract requires.
list(
  name    = 'forcing-breakdown-2',
  commit  = 'forcing-breakdown',
  config  = 'configs/legacy_config.R',
  seed    = c('forcing', 'coarse_differences'),
  scripts = c('scripts/9_forcing_barplot.R', 'scripts/10_forcing_breakdown.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-forcing-2026-10-08', '.rundirs/forcing-breakdown-2', '--all'),
                 'tests/test_paths.R', 'tests/test_config.R', 'tests/test_forcing_breakdown.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-23'))
