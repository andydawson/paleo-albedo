# Script 10 after merging the AIC-only ladder change from the trunk: the same checks as forcing-breakdown-2 on the combined code.
list(
  name    = 'forcing-breakdown-3',
  commit  = 'forcing-breakdown',
  config  = 'configs/legacy_config.R',
  seed    = c('forcing', 'coarse_differences'),
  scripts = c('scripts/9_forcing_barplot.R', 'scripts/10_forcing_breakdown.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-forcing-2026-10-08', '.rundirs/forcing-breakdown-3', '--all'),
                 'tests/test_paths.R', 'tests/test_config.R', 'tests/test_forcing_breakdown.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-23'))
