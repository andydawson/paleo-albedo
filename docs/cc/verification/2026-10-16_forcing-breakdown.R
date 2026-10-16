# Script 9's period steps moved to R/forcing_periods.R, and the new script 10: under the legacy
# configuration from script 8's frozen forcing table, script 9's four tables must be produced
# and identical to their anchor (--all), and script 10 must run (it stops itself unless its
# parts add up to script 9's totals); with the config and path tests.
list(
  name    = 'forcing-breakdown',
  commit  = 'forcing-breakdown',
  config  = 'configs/legacy_config.R',
  seed    = c('forcing', 'coarse_differences'),
  scripts = c('scripts/9_forcing_barplot.R', 'scripts/10_forcing_breakdown.R'),
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-forcing-2026-10-08', '.rundirs/forcing-breakdown', '--all'),
                 'tests/test_paths.R', 'tests/test_config.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '4', CORES = '16-19'))
