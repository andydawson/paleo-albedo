# Headline switched to partial ice in config.R and limited to totals (2026-10-19), legacy
# configuration: scripts 9 and 10 under configs/legacy_config.R from the frozen
# original-arithmetic forcing tables. The legacy headline stays all-or-nothing, so all four
# script 9 tables must be byte-identical to the anchors.
list(
  name    = 'headline-partial-ice-legacy',
  commit  = 'headline-partial-ice',
  config  = 'configs/legacy_config.R',
  seed    = c('forcing', 'coarse_differences'),
  scripts = c('scripts/9_forcing_barplot.R', 'scripts/10_forcing_breakdown.R'),
  anchors = 'tests/anchors/interp-forcing-2026-10-08',
  checks  = list('tests/test_config.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '2', CORES = '46-47'))
