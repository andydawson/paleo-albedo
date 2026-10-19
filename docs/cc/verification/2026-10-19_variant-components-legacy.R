# Variant-components fix after the reviews (2026-10-19), legacy configuration: script 9 under
# configs/legacy_config.R from the frozen original-arithmetic forcing tables, as I15c did. Chris
# chose to keep the legacy outputs as they were, so all four script 9 tables must be
# byte-identical to the anchors.
list(
  name    = 'variant-components-legacy',
  commit  = 'fix-variant-components',
  config  = 'configs/legacy_config.R',
  seed    = c('forcing', 'coarse_differences'),
  scripts = 'scripts/9_forcing_barplot.R',
  anchors = 'tests/anchors/interp-forcing-2026-10-08',
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '2', CORES = '46-47'))
