# Second attempt: the first failed because script 9 also reads script 7's coarse differences (EGU recipe).
# Variant-components fix (2026-10-19): script 9 alone, rerun on the per-cell forcing table of the
# full run of 18 October (same code at 4ba4d2a for everything script 9 reads), checked by
# docs/cc/verification/2026-10-19_variant-components_check.R.
list(
  name    = 'variant-components-2',
  commit  = 'fix-variant-components',
  config  = 'config.R',
  copy    = list(c('/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/forcing/RF_holocene_all_cases.RDS',
                   'output/forcing/RF_holocene_all_cases.RDS'),
                 c('/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/data/alb_interp_preds_diffs_bluesky.RDS',
                   'data/alb_interp_preds_diffs_bluesky.RDS')),
  scripts = 'scripts/9_forcing_barplot.R',
  checks  = list(c('docs/cc/verification/2026-10-19_variant-components_check.R', '.rundirs/variant-components-2'),
                 'tests/test_config.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '2', CORES = '46-47'))
