# Second run, after the change review's comment fixes and the figure subtitles (Chris,
# 2026-10-19): the same checks; the check now also compares every table with the first run.
# Headline forcing switched to partial ice (veg_ice_parts), and the headline setting limited to
# the four totals (2026-10-19), current configuration: scripts 9 and 10 rerun on the per-cell
# forcing table of the full run of 18 October (script 8 writes every variant, so it is not
# rerun), checked by 2026-10-19_headline-partial-ice_check.R and the unit tests.
list(
  name    = 'headline-partial-ice-2',
  commit  = 'headline-partial-ice',
  config  = 'config.R',
  copy    = list(c('/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/forcing/RF_holocene_all_cases.RDS',
                   'output/forcing/RF_holocene_all_cases.RDS'),
                 c('/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/data/alb_interp_preds_diffs_bluesky.RDS',
                   'data/alb_interp_preds_diffs_bluesky.RDS')),
  scripts = c('scripts/9_forcing_barplot.R', 'scripts/10_forcing_breakdown.R'),
  checks  = list(c('docs/cc/verification/2026-10-19_headline-partial-ice_check.R', '.rundirs/headline-partial-ice-2'),
                 'tests/test_config.R', 'tests/test_forcing_components.R', 'tests/test_forcing_breakdown.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '2', CORES = '46-47'))
