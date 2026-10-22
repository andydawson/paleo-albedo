# Convergence-report change (2026-10-22): only printed text changes, so no script is rerun.
# The check applies the new helper to all 204 saved fits of the full run of 18 October
# (script 4 at 4ba4d2a, the same fitting code), confirms script 4's two print lines, and runs
# the helper's test on made-up fits.
list(
  name    = 'convergence-report',
  commit  = 'convergence-report',
  config  = 'config.R',
  scripts = character(0),
  checks  = list(c('docs/cc/verification/2026-10-22_convergence-report_check.R',
                   '/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/calibration'),
                 'tests/test_model_registry.R',
                 'tests/test_config.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '2', CORES = '44-45'))
