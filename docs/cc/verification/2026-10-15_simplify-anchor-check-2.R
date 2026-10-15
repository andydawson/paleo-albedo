# Anchor-check simplification: unit tests of the checker and the verification tool, and a
# real run (script 8 under the legacy configuration) checked through the command line.
list(
  name    = 'simplify-anchor-check-2',
  commit  = 'simplify-anchor-check',
  config  = 'configs/legacy_config.R',
  seed    = c('albedo_differences'),
  scripts = 'scripts/8_radiative.R',
  checks  = list('tests/test_tools.R', 'tests/test_verify_change.R',
                 c('tools/check_anchors.R', 'tests/anchors/interp-tail-2026-09-21', '.rundirs/simplify-anchor-check-2')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '4', CORES = '16-19'))
