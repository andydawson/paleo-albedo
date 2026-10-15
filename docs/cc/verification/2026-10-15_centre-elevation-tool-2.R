# Elevation lookup moved to a preparation tool: script 1 under the legacy configuration must still match the anchors (and run without elevatr).
list(
  name    = 'centre-elevation-tool-2',
  commit  = 'centre-elevation-tool',
  config  = 'configs/legacy_config.R',
  scripts = 'scripts/1_veg_lct_prep.R',
  checks  = list(c('tools/check_anchors.R', 'tests/anchors/interp-allmonths-2026-09-20', '.rundirs/centre-elevation-tool-2'),
                 'tests/test_paths.R', 'tests/test_config.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '4', CORES = '16-19'))
