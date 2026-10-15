# I15b (#22): local names in the R/ helpers. No script runs: every test file must pass
# (the path, configuration, calendar, preparation and run-manifest helpers are the renamed ones).
list(
  name    = 'i15b-helper-names',
  commit  = 'i15b-helpers',
  config  = 'config.R',
  scripts = character(0),
  checks  = list('tests/test_calendar.R', 'tests/test_check_comments_only.R', 'tests/test_check_merge_evidence.R', 'tests/test_config.R', 'tests/test_differences.R', 'tests/test_paths.R', 'tests/test_preparation_ice.R', 'tests/test_preparation.R', 'tests/test_random.R', 'tests/test_run_manifest.R', 'tests/test_tools.R', 'tests/test_verify_change.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-11'))
