# The verification tool and the merge check (branch verify-tool): tools and tests only, so
# no pipeline script runs; every test file must pass, including the tool's own tests.
list(
  name    = 'verify-tool-tests',
  commit  = 'verify-tool',
  config  = 'config.R',
  scripts = character(0),
  checks  = list('tests/test_calendar.R', 'tests/test_check_merge_evidence.R', 'tests/test_config.R', 'tests/test_differences.R', 'tests/test_paths.R', 'tests/test_preparation.R', 'tests/test_random.R', 'tests/test_run_manifest.R', 'tests/test_tools.R', 'tests/test_verify_change.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '16-19'))
