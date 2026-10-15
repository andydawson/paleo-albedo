# Merge check applies only to trunks; one log per verification check. Tools and tests only.
list(
  name    = 'merge-check-trunks',
  commit  = 'merge-check-trunks',
  config  = 'config.R',
  scripts = character(0),
  checks  = list('tests/test_calendar.R', 'tests/test_check_comments_only.R', 'tests/test_check_merge_evidence.R', 'tests/test_config.R', 'tests/test_differences.R', 'tests/test_paths.R', 'tests/test_preparation_ice.R', 'tests/test_preparation.R', 'tests/test_random.R', 'tests/test_run_manifest.R', 'tests/test_tools.R', 'tests/test_verify_change.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-11'))
