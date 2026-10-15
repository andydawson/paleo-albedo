# I15a (#21): comments and indentation only. No script runs: every changed R file must parse
# to exactly the same code as at 99042c9 (the commit before the first edit, which added the
# checker), and every test file must pass.
list(
  name    = 'i15a-comments-only-2',
  commit  = 'i15a-comments',
  config  = 'config.R',
  scripts = character(0),
  checks  = list(c('tools/check_comments_only.R', '99042c9'), 'tests/test_calendar.R', 'tests/test_check_comments_only.R', 'tests/test_check_merge_evidence.R', 'tests/test_config.R', 'tests/test_differences.R', 'tests/test_paths.R', 'tests/test_preparation_ice.R', 'tests/test_preparation.R', 'tests/test_random.R', 'tests/test_run_manifest.R', 'tests/test_tools.R', 'tests/test_verify_change.R'),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-11'))
