# Tests for tools/verify_change.R. Run from the repository root: Rscript tests/test_verify_change.R
#
# A throwaway git repository stands in for the pipeline: fake scripts that succeed, fail,
# leave out an output, or leave their run record unfinished, with a frozen input that is not
# in git. The tool must PASS only the good run, and FAIL each of the others for its reason.
source('tools/verify_change.R')
repo_root = getwd()

fixture = file.path(tempfile('verify_fixture_'), 'repo')
dir.create(fixture, recursive = TRUE)
in_fixture = function(path, lines) {
  f = file.path(fixture, path); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); writeLines(lines, f)
}
git = function(...) system2('git', shQuote(c('-C', fixture, ...)), stdout = FALSE, stderr = FALSE)

# The pieces of the real repository the tool uses.
for (f in c('R/run_manifest.R', 'tools/frozen.R', 'tools/check_anchors.R'))
  in_fixture(f, readLines(file.path(repo_root, f)))
in_fixture('config.R', 'list(run = list(dir = NULL, allow_stale = FALSE))')
in_fixture('.gitignore', c('.rundirs/', 'data/', 'docs/cc/verification/'))
in_fixture('data/frozen.txt', 'a frozen input, not in git')
in_fixture('tests/frozen_inputs.md5', paste0(unname(tools::md5sum(file.path(fixture, 'data/frozen.txt'))), '  data/frozen.txt'))

# A fake pipeline script: reads the frozen input, writes out.txt into the run folder and a
# run record in the manifest format (a JSON block), as the real scripts do.
fake_script = function(name, exit = 0, status = 'completed', write_output = TRUE) c(
  "args = commandArgs(TRUE); cfg = source(args[which(args == '--config') + 1])$value",
  "stopifnot(file.exists('data/frozen.txt'))",
  sprintf("dir.create(file.path(cfg$run$dir, 'runs', '%s'), recursive = TRUE, showWarnings = FALSE)", name),
  "out = file.path(cfg$run$dir, 'out.txt')",
  if (write_output) "writeLines('result', out)",
  "md5 = if (file.exists(out)) unname(tools::md5sum(out)) else 'none'",
  sprintf("rec = list(script = '%s', status = '%s', outputs = list(list(path = out, md5 = md5)))", name, status),
  sprintf("writeLines(c('# Run', '', '```json', jsonlite::toJSON(rec, auto_unbox = TRUE), '```'), file.path(cfg$run$dir, 'runs', '%s', 'r.md'))", name),
  sprintf("quit(status = %d)", exit))
in_fixture('scripts/good.R', fake_script('good'))
in_fixture('scripts/fails.R', fake_script('fails', exit = 1))
in_fixture('scripts/no_output.R', fake_script('no_output', write_output = FALSE))
in_fixture('scripts/unfinished.R', fake_script('unfinished', status = 'started'))
in_fixture('scripts/no_record.R', 'quit(status = 0)')
# An anchor that the good script's output matches, and one it does not.
in_fixture('tests/anchors/live_paths.tsv', 'out.txt\tout.txt')
writeLines('result', tmp <- tempfile()); right_md5 = unname(tools::md5sum(tmp))
in_fixture('tests/anchors/right/MD5SUMS.txt', paste0(right_md5, '  out.txt'))
in_fixture('tests/anchors/wrong/MD5SUMS.txt', paste0(strrep('0', 32), '  out.txt'))

git('init', '-q'); git('add', '-A')
git('-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', 'fixture')

spec = function(name, scripts, ...) {
  f = tempfile(fileext = '.R')
  writeLines(deparse(list(name = name, commit = 'HEAD', config = 'config.R', scripts = scripts, ...)), f)
  f
}
run = function(...) { r = NULL; invisible(utils::capture.output(r <- verify_change(spec(...), main = fixture))); r }
has = function(r, pattern) any(grepl(pattern, r$failures))

r = run('good', 'scripts/good.R', anchors = 'tests/anchors/right')
stopifnot(r$verdict == 'PASS', file.exists(r$report), file.exists(sub('_report\\.md$', '.R', r$report)),
          file.exists(file.path(r$worktree, 'data/frozen.txt')),            # linked from the main checkout
          Sys.readlink(file.path(r$worktree, 'data/frozen.txt')) != '')

r = run('fails', c('scripts/fails.R', 'scripts/good.R'))
stopifnot(r$verdict == 'FAIL', has(r, 'fails.R exited with status 1'),
          any(grepl('good.R: not run', readLines(r$report))))              # stops at the first failure

r = run('no_output', 'scripts/no_output.R')
stopifnot(r$verdict == 'FAIL', has(r, 'output missing'))

r = run('unfinished', 'scripts/unfinished.R')
stopifnot(r$verdict == 'FAIL', has(r, 'status "started"'))

r = run('no_record', 'scripts/no_record.R')
stopifnot(r$verdict == 'FAIL', has(r, 'no run record'))

r = run('wrong_anchor', 'scripts/good.R', anchors = 'tests/anchors/wrong')
stopifnot(r$verdict == 'FAIL', has(r, 'anchor not matched'))

r = run('failing_check', 'scripts/good.R', checks = list(c('-e', 'quit(status = 3)')))
stopifnot(r$verdict == 'FAIL', has(r, 'check failed'), any(grepl('exit status 3', readLines(r$report))))

# A run folder name is never reused: a second verification must not mix with the first.
stopifnot(inherits(try(run('good', 'scripts/good.R'), silent = TRUE), 'try-error'))

# A frozen input changed in the main checkout fails the verification.
writeLines('changed', file.path(fixture, 'data/frozen.txt'))
r = run('frozen_changed', 'scripts/good.R')
stopifnot(r$verdict == 'FAIL', has(r, 'frozen inputs changed'))

# The products named by catalogue id, on the real catalogue: every month and model id of a
# template (a bug once expanded the per-month ones to nothing).
found = catalogue_files(repo_root, 'config.R', c('calibration_ladder', 'calibration_spatial', 'calibration_table'))
stopifnot(length(found$files) == 12 * 8 + 12 * 6 + 1,
          inherits(try(catalogue_files(repo_root, 'config.R', 'no_such_id'), silent = TRUE), 'try-error'))

cat('test_verify_change: all tests passed\n')
