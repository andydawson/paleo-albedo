# Tests for tools/check_merge_evidence.R. Run from the repository root:
# Rscript tests/test_check_merge_evidence.R
source('tools/check_merge_evidence.R')

repo = tempfile('merge_fixture_'); dir.create(repo)
old = setwd(repo)
git = function(...) invisible(system2('git', shQuote(c(...)), stdout = FALSE, stderr = FALSE))
commit = function(path, lines, message) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(lines, path)
  git('add', '-A'); git('-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', message)
  system2('git', c('rev-parse', 'HEAD'), stdout = TRUE)
}
report = function(sha, verdict) c(sprintf('# Verification x: %s', verdict), '', sprintf('- verdict: **%s**', verdict), sprintf('- commit: %s', sha))
quietly = function(expr) { r = NULL; invisible(capture.output(r <- expr)); r }

git('init', '-q', '-b', 'trunk')
base = commit('R/a.R', 'x = 1', 'base')

# Documentation only: no verification needed.
git('checkout', '-q', '-b', 'docs_only')
docs = commit('docs/note.md', 'text', 'docs')
stopifnot(quietly(check_merge(docs, base)))

# Code without a report: refused.
git('checkout', '-q', '-b', 'code', base)
code = commit('R/a.R', 'x = 2', 'code change')
stopifnot(!quietly(check_merge(code, base)))

# A FAIL report: refused.
failed = commit('docs/cc/verification/2026-10-15_x_report.md', report(code, 'FAIL'), 'fail report')
stopifnot(!quietly(check_merge(failed, base)))

# A PASS report for this code, committed after it: accepted.
passed = commit('docs/cc/verification/2026-10-15_x_report.md', report(code, 'PASS'), 'pass report')
stopifnot(quietly(check_merge(passed, base)))

# Code changed again after the PASS report: refused until verified again.
changed = commit('scripts/s.R', 'y = 1', 'more code')
stopifnot(!quietly(check_merge(changed, base)))

setwd(old)
cat('test_check_merge_evidence: all tests passed\n')
