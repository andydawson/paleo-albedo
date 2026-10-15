# Tests for tools/check_comments_only.R. Run from the repository root:
# Rscript tests/test_check_comments_only.R
source('tools/check_comments_only.R')

# The comparison itself: comments and layout do not count; any code change does.
same = function(a, b) identical(code_of(a), code_of(b))
stopifnot(
  same('x = 1  # a comment', '# a new comment\nx = 1'),
  same('f = function(a) {\nif (a) 1 else 2\n}', 'f = function(a) {\n    if (a) 1 else 2   # indented\n}'),
  !same('x = 1', 'x = 2'),
  !same('x = 1', 'y = 1'),
  !same('plot(x, main = "a")', 'plot(x, main = "b")'),        # strings are code
  !same('x = 1', 'x = 1\ny = 2'))

# On a throwaway repository: a comment edit passes; a code edit fails.
repo = tempfile('comments_fixture_'); dir.create(repo); old = setwd(repo)
git = function(...) invisible(system2('git', shQuote(c(...)), stdout = FALSE, stderr = FALSE))
git('init', '-q'); writeLines(c('# old comment', 'x = 1'), 'a.R'); git('add', 'a.R')
git('-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', 'base')
writeLines(c('# a clearer comment', 'x = 1   # why'), 'a.R')
ok_comment = check_comments_only('HEAD')
writeLines(c('# a clearer comment', 'x = 2'), 'a.R')
ok_code = check_comments_only('HEAD')
setwd(old)
stopifnot(ok_comment, !ok_code)

cat('test_check_comments_only: all tests passed\n')
