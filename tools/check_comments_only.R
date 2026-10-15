# Check that a change touched only comments and layout: every R file changed since a base
# commit parses to exactly the same expressions as before.
#
# Why: the readability work of issue I15a edits comments and indentation in the scripts and
# R/ helpers; it must not change a single executable expression. Comparing the parsed code
# (comments dropped, layout normalised) proves that for every changed file, without running
# the pipeline.
#
# Usage, from the repository root:
#   Rscript tools/check_comments_only.R <base-commit>
# Compares each .R file that differs between <base-commit> and the working tree. Exit status
# 0 if every one parses to the same code, 1 otherwise (listing the files and the first
# differing expression); a file added or removed counts as a code change.

# The code of R source text, comments and layout removed: one deparsed string per expression.
code_of = function(text) {
  exprs = parse(text = text, keep.source = FALSE)
  vapply(exprs, function(e) paste(deparse(e, width.cutoff = 500L, control = c('keepInteger', 'keepNA', 'niceNames', 'showAttributes')), collapse = '\n'), '')
}

changed_r_files = function(base) {
  out = system2('git', shQuote(c('diff', '--name-only', base, '--', '*.R')), stdout = TRUE)
  out[nzchar(out)]
}

check_comments_only = function(base) {
  files = changed_r_files(base)
  bad = character()
  for (f in files) {
    old = suppressWarnings(system2('git', shQuote(c('show', paste0(base, ':', f))), stdout = TRUE, stderr = FALSE))
    if (!is.null(attr(old, 'status')) || !file.exists(f)) { bad = c(bad, paste(f, '(added or removed)')); next }
    a = code_of(old); b = code_of(readLines(f))
    if (!identical(a, b)) {
      n = min(length(a), length(b))
      i = which(a[seq_len(n)] != b[seq_len(n)])[1]   # the first differing expression ...
      if (is.na(i)) i = n + 1                         # ... or the first one added or removed
      bad = c(bad, sprintf('%s (first difference at expression %d)', f, i))
    }
  }
  cat(sprintf('%d R file(s) changed since %s; %d with code changes\n', length(files), base, length(bad)))
  if (length(bad)) cat(paste0('  ', bad, '\n'), sep = '')
  invisible(length(bad) == 0)
}

if (sys.nframe() == 0L) {
  a = commandArgs(trailingOnly = TRUE)
  if (length(a) != 1) { cat('usage: Rscript tools/check_comments_only.R <base-commit>\n'); quit(status = 2) }
  quit(status = if (check_comments_only(a[1])) 0 else 1)
}
