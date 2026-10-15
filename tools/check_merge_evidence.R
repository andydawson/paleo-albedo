# Refuse a merge that brings code changes without a PASS verification report for that code.
#
# Why: Chris asked how the verification rules would actually be followed (2026-10-14); this
# check makes git enforce one of them (decided 2026-10-15). It runs as git's
# pre-merge-commit hook, so it applies to every merge that makes a merge commit (the project
# merges with --no-ff).
#
# The rule: if the branch being merged changes the code (R/, scripts/, tools/, tests/,
# config.R or configs/) relative to where it starts from, it must contain a report
# docs/cc/verification/*_report.md, written by tools/verify_change.R with verdict PASS, for a
# commit whose code is the same as the merged commit's (later commits may change only
# documentation, such as the report itself). Documentation-only merges need nothing.
#
# Usage, from the repository root:
#   Rscript tools/check_merge_evidence.R install   install the git hook (once per clone)
#   Rscript tools/check_merge_evidence.R check     what the hook runs during a merge
#   Rscript tools/check_merge_evidence.R check <commit> [<base>]   check a commit by hand
# To merge without the check (a deliberate exception, to be recorded in the merge
# message): git merge --no-verify.

CODE_PATHS = c('R', 'scripts', 'tools', 'tests', 'config.R', 'configs')

git_lines = function(...) {
  out = suppressWarnings(system2('git', shQuote(c(...)), stdout = TRUE, stderr = FALSE))   # quoted: system2 passes text to the shell
  if (!is.null(attr(out, 'status')) && attr(out, 'status') != 0) character() else out
}
same_code = function(a, b) {
  status = suppressWarnings(system2('git', shQuote(c('diff', '--quiet', a, b, '--', CODE_PATHS))))
  status == 0
}

# TRUE if the merge may go ahead; prints why not otherwise.
check_merge = function(merged, base) {
  if (same_code(base, merged)) { cat('merge check: no code changes; no verification needed\n'); return(TRUE) }
  reports = grep('^docs/cc/verification/.*_report\\.md$',
                 git_lines('ls-tree', '-r', '--name-only', merged, 'docs/cc/verification'), value = TRUE)
  for (r in rev(sort(reports))) {
    text = git_lines('show', paste0(merged, ':', r))
    verdict = sub('^- verdict: \\*\\*(\\w+)\\*\\*$', '\\1', grep('^- verdict: ', text, value = TRUE)[1])
    commit  = sub('^- commit: ', '', grep('^- commit: [0-9a-f]{40}$', text, value = TRUE)[1])
    if (identical(verdict, 'PASS') && !is.na(commit) && same_code(commit, merged)) {
      cat('merge check: PASS report', r, 'covers this code (verified commit', substr(commit, 1, 7), ')\n')
      return(TRUE)
    }
  }
  cat('merge check: REFUSED. This merge changes code (', paste(CODE_PATHS, collapse = ', '), ') but no PASS\n',
      'report in docs/cc/verification/ covers the merged code. Run tools/verify_change.R on the\n',
      'branch, commit its report, and merge again; or, as a recorded exception, git merge --no-verify.\n', sep = '')
  FALSE
}

install_hook = function() {
  hooks = git_lines('rev-parse', '--git-path', 'hooks')
  file = file.path(hooks, 'pre-merge-commit')
  rscript = file.path(R.home('bin'), 'Rscript')
  # A branch that predates the check (e.g. the trunk before this tool is merged into it) has
  # no checker in its tree: the hook then lets the merge go ahead and says so.
  writeLines(c('#!/bin/sh', '# Installed by tools/check_merge_evidence.R install: the merge check (see that file).',
               'if [ ! -f tools/check_merge_evidence.R ]; then echo "merge check: not in this branch yet; skipped"; exit 0; fi',
               sprintf('exec "%s" tools/check_merge_evidence.R check', rscript)), file)
  Sys.chmod(file, '755')
  cat('installed', file, '\n')
}

if (sys.nframe() == 0L) {
  a = commandArgs(TRUE)
  if (identical(a[1], 'install')) {
    install_hook()
  } else if (identical(a[1], 'check')) {
    merged = if (length(a) >= 2) a[2] else 'MERGE_HEAD'
    base   = if (length(a) >= 3) a[3] else git_lines('merge-base', 'HEAD', merged)
    quit(status = if (check_merge(merged, base)) 0 else 1)
  } else {
    cat('usage: Rscript tools/check_merge_evidence.R install | check [<commit> [<base>]]\n'); quit(status = 2)
  }
}
