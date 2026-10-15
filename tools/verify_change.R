# Verify one commit of the pipeline: run the scripts a change affects, from a separate
# worktree at that exact commit, and check the evidence. Writes a PASS/FAIL report.
#
# Why: verification used to be done by drivers written afresh for each issue in a scratch
# folder. Two of them gave wrong evidence (an exit status read from the wrong command; two
# runs writing into one folder), and switching branches in the checkout a run was reading
# could have corrupted it. This tool is written once, tested (tests/test_verify_change.R),
# and run the same way every time (decided by Chris, 2026-10-15).
#
# Usage, from the repository root:
#   Rscript tools/verify_change.R <spec.R>
# The spec is a small R file whose value is a list (example: docs/cc/verification/README.md):
#   name      short name of this verification, used for the run folder and the report
#   commit    the commit to verify (a SHA, or a branch name, resolved once at the start)
#   config    the configuration to start from, a path inside the repository
#   settings  optional: settings that differ from that configuration, as a nested list
#   seed      optional: catalogue ids of baseline products to copy into the run folder
#   copy      optional: further files to copy in, list(c(from, to), ...), from relative to
#             the main checkout, to relative to the run folder
#   scripts   the scripts to run, in order
#   anchors   optional: anchor directories whose files must all MATCH (byte for byte)
#   checks    optional: further commands to run in the worktree, each a character vector
#             (an R script and its arguments); each must exit with status 0
#   rscript   optional: the command that runs an R script (default: this R's Rscript); on this
#             machine "tools/rscript.sh" for like-for-like runs (see that file)
#   env       optional: environment variables for the runs, e.g. c(THREADS = "8", CORES = "0-7")
#
# What it does:
#   1. checks the frozen inputs of the main checkout (tools/frozen.R);
#   2. makes a detached worktree at the commit, outside the repository
#      (../paleo-RF-verify/<first 7 of the SHA>), or reuses one that is clean and at it;
#      the main checkout is never switched or edited;
#   3. links the frozen inputs (the fitted models and data not in git) from the main
#      checkout into the worktree, and makes a run folder there with its own configuration;
#   4. runs each script, recording its exit status and time, and stops at the first failure;
#   5. requires: every script exited 0; its newest run record says "completed" and was
#      written by this run; every output in the record exists with the recorded MD5; the
#      worktree is still clean; every anchor file MATCHes; every check exits 0; the frozen
#      inputs are unchanged;
#   6. writes the report to docs/cc/verification/<date>_<name>_report.md in the main checkout,
#      with a copy of the spec beside it (<date>_<name>.R), and exits with status 0 for PASS,
#      1 for FAIL.

# --- small helpers ----------------------------------------------------------------------

git_out = function(dir, ...) {
  out = suppressWarnings(system2('git', shQuote(c('-C', dir, ...)), stdout = TRUE, stderr = TRUE))   # quoted: system2 passes text to the shell
  status = attr(out, 'status')
  list(ok = is.null(status) || status == 0, out = out)
}

now_utc = function() format(Sys.time(), '%Y-%m-%d %H:%M:%S UTC', tz = 'UTC')

# The value of a spec file: a list with at least name, commit, config and scripts.
read_spec = function(file) {
  spec = source(file, local = new.env())$value
  missing = setdiff(c('name', 'commit', 'config', 'scripts'), names(spec))
  if (length(missing)) stop('verify_change: the spec lacks ', paste(missing, collapse = ', '))
  if (!grepl('^[A-Za-z0-9_.-]+$', spec$name)) stop('verify_change: name may use letters, digits, ., _ and - only')
  # The Rscript of the R running this tool, unless the spec names another runner.
  spec$rscript = if (is.null(spec$rscript)) file.path(R.home('bin'), 'Rscript') else spec$rscript
  spec
}

# --- the steps --------------------------------------------------------------------------

# Frozen inputs of the main checkout unchanged: TRUE or FALSE (tools/frozen.R's own check).
frozen_ok = function(main) {
  inventory = file.path(main, 'tests/frozen_inputs.md5')
  if (!file.exists(inventory)) return(TRUE)   # a checkout with no frozen inputs
  env = new.env(); sys.source(file.path(main, 'tools/frozen.R'), envir = env)
  old = setwd(main); on.exit(setwd(old))
  isTRUE(env$frozen_verify(inventory))
}

# The worktree for a commit: made if absent; reused only if clean and at the commit.
make_worktree = function(main, sha) {
  dir = file.path(dirname(normalizePath(main)), paste0(basename(normalizePath(main)), '-verify'), substr(sha, 1, 7))
  if (!dir.exists(dir)) {
    dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
    r = git_out(main, 'worktree', 'add', '--detach', dir, sha)
    if (!r$ok) stop('verify_change: could not make the worktree: ', paste(r$out, collapse = ' '))
  }
  head = git_out(dir, 'rev-parse', 'HEAD')$out
  if (!identical(head, sha)) stop('verify_change: worktree ', dir, ' is at ', head, ', not ', sha)
  if (length(git_out(dir, 'status', '--porcelain')$out)) stop('verify_change: worktree ', dir, ' is not clean')
  dir
}

# Link every frozen input missing from the worktree to the main checkout's file. Tracked
# files are already there from git; the fitted models and most data are not in git.
link_inputs = function(main, worktree) {
  inventory = file.path(main, 'tests/frozen_inputs.md5')
  if (!file.exists(inventory)) return(0L)
  paths = sub('^[0-9a-f]+ +', '', readLines(inventory))
  absent = paths[!file.exists(file.path(worktree, paths))]
  for (p in absent) {
    dir.create(dirname(file.path(worktree, p)), recursive = TRUE, showWarnings = FALSE)
    if (!file.symlink(normalizePath(file.path(main, p)), file.path(worktree, p))) stop('verify_change: could not link ', p)
  }
  length(absent)
}

# The run folder's configuration: the spec's configuration with its settings, run$dir set
# to the run folder, and stale inputs allowed when products were copied in (recorded in each
# run record). Written as plain R, so the run's manifests record exactly what was run.
write_run_config = function(worktree, spec, run_dir) {
  cfg = source(file.path(worktree, spec$config), local = new.env())$value
  if (!is.null(spec$settings)) cfg = utils::modifyList(cfg, spec$settings)
  cfg$run$dir = run_dir
  if (length(spec$seed) || length(spec$copy)) cfg$run$allow_stale = TRUE
  file = file.path(worktree, run_dir, 'config_used.R')
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(sprintf('# Written by tools/verify_change.R for "%s": %s with the spec\'s settings.', spec$name, spec$config),
               paste(deparse(cfg, control = c('keepNA', 'keepInteger', 'niceNames', 'showAttributes')), collapse = '\n')),
             file)
  file.path(run_dir, 'config_used.R')
}

# The baseline files of some catalogue ids (every month and model id a template takes), as
# paths relative to the checkout, and the path setups for copying them into a run folder.
catalogue_files = function(checkout, config_file, ids, run_dir = tempfile()) {
  old = setwd(checkout); on.exit(setwd(old))
  env = new.env()
  sys.source('R/config.R', envir = env); sys.source('R/paths.R', envir = env)
  cfg = env$load_config(config_file)
  base = suppressMessages(env$pipeline_paths(cfg, env$baseline_paths()))
  run  = suppressMessages(env$pipeline_paths(cfg, env$redirected_paths(run_dir)))
  files = unlist(lapply(ids, function(id) {
    t = env$PATH_CATALOGUE[id, 'template']
    if (is.na(t)) stop('verify_change: no catalogue id ', id)
    # MONTHS comes from R/calendar.R, which R/paths.R sources into the global environment.
    months = if (grepl('{month}', t, fixed = TRUE)) get('MONTHS', envir = env) else list(NULL)
    models = if (grepl('{model}', t, fixed = TRUE)) env$TEMPLATE_MODELS[[id]] else list(NULL)
    files = unlist(lapply(months, function(m) lapply(models, function(k) env$path_file(base, id, month = m, model = k))))
    if (!length(files)) stop('verify_change: catalogue id ', id, ' named no files')
    files
  }))
  list(files = files, run = run, seed_run_dir = env$seed_run_dir)
}

# Copy baseline products (by catalogue id) and further files into the run folder.
seed_run = function(worktree, spec, run_dir, config_file) {
  if (!length(spec$seed) && !length(spec$copy)) return(character())
  found = catalogue_files(worktree, config_file, spec$seed, run_dir)
  old = setwd(worktree); on.exit(setwd(old))
  if (length(found$files)) found$seed_run_dir(found$run, found$files)
  for (pair in spec$copy) {
    target = file.path(run_dir, pair[2])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(pair[1], target, overwrite = TRUE)) stop('verify_change: could not copy ', pair[1])
    cat(pair[2], '\n', file = file.path(run_dir, 'SEEDED.txt'), append = TRUE)
  }
  c(found$files, vapply(spec$copy, `[`, '', 1))
}

# Run one R script in the worktree; its output goes to a log in the run folder.
run_script = function(worktree, spec, script, config_file, log) {
  old = setwd(worktree); on.exit(setwd(old))
  env = if (length(spec$env)) paste0(names(spec$env), '=', spec$env) else character()
  t0 = Sys.time()
  status = system2(spec$rscript, shQuote(c(script, '--config', config_file)), stdout = log, stderr = log, env = env)
  list(status = status, minutes = round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 1))
}

# The newest run record of a script in the run folder, if written after `since`; and what is
# wrong with it (an empty vector if nothing).
check_record = function(worktree, run_dir, script, since) {
  name = sub('\\.R$', '', basename(script))
  dir = file.path(worktree, run_dir, 'runs', name)
  files = sort(list.files(dir, pattern = '\\.md$', full.names = TRUE))
  if (!length(files) || file.mtime(files[length(files)]) < since) return(list(file = NA, problems = 'no run record written by this run'))
  env = new.env(); sys.source(file.path(worktree, 'R/run_manifest.R'), envir = env)
  rec = env$read_manifest(files[length(files)])
  problems = character()
  if (!identical(rec$status, 'completed')) problems = c(problems, sprintf('run record status "%s"', rec$status))
  for (o in rec$outputs) {
    p = file.path(worktree, o$path)
    if (!file.exists(p)) problems = c(problems, paste('output missing:', o$path))
    else if (unname(tools::md5sum(p)) != o$md5) problems = c(problems, paste('output differs from its record:', o$path))
  }
  list(file = files[length(files)], problems = problems, outputs = length(rec$outputs))
}

# Anchor comparison through tools/check_anchors.R; every file must MATCH.
check_anchor = function(worktree, run_dir, anchor) {
  env = new.env(); sys.source(file.path(worktree, 'tools/check_anchors.R'), envir = env)
  old = setwd(worktree); on.exit(setwd(old))
  lines = utils::capture.output(env$check_anchors(anchor, run_dir))
  status = sub('^\\S+\\s+', '', lines)
  list(lines = lines, ok = length(lines) > 0 && all(status == 'MATCH'))
}

run_check = function(worktree, spec, command, log) {
  old = setwd(worktree); on.exit(setwd(old))
  env = if (length(spec$env)) paste0(names(spec$env), '=', spec$env) else character()
  system2(spec$rscript, shQuote(command), stdout = log, stderr = log, env = env)   # quoted: system2 passes text to the shell
}

# --- the whole verification -------------------------------------------------------------

verify_change = function(spec_file, main = '.') {
  spec = read_spec(spec_file)
  main = normalizePath(main)
  started = now_utc()
  sha = git_out(main, 'rev-parse', '--verify', paste0(spec$commit, '^{commit}'))
  if (!sha$ok) stop('verify_change: unknown commit ', spec$commit)
  sha = sha$out
  failures = character()
  lines = character()
  note = function(...) lines <<- c(lines, sprintf(...))

  frozen_before = frozen_ok(main)
  if (!frozen_before) failures = c(failures, 'frozen inputs changed before the run')
  worktree = make_worktree(main, sha)
  linked = link_inputs(main, worktree)
  run_dir = file.path('.rundirs', spec$name)
  if (dir.exists(file.path(worktree, run_dir))) stop('verify_change: run folder ', run_dir, ' already exists in ', worktree, '; use a new name')
  config_file = write_run_config(worktree, spec, run_dir)
  seeded = seed_run(worktree, spec, run_dir, config_file)
  note('- worktree `%s` at %s; %d frozen inputs linked from the main checkout', worktree, sha, linked)
  note('- run folder `%s`; configuration `%s` (from `%s`); %d files copied in', run_dir, config_file, spec$config, length(seeded))
  note('- runner `%s`%s', spec$rscript, if (length(spec$env)) paste0(' with ', paste0(names(spec$env), '=', spec$env, collapse = ' ')) else '')

  note(''); note('## Scripts'); note('')
  for (script in spec$scripts) {
    if (length(failures)) { note('- %s: not run (an earlier step failed)', script); next }
    since = Sys.time() - 1
    log = file.path(worktree, run_dir, paste0(sub('\\.R$', '', basename(script)), '.log'))
    r = run_script(worktree, spec, script, config_file, log)
    rec = check_record(worktree, run_dir, script, since)
    note('- %s: exit status %d, %.1f min, run record `%s`%s', script, r$status, r$minutes,
         if (is.na(rec$file)) 'none' else basename(rec$file),
         if (!is.null(rec$outputs)) sprintf(', %d outputs', rec$outputs) else '')
    if (r$status != 0) failures = c(failures, sprintf('%s exited with status %d (log: %s)', script, r$status, log))
    if (length(rec$problems)) failures = c(failures, paste0(script, ': ', rec$problems))
  }

  note(''); note('## Anchors'); note('')
  for (anchor in spec$anchors) {
    if (length(failures)) { note('- %s: not compared (an earlier step failed)', anchor); next }
    a = check_anchor(worktree, run_dir, anchor)
    note('- %s: %s', anchor, if (a$ok) 'every file MATCH' else 'NOT all MATCH')
    note('```'); lines = c(lines, a$lines); note('```')
    if (!a$ok) failures = c(failures, paste('anchor not matched:', anchor))
  }

  note(''); note('## Checks'); note('')
  for (command in spec$checks) {
    if (length(failures)) { note('- `%s`: not run (an earlier step failed)', paste(command, collapse = ' ')); next }
    log = file.path(worktree, run_dir, 'checks.log')
    status = run_check(worktree, spec, command, log)
    note('- `%s`: exit status %d', paste(command, collapse = ' '), status)
    if (status != 0) failures = c(failures, sprintf('check failed: %s (log: %s)', paste(command, collapse = ' '), log))
  }

  if (length(git_out(worktree, 'status', '--porcelain')$out)) failures = c(failures, 'the worktree is no longer clean')
  if (!frozen_ok(main)) failures = c(failures, 'frozen inputs changed during the run')

  verdict = if (length(failures)) 'FAIL' else 'PASS'
  report = file.path(main, 'docs/cc/verification', sprintf('%s_%s_report.md', format(Sys.Date()), spec$name))
  dir.create(dirname(report), recursive = TRUE, showWarnings = FALSE)
  # Keep the spec beside its report, so the pair records what was run.
  spec_copy = sub('_report\\.md$', '.R', report)
  if (normalizePath(spec_file) != normalizePath(spec_copy, mustWork = FALSE)) file.copy(spec_file, spec_copy, overwrite = TRUE)
  writeLines(c(sprintf('# Verification %s: %s', spec$name, verdict), '',
               sprintf('- verdict: **%s**', verdict),
               sprintf('- commit: %s', sha),
               sprintf('- spec: `%s`', spec_file),
               sprintf('- started %s, finished %s', started, now_utc()),
               lines, '',
               '## Failures', '', if (length(failures)) paste('-', failures) else '- none'),
             report)
  cat(verdict, ': ', report, '\n', sep = '')
  invisible(list(verdict = verdict, report = report, failures = failures, worktree = worktree, sha = sha))
}

if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly = TRUE)
  if (length(args) != 1) { cat('usage: Rscript tools/verify_change.R <spec.R>\n'); quit(status = 2) }
  result = verify_change(args[1])
  quit(status = if (result$verdict == 'PASS') 0 else 1)
}
