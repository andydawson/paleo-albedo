# Run manifests: one record of every run of a pipeline script.
#
# Why: a number in the paper can only be trusted if we can say which code, which settings
# and which input files made it. Each run of a script writes runs/<script>/<time>_<pid>.md
# (inside the run directory for an alternative run) the moment it starts, and rewrites it
# as the run reads and writes files, so a run that fails or is killed still leaves its
# record, with status 'failed' or 'started'. The file is readable Markdown with the full
# record as JSON at the end.
#
# Before a script reads a file made by an earlier script, read_input() checks that the file
# is the one the earlier script's latest run wrote, and that the run used the same
# `analysis` settings as this one (presentation and run settings may differ). If not it
# stops and says what differs; `run$allow_stale = TRUE` in the config lets the run go on,
# and the manifest lists every such file. Original inputs (data/, kernels ...) are hashed
# but not checked.
#
# Usage. Scripts stay flat, so they can still be run line by line:
#
#   source('R/run_manifest.R')
#   run   = run_begin('8_radiative')     # reads config.R (or --config <file>), starts the manifest
#   cfg   = run$cfg
#   paths = run$paths
#   alb_diff = read_input(run, path_file(paths, 'albedo_differences'))       # readRDS by default
#   rk_cam5  = read_input(run, path_file(paths, 'kernel_cam5'), raster, varname = 'FSNS')
#   ...
#   write_output(run, alb_diff, path_file(paths, 'forcing'))                          # saveRDS by default
#   write_output(run, main, path_file(paths, 'forcing_by_period'), write.csv, row.names = FALSE)
#   run_finish(run)                      # closes plot devices, lists the figures, 'completed'

source('R/config.R')
source('R/paths.R')

# Start a run: read and check the configuration, set up the paths and write the manifest
# with status 'started'. A configuration that cannot be read is recorded (in runs/ of the
# repository, as no run directory is known) and stops the run. In a batch run (Rscript),
# an error marks the manifest 'failed' and ends the run with exit status 1; in an
# interactive session (RStudio) errors are left to the session as usual, and a run that
# is not finished stays 'started'. `run` is an environment: the functions below update it
# in place, so the script never has to reassign it.
run_begin = function(script, config_file = config_path(), interactive_session = interactive()) {
  run = new.env(parent = emptyenv())
  run$started = Sys.time()
  # What the session had before the run, restored when it ends: the error handler, the
  # default plot device, and the plot devices already open (which the run leaves alone).
  run$saved_options = list(error = getOption('error'), device = getOption('device'))
  run$devices_before = grDevices::dev.list()
  run$record = list(script = script, status = 'started',
                    started = utc_time(run$started), finished = NULL, error = NULL,
                    config_file = config_file,
                    config_md5 = if (file.exists(config_file)) unname(tools::md5sum(config_file)) else NULL,
                    code = code_version(config_file),
                    config = NULL, details = list(), inputs = list(), outputs = list(),
                    figures = list(), stale_inputs = list())
  cfg = tryCatch(load_config(config_file), error = function(e) e)
  if (inherits(cfg, 'error')) {
    run$file = manifest_file('runs', script, run$started)
    run_failed(run, conditionMessage(cfg))
    stop(cfg)
  }
  run$cfg = cfg
  run$paths = pipeline_paths(cfg)
  run$record$config = structure(config_json(cfg), class = 'json')
  run$file = manifest_file(path_runs(run$paths), script, run$started)
  if (isTRUE(run$record$code$dirty))
    warning('config.R, scripts/ or R/ have uncommitted changes; this run cannot be ',
            'reproduced from a commit', call. = FALSE, immediate. = TRUE)
  # In an interactive session (stepping through a script in RStudio) the manifest is first
  # written when the run writes an output or finishes, so trying out a few lines leaves no
  # stray 'started' manifest behind. A batch run (Rscript) writes it at once.
  run$deferred = interactive_session
  write_manifest(run)
  FIGURES_REQUESTED$paths = character()
  # Plots printed with no device open go to Rplots.pdf. Note whether this run did that, so
  # the file is claimed only by a run that wrote to it. (Not in an interactive session,
  # whose screen device must stay as it is.)
  if (!interactive_session) {
    device = getOption('device')
    if (is.character(device)) device = get(device, mode = 'function')
    options(device = function(...) { run$used_default_device = TRUE; device(...) })
  }
  if (!interactive_session) {
    options(error = function() {
      run_failed(run, geterrmessage())
      quit(save = 'no', status = 1)
    })
  }
  if (!run$deferred) message('run manifest: ', run$file)
  run
}

# Read an input through the run: stop if it is missing, record its MD5 just before it is
# read, check a product of an earlier script (see check_producer), then read it. Extra
# arguments go to the reader.
read_input = function(run, path, reader = readRDS, ...) {
  if (!file.exists(path)) stop('read_input: ', path, ' does not exist', call. = FALSE)
  md5 = unname(tools::md5sum(path))
  # A file read again unchanged (script 8 reads each kernel once per month) is recorded once.
  already = Filter(function(e) e$path == path && e$md5 == md5, run$record$inputs)
  message('reading ', path)
  if (!length(already)) {
    made_by = producer(run$paths, path)
    entry = list(path = path, md5 = md5)
    if (!is.na(made_by)) entry$check = check_producer(run, path, md5, made_by)
    run$record$inputs = c(run$record$inputs, list(entry))
    write_manifest(run)
  }
  reader(path, ...)
}

# Write an output through the run: write `object` to `path` with the writer (extra
# arguments go to it), then record the file's MD5. Only a write that succeeded is recorded.
write_output = function(run, object, path, writer = saveRDS, ...) {
  writer(object, path, ...)
  if (!file.exists(path)) stop('write_output: nothing was written to ', path, call. = FALSE)
  message('wrote ', path)
  run$deferred = FALSE
  run$record$outputs = c(run$record$outputs,
                         list(list(path = path, md5 = unname(tools::md5sum(path)))))
  write_manifest(run)
  invisible(path)
}

# Record a fact about the run in its manifest under `details`, e.g. the base random seed
# or the selected model's formula. Returns the value.
run_record = function(run, name, value) {
  run$record$details[[name]] = value
  write_manifest(run)
  invisible(value)
}

# Finish a run: close the plot devices the run opened (so figure files are complete),
# record every figure it wrote, mark the manifest 'completed', and give the session back
# its error handler and default plot device.
run_finish = function(run) {
  for (device in setdiff(grDevices::dev.list(), run$devices_before)) grDevices::dev.off(device)
  run$record$figures = new_figures(run)
  run$deferred = FALSE
  run$record$status = 'completed'
  run$record$finished = utc_time(Sys.time())
  write_manifest(run)
  options(run$saved_options)
  message('run completed; manifest: ', run$file)
  invisible(run$file)
}

# Mark the run failed with the error message.
run_failed = function(run, message) {
  run$record$status = 'failed'
  run$record$error = trimws(message)
  run$record$finished = utc_time(Sys.time())
  write_manifest(run)
}

# --- the consistency check -------------------------------------------------------------

# Check a product of an earlier script before it is read. The earlier script's latest run
# (newest manifest in runs/<script>/) must have written this file with this MD5, using the
# same `analysis` settings as this run. Only the latest run is consulted: an older output
# is used only by the override. Returns 'ok', or with run$allow_stale 'stale (allowed)'
# after recording the problems; otherwise stops with them.
check_producer = function(run, path, md5, made_by) {
  dir = file.path(path_runs(run$paths), made_by)
  manifests = sort(list.files(dir, pattern = '\\.md$', full.names = TRUE))
  problems = if (!length(manifests)) {
    sprintf('no run of %s is recorded in %s (files made before run manifests existed, October 2026, have none)', made_by, dir)
  } else {
    latest = read_manifest(manifests[length(manifests)])
    rel = root_relative(run$paths, path)
    written = Filter(function(o) root_relative(run$paths, o$path) == rel, latest$outputs)
    if (!length(written)) {
      sprintf('the latest run of %s (%s, status %s) did not write it',
              made_by, basename(manifests[length(manifests)]), latest$status)
    } else if (written[[length(written)]]$md5 != md5) {
      sprintf('the file differs from what the latest run of %s (%s, status %s) wrote',
              made_by, basename(manifests[length(manifests)]), latest$status)
    } else {
      differences = analysis_differences(latest$config$analysis, config_record(run$cfg)$analysis)
      if (length(differences))
        c(sprintf('the latest run of %s (%s) used different analysis settings:',
                  made_by, basename(manifests[length(manifests)])), differences)
    }
  }
  if (!length(problems)) return('ok')
  text = paste0(path, ':\n  ', paste(problems, collapse = '\n  '))
  if (!isTRUE(run$cfg$run$allow_stale))
    stop('read_input: ', text, '\nRerun ', made_by, ' first, or set run$allow_stale = TRUE ',
         'in the config to go on regardless (it is recorded in the run manifest).', call. = FALSE)
  warning('stale input allowed by run$allow_stale: ', text, call. = FALSE, immediate. = TRUE)
  run$record$stale_inputs = c(run$record$stale_inputs, list(list(path = path, problems = problems)))
  'stale (allowed)'
}

# The settings that differ between two `analysis` sections (as read back from JSON), one
# line per setting: 'forcing.variant: "veg_ice_thresh" -> "veg_ice_parts"'.
analysis_differences = function(old, new) {
  old = flatten_settings(old); new = flatten_settings(new)
  keys = sort(union(names(old), names(new)))
  shown = function(x) if (is.null(x)) '(absent)' else x
  unlist(lapply(keys, function(k)
    if (!identical(old[[k]], new[[k]])) sprintf('%s: %s -> %s', k, shown(old[[k]]), shown(new[[k]]))))
}

# A settings tree as one JSON value per dotted key ('forcing.variant' = '["veg_ice_thresh"]').
flatten_settings = function(x, prefix = NULL) {
  if (is.list(x) && length(x) && !is.null(names(x))) {
    unlist(lapply(names(x), function(k) flatten_settings(x[[k]], paste(c(prefix, k), collapse = '.'))),
           recursive = FALSE)
  } else {
    setNames(list(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = 'null'))), prefix)
  }
}

# The configuration as it reads back from a manifest, so the two sides compare like for like.
config_record = function(cfg) jsonlite::fromJSON(config_json(cfg), simplifyVector = FALSE)

# --- the manifest file -----------------------------------------------------------------

# runs/<script>/<UTC time to the millisecond>_<process id>.md: names sort by start time,
# and two runs started in the same instant by different processes cannot collide.
manifest_file = function(runs_dir, script, started) {
  dir = file.path(runs_dir, script)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, sprintf('%s_%d.md', format(started, '%Y%m%dT%H%M%OS3Z', tz = 'UTC'), Sys.getpid()))
}

# Write the manifest: a short readable summary, then the full record as JSON. Written to a
# temporary file and renamed, so a reader never sees a half-written manifest.
write_manifest = function(run) {
  if (isTRUE(run$deferred)) return(invisible(NULL))
  r = run$record
  r$session = session_info()
  json = jsonlite::prettify(jsonlite::toJSON(r, auto_unbox = TRUE, json_verbatim = TRUE,
                                             digits = NA, null = 'null', na = 'null'), indent = 2)
  file_lines = function(entries, what) {
    if (!length(entries)) return(sprintf('- (no %s)', what))
    vapply(entries, function(e) sprintf('- `%s` md5 %s%s', e$path, e$md5,
                                        if (!is.null(e$check)) paste0(', check: ', e$check) else ''), '')
  }
  code = r$code
  lines = c(
    sprintf('# Run of %s: %s', r$script, r$status), '',
    sprintf('- started %s%s', r$started, if (!is.null(r$finished)) sprintf(', finished %s', r$finished) else ''),
    sprintf('- config: `%s`', r$config_file),
    sprintf('- code: %s', if (is.na(code$commit)) 'unknown (git not available)' else
      sprintf('commit %s on %s, %s', code$commit, code$branch,
              if (is.na(code$dirty)) 'uncommitted changes unknown'
              else if (code$dirty) '**with uncommitted changes** (see the record)' else 'no uncommitted changes')),
    if (!is.null(r$error)) c('', '**Error.**', '', '```', r$error, '```'),
    if (length(r$details)) vapply(names(r$details), function(k)
      sprintf('- %s: %s', k, jsonlite::toJSON(r$details[[k]], auto_unbox = TRUE, digits = NA)), ''),
    if (length(r$stale_inputs)) c('', sprintf('**%d stale input(s) allowed by run$allow_stale** (see the record).',
                                              length(r$stale_inputs))),
    '', '## Inputs', '', file_lines(r$inputs, 'inputs'),
    '', '## Outputs', '', file_lines(r$outputs, 'outputs'),
    '', '## Figures', '', file_lines(r$figures, 'figures'),
    '', '## Full record', '', 'Read by later runs to check their inputs.', '',
    '```json', json, '```')
  tmp = paste0(run$file, '.tmp')
  writeLines(lines, tmp)
  file.rename(tmp, run$file)
}

# The record from a manifest file.
read_manifest = function(file) {
  lines = readLines(file)
  start = match('```json', lines)
  end = start + match('```', lines[-seq_len(start)])
  jsonlite::fromJSON(paste(lines[(start + 1):(end - 1)], collapse = '\n'), simplifyVector = FALSE)
}

# --- what the record holds ------------------------------------------------------------

utc_time = function(t) format(t, '%Y-%m-%d %H:%M:%OS3 UTC', tz = 'UTC')

# The commit, and whether config.R, the config file used, scripts/ or R/ differ from it
# (untracked files included). NA when git is not installed or this is not a git checkout:
# the run goes on without it.
code_version = function(config_file) {
  git = function(...) {
    if (!nzchar(Sys.which('git'))) return(NA_character_)
    out = suppressWarnings(tryCatch(system2('git', c(...), stdout = TRUE, stderr = FALSE),
                                    error = function(e) NA_character_))
    if (!is.null(attr(out, 'status'))) NA_character_ else out
  }
  commit = git('rev-parse', 'HEAD')
  if (length(commit) != 1 || is.na(commit)) return(list(commit = NA_character_, branch = NA_character_, dirty = NA))
  # A config file outside the checkout is not git's to judge (git status would fail on it);
  # its MD5 in the record identifies it.
  inside = !grepl('^(/|[A-Za-z]:|\\.\\.)', config_file)
  watched = unique(c('config.R', 'scripts', 'R', if (inside) config_file))
  changed = git('status', '--porcelain', '--untracked-files=all', '--', watched)
  if (length(changed) == 1 && is.na(changed)) return(list(commit = commit, branch = NA_character_, dirty = NA))
  list(commit = commit, branch = git('branch', '--show-current')[1],
       dirty = length(changed) > 0, uncommitted = as.list(changed))
}

# R, the packages loaded so far, the RNG kind and the thread settings: what can make the
# same code give different numbers on another machine.
session_info = function() {
  pkgs = sort(loadedNamespaces())
  list(r = R.version.string, platform = R.version$platform,
       host = unname(Sys.info()['nodename']),
       packages = as.list(setNames(vapply(pkgs, function(p) as.character(utils::packageVersion(p)), ''), pkgs)),
       rng_kind = RNGkind(),
       threads = as.list(Sys.getenv(c('OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS', 'MKL_NUM_THREADS'))))
}

# The figures this run wrote: the figure paths it asked path_figure() for (and Rplots.pdf if
# it plotted to the default device), modified since the run started, so neither figures left
# by earlier runs nor those of a run working alongside are claimed.
new_figures = function(run) {
  candidates = c(FIGURES_REQUESTED$paths,
                 if (isTRUE(run$used_default_device)) under_root(run$paths, 'Rplots.pdf'))
  candidates = candidates[file.exists(candidates)]
  fresh = candidates[file.mtime(candidates) >= trunc(run$started, 'secs')]
  lapply(sort(fresh), function(f) list(path = f, md5 = unname(tools::md5sum(f))))
}
