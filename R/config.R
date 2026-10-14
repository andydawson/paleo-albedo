# Load and check the project configuration, config.R at the repository root.
#
# Usage, in any script:
#   source('R/config.R')
#   cfg = load_config()          # config.R, or the file given with --config <file>
#   cfg$analysis$forcing$variant
#
# config.R is an ordinary R file whose value is one list of settings. It is run with
# source() in a fresh environment, so it cannot see or change a script's variables, and
# the list it returns is checked against the schema in R/config_schema.R: unknown or
# missing keys and illegal values stop the script with a message naming every problem.
# Each run manifest records the settings as evaluated. (Until 2026-10-14 the file was read
# by a restricted parser that allowed only list(), c(), seq() and literal values; Chris
# replaced it with plain source() for readability, issue I06.) Single-config migration,
# issue I03, 2026-10-13.

source('R/config_schema.R')

# Check a value against a schema node; returns messages, each prefixed with the key path.
config_schema_errors = function(value, schema, path = 'cfg') {
  if (is.function(schema)) {
    if (is.list(value)) return(sprintf('%s must be a value, not a list', path))
    msg = schema(value)
    return(if (is.null(msg)) character() else sprintf('%s %s', path, msg))
  }
  if (!is.list(value) || (length(value) && (is.null(names(value)) || any(!nzchar(names(value))))))
    return(sprintf('%s must be a list with a name on every entry', path))
  errs = character()
  dup = unique(names(value)[duplicated(names(value))])
  if (length(dup)) errs = c(errs, sprintf('%s has %s more than once', path, paste(dup, collapse = ', ')))
  unknown = setdiff(names(value), names(schema))
  if (length(unknown)) errs = c(errs, sprintf('%s has unknown key(s) %s', path, paste(unknown, collapse = ', ')))
  missing = setdiff(names(schema), names(value))
  if (length(missing)) errs = c(errs, sprintf('%s is missing key(s) %s', path, paste(missing, collapse = ', ')))
  for (k in intersect(names(schema), names(value)))
    errs = c(errs, config_schema_errors(value[[k]], schema[[k]], paste0(path, '$', k)))
  errs
}

# Which config file this run uses: config.R, unless the script was started with
# `--config <file>` (or `--config=<file>`), e.g.
#   Rscript scripts/6_prediction_model.R --config configs/median.R
# An alternative run is then described completely by one file (its settings and its run
# directory), which can be committed like config.R, and runs with different config files
# can go side by side from one checkout (issue I07, 2026-10-13).
config_path = function(args = commandArgs(trailingOnly = TRUE)) {
  i = which(args == '--config' | startsWith(args, '--config='))
  if (!length(i)) return('config.R')
  if (length(i) > 1) stop('--config given more than once', call. = FALSE)
  if (args[i] != '--config') return(sub('^--config=', '', args[i]))
  if (i == length(args)) stop('--config needs a file name', call. = FALSE)
  args[i + 1]
}

# Read, check and return the configuration as a list with sections analysis,
# presentation and run. Stops with every problem found.
load_config = function(path = config_path()) {
  if (!file.exists(path)) stop('config file not found: ', path, call. = FALSE)
  cfg = tryCatch(source(path, local = new.env(parent = baseenv()))$value,
                 error = function(e) stop('config file ', path, ' could not be read: ', conditionMessage(e), call. = FALSE))
  errs = config_schema_errors(cfg, CONFIG_SCHEMA)
  if (!length(errs)) errs = config_combination_errors(cfg)
  if (length(errs)) stop('config file ', path, ':\n  ', paste(errs, collapse = '\n  '), call. = FALSE)
  cfg
}

# The configuration as one JSON string, for run manifests. Every number at full
# precision (jsonlite's default rounds to 4 significant digits); every vector, even of
# length one, as an array, so a one-element setting reads back as what it is; a named
# vector as an object, because jsonlite would otherwise drop the names without a word.
config_json = function(cfg) {
  keep_names = function(x) {
    if (is.list(x)) return(lapply(x, keep_names))
    if (is.atomic(x) && !is.null(names(x))) return(as.list(x))
    x
  }
  as.character(jsonlite::toJSON(keep_names(cfg), digits = NA, auto_unbox = FALSE, null = 'null'))
}
