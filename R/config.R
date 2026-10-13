# Load and check the project configuration, config.R at the repository root.
#
# Usage, in any script:
#   source('R/config.R')
#   cfg = load_config()          # config.R, or the file given with --config <file>
#   cfg$analysis$forcing$variant
#
# config.R is one R list expression, but it is not run as R code. The loader reads it as
# a syntax tree and accepts only list(), c(), seq(), arithmetic, parentheses and literal
# numbers, text, TRUE/FALSE/NULL/NA. Anything else (an assignment, any other function,
# a variable, pkg::fun, a file or environment read) is refused before evaluation, and the
# expression is then evaluated in an environment holding only those functions. A config
# therefore cannot depend on anything but its own text, so the text in a run manifest is
# a complete record of the settings. The result is then checked against the schema in
# R/config_schema.R: unknown or missing keys and illegal values stop the script with a
# message naming every problem. Single-config migration, issue I03, 2026-10-13.

source('R/config_schema.R')

# The only functions config.R may call.
CONFIG_FUNCTIONS = c('list', 'c', 'seq', '+', '-', '*', '/', '^', '(')

# Every disallowed construct in an expression, as messages; empty if it is acceptable.
config_syntax_errors = function(e) {
  if (is.call(e)) {
    f = e[[1]]
    if (!is.symbol(f) || !as.character(f) %in% CONFIG_FUNCTIONS) {
      what = paste(deparse(f), collapse = ' ')
      return(sprintf('`%s` is not allowed; config.R may use only %s and literal values', what,
                     paste(CONFIG_FUNCTIONS, collapse = ' ')))
    }
    args = as.list(e)[-1]
    if (any(vapply(args, function(a) identical(a, quote(expr = )), logical(1))))
      return(sprintf('empty argument in %s(...)', as.character(f)))
    return(unlist(lapply(args, config_syntax_errors)))
  }
  if (is.symbol(e)) return(sprintf('`%s` is not allowed: config.R may not refer to variables', as.character(e)))
  if (is.null(e) || (is.atomic(e) && length(e) == 1)) return(character())
  sprintf('unsupported element `%s`', paste(deparse(e), collapse = ' '))
}

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
  exprs = tryCatch(parse(path, keep.source = FALSE),
                   error = function(e) stop('config file ', path, ' does not parse: ', conditionMessage(e), call. = FALSE))
  if (length(exprs) != 1)
    stop('config file ', path, ' must contain exactly one expression, a list(...); it has ', length(exprs), call. = FALSE)
  errs = config_syntax_errors(exprs[[1]])
  if (length(errs)) stop('config file ', path, ':\n  ', paste(unique(errs), collapse = '\n  '), call. = FALSE)
  env = new.env(parent = emptyenv())
  for (f in CONFIG_FUNCTIONS) assign(f, get(f, envir = baseenv()), envir = env)
  cfg = eval(exprs[[1]], env)
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
