# The schema of config.R: every key the pipeline accepts and the rule its value must meet.
#
# A key is added here only when a script reads it (single-config migration, issue I03 and
# later). The tree mirrors the config: a list node lists its keys, and the config must
# have exactly those keys, no more and no fewer; a function node checks one value and
# returns NULL if it is acceptable or a sentence saying what is wrong. Rules that involve
# more than one key are in config_combination_errors() at the end.
#
# Values are plain unnamed vectors throughout: names would be dropped silently by the
# JSON written to run manifests unless config_json() keeps them, and no key so far needs
# them. Written 2026-10-13.

# The calibration model ids (LADDER_MODELS) are defined with their formulas.
source('R/model_registry.R')

# Script 8 computes every forcing as rf_<kernel>_<variant>; these are the ten variants
# (scripts/8_radiative.R, the rf_* columns) and the three kernels.
FORCING_VARIANTS = c('veg_thresh', 'ice_thresh', 'icesc_thresh',
                     'veg_ice_thresh', 'veg_icesc_thresh',
                     'veg_part', 'ice_part', 'icesc_part',
                     'veg_ice_parts', 'veg_icesc_parts')
FORCING_KERNELS  = c('hadgem', 'cam5', 'cack')

# Numbers: no NA, no names, optionally a fixed length, strictly increasing, or a range.
check_numbers = function(v, len = NULL, min_len = 1, increasing = FALSE, range = NULL) {
  if (!is.numeric(v) || anyNA(v) || any(!is.finite(v))) return('must be finite numbers')
  if (!is.null(names(v)))                      return('must not have names')
  if (!is.null(len) && length(v) != len)       return(sprintf('must have exactly %d values', len))
  if (length(v) < min_len)                     return(sprintf('must have at least %d values', min_len))
  if (increasing && any(diff(v) <= 0))         return('must be strictly increasing')
  if (!is.null(range) && any(v < range[1] | v > range[2]))
    return(sprintf('must lie between %s and %s', range[1], range[2]))
  NULL
}

# Text from a fixed list of legal values: no NA, no names, no duplicates.
check_choice = function(v, legal, single = FALSE) {
  if (!is.character(v) || anyNA(v) || !length(v)) return('must be text, not empty')
  if (!is.null(names(v)))                         return('must not have names')
  if (single && length(v) != 1)                   return('must be a single value')
  if (anyDuplicated(v))                           return('must not repeat a value')
  bad = setdiff(v, legal)
  if (length(bad)) return(sprintf('has %s; legal values are %s',
                                  paste(sQuote(bad, FALSE), collapse = ', '),
                                  paste(legal, collapse = ', ')))
  NULL
}

# A map window: longitude and latitude bounds in degrees, each low then high.
MAP_WINDOW_SCHEMA = list(x = function(v) check_numbers(v, len = 2, increasing = TRUE, range = c(-180, 180)),
                         y = function(v) check_numbers(v, len = 2, increasing = TRUE, range = c(-90, 90)))

# The run directory: NULL for the repository's own locations, or one relative or absolute
# directory path that is not itself one of the baseline locations.
check_run_dir = function(v) {
  if (is.null(v)) return(NULL)
  if (!is.character(v) || length(v) != 1 || is.na(v) || !nzchar(v)) return('must be NULL or one directory path')
  if (!is.null(names(v))) return('must not have names')
  top = strsplit(gsub('\\\\', '/', v), '/', fixed = TRUE)[[1]]
  top = top[nzchar(top) & top != '.']
  if (!length(top)) return('must not be the repository itself; use NULL for the baseline locations')
  absolute = grepl('^(/|\\\\|[A-Za-z]:)', v)
  if (!absolute && top[1] %in% c('data', 'output', 'figures', 'runs', 'scripts', 'R', 'tests'))
    return(sprintf("must not be inside the repository's %s/ directory", top[1]))
  if ('..' %in% top) return("must not contain '..'")
  NULL
}

# A count of draws: one whole number, at least 2 (a standard deviation needs two).
check_draws = function(v) {
  msg = check_numbers(v, len = 1, range = c(2, 1e5))
  if (is.null(msg) && v != round(v)) msg = 'must be a whole number'
  msg
}

# The random seed: "random", or one whole number from 0 to 2147483647 (R's seed range).
check_seed = function(v) {
  if (identical(v, 'random')) return(NULL)
  if (is.numeric(v) && length(v) == 1 && is.null(names(v)) && is.finite(v) && v == round(v) &&
      v >= 0 && v <= 2147483647) return(NULL)
  'must be "random" or one whole number from 0 to 2147483647'
}

# A setting that is TRUE or FALSE.
check_flag = function(v) if (!is.logical(v) || length(v) != 1 || is.na(v)) 'must be TRUE or FALSE'

CONFIG_SCHEMA = list(
  analysis = list(
    land_cover = list(
      reduction = function(v) check_choice(v, c('mean', 'median'), single = TRUE)),
    calibration = list(
      model_id = function(v) check_choice(v, LADDER_MODELS, single = TRUE)),
    draws = list(
      evaluation = check_draws,
      prediction = check_draws,
      seed       = check_seed),
    coarse = list(
      ages = function(v) check_numbers(v, min_len = 2, increasing = TRUE, range = c(0, 100000))),
    forcing = list(
      earth_area_m2        = function(v) check_numbers(v, len = 1, range = c(1, Inf)),
      variant              = function(v) check_choice(v, FORCING_VARIANTS, single = TRUE),
      report_kernels       = function(v) check_choice(v, FORCING_KERNELS),
      sensitivity_variants = function(v) check_choice(v, FORCING_VARIANTS))),
  presentation = list(
    map_windows = list(calibration = MAP_WINDOW_SCHEMA,
                       tail        = MAP_WINDOW_SCHEMA,
                       diagnostics = MAP_WINDOW_SCHEMA)),
  run = list(
    dir         = check_run_dir,
    allow_stale = check_flag))

# Rules across keys. Called only once every key has passed its own check.
config_combination_errors = function(cfg) {
  errs = character()
  # Script 9's slide-layout figure and its variant-sensitivity table use HadGEM3.
  if (!'hadgem' %in% cfg$analysis$forcing$report_kernels)
    errs = c(errs, 'analysis$forcing$report_kernels must include hadgem: script 9 draws its headline figure and the variant sensitivity with it')
  errs
}
