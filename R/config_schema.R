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

CONFIG_SCHEMA = list(
  analysis = list(
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
                       diagnostics = MAP_WINDOW_SCHEMA)))

# Rules across keys. Called only once every key has passed its own check.
config_combination_errors = function(cfg) {
  errs = character()
  # Script 9's slide-layout figure and its variant-sensitivity table use HadGEM3.
  if (!'hadgem' %in% cfg$analysis$forcing$report_kernels)
    errs = c(errs, 'analysis$forcing$report_kernels must include hadgem: script 9 draws its headline figure and the variant sensitivity with it')
  errs
}
