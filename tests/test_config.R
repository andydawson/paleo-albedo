# Tests for R/config.R and R/config_schema.R. Run from the repository root: Rscript tests/test_config.R
source("R/config.R")
base_text <- readLines("config.R")

# Load a config made by applying one substitution to config.R (or from given text).
load_variant <- function(from = NULL, to = NULL, text = NULL) {
  if (is.null(text)) {
    stopifnot(sum(grepl(from, base_text, fixed = TRUE)) == 1)   # the edit hits exactly one line
    text <- sub(from, to, base_text, fixed = TRUE)
  }
  f <- tempfile(fileext = ".R"); writeLines(text, f); on.exit(unlink(f))
  load_config(f)
}
# TRUE if loading fails with a message matching `pattern`.
rejects <- function(pattern, ...) {
  msg <- tryCatch({ load_variant(...); "" }, error = conditionMessage)
  grepl(pattern, msg)
}

cfg <- load_config()

stopifnot(
  # the shipped config loads and holds today's script settings
  identical(names(cfg), c("analysis", "presentation", "run")),
  is.null(cfg$run$dir),
  "dir" %in% names(cfg$run),
  identical(cfg$analysis$coarse$ages, c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),
  identical(cfg$analysis$forcing$earth_area_m2, 5.101e14),
  identical(cfg$analysis$forcing$latitude_band_width, 10),
  identical(cfg$presentation$map_windows$calibration, list(x = c(-172, -50), y = c(15, 80))),
  identical(cfg$presentation$map_windows$tail,        list(x = c(-166, -50), y = c(12, 82))),
  identical(cfg$presentation$map_windows$diagnostics, list(x = c(-172, -50), y = c(17, 79))),

  # the file is ordinary R (plain source() since issue I06): parse errors, a value that is
  # not a list, and anything the schema refuses are reported; the file cannot see the caller
  rejects("could not be read",            'earth_area_m2 = 5.101e14', 'earth_area_m2 = 5.101e14 +'),
  rejects("cfg must be a list",           text = c("x <- 1")),
  rejects("must be finite numbers",       'earth_area_m2 = 5.101e14', 'earth_area_m2 = "big"'),
  identical(load_variant('months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")',
                         'months = tolower(month.abb)')$analysis$months, MONTHS),
  { secret_variable_of_the_caller <- 1
    rejects("could not be read", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = secret_variable_of_the_caller') },
  # allowed constructors and arithmetic evaluate as R would
  identical(load_variant('ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000),',
                         'ages = c(50, seq(500, 2000, by = 1500), 2 * 2000, 6000, 8000, 10000, 12000),')$analysis$coarse$ages,
            cfg$analysis$coarse$ages),
  identical(load_variant('earth_area_m2 = 5.101e14', 'earth_area_m2 = 5.101 * 10^14')$analysis$forcing$earth_area_m2,
            5.101 * 10^14),

  # the schema: unknown, missing and duplicated keys
  rejects("analysis\\$forcing has unknown key\\(s\\) kernel",  'earth_area_m2 = 5.101e14,', 'earth_area_m2 = 5.101e14, kernel = "cam5",'),
  rejects("analysis\\$forcing is missing key\\(s\\) variant", 'variant = "veg_ice_parts",', ''),
  rejects("has earth_area_m2 more than once", 'earth_area_m2 = 5.101e14,', 'earth_area_m2 = 5.101e14, earth_area_m2 = 1,'),
  rejects("analysis\\$domain must be a list", 'domain = list(lon_min = -170, lat_min = 27, lat_max = 74, lon_max_coarse = -60),',
          'domain = c(50, 500),'),
  rejects("must be a value, not a list", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = list(5.101e14)'),
  # illegal values
  rejects("variant has 'veg_ice'; legal values", 'variant = "veg_ice_parts"', 'variant = "veg_ice"'),
  rejects("variant must be a single value", 'variant = "veg_ice_parts"', 'variant = c("veg_thresh", "ice_thresh")'),
  rejects("report_kernels must not repeat", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c("hadgem", "cam5", "cam5")'),
  rejects("report_kernels must be text, not empty", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c()'),
  rejects("ages must be strictly increasing", '2000, 4000, 6000', '4000, 2000, 6000'),
  rejects("ages must have at least 2 values", 'ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000),',
          'ages = 50,'),
  rejects("ages must be finite numbers", '2000, 4000, 6000', '2000, NA, 6000'),
  rejects("ages must be finite numbers", '2000, 4000, 6000', '2000, "4000", 6000'),
  rejects("ages must not have names", '2000, 4000, 6000', '2000, mid = 4000, 6000'),
  rejects("latitude_band_width must lie between 1 and 90", 'latitude_band_width = 10', 'latitude_band_width = 0'),
  rejects("latitude_band_width must be a whole number", 'latitude_band_width = 10', 'latitude_band_width = 2.5'),
  rejects("earth_area_m2 must have exactly 1 values", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = c(1, 2)'),
  rejects("tail\\$x must be strictly increasing", 'list(x = c(-166, -50)', 'list(x = c(-50, -166)'),
  rejects("tail\\$y must lie between -90 and 90", 'y = c(12, 82)', 'y = c(12, 95)'),
  rejects("calibration\\$x must have exactly 2 values", 'list(x = c(-172, -50), y = c(15, 80))', 'list(x = c(-172), y = c(15, 80))'),
  # the run directory
  identical(load_variant('dir = NULL', 'dir = ".rundirs/median_test"')$run$dir, ".rundirs/median_test"),
  rejects("run\\$dir must not be inside the repository's output/", 'dir = NULL', 'dir = "output/alt"'),
  rejects("run\\$dir must not contain '..'", 'dir = NULL', 'dir = "../elsewhere"'),
  rejects("run\\$dir must be NULL or one directory path", 'dir = NULL', 'dir = c("a", "b")'),
  rejects("run is missing key\\(s\\) dir", 'dir = NULL,', ''),
  # the model, draw and reduction settings (issue I04a)
  identical(cfg$analysis$land_cover$reduction, "mean"),
  identical(cfg$analysis$calibration$model_id, "mod8"),
  identical(cfg$analysis$draws, list(evaluation = 100, prediction = 100, seed = "random")),
  identical(load_variant('reduction = "mean"', 'reduction = "median"')$analysis$land_cover$reduction, "median"),
  rejects("land_cover\\$reduction has 'trimmed'", 'reduction = "mean"', 'reduction = "trimmed"'),
  identical(load_variant('model_id = "mod8"', 'model_id = "mod7"')$analysis$calibration$model_id, "mod7"),
  rejects("model_id has 'spatial'", 'model_id = "mod8"', 'model_id = "spatial"'),
  rejects("model_id must be a single value", 'model_id = "mod8"', 'model_id = c("mod7", "mod8")'),
  rejects("draws\\$evaluation must lie between 2", 'evaluation = 100', 'evaluation = 1'),
  rejects("draws\\$prediction must be a whole number", 'prediction = 100', 'prediction = 100.5'),
  identical(load_variant('seed = "random"', 'seed = 20261014')$analysis$draws$seed, 20261014),
  identical(load_variant('seed = "random"', 'seed = 0')$analysis$draws$seed, 0),
  rejects("draws\\$seed must be \"random\" or one whole number", 'seed = "random"', 'seed = "fixed"'),
  rejects("draws\\$seed must be \"random\" or one whole number", 'seed = "random"', 'seed = 1.5'),
  rejects("draws\\$seed must be \"random\" or one whole number", 'seed = "random"', 'seed = 2^31'),
  # months, ages, slice alias, domain, ice and kernels (issue I04b)
  identical(cfg$analysis$months, MONTHS),
  identical(cfg$analysis$ages, c(50, 200, seq(500, 11500, by = 500))),
  identical(cfg$analysis$coarse$slice_alias, c(11500, 12000)),
  identical(load_variant('months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")',
                         'months = c("feb", "may", "aug", "nov")')$analysis$months, c("feb", "may", "aug", "nov")),
  rejects("months.*unknown month name", 'months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")', 'months = c("jan", "January")'),
  rejects("months.*more than once", 'months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")', 'months = c("jan", "jan")'),
  rejects("analysis\\$ages must be strictly increasing", 'ages = c(50, 200, seq(500, 11500, by = 500))', 'ages = c(200, 50)'),
  is.null(load_variant('slice_alias = c(11500, 12000)', 'slice_alias = NULL')$analysis$coarse$slice_alias),
  rejects("slice_alias must have exactly 2 values", 'slice_alias = c(11500, 12000)', 'slice_alias = 11500'),
  rejects("slice_alias: its first value must be one of analysis\\$ages", 'slice_alias = c(11500, 12000)', 'slice_alias = c(11600, 12000)'),
  rejects("slice_alias: its second value must be one of analysis\\$coarse\\$ages", 'slice_alias = c(11500, 12000)', 'slice_alias = c(11500, 13000)'),
  rejects("lat_min must be below lat_max", 'lat_min = 27, lat_max = 74', 'lat_min = 74, lat_max = 27'),
  rejects("lon_min must be west of lon_max_coarse", 'lon_min = -170', 'lon_min = -50'),
  rejects("domain\\$lat_max must lie between", 'lat_min = 27, lat_max = 74', 'lat_min = 27, lat_max = 95'),
  identical(load_variant('threshold = 0.5', 'threshold = 0.6')$analysis$ice$threshold, 0.6),
  rejects("ice\\$threshold must lie between 0 and 1", 'threshold = 0.5', 'threshold = 1.5'),
  identical(load_variant('albedo_coarse = "ice_albedo"', 'albedo_coarse = "ice_albedo_sc"')$analysis$ice$albedo_coarse, "ice_albedo_sc"),
  rejects("albedo_fixed has 'albedo'", 'albedo_fixed  = "ice_albedo_fixed"', 'albedo_fixed  = "albedo"'),
  identical(load_variant('cack_year       = 2003', 'cack_year       = 2010')$analysis$kernels$cack_year, 2010),
  rejects("cack_year must lie between 2001 and 2016", 'cack_year       = 2003', 'cack_year       = 2000'),
  rejects("cack_year must be a whole number", 'cack_year       = 2003', 'cack_year       = 2003.5'),
  # kernel variables and the difference method (issue I16); the legacy configuration loads
  identical(cfg$analysis$kernels[c("hadgem_variable", "cam5_variable", "cack_variable")],
            list(hadgem_variable = "albedo_sw", cam5_variable = "FSNT", cack_variable = "CACK CM")),
  identical(cfg$analysis$ice$difference_method, "endpoint"),
  rejects("hadgem_variable has 'FSNT'", 'kernels = list(hadgem_variable = "albedo_sw",', 'kernels = list(hadgem_variable = "FSNT",'),
  rejects("cam5_variable has 'albedo_sw'", 'cam5_variable   = "FSNT",', 'cam5_variable   = "albedo_sw",'),
  rejects("difference_method has 'fancy'", 'difference_method = "endpoint",', 'difference_method = "fancy",'),
  identical(load_config("configs/legacy_config.R")$analysis$kernels$cam5_variable, "FSNSC"),
  identical(load_config("configs/legacy_config.R")$analysis$draws$seed, 1),
  # the ladder's comparison method (issue I16): ML, the original REML in the legacy file
  identical(cfg$analysis$calibration$comparison_method, "ML"),
  identical(load_config("configs/legacy_config.R")$analysis$calibration$comparison_method, "REML"),
  rejects("comparison_method has 'GCV'", 'comparison_method = "ML"', 'comparison_method = "GCV"'),
  identical(cfg$analysis$calibration$ice_mask, "centre_in_1ka_polygon"),
  identical(load_config("configs/legacy_config.R")$analysis$calibration$ice_mask, "none"),
  rejects("ice_mask has 'all'", 'ice_mask = "centre_in_1ka_polygon"', 'ice_mask = "all"'),
  # the stale-input override (issue I05): off by default, TRUE or FALSE only
  identical(cfg$run$allow_stale, FALSE),
  # script 1's diagnostic switch (issue I06)
  identical(cfg$run$diagnostic, FALSE),
  isTRUE(load_variant('diagnostic = FALSE', 'diagnostic = TRUE')$run$diagnostic),
  rejects("run\\$diagnostic must be TRUE or FALSE", 'diagnostic = FALSE', 'diagnostic = "yes"'),
  isTRUE(load_variant('allow_stale = FALSE', 'allow_stale = TRUE')$run$allow_stale),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = 1'),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = c(TRUE, FALSE)'),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = NA'),
  # the headline forcing (Chris, 2026-10-19): partial ice, the original all-or-nothing in
  # the legacy file; only the four totals are accepted, never a vegetation or ice part
  identical(cfg$analysis$forcing$variant, "veg_ice_parts"),
  identical(load_config("configs/legacy_config.R")$analysis$forcing$variant, "veg_ice_thresh"),
  identical(load_variant('variant = "veg_ice_parts"', 'variant = "veg_icesc_thresh"')$analysis$forcing$variant, "veg_icesc_thresh"),
  rejects("forcing\\$variant has 'veg_thresh'", 'variant = "veg_ice_parts"', 'variant = "veg_thresh"'),
  rejects("forcing\\$variant has 'ice_part'", 'variant = "veg_ice_parts"', 'variant = "ice_part"'),
  # unsupported combinations
  rejects("report_kernels must include hadgem", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c("cam5", "cack")'),
  # every problem is reported at once, not just the first
  rejects("variant has.*\n.*report_kernels must not repeat",
          text = sub('"cam5", "cack")', '"cam5", "cam5")', sub('variant = "veg_ice_parts"', 'variant = "nope"', base_text, fixed = TRUE), fixed = TRUE))
)

# JSON for the manifests: full precision, one-element vectors kept as arrays, names kept,
# and stable: reading the JSON back and writing it again gives the same text.
j <- config_json(cfg)
back <- jsonlite::fromJSON(j, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
named <- list(a = c(`11500` = 12000), b = "hadgem", c = 0.123456789012345, d = c(-172, -50))
jn <- config_json(named)
stopifnot(
  grepl('"earth_area_m2":[510100000000000]', j, fixed = TRUE),
  grepl('"variant":["veg_ice_parts"]', j, fixed = TRUE),
  identical(config_json(back), j),
  isTRUE(all.equal(back, cfg)),
  identical(jn, '{"a":{"11500":[12000]},"b":["hadgem"],"c":[0.123456789012345],"d":[-172,-50]}'),
  identical(config_json(jsonlite::fromJSON(jn, simplifyVector = TRUE)), jn)
)
# Which config file: config.R by default, or the one named with --config.
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
stopifnot(
  identical(config_path(character()), "config.R"),
  identical(config_path(c("--config", "configs/median.R")), "configs/median.R"),
  identical(config_path(c("--config=configs/median.R")), "configs/median.R"),
  identical(config_path(c("other", "--config", "a.R", "x")), "a.R"),
  fails(config_path(c("--config"))),
  fails(config_path(c("--config", "a.R", "--config=b.R")))
)
cat("test_config: all tests passed\n")
