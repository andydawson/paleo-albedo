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
  identical(cfg$presentation$map_windows$calibration, list(x = c(-172, -50), y = c(15, 80))),
  identical(cfg$presentation$map_windows$tail,        list(x = c(-166, -50), y = c(12, 82))),
  identical(cfg$presentation$map_windows$diagnostics, list(x = c(-172, -50), y = c(17, 79))),

  # the language: one list expression of allowed functions and literals only
  rejects("exactly one expression", text = c("x <- 1", base_text)),
  rejects("`<-` is not allowed",    text = c("x <- list()")),
  rejects("`Sys.getenv` is not allowed", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = Sys.getenv("A")'),
  rejects("`readRDS` is not allowed",    'variant = "veg_ice_thresh"', 'variant = readRDS("v.RDS")'),
  rejects("`base::c` is not allowed",    'report_kernels = c(', 'report_kernels = base::c('),
  rejects("`pi` is not allowed: config.R may not refer to variables", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = pi'),
  rejects("`T` is not allowed",          'earth_area_m2 = 5.101e14', 'earth_area_m2 = T'),
  rejects("`\\(function\\(\\) 1\\)` is not allowed",   'earth_area_m2 = 5.101e14', 'earth_area_m2 = (function() 1)()'),
  rejects("does not parse",              'earth_area_m2 = 5.101e14', 'earth_area_m2 = 5.101e14 +'),
  # allowed constructors and arithmetic evaluate as R would
  identical(load_variant('coarse = list(ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),',
                         'coarse = list(ages = c(50, seq(500, 2000, by = 1500), 2 * 2000, 6000, 8000, 10000, 12000)),')$analysis$coarse$ages,
            cfg$analysis$coarse$ages),
  identical(load_variant('earth_area_m2 = 5.101e14', 'earth_area_m2 = 5.101 * 10^14')$analysis$forcing$earth_area_m2,
            5.101 * 10^14),

  # the schema: unknown, missing and duplicated keys
  rejects("analysis\\$forcing has unknown key\\(s\\) kernel",  'earth_area_m2 = 5.101e14,', 'earth_area_m2 = 5.101e14, kernel = "cam5",'),
  rejects("analysis\\$forcing is missing key\\(s\\) variant", 'variant = "veg_ice_thresh",', ''),
  rejects("has earth_area_m2 more than once", 'earth_area_m2 = 5.101e14,', 'earth_area_m2 = 5.101e14, earth_area_m2 = 1,'),
  rejects("analysis\\$coarse must be a list", 'coarse = list(ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),',
          'coarse = c(50, 500),'),
  rejects("must be a value, not a list", 'earth_area_m2 = 5.101e14', 'earth_area_m2 = list(5.101e14)'),
  # illegal values
  rejects("variant has 'veg_ice'; legal values", 'variant = "veg_ice_thresh"', 'variant = "veg_ice"'),
  rejects("variant must be a single value", 'variant = "veg_ice_thresh"', 'variant = c("veg_thresh", "ice_thresh")'),
  rejects("report_kernels must not repeat", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c("hadgem", "cam5", "cam5")'),
  rejects("report_kernels must be text, not empty", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c()'),
  rejects("ages must be strictly increasing", '2000, 4000, 6000', '4000, 2000, 6000'),
  rejects("ages must have at least 2 values", 'coarse = list(ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),',
          'coarse = list(ages = 50),'),
  rejects("ages must be finite numbers", '2000, 4000, 6000', '2000, NA, 6000'),
  rejects("ages must be finite numbers", '2000, 4000, 6000', '2000, "4000", 6000'),
  rejects("ages must not have names", '2000, 4000, 6000', '2000, mid = 4000, 6000'),
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
  # the stale-input override (issue I05): off by default, TRUE or FALSE only
  identical(cfg$run$allow_stale, FALSE),
  isTRUE(load_variant('allow_stale = FALSE', 'allow_stale = TRUE')$run$allow_stale),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = 1'),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = c(TRUE, FALSE)'),
  rejects("run\\$allow_stale must be TRUE or FALSE", 'allow_stale = FALSE', 'allow_stale = NA'),
  # unsupported combinations
  rejects("report_kernels must include hadgem", 'report_kernels = c("hadgem", "cam5", "cack")', 'report_kernels = c("cam5", "cack")'),
  # every problem is reported at once, not just the first
  rejects("variant has.*\n.*report_kernels must not repeat",
          text = sub('"cam5", "cack")', '"cam5", "cam5")', sub('= "veg_ice_thresh"', '= "nope"', base_text, fixed = TRUE), fixed = TRUE))
)

# JSON for the manifests: full precision, one-element vectors kept as arrays, names kept,
# and stable: reading the JSON back and writing it again gives the same text.
j <- config_json(cfg)
back <- jsonlite::fromJSON(j, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
named <- list(a = c(`11500` = 12000), b = "hadgem", c = 0.123456789012345, d = c(-172, -50))
jn <- config_json(named)
stopifnot(
  grepl('"earth_area_m2":[510100000000000]', j, fixed = TRUE),
  grepl('"variant":["veg_ice_thresh"]', j, fixed = TRUE),
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
