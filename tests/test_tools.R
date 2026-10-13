# Tests for tools/frozen.R and tools/check_anchors.R. Run: Rscript tests/test_tools.R
source("tools/frozen.R")
source("tools/check_anchors.R")
source("R/config.R"); source("R/paths.R")

# Locking: a locked file cannot be overwritten; unlocking restores it.
f <- tempfile(); writeLines("a", f)
set_writable(f, FALSE)
locked_write_fails <- inherits(try(suppressWarnings(writeLines("b", f)), silent = TRUE), "try-error")
set_writable(f, TRUE); writeLines("c", f)
stopifnot(locked_write_fails, identical(readLines(f), "c"))

# check_anchors: a run directory with one product made there and one seeded from the
# baseline; only the first may be reported as a match.
anchor <- "tests/anchors/interp-forcing-2026-10-08"
run <- file.path(tempdir(), "anchors_test_run")
alt <- suppressMessages(pipeline_paths(load_config(), redirected_paths(run)))
invisible(file.copy(file.path(anchor, "forcing_by_period.csv"), path_file(alt, "forcing_by_period")))
invisible(seed_run_dir(alt, "output/forcing/modern_ipcc_ar6_erf.csv"))
out <- capture.output(res <- check_anchors(anchor, run))
stopifnot(
  isTRUE(res),
  any(grepl("^forcing_by_period.csv +MATCH", out)),
  any(grepl("^modern_ipcc_ar6_erf.csv +seeded from the baseline", out)),
  any(grepl("^forcing_by_period_variant_sensitivity.csv +not produced", out))
)
writeLines("changed", path_file(alt, "forcing_by_period"))
out <- capture.output(res <- check_anchors(anchor, run))
stopifnot(!res, any(grepl("^forcing_by_period.csv +DIFFER", out)))
unlink(run, recursive = TRUE)
cat("test_tools: all tests passed\n")
