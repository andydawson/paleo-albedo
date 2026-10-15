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
result_of <- function(res, f) res$result[res$file == f]
stopifnot(
  identical(result_of(res, "forcing_by_period.csv"), "MATCH"),
  identical(result_of(res, "modern_ipcc_ar6_erf.csv"), "SEEDED"),
  identical(result_of(res, "forcing_by_period_variant_sensitivity.csv"), "NOT PRODUCED"),
  any(grepl("^forcing_by_period.csv +MATCH", out)),
  anchors_ok(res),                          # a partial run passes on what it produced
  !anchors_ok(res, require_all = TRUE)      # but does not reproduce the whole anchor
)
writeLines("changed", path_file(alt, "forcing_by_period"))
out <- capture.output(res <- check_anchors(anchor, run))
stopifnot(identical(result_of(res, "forcing_by_period.csv"), "DIFFER"), !anchors_ok(res),
          any(grepl("^forcing_by_period.csv +DIFFER \\(anchor", out)))
# The command line: the default rule passes this partial run's MATCH, --all fails it.
writeLines(readLines(file.path(anchor, "forcing_by_period.csv")), path_file(alt, "forcing_by_period"))
rscript <- file.path(R.home("bin"), "Rscript")
status <- function(...) system2(rscript, c("tools/check_anchors.R", ...), stdout = FALSE, stderr = FALSE)
stopifnot(status(anchor, run) == 0, status(anchor, run, "--all") == 1)
# A file in an anchor with no entry in live_paths.tsv is NO MAPPING, which fails.
odd <- file.path(tempdir(), "anchor_unmapped"); dir.create(odd, showWarnings = FALSE)
writeLines("0123456789abcdef0123456789abcdef  not_in_live_paths.csv", file.path(odd, "MD5SUMS.txt"))
out <- capture.output(res <- check_anchors(odd, run))
stopifnot(identical(res$result, "NO MAPPING"), !anchors_ok(res))
# A run directory with nothing in it (for example a mistyped one) fails: nothing was compared.
out <- capture.output(res <- check_anchors(anchor, file.path(tempdir(), "no_such_run_dir")))
stopifnot(all(res$result == "NOT PRODUCED"), !anchors_ok(res),
          status(anchor, file.path(tempdir(), "no_such_run_dir")) == 1)
unlink(c(run, odd), recursive = TRUE)
cat("test_tools: all tests passed\n")
