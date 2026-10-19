# Tests for R/forcing_components.R (2026-10-19). Run from the repository root:
#   Rscript tests/test_forcing_components.R
#
# Hand-made cells, one month each, slices in ascending age (row 1 is the youngest), passed
# through R/differences.R as script 7a does. The albedo differences stand in for the forcing
# (a kernel of 1), which is enough: script 8 multiplies all three columns by the same kernel.
source("R/differences.R")
source("R/forcing_components.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

# A cell as script 7a builds it (as in tests/test_differences.R): glacier albedo 0.68 (fixed)
# or 0.73 (month-by-month); the *_thresh glacier albedo is NA where the slice is vegetation.
cell <- function(ice_frac, alb_veg, threshold = 0.5) {
  is_veg <- !is.na(ice_frac) & ice_frac <= threshold
  data.frame(ice_frac = ice_frac, alb_veg = alb_veg, ice_albedo_fixed = 0.68, ice_albedo_sc = 0.73,
             alb_ice_thresh = ifelse(is_veg, NA, 0.68), alb_icesc_thresh = ifelse(is_veg, NA, 0.73))
}
# The pairs of several cells, with the three threshold columns renamed as script 8 names
# them for a kernel ('test').
pairs_of <- function(cells, method) {
  rows <- lapply(cells, function(c) {
    invisible(capture.output(r <- pair_albedo_differences(do.call(cell, c), 0.5, method)))
    data.frame(ice_frac_young = r$ice_frac_young, ice_frac_old = r$ice_frac_old,
               rf_test_veg_thresh = r$alb_diff_veg_thresh, rf_test_ice_thresh = r$alb_diff_ice_thresh,
               rf_test_veg_ice_thresh = r$alb_diff_veg_ice_thresh)
  })
  do.call(rbind, rows)
}

cells <- list(
  vegetation = list(ice_frac = c(0, 0),               alb_veg = c(0.20, 0.25)),
  retreat    = list(ice_frac = c(0.1, 0.9),           alb_veg = c(0.20, 0.30)),
  still_ice  = list(ice_frac = c(0.9, 0.95),          alb_veg = c(0.20, 0.30)),
  advance    = list(ice_frac = c(0.95, 0.95, 0.9, 0.1), alb_veg = c(0.20, 0.25, 0.30, 0.35)),  # pair 3: 0.525 young, 0.1 old
  at_thresh  = list(ice_frac = c(0.5, 0.5),           alb_veg = c(0.20, 0.25)),
  missing    = list(ice_frac = c(0.2, NA),            alb_veg = c(0.20, 0.25)))

# 1. "endpoint": every known pair is exactly one kind of step, and the kind matches the
#    readvance-adjusted ice fractions (ice step = ice above 0.5 at either end).
d <- pairs_of(cells, "endpoint")
known <- !is.na(d$rf_test_veg_ice_thresh)
ice_step <- d$ice_frac_young > 0.5 | d$ice_frac_old > 0.5
stopifnot(sum(!known) == 1,                                              # only the missing pair
          all(xor(!is.na(d$rf_test_veg_thresh), !is.na(d$rf_test_ice_thresh))[known]),
          identical((!is.na(d$rf_test_ice_thresh))[known], ice_step[known]))

# 2. After completion: no NA in a known pair, each part 0 where it does not apply, and the
#    parts add up to the total, pair by pair and summed (a period sum telescopes the same way).
k <- complete_threshold_components(d, "test", "endpoint")
stopifnot(!anyNA(k$rf_test_veg_thresh[known]), !anyNA(k$rf_test_ice_thresh[known]),
          all(k$rf_test_veg_thresh[known & ice_step] == 0), all(k$rf_test_ice_thresh[known & !ice_step] == 0),
          same(k$rf_test_veg_thresh + k$rf_test_ice_thresh, k$rf_test_veg_ice_thresh),
          same(sum(k$rf_test_veg_thresh[known]) + sum(k$rf_test_ice_thresh[known]), sum(k$rf_test_veg_ice_thresh[known])),
          is.na(k$rf_test_veg_thresh[!known]), is.na(k$rf_test_ice_thresh[!known]))
# The advance counts as ice: vegetation albedo before (0.35) to glacier albedo now (0.68).
adv <- which(abs(d$ice_frac_young - 0.525) < 1e-12 & abs(d$ice_frac_old - 0.1) < 1e-12)
stopifnot(length(adv) == 1, same(k$rf_test_ice_thresh[adv], 0.68 - 0.35), k$rf_test_veg_thresh[adv] == 0)

# 3. The original arithmetic: its known anomalies (question C2) are left untouched.
#    The advance has both parts NA and a total of 0; the double count (tests/test_differences.R
#    case 7b) has both parts set.
l <- pairs_of(c(cells, list(double_count = list(ice_frac = c(0.22, 0.60, 0.48), alb_veg = c(0.20, 0.25, 0.30)))),
              "legacy_components")
stopifnot(any(is.na(l$rf_test_veg_thresh) & is.na(l$rf_test_ice_thresh) & l$rf_test_veg_ice_thresh %in% 0),
          any(!is.na(l$rf_test_veg_thresh) & !is.na(l$rf_test_ice_thresh)),
          identical(complete_threshold_components(l, "test", "legacy_components"), l))
# ... and the endpoint rule refuses that table rather than splitting it wrongly.
stopifnot(fails(complete_threshold_components(l, "test", "endpoint")))

# 4. Parts that do not add up to their total stop the run.
bad <- d; bad$rf_test_veg_ice_thresh[1] <- bad$rf_test_veg_ice_thresh[1] + 0.01
stopifnot(fails(complete_threshold_components(bad, "test", "endpoint")))

cat("tests/test_forcing_components.R: all passed\n")
