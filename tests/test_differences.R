# Tests for R/differences.R (issue I11). Run from the repository root: Rscript tests/test_differences.R
#
# Small hand-made cells, one month each, slices in ascending age (row 1 is the youngest).
# These document the CURRENT behaviour, inherited from script 7a, including two known
# faults (question C2): an ice advance across the threshold comes out as zero change, and a
# readvance in the first pair stops with an error. Issue I16 fixes them behind a setting;
# the tests for those two cases will then change on purpose.
source("R/differences.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

# A cell as script 7a builds it: glacier albedo 0.68 (fixed) / 0.73 (scaled); the *_thresh
# glacier albedo is NA where the slice counts as vegetation (ice fraction <= threshold).
cell <- function(ice_frac, alb_veg, threshold = 0.5) {
  is_veg <- !is.na(ice_frac) & ice_frac <= threshold
  data.frame(ice_frac = ice_frac, alb_veg = alb_veg, ice_albedo_fixed = 0.68, ice_albedo_sc = 0.73,
             alb_ice_thresh = ifelse(is_veg, NA, 0.68), alb_icesc_thresh = ifelse(is_veg, NA, 0.73))
}
diffs <- function(...) { invisible(capture.output(r <- pair_albedo_differences(cell(...), 0.5))); r }

# 1. Vegetation only: albedo 0.20 now, 0.25 before; younger minus older = -0.05.
r <- diffs(ice_frac = c(0, 0), alb_veg = c(0.20, 0.25))
stopifnot(nrow(r) == 1,
          same(r$alb_diff_veg_thresh, -0.05), is.na(r$alb_diff_ice_thresh), same(r$alb_diff_veg_ice_thresh, -0.05),
          same(r$alb_diff_veg_part, -0.05), same(r$alb_diff_ice_part, 0), same(r$alb_diff_veg_ice_parts, -0.05))

# 2. Ice retreat: the older slice is 90% ice, the younger 10%.
r <- diffs(ice_frac = c(0.1, 0.9), alb_veg = c(0.20, 0.30))
stopifnot(is.na(r$alb_diff_veg_thresh),                          # an ice end: no vegetation difference
          same(r$alb_diff_ice_thresh, 0.20 - 0.68),               # vegetation now minus ice before
          same(r$alb_diff_veg_ice_thresh, 0.20 - 0.68),
          same(r$alb_diff_icesc_thresh, 0.20 - 0.73),
          same(r$alb_diff_veg_part, (0.20 - 0.30) * (1 - 0.9)),
          same(r$alb_diff_ice_part, (0.20 - 0.68) * (0.9 - 0.1)),
          same(r$alb_diff_veg_ice_parts, (0.20 - 0.30) * 0.1 + (0.20 - 0.68) * 0.8))

# 3. Ice at both ends ("still ice"): no change attributed to ice.
r <- diffs(ice_frac = c(0.9, 0.95), alb_veg = c(0.20, 0.30))
stopifnot(same(r$alb_diff_ice_thresh, 0), is.na(r$alb_diff_veg_thresh), same(r$alb_diff_veg_ice_thresh, 0))

# 4. Exactly at the threshold counts as vegetation (ice means > threshold).
r <- diffs(ice_frac = c(0.5, 0.5), alb_veg = c(0.20, 0.25))
stopifnot(same(r$alb_diff_veg_thresh, -0.05), same(r$alb_diff_veg_ice_thresh, -0.05))

# 5. A missing ice fraction: the ice and combined differences are unknown.
r <- diffs(ice_frac = c(0.2, NA), alb_veg = c(0.20, 0.25))
stopifnot(is.na(r$alb_diff_ice_thresh), is.na(r$alb_diff_veg_ice_thresh), is.na(r$alb_diff_veg_ice_parts))

# 6. The readvance adjustment: ice that increases towards the present is replaced, once, by
#    the mean of its neighbouring slices. Here slice 3 (0.9) follows 0.1 in slice 4 and is
#    set to (0.1 + 0.95) / 2 = 0.525, still above the threshold.
r <- diffs(ice_frac = c(0.95, 0.95, 0.9, 0.1), alb_veg = c(0.20, 0.25, 0.30, 0.35))
stopifnot(same(r$ice_frac_young, c(0.95, 0.925, 0.525)), same(r$ice_frac_old, c(0.925, 0.525, 0.1)))

# 7. KNOWN FAULT (C2): the last pair of case 6 is an ice advance across the threshold (young
#    0.525 ice, old 0.1 vegetation). Both components are NA and their na.rm sum reports a
#    confident zero change. I16 fixes this behind ice$difference_method.
stopifnot(is.na(r$alb_diff_veg_thresh[3]), is.na(r$alb_diff_ice_thresh[3]), same(r$alb_diff_veg_ice_thresh[3], 0))

# 8. KNOWN FAULT (C2): a readvance in the first pair has no older neighbour on one side and
#    stops with an error (not met in the data so far).
stopifnot(fails(diffs(ice_frac = c(0.9, 0.1), alb_veg = c(0.20, 0.25))))

cat("test_differences: all tests passed\n")
