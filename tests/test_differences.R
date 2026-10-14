# Tests for R/differences.R (issue I11). Run from the repository root: Rscript tests/test_differences.R
#
# Small hand-made cells, one month each, slices in ascending age (row 1 is the youngest).
# Cases 1-8 document the original behaviour ("legacy_components"), inherited from script
# 7a, including three known faults (question C2): an ice advance across the threshold comes
# out as zero change; a pair whose older slice the readvance smoothing moves below the
# threshold is counted twice; and a readvance in the youngest pair stops with an error or
# averages the wrong values. Cases 9 on test the "endpoint" method of issue I16, which
# fixes the first two; the youngest-pair fault stays in both, because the readvance rule
# itself is Andria's call (C2).
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
diffs <- function(..., method = "legacy_components") { invisible(capture.output(r <- pair_albedo_differences(cell(...), 0.5, method))); r }

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

# 7b. KNOWN FAULT (C2): the readvance adjustment moves the older slice below the threshold
#     (0.60 becomes (0.22 + 0.48) / 2 = 0.35) but alb_ice_thresh was chosen from the
#     unadjusted 0.60, so the original adds a vegetation change AND an ice change for the
#     same pair: -0.05 + (0.20 - 0.68) = -0.53, where only -0.05 is real (6 cell-pairs in the
#     data). "endpoint" classifies both ends from the adjusted fraction and reports -0.05.
d <- diffs(ice_frac = c(0.22, 0.60, 0.48), alb_veg = c(0.20, 0.25, 0.30))
stopifnot(same(d$ice_frac_old[1], 0.35), same(d$alb_diff_veg_thresh[1], -0.05),
          same(d$alb_diff_ice_thresh[1], 0.20 - 0.68), same(d$alb_diff_veg_ice_thresh[1], -0.53))
d_e <- diffs(ice_frac = c(0.22, 0.60, 0.48), alb_veg = c(0.20, 0.25, 0.30), method = "endpoint")
stopifnot(same(d_e$alb_diff_veg_ice_thresh[1], -0.05), is.na(d_e$alb_diff_ice_thresh[1]))
# ... and "endpoint" never reads the alb_ice_thresh columns: blanking them changes nothing.
blank <- cell(ice_frac = c(0.22, 0.60, 0.48), alb_veg = c(0.20, 0.25, 0.30)); blank$alb_ice_thresh <- NA; blank$alb_icesc_thresh <- NA
invisible(capture.output(e2 <- pair_albedo_differences(blank, 0.5, "endpoint")))
stopifnot(same(e2$alb_diff_veg_ice_thresh, d_e$alb_diff_veg_ice_thresh), same(e2$alb_diff_veg_icesc_thresh, d_e$alb_diff_veg_icesc_thresh))

# 8. KNOWN FAULT (C2): a readvance in the youngest pair has no younger neighbour. With two
#    slices it stops with an error (not met in the data so far) ...
stopifnot(fails(diffs(ice_frac = c(0.9, 0.1), alb_veg = c(0.20, 0.25))))
# ... and with more slices R recycles the slice's own value in place of the missing
#    neighbour: 0.9 becomes (0.8 + 0.9) / 2 = 0.85, with no error.
y <- diffs(ice_frac = c(0.9, 0.8, 0.1), alb_veg = c(0.20, 0.25, 0.30))
stopifnot(same(y$ice_frac_young, c(0.85, 0.5)))

# 9. "endpoint": the advance of case 7 now counts: ice (0.68) in the younger slice minus
#    vegetation (0.35) in the older one.
e <- diffs(ice_frac = c(0.95, 0.95, 0.9, 0.1), alb_veg = c(0.20, 0.25, 0.30, 0.35), method = "endpoint")
stopifnot(same(e$alb_diff_veg_ice_thresh[3], 0.68 - 0.35), same(e$alb_diff_ice_thresh[3], 0.68 - 0.35),
          is.na(e$alb_diff_veg_thresh[3]), same(e$alb_diff_veg_icesc_thresh[3], 0.73 - 0.35),
          same(e$alb_diff_veg_ice_thresh[1:2], c(0, 0)))                     # still ice: 0, as before
# ... the area-mixed variants are the same under both methods
stopifnot(identical(e[, c("alb_diff_veg_part", "alb_diff_ice_part", "alb_diff_veg_ice_parts", "alb_diff_icesc_part", "alb_diff_veg_icesc_parts")],
                    r[, c("alb_diff_veg_part", "alb_diff_ice_part", "alb_diff_veg_ice_parts", "alb_diff_icesc_part", "alb_diff_veg_icesc_parts")]))

# 10. "endpoint" agrees with the original where the original was right: retreat, still ice,
#     vegetation only.
for (case in list(list(c(0.1, 0.9), c(0.20, 0.30)), list(c(0.9, 0.95), c(0.20, 0.30)), list(c(0, 0), c(0.20, 0.25)))) {
  a <- diffs(ice_frac = case[[1]], alb_veg = case[[2]]); b <- diffs(ice_frac = case[[1]], alb_veg = case[[2]], method = "endpoint")
  stopifnot(identical(a$alb_diff_veg_ice_thresh, b$alb_diff_veg_ice_thresh), identical(a$alb_diff_veg_icesc_thresh, b$alb_diff_veg_icesc_thresh))
}

# 11. "endpoint": a missing ice fraction makes every threshold difference unknown.
e <- diffs(ice_frac = c(0.2, NA), alb_veg = c(0.20, 0.25), method = "endpoint")
stopifnot(is.na(e$alb_diff_veg_ice_thresh), is.na(e$alb_diff_veg_thresh), is.na(e$alb_diff_ice_thresh))

# 12. "endpoint": consecutive changes add up to the change between the first and last slice
#     (a monotone retreat: vegetation 0.10 now, ice before).
e <- diffs(ice_frac = c(0, 0.3, 0.6, 0.9), alb_veg = c(0.10, 0.15, 0.20, 0.25), method = "endpoint")
stopifnot(same(sum(e$alb_diff_veg_ice_thresh), 0.10 - 0.68))

# 13. Swapping the two slices reverses the sign of every change.
f <- endpoint_albedo_differences(c(TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE), c(0.2, 0.3, 0.4), c(0.25, 0.35, 0.45), 0.68, 0.68)
g <- endpoint_albedo_differences(c(FALSE, TRUE, FALSE), c(TRUE, FALSE, FALSE), c(0.25, 0.35, 0.45), c(0.2, 0.3, 0.4), 0.68, 0.68)
stopifnot(same(f$total, -g$total))

# 14. The first-pair readvance still stops, under either method (readvance policy: C2).
stopifnot(fails(diffs(ice_frac = c(0.9, 0.1), alb_veg = c(0.20, 0.25), method = "endpoint")))

# 15. A misspelt method stops rather than falling back to the original arithmetic.
stopifnot(fails(diffs(ice_frac = c(0, 0), alb_veg = c(0.20, 0.25), method = "endpiont")))

cat("test_differences: all tests passed\n")
