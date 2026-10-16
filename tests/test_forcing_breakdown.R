# Tests for R/forcing_breakdown.R (script 10). Run from the repository root:
#   Rscript tests/test_forcing_breakdown.R
#
# A hand-worked case (from Astra's review, 2026-10-16): one period of two slice-pairs, two
# cells in one band, two months. Cell A has area 2, cell B area 1; Earth's area is 100.
#   A: January pairs 1 and 2 (sum 3), February 2 and 4 (sum 6), so 2 months with a value.
#   B: January has no value (NA, NA: polar night), February 3 and 6 (sum 9): 1 month.
# Script 9's annual figure: A's mean 4.5, B's 9, so (2 x 4.5 + 1 x 9) / 100 = 0.18.
suppressMessages(library(dplyr))
source("R/calendar.R")
source("R/forcing_breakdown.R")
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

cells <- data.frame(period = "p", lat_band = "30-40 N",
                    cell_id = c("A", "A", "A", "A", "B", "B", "B", "B"),
                    area    = c( 2,   2,   2,   2,   1,   1,   1,   1),
                    month   = c("jan", "jan", "feb", "feb", "jan", "jan", "feb", "feb"),
                    rf      = c( 1,   2,   2,   4,   NA,  NA,  3,   6))
r <- as.data.frame(breakdown_forcing(cells, "rf", 100))
jan <- r[r$month == "jan", ]
feb <- r[r$month == "feb", ]
stopifnot(
  nrow(r) == 2,
  # January: only A has a value.
  same(jan$global_equiv_month, 0.06),          # 3 x 2 / 100
  same(jan$contribution_to_annual, 0.03),      # 3 x 2 / 2 months / 100
  same(jan$band_mean, 3),
  same(jan$area_m2, 3), same(jan$valid_area_m2, 2),
  # February: both.
  same(feb$global_equiv_month, 0.21),          # (6 x 2 + 9 x 1) / 100
  same(feb$contribution_to_annual, 0.15),      # (6 x 2 / 2 + 9 x 1 / 1) / 100
  same(feb$band_mean, 7),                      # 21 / 3
  same(feb$area_m2, 3), same(feb$valid_area_m2, 3),
  # The parts add up to script 9's annual figure.
  same(sum(r$contribution_to_annual), 0.18))

# A cell with no value in any month adds nothing, and its area shows as not valid.
cells_c <- rbind(cells, data.frame(period = "p", lat_band = "30-40 N", cell_id = "C", area = 5,
                                   month = c("jan", "jan", "feb", "feb"), rf = NA))
r_c <- as.data.frame(breakdown_forcing(cells_c, "rf", 100))
stopifnot(same(sum(r_c$contribution_to_annual), 0.18),
          same(r_c$area_m2[r_c$month == "feb"], 8), same(r_c$valid_area_m2[r_c$month == "feb"], 3))

# Band edges on the 27-74 N domain, and a narrow domain with no multiple inside.
stopifnot(
  identical(latitude_band_edges(27, 74, 10), c(27, 30, 40, 50, 60, 70, 74)),
  identical(latitude_band_edges(27, 74, 7),  c(27, 28, 35, 42, 49, 56, 63, 70, 74)),
  identical(latitude_band_edges(27, 74, 47), c(27, 47, 74)),
  identical(latitude_band_edges(27, 74, 90), c(27, 74)),
  identical(latitude_band_edges(31, 38, 10), c(31, 38)),
  identical(latitude_band_edges(27, 74, 1),  c(27, 28:73, 74)),
  identical(latitude_band_labels(c(27, 30, 40)), c("27-30 N", "30-40 N")))

cat("test_forcing_breakdown: all tests passed\n")
