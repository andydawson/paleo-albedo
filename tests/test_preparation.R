# Tests for R/preparation.R (issue I04a). Run from the repository root: Rscript tests/test_preparation.R
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("R/preparation.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

# Posterior draws in the layout of data/veg_posts_interp_ice.RDS: one row per cell, slice,
# draw and class. Cell 1 has skewed draws whose medians are worked out by hand below; every
# draw's three fractions sum to 1, as in the real posteriors.
draws_of <- function(cell_id, ages, ET, OL) {
  n <- length(ET)
  data.frame(cell_id = cell_id, x = cell_id * 2, y = 40, ages = ages, iter = rep(seq_len(n), 3),
             LCT = rep(c("ET", "OL", "ST"), each = n), value = c(ET, OL, 1 - ET - OL))
}
# Cell 1, 200 draws: draws 1-50 are (ET .1, OL .6, ST .3), 51-100 (.1, .2, .7), 101-200 (.3, .2, .5).
#   ET: 100 x 0.1 and 100 x 0.3                -> median (0.1 + 0.3) / 2 = 0.2
#   OL: 50 x 0.6 and 150 x 0.2                 -> median 0.2
#   ST: 50 x 0.3, 100 x 0.5 and 50 x 0.7       -> median 0.5
#   the medians sum to 0.9, so the rescaled fractions are 0.2/0.9, 0.2/0.9, 0.5/0.9.
cell1 <- draws_of(1, 500, ET = rep(c(0.1, 0.3), each = 100), OL = c(rep(0.6, 50), rep(0.2, 150)))
# Cell 2 at two slices, random draws.
set.seed(1)
cell2 <- rbind(draws_of(2, 500, ET = runif(200, 0, 0.5), OL = runif(200, 0, 0.5)),
               draws_of(2, 1000, ET = runif(200, 0, 0.5), OL = runif(200, 0, 0.5)))
posts <- rbind(cell1, cell2)

# The original reduction of script 1, line for line, to compare the mean path with.
original <- function(lct_interp) {
  lct_interp <- suppressMessages(lct_interp %>% group_by(cell_id, x, y, ages, LCT) %>% dplyr::summarize(value = mean(value)))
  data.frame(pivot_wider(lct_interp, id_cols = c("cell_id", "x", "y", "ages"), names_from = c("LCT"), values_from = c("value")))
}

# Cell 3: every draw of every class is zero, so its median land cover is all zero.
all_zero <- transform(draws_of(3, 500, ET = rep(0, 200), OL = rep(0, 200)), value = 0)

med <- reduce_land_cover(posts, "median")
stopifnot(
  # the mean path is the original, bit for bit
  identical(reduce_land_cover(posts, "mean"), original(posts)),
  identical(reduce_land_cover(posts), original(posts)),
  # the median path: same layout as the mean output
  identical(names(med), names(original(posts))),
  nrow(med) == nrow(original(posts)),
  # the hand-computed medians, rescaled
  isTRUE(all.equal(unlist(med[med$cell_id == 1, c("ET", "OL", "ST")], use.names = FALSE), c(0.2, 0.2, 0.5) / 0.9, tolerance = 1e-15)),
  # every row sums to 1
  all(abs(rowSums(med[, c("ET", "OL", "ST")]) - 1) < 1e-12),
  # before rescaling, the medians are the plain medians of the draws, grouped independently
  isTRUE(all.equal(
    med[med$cell_id == 2 & med$ages == 1000, "OL"] /
      med[med$cell_id == 2 & med$ages == 1000, "ET"],
    median(cell2$value[cell2$ages == 1000 & cell2$LCT == "OL"]) /
      median(cell2$value[cell2$ages == 1000 & cell2$LCT == "ET"]))),
  # a cell whose median land cover is all zero cannot be rescaled: stop, naming it
  fails(reduce_land_cover(rbind(posts, all_zero), "median")),
  grepl("cell 3 at 500 BP", tryCatch(close_fractions(data.frame(cell_id = 3, ages = 500, ET = 0, OL = 0, ST = 0)),
                                     error = conditionMessage)),
  fails(close_fractions(data.frame(cell_id = 4, ages = 50, ET = NA, OL = 0.5, ST = 0.5))),
  fails(close_fractions(data.frame(cell_id = 4, ages = 50, ET = -0.1, OL = 0.6, ST = 0.5))),
  fails(reduce_land_cover(posts, "trimmed"))
)
cat("test_preparation: all tests passed\n")
