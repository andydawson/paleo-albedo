# Tests for R/random.R (issue I04a). Run from the repository root: Rscript tests/test_random.R
suppressPackageStartupMessages({ library(mgcv); library(gratia) })
source("R/random.R")

# derive_seed: fixed, in R's seed range, and different for each script, month and call.
seeds <- c(derive_seed(1, "6_prediction_model", "jan", "prediction"),
           derive_seed(1, "6_prediction_model", "feb", "prediction"),
           derive_seed(1, "5_calibration_eval", "jan", "prediction"),
           derive_seed(1, "6_prediction_model", "jan", "statistics"),
           derive_seed(2, "6_prediction_model", "jan", "prediction"))
stopifnot(
  identical(derive_seed(1, "6_prediction_model", "jan", "prediction"), seeds[1]),
  is.integer(seeds), all(seeds >= 0 & seeds < 2147483647), !anyDuplicated(seeds),
  # a known value, so a change to the hash (which would change every seeded run) is noticed
  identical(derive_seed(1, "6_prediction_model", "jan", "prediction"), 1660093762L),
  identical(base_seed(20261014), 20261014L),
  identical(base_seed(0), 0L),
  is.integer(base_seed("random")), base_seed("random") >= 0
)
r1 <- base_seed("random"); Sys.sleep(0.01); r2 <- base_seed("random")
stopifnot(r1 != r2)

# draw_index: the draw number from simulate()'s column names, including past 999.
stopifnot(identical(draw_index(c("V1", "V99", "V100", "V999", "V1000", "V12345")), c(1, 99, 100, 999, 1000, 12345)),
          identical(draw_index(factor(c("X1", "X1000"))), c(1, 1000)))

# Seeded simulate(): a month's draws are the same whatever is simulated before it, so a
# run of some months reproduces those months of a run of all twelve; R's own random state
# is left as it was.
set.seed(42)
dat <- data.frame(x = runif(200), y = runif(200))
dat$alb <- plogis(sin(6 * dat$x) + dat$y + rnorm(200, sd = 0.3))
fit <- gam(alb ~ s(x) + s(y), data = dat, family = betar(link = "logit"))
draws_for <- function(month) simulate(fit, nsim = 5, seed = derive_seed(7, "6_prediction_model", month, "prediction"), data = dat)
all_months <- lapply(c("jan", "feb", "mar"), draws_for)   # a three-month run
feb_alone  <- draws_for("feb")                             # a one-month run
state_before <- .Random.seed; invisible(draws_for("mar")); state_after <- .Random.seed
stopifnot(
  identical(all_months[[2]], feb_alone),
  !identical(all_months[[1]], all_months[[2]]),
  identical(state_before, state_after)
)
cat("test_random: all tests passed\n")
