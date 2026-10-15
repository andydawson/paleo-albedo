# 6_prediction_model.R: hindcast albedo for every Holocene slice
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('6_prediction_model')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(dplyr)
library(mgcv)
library(gratia)
library(reshape2)

# Simulated draws per prediction (config.R analysis$draws, questions A2, A3), and the base
# random seed of this run, recorded in its manifest (R/random.R).
source('R/random.R')
n_draws   = cfg$analysis$draws$prediction
seed_base = base_seed(cfg$analysis$draws$seed)
run_record(run, 'random_seed', list(setting = cfg$analysis$draws$seed, base = seed_base))


# The months analysed, from config.R (analysis$months); month names and
# numbers come from the shared calendar, R/calendar.R.
source('R/calendar.R')
months = cfg$analysis$months

lct_interp_paleo = read_input(run, path_file(paths, 'lct_paleo'))
# The slice column is called 'year' in the prediction tables.
names(lct_interp_paleo)[names(lct_interp_paleo) == 'ages'] = 'year'

for (month in months){

  print(month)

  cal_interp_model = read_input(run, path_file(paths, 'calibration_selected', month = month))

  print(">>Predict")
  fitted_albedo = predict.gam(cal_interp_model,
                                             newdata = lct_interp_paleo ,
                                             type    = 'response')
  point_predictions = data.frame(lct_interp_paleo,
                                        alb_mean = fitted_albedo)

  # One row per cell and slice (year): this month's point prediction (alb_mean).
  write_output(run, point_predictions, path_file(paths, 'prediction_point_month', month = month))

  print(">>Simulate")
  response_draws = simulate(cal_interp_model,
                                  nsim = n_draws,
                                  seed = derive_seed(seed_base, '6_prediction_model', month, 'prediction'),
                                  data = lct_interp_paleo)

  response_table = data.frame(lct_interp_paleo,
                                       response_draws)

  response_samples = melt(response_table, id.vars = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
  colnames(response_samples) = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'iter', 'value')

  response_samples$iter = draw_index(response_samples$iter)

  # One row per cell, slice and simulated draw (iter).
  write_output(run, response_samples, path_file(paths, 'prediction_samples_month', month = month))

  print(">>Summarize")
  response_summary = response_samples %>%
    group_by(year, x, y, elev, ET, OL, ST) %>%
    summarize(alb_mean = mean(value),
              alb_sd = sd(value),
              alb_lo = quantile(value, c(0.025)),
              alb_mid = quantile(value, c(0.5)),
              alb_hi = quantile(value, c(0.975)),
              .groups = 'keep')

  # One row per cell and slice: mean, sd and quantiles of the draws.
  write_output(run, response_summary, path_file(paths, 'prediction_summary_month', month = month))

}

alb_preds_summary_months = data.frame(matrix(NA, nrow=0, ncol=13))
colnames(alb_preds_summary_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
               "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi", "month")

alb_preds_months = data.frame(matrix(NA, nrow=0, ncol=9))
colnames(alb_preds_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
                                       "alb_mean", "month")

for (month in months) {

  print(month)

  alb_preds_month = read_input(run, path_file(paths, 'prediction_point_month', month = month))
  alb_preds_months = rbind(alb_preds_months,
                           data.frame(alb_preds_month, month = rep(month)))

  alb_preds_summary_month = read_input(run, path_file(paths, 'prediction_summary_month', month = month))
  alb_preds_summary_months = rbind(alb_preds_summary_months,
                                   data.frame(alb_preds_summary_month, month = rep(month)))
}

# All months together: one row per cell, slice and month.
write_output(run, alb_preds_months, path_file(paths, 'prediction_point'))
write_output(run, alb_preds_summary_months, path_file(paths, 'prediction_summary'))

run_finish(run)
