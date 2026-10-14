# 6_prediction_model.R: hindcast albedo for every Holocene slice
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Since issue I04a the draw count and random seed come from config.R and files
# are read and written through the run manifest; the computation is unchanged.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R (issue I07) and starts the run's
# manifest; files are then read and written through it (issues I05, I04a).
source('R/run_manifest.R')
run   = run_begin('6_prediction_model')
cfg   = run$cfg
paths = run$paths
alb_prod = paths$tag   # the albedo product in file names

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run
# (issue I06).
library(dplyr)
library(mgcv)
library(gratia)
library(reshape2)

# Simulated draws per prediction (config.R analysis$draws, questions A2, A3), and the base
# random seed of this run, recorded in its manifest (R/random.R).
source('R/random.R')
n_draws   = cfg$analysis$draws$prediction
seed_base = run_record(run, 'random_seed', list(setting = cfg$analysis$draws$seed,
                                                base = base_seed(cfg$analysis$draws$seed)))$base


# The months analysed, from config.R (analysis$months, issue I04b); month names and
# numbers come from the shared calendar, R/calendar.R (issue I02).
source('R/calendar.R')
months = cfg$analysis$months

lct_interp_paleo = read_input(run, path_file(paths, 'lct_paleo'))
colnames(lct_interp_paleo)[1] = 'year'

for (month in months){

  print(month)

  cal_interp_model = read_input(run, path_calibration_model(paths, 'selected', month))

  print(">>Predict")
  paleo_interp_predict_gam_vec = predict.gam(cal_interp_model,
                                             newdata = lct_interp_paleo ,
                                             type    = 'response')
  paleo_interp_predict_gam = data.frame(lct_interp_paleo,
                                        alb_mean = paleo_interp_predict_gam_vec)

  write_output(run, paleo_interp_predict_gam, path_prediction(paths, 'point', month))

  print(">>Simulate")
  paleo_interp_sim_gam = simulate(cal_interp_model,
                                  nsim = n_draws,
                                  seed = derive_seed(seed_base, '6_prediction_model', month, 'prediction'),
                                  data = lct_interp_paleo)

  paleo_interp_sim_gam_df = data.frame(lct_interp_paleo,
                                       paleo_interp_sim_gam)

  paleo_interp_sim_gam_melt = melt(paleo_interp_sim_gam_df, id.vars = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
  colnames(paleo_interp_sim_gam_melt) = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'iter', 'value')#c('cell_idx', 'iter', 'value')

  paleo_interp_sim_gam_melt$iter = draw_index(paleo_interp_sim_gam_melt$iter)

  write_output(run, paleo_interp_sim_gam_melt, path_prediction(paths, 'samples', month))

  print(">>Summarize")
  paleo_interp_sim_gam_sum = paleo_interp_sim_gam_melt %>%
    group_by(year, x, y, elev, ET, OL, ST) %>%
    summarize(alb_mean = mean(value),
              alb_sd = sd(value),
              alb_lo = quantile(value, c(0.025)),
              alb_mid = quantile(value, c(0.5)),
              alb_hi = quantile(value, c(0.975)),
              .groups = 'keep')

  write_output(run, paleo_interp_sim_gam_sum, path_prediction(paths, 'summary', month))

}

alb_preds_summary_months = data.frame(matrix(NA, nrow=0, ncol=13))
colnames(alb_preds_summary_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
               "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi", "month")

alb_preds_months = data.frame(matrix(NA, nrow=0, ncol=9))
colnames(alb_preds_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
                                       "alb_mean", "month")

for (month in months) {

  print(month)

  alb_preds_month = read_input(run, path_prediction(paths, 'point', month))
  alb_preds_months = rbind(alb_preds_months,
                           data.frame(alb_preds_month, month = rep(month)))

  alb_preds_summary_month = read_input(run, path_prediction(paths, 'summary', month))
  alb_preds_summary_months = rbind(alb_preds_summary_months,
                                   data.frame(alb_preds_summary_month, month = rep(month)))
}

write_output(run, alb_preds_months, path_prediction(paths, 'point'))
write_output(run, alb_preds_summary_months, path_prediction(paths, 'summary'))

run_finish(run)
