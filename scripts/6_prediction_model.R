# 6_prediction_model.R: hindcast albedo for every Holocene slice
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(dplyr)
library(mgcv)
library(gratia)
library(reshape2)

# Settings and file locations: config.R, and R/paths.R for every file read or written. The
# directories products go to are created by pipeline_paths() (issue I07).
source('R/config.R')
source('R/paths.R')
cfg   = load_config()
paths = pipeline_paths(cfg)
alb_prod = paths$tag   # the albedo product in file names


# Month list from the shared calendar (R/calendar.R) rather than typed out in each script
# (issue I02, 2026-10-12).
source('R/calendar.R')
months = MONTHS

lct_interp_paleo = readRDS(path_file(paths, 'lct_paleo'))
colnames(lct_interp_paleo)[1] = 'year'

for (month in months){

  print(month)

  cal_interp_model = readRDS(path_calibration_model(paths, 'selected', month))

  print(">>Predict")
  paleo_interp_predict_gam_vec = predict.gam(cal_interp_model,
                                             newdata = lct_interp_paleo ,
                                             type    = 'response')
  paleo_interp_predict_gam = data.frame(lct_interp_paleo,
                                        alb_mean = paleo_interp_predict_gam_vec)

  saveRDS(paleo_interp_predict_gam, path_prediction(paths, 'point', month))

  print(">>Simulate")
  paleo_interp_sim_gam = simulate(cal_interp_model,
                                  nsim = 100,
                                  data = lct_interp_paleo)

  paleo_interp_sim_gam_df = data.frame(lct_interp_paleo,
                                       paleo_interp_sim_gam)

  paleo_interp_sim_gam_melt = melt(paleo_interp_sim_gam_df, id.vars = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
  colnames(paleo_interp_sim_gam_melt) = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'iter', 'value')#c('cell_idx', 'iter', 'value')

  paleo_interp_sim_gam_melt$iter = as.numeric(substr(paleo_interp_sim_gam_melt$iter, 2, 4))

  saveRDS(paleo_interp_sim_gam_melt, path_prediction(paths, 'samples', month))

  print(">>Summarize")
  paleo_interp_sim_gam_sum = paleo_interp_sim_gam_melt %>%
    group_by(year, x, y, elev, ET, OL, ST) %>%
    summarize(alb_mean = mean(value),
              alb_sd = sd(value),
              alb_lo = quantile(value, c(0.025)),
              alb_mid = quantile(value, c(0.5)),
              alb_hi = quantile(value, c(0.975)),
              .groups = 'keep')

  saveRDS(paleo_interp_sim_gam_sum, path_prediction(paths, 'summary', month))

}

alb_preds_summary_months = data.frame(matrix(NA, nrow=0, ncol=13))
colnames(alb_preds_summary_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
               "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi", "month")

alb_preds_months = data.frame(matrix(NA, nrow=0, ncol=9))
colnames(alb_preds_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST",
                                       "alb_mean", "month")

for (month in months) {

  print(month)

  alb_preds_month = readRDS(path_prediction(paths, 'point', month))
  alb_preds_months = rbind(alb_preds_months,
                           data.frame(alb_preds_month, month = rep(month)))

  alb_preds_summary_month = readRDS(path_prediction(paths, 'summary', month))
  alb_preds_summary_months = rbind(alb_preds_summary_months,
                                   data.frame(alb_preds_summary_month, month = rep(month)))
}

saveRDS(alb_preds_months, path_prediction(paths, 'point'))
saveRDS(alb_preds_summary_months, path_prediction(paths, 'summary'))
