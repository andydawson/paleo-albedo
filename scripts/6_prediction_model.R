library(raster)
library(tidyr)
library(dplyr)
# library(rgdal)
library(sp)
library(ggplot2)
library(mgcv)
library(SemiPar)
library(gratia)
library(reshape2)

alb_prod = "bluesky"

# months = c('feb', 'may', 'aug', 'nov')

months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
# months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'nov', 'dec')


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_interp_paleo = readRDS('data/lct_paleo_reveals_interp.RDS')
colnames(lct_interp_paleo)[1] = 'year'

###############################################################################################################
## PREDICT FROM INTERP CALIBRATION MODEL
###############################################################################################################

for (month in months){
  
  print(month)
  
  cal_interp_model = readRDS(paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
  
  print(">>Predict")
  paleo_interp_predict_gam_vec = predict.gam(cal_interp_model, 
                                             newdata = lct_interp_paleo , 
                                             type    = 'response')
  paleo_interp_predict_gam = data.frame(lct_interp_paleo,
                                        alb_mean = paleo_interp_predict_gam_vec)
  
  # paleo_interp_predict_gam = data.frame(lct_interp_paleo)
  # paleo_interp_predict_gam$get(month) = paleo_interp_predict_gam_vec
  
  saveRDS(paleo_interp_predict_gam, paste0('output/prediction/paleo_interp_predict_gam_', month, '_', alb_prod, '.RDS'))
  
  print(">>Simulate")
  paleo_interp_sim_gam = simulate(cal_interp_model,
                                  nsim = 100,
                                  data = lct_interp_paleo)
  
  paleo_interp_sim_gam_df = data.frame(lct_interp_paleo,  
                                       paleo_interp_sim_gam)  
  
  # paleo_interp_sim_gam_melt = melt(paleo_interp_sim_gam_df, id.vars = c('year', 'long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
  paleo_interp_sim_gam_melt = melt(paleo_interp_sim_gam_df, id.vars = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
  colnames(paleo_interp_sim_gam_melt) = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'iter', 'value')#c('cell_idx', 'iter', 'value')
  
  paleo_interp_sim_gam_melt$iter = as.numeric(substr(paleo_interp_sim_gam_melt$iter, 2, 4))
  
  saveRDS(paleo_interp_sim_gam_melt, paste0('output/prediction/paleo_interp_predict_gam_samps_', month, '_', alb_prod, '.RDS'))
  
  print(">>Summarize")
  paleo_interp_sim_gam_sum = paleo_interp_sim_gam_melt %>% 
    # group_by(year, long, lat, x, y, elev, ET, OL, ST) %>%
    group_by(year, x, y, elev, ET, OL, ST) %>%
    summarize(alb_mean = mean(value), 
              alb_sd = sd(value),
              alb_lo = quantile(value, c(0.025)), 
              alb_mid = quantile(value, c(0.5)), 
              alb_hi = quantile(value, c(0.975)), 
              .groups = 'keep')
  
  # paleo_sim_gam_sum_df = data.frame(lct_paleo,  paleo_sim_gam_sum)  
  
  saveRDS(paleo_interp_sim_gam_sum, paste0('output/prediction/paleo_interp_predict_gam_summary_', month, '_', alb_prod, '.RDS'))
  
}

###############################################################################################################
## merge data frames 
###############################################################################################################

alb_preds_summary_months = data.frame(matrix(NA, nrow=0, ncol=13))
colnames(alb_preds_summary_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST", 
               "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi", "month")

alb_preds_months = data.frame(matrix(NA, nrow=0, ncol=9))
colnames(alb_preds_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST", 
                                       "alb_mean", "month")

for (month in months) {
  
  print(month)
  
  alb_preds_month = readRDS(paste0('output/prediction/paleo_interp_predict_gam_', month, '_', alb_prod, '.RDS'))
  alb_preds_months = rbind(alb_preds_months, 
                           data.frame(alb_preds_month, month = rep(month)))
  
  alb_preds_summary_month = readRDS(paste0('output/prediction/paleo_interp_predict_gam_summary_', month, '_', alb_prod, '.RDS'))
  alb_preds_summary_months = rbind(alb_preds_summary_months, 
                                   data.frame(alb_preds_summary_month, month = rep(month)))
}

saveRDS(alb_preds_months, paste0('output/prediction/paleo_interp_predict_gam_', alb_prod, '.RDS'))
saveRDS(alb_preds_summary_months, paste0('output/prediction/paleo_interp_predict_gam_summary_', alb_prod, '.RDS'))

###############################################################################################################
## XXX OLD
###############################################################################################################


lct_paleo = readRDS('data/lct_paleo_reveals.RDS')
colnames(lct_paleo)[1] = 'year'

# lct_paleo = lct_paleo[which(lct_paleo$month == 4),]

cal_model = readRDS(paste0('data/calibration_model_selected_', alb_prod, '.RDS'))


paleo_predict_gam = predict.gam(cal_model, 
                                newdata = lct_paleo , 
                                type    = 'response')
paleo_predict_gam = data.frame(lct_paleo, alb_mean = paleo_predict_gam)  

saveRDS(paleo_predict_gam, paste0('data/paleo_predict_gam_', alb_prod, '.RDS'))


paleo_sim_gam = simulate(cal_model,
                         nsim = 100,
                         data = lct_paleo)

paleo_sim_gam_df = data.frame(lct_paleo,  paleo_sim_gam)  

paleo_sim_gam_melt = melt(paleo_sim_gam_df, id.vars = c('year', 'long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(paleo_sim_gam_melt) = c('year', 'long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'iter', 'value')#c('cell_idx', 'iter', 'value')

paleo_sim_gam_melt$iter = as.numeric(substr(paleo_sim_gam_melt$iter, 2, 4))

saveRDS(paleo_sim_gam_melt, paste0('data/paleo_predict_gam_samps_', alb_prod, '.RDS'))

paleo_sim_gam_sum = paleo_sim_gam_melt %>% 
  group_by(year, long, lat, x, y, elev, ET, OL, ST) %>%
  summarize(alb_mean = mean(value), 
            alb_sd = sd(value),
            alb_lo = quantile(value, c(0.025)), 
            alb_mid = quantile(value, c(0.5)), 
            alb_hi = quantile(value, c(0.975)), 
            .groups = 'keep')

# paleo_sim_gam_sum_df = data.frame(lct_paleo,  paleo_sim_gam_sum)  

saveRDS(paleo_sim_gam_sum, paste0('data/paleo_predict_gam_summary_', alb_prod, '.RDS'))

# fix.family.rd(betar())$rd
# function (mu, wt, scale) 
# {
#   Theta <- exp(get(".Theta"))
#   r <- rbeta(n = length(mu), shape1 = Theta * mu, shape2 = Theta * 
#                (1 - mu))
#   eps <- get(".betarEps")
#   r[r >= 1 - eps] <- 1 - eps
#   r[r < eps] <- eps
#   r
# }

###############################################################################################################
## summarize by region
###############################################################################################################

# rmvn <- function(n, mu, sig) { ## MVN random deviates
#   L <- mroot(sig)
#   m <- ncol(L)
#   t(mu + L %*% matrix(rnorm(m*n), m, n))
# }
# 
# Vb <- vcov(cal_model)
# # newd <- with(lct_paleo, data.frame(age = seq(min(age), max(age), length = 200)))
# newd = lct_paleo
# pred <- predict(cal_model, newd, se.fit = TRUE)
# se.fit <- pred$se.fit
# 
# set.seed(42)
# N <- 10000
# BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
# 
# Cg <- predict(cal_model, newd, type = "lpmatrix")
# simDev <- Cg %*% t(BUdiff)
# 
# absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
# masd <- apply(absDev, 2L, max)
# crit <- quantile(masd, prob = 0.95, type = 8)
# 
# pred <- transform(cbind(data.frame(pred), newd),
#                   uprP = fit + (2 * se.fit),
#                   lwrP = fit - (2 * se.fit),
#                   uprS = fit + (crit * se.fit),
#                   lwrS = fit - (crit * se.fit))

# predict_paleo = predict.gam(cal_model, 
#                             newdata = lct_paleo , 
#                             type    = 'response')
# lct_preds = data.frame(lct_paleo, alb_pred = predict_paleo)  
# 
# saveRDS(lct_preds,'data/lct_paleo_preds_albclim.RDS')
# 
# 
# 
# preds_albclim = readRDS('data/lct_paleo_preds_albclim.RDS')
# preds_bluesky = readRDS('data/lct_paleo_preds_bluesky.RDS')
# 
# preds_merged = merge(preds_albclim[,c('ages', 'long', 'lat', 'alb_pred')], 
#       preds_bluesky[,c('ages', 'long', 'lat', 'alb_pred')], by = c('ages', 'long', 'lat'))
# 
# ggplot(data=preds_merged) +
#   geom_point(aes(alb_pred.x, y=alb_pred.y)) +
#   geom_abline(slope=1, intercept = 0) +
#   xlab('alb clim') +
#   ylab('blue sky')
# 
# 
# # cal_model = readRDS('data/calibration_model5.RDS')
# # cal_model = readRDS('data/calibration_mod5_albclim.RDS')
# cal_brm = readRDS(paste0('data/calibration_brms_m4_', alb_prod, '.RDS'))
# 
# paleo_predict_brm = predict(cal_brm, 
#                             newdata = lct_paleo,
#                             summary = TRUE)
# paleo_predict_brm = data.frame(lct_paleo, 
#                                alb_mean = paleo_predict_brm[,'Estimate'], 
#                                alb_sd = paleo_predict_brm[,'Est.Error'])  
# 
# saveRDS(paleo_predict_brm, paste0('data/paleo_predict_brms_', alb_prod, '.RDS'))
# 
# 

###############################################################################################################
## summarize by region
###############################################################################################################

# rmvn <- function(n, mu, sig) { ## MVN random deviates
#   L <- mroot(sig)
#   m <- ncol(L)
#   t(mu + L %*% matrix(rnorm(m*n), m, n))
# }
# 
# Vb <- vcov(cal_model)
# # newd <- with(lct_paleo, data.frame(age = seq(min(age), max(age), length = 200)))
# newd = lct_paleo
# pred <- predict(cal_model, newd, se.fit = TRUE)
# se.fit <- pred$se.fit
# 
# set.seed(42)
# N <- 10000
# BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
# 
# Cg <- predict(cal_model, newd, type = "lpmatrix")
# simDev <- Cg %*% t(BUdiff)
# 
# absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
# masd <- apply(absDev, 2L, max)
# crit <- quantile(masd, prob = 0.95, type = 8)
# 
# pred <- transform(cbind(data.frame(pred), newd),
#                   uprP = fit + (2 * se.fit),
#                   lwrP = fit - (2 * se.fit),
#                   uprS = fit + (crit * se.fit),
#                   lwrS = fit - (crit * se.fit))

# predict_paleo = predict.gam(cal_model, 
#                             newdata = lct_paleo , 
#                             type    = 'response')
# lct_preds = data.frame(lct_paleo, alb_pred = predict_paleo)  
# 
# saveRDS(lct_preds,'data/lct_paleo_preds_albclim.RDS')
# 
# 
# 
# preds_albclim = readRDS('data/lct_paleo_preds_albclim.RDS')
# preds_bluesky = readRDS('data/lct_paleo_preds_bluesky.RDS')
# 
# preds_merged = merge(preds_albclim[,c('ages', 'long', 'lat', 'alb_pred')], 
#       preds_bluesky[,c('ages', 'long', 'lat', 'alb_pred')], by = c('ages', 'long', 'lat'))
# 
# ggplot(data=preds_merged) +
#   geom_point(aes(alb_pred.x, y=alb_pred.y)) +
#   geom_abline(slope=1, intercept = 0) +
#   xlab('alb clim') +
#   ylab('blue sky')


# cal_model = readRDS('data/calibration_model5.RDS')
# cal_model = readRDS('data/calibration_mod5_albclim.RDS')
# cal_brm = readRDS(paste0('data/calibration_brms_m4_', alb_prod, '.RDS'))
# 
# paleo_predict_brm = predict(cal_brm, 
#                             newdata = lct_paleo,
#                             summary = TRUE)
# paleo_predict_brm = data.frame(lct_paleo, 
#                                alb_mean = paleo_predict_brm[,'Estimate'], 
#                                alb_sd = paleo_predict_brm[,'Est.Error'])  
# 
# saveRDS(paleo_predict_brm, paste0('data/paleo_predict_brms_', alb_prod, '.RDS'))
