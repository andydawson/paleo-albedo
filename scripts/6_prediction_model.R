library(raster)
library(tidyr)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(mgcv)
library(SemiPar)
library(gratia)

alb_prod = "bluesky"

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_paleo = readRDS('data/lct_paleo_reveals.RDS')
colnames(lct_paleo)[1] = 'year'

# lct_paleo = lct_paleo[which(lct_paleo$month == 4),]

cal_model = readRDS(paste0('data/calibration_mod7_', alb_prod, '.RDS'))


paleo_predict_gam = predict.gam(cal_model, 
                           newdata = lct_paleo , 
                           type    = 'response')
paleo_predict_gam = data.frame(lct_paleo, alb_mean = paleo_predict_gam)  

saveRDS(paleo_predict_gam, paste0('data/paleo_predict_gam_', alb_prod, '.RDS'))


paleo_sim_gam = simulate(cal_model,
                   nsim = 100,
                   data = lct_paleo)

paleo_sim_gam_melt = melt(paleo_sim_gam)
colnames(paleo_sim_gam_melt) = c('cell_idx', 'iter', 'value')

paleo_sim_gam_sum = paleo_sim_gam_melt %>% 
  group_by(cell_idx) %>%
  summarize(alb_mean = mean(value), 
            alb_sd = sd(value),
            alb_lo = quantile(value, c(0.025)), 
            alb_mid = quantile(value, c(0.5)), 
            alb_hi = quantile(value, c(0.975)))

paleo_sim_gam_df = data.frame(lct_paleo,  paleo_sim_gam_sum)  

saveRDS(paleo_sim_gam_df, paste0('data/paleo_predict_gam_', alb_prod, '.RDS'))


##modify families for use in GAM fitting and checking 
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


# cal_model = readRDS('data/calibration_model5.RDS')
# cal_model = readRDS('data/calibration_mod5_albclim.RDS')
cal_brm = readRDS(paste0('data/calibration_brms_m4_', alb_prod, '.RDS'))

paleo_predict_brm = predict(cal_brm, 
                        newdata = lct_paleo,
                        summary = TRUE)
paleo_predict_brm = data.frame(lct_paleo, 
                               alb_mean = paleo_predict_brm[,'Estimate'], 
                               alb_sd = paleo_predict_brm[,'Est.Error'])  

saveRDS(paleo_predict_brm, paste0('data/paleo_predict_brms_', alb_prod, '.RDS'))

