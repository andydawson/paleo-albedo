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


N_iter = 20


# print(paste0('Fitting calibration models for ', month, ' albedo'))
# 
# print('Fitting model: (x,y)')
# 
# saveRDS(mod_spatial, paste0('output/calibration/calibration_mod_spatial_', month, '_', alb_prod, '.RDS'))
# 
# print('Fitting model: (x,y) + elevation')
# 
# saveRDS(mod_spatial_elev, paste0('output/calibration/calibration_mod_spatial_elev_', month, '_', alb_prod, '.RDS'))
# 
# print('Fitting model: tp(OL, ET, ST)')
#
# saveRDS(mod_cover, paste0('output/calibration/calibration_mod_cover_', month, '_', alb_prod, '.RDS'))
# 
# print('Fitting model: tp(OL, ET, ST) + elev')
#
# saveRDS(mod_cover_elev, paste0('output/calibration/calibration_mod_cover_elev_', month, '_', alb_prod, '.RDS'))

###############################################################################################################
## PREDICT FROM INTERP CALIBRATION MODEL
###############################################################################################################

for (month in months){
  
  print(paste0('Holocene predictions: ', toupper(month)))
  
  print(">>Predict")
  
  cal_SP_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_interp_selected_', month, '_', alb_prod, '.RDS'))
  paleo_predict_SP_ELEV_LC = predict.gam(cal_SP_ELEV_LC, 
                                         newdata = lct_interp_paleo , 
                                         type    = 'response')
  
  
  cal_SP = readRDS(paste0('output/calibration/calibration_mod_spatial_', month, '_', alb_prod, '.RDS'))
  paleo_predict_SP = predict.gam(cal_SP, 
                                 newdata = lct_interp_paleo , 
                                 type    = 'response')
  
  # print('Fitting model: (x,y) + elevation')
  # 
  cal_SP_ELEV = readRDS(paste0('output/calibration/calibration_mod_spatial_elev_', month, '_', alb_prod, '.RDS'))
  paleo_predict_SP_ELEV = predict.gam(cal_SP_ELEV, 
                                      newdata = lct_interp_paleo , 
                                      type    = 'response')
  
  # 
  # print('Fitting model: tp(OL, ET, ST)')
  #
  cal_LC = readRDS(paste0('output/calibration/calibration_mod_cover_', month, '_', alb_prod, '.RDS'))
  paleo_predict_LC = predict.gam(cal_LC, 
                                 newdata = lct_interp_paleo , 
                                 type    = 'response')
  # 
  # print('Fitting model: tp(OL, ET, ST) + elev')
  #
  cal_ELEV_LC = readRDS(paste0('output/calibration/calibration_mod_cover_elev_', month, '_', alb_prod, '.RDS'))
  paleo_predict_ELEV_LC = predict.gam(cal_ELEV_LC, 
                                      newdata = lct_interp_paleo , 
                                      type    = 'response')
  
  paleo_predict = data.frame(lct_interp_paleo,
                             SP_ELEV_LC = paleo_predict_SP_ELEV_LC,
                             SP = paleo_predict_SP,
                             SP_ELEV = paleo_predict_SP_ELEV,
                             LC = paleo_predict_LC,
                             ELEV_LC = paleo_predict_ELEV_LC)
  
  # paleo_interp_predict_gam = data.frame(lct_interp_paleo)
  # paleo_interp_predict_gam$get(month) = paleo_interp_predict_gam_vec
  
  saveRDS(paleo_predict, paste0('output/prediction/paleo_predict_spatial_eval_', month, '_', alb_prod, '.RDS'))
  
  print(">>Simulate")
  paleo_sim_SP_ELEV_LC = simulate(cal_SP_ELEV_LC,
                                  nsim = N_iter,
                                  data = lct_interp_paleo)
  
  paleo_sim_SP = simulate(cal_SP,
                          nsim = N_iter,
                          data = lct_interp_paleo)
  
  paleo_sim_SP_ELEV = simulate(cal_SP_ELEV,
                               nsim = N_iter,
                               data = lct_interp_paleo)
  
  paleo_sim_LC = simulate(cal_LC,
                          nsim = N_iter,
                          data = lct_interp_paleo)
  
  paleo_sim_ELEV_LC = simulate(cal_ELEV_LC,
                               nsim = N_iter,
                               data = lct_interp_paleo)
  # colnames(paleo_sim_ELEV_LC) = paste0(seq(1, 100)
  
  
  # paleo_sim = data.frame(lct_interp_paleo,  
  #                        paleo_sim_SP_ELEV_LC)  
  
  
  paleo_sim = bind_rows(data.frame(lct_interp_paleo, paleo_sim_SP_ELEV_LC, type = 'SP_ELEV_LC'),
            data.frame(lct_interp_paleo, paleo_sim_SP, type = 'SP'),
            data.frame(lct_interp_paleo, paleo_sim_SP_ELEV, type = 'SP_ELEV'),
            data.frame(lct_interp_paleo, paleo_sim_LC, type = 'LC'),
            data.frame(lct_interp_paleo, paleo_sim_ELEV_LC, type = 'ELEV_LC'))
  
  paleo_sim_melt = melt(paleo_sim, id.vars = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'type'))
  colnames(paleo_sim_melt) = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'type', 'iter', 'value')#c('cell_idx', 'iter', 'value')

  paleo_sim_melt$iter = as.numeric(substr(paleo_sim_melt$iter, 2, 4))
  
  saveRDS(paleo_sim_melt, paste0('output/prediction/paleo_predict_samps_spatial_eval_', month, '_', alb_prod, '.RDS'))
  
  print(">>Summarize")
  paleo_sim_sum = paleo_sim_melt %>% 
    # group_by(year, long, lat, x, y, elev, ET, OL, ST) %>%
    group_by(year, x, y, elev, ET, OL, ST, type) %>%
    summarize(alb_mean = mean(value), 
              alb_sd = sd(value),
              alb_lo = quantile(value, c(0.025)), 
              alb_mid = quantile(value, c(0.5)), 
              alb_hi = quantile(value, c(0.975)), 
              .groups = 'keep')
  
  # paleo_sim_gam_sum_df = data.frame(lct_paleo,  paleo_sim_gam_sum)  
  
  saveRDS(paleo_sim_sum, paste0('output/prediction/paleo_predict_samps_spatial_eval_summary_', month, '_', alb_prod, '.RDS'))
  
}

###############################################################################################################
## merge data frames 
###############################################################################################################

alb_preds_summary_months = data.frame(matrix(NA, nrow=0, ncol=14))
colnames(alb_preds_summary_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST", "type",
                                       "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi", "month")

alb_preds_months = data.frame(matrix(NA, nrow=0, ncol=10))
colnames(alb_preds_months) = c("year", "x", "y", "elev",  "ET", "OL", "ST", "type",
                               "alb_mean", "month")

for (month in months) {
  
  print(month)
  
  alb_preds_summary_month = readRDS(paste0('output/prediction/paleo_predict_samps_spatial_eval_summary_', month, '_', alb_prod, '.RDS'))
  alb_preds_summary_months = rbind(alb_preds_summary_months, 
                           data.frame(alb_preds_summary_month[,c("year", "x", "y", "elev",  "ET", "OL", "ST", "type",
                                                       "alb_mean", "alb_sd",  "alb_lo", "alb_mid",  "alb_hi")], 
                                      month = rep(month)))
  }

saveRDS(alb_preds_summary_months, paste0('output/prediction/ALB_paleo_predict_spatial_eval_summary.RDS'))

###############################################################################################################
## wide
###############################################################################################################

library(pwiser)

alb_preds_summary_months = readRDS(paste0('output/prediction/ALB_paleo_predict_spatial_eval_summary.RDS'))


alb_preds_summary_months_wide = alb_preds_summary_months[,c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'type', 'alb_mean')] %>% 
  pivot_wider(id_cols = c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month'), 
              names_from = 'type', 
              values_from = 'alb_mean')

alb_preds_diff_month = alb_preds_summary_months_wide[which(alb_preds_summary_months_wide$month == 'mar'),]

alb_preds_diff_month = alb_preds_summary_months_wide

alb_preds_diff_month$diff_ELEV_LC = alb_preds_diff_month$SP_ELEV_LC - alb_preds_diff_month$ELEV_LC
alb_preds_diff_month$diff_SP_ELEV = alb_preds_diff_month$SP_ELEV_LC - alb_preds_diff_month$SP_ELEV
alb_preds_diff_month$diff_SP = alb_preds_diff_month$SP_ELEV_LC - alb_preds_diff_month$SP
alb_preds_diff_month$diff_LC = alb_preds_diff_month$SP_ELEV_LC - alb_preds_diff_month$LC


foo = alb_preds_diff_month[,c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 
                              'diff_ELEV_LC', 'diff_SP_ELEV', 'diff_SP',  'diff_LC')] %>%
  pivot_longer(cols=c('diff_ELEV_LC', 'diff_SP_ELEV', 'diff_SP',  'diff_LC'))




ggpairs(alb_preds_diff_month, columns = 9:13)






# bar %>%
#   group_by(year, x, y, elev, month) %>%
#   mutate(pairwise(c('SP_ELEV_LC', 'SP_ELEV', 'SP'), list(difference = `-`)))
# 
# 
# alb_preds_summary_months %>%
#   group_by(year, x, y, elev, month) %>%
#   summarize(diff = value)

###############################################################################################################
## wide
###############################################################################################################

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
                                        # labels = labels,
                                        na.value="grey",
                                        name="Albedo diff",
                                        limits = c(-0.7,0.7),
                                        values = values)

values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)


pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)

foo_sub = foo[which(foo$year %in% ages_sub),]

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=subset(foo_sub, month=='feb'), aes(x=x,y=y, fill=value)) +
  sc_fill_diverge + 
  facet_grid(year~name)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) 
ggsave(paste0('figures/alb_diff_spatial_eval_tile_grid_', alb_prod, '.pdf'))#, width=12, height=14)
ggsave(paste0('figures/alb_diff_spatial_eval_tile_grid__', alb_prod, '.png'))#, width=12, height=14)

###############################################################################################################
## wide
###############################################################################################################

foo = alb_preds_summary_months_wide[,c('year', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 
                              'SP_ELEV_LC', 'ELEV_LC', 'SP_ELEV', 'SP',  'LC')] %>%
  pivot_longer(cols=c('ELEV_LC', 'SP_ELEV', 'SP',  'LC'))

foo_sub = foo[which(foo$year %in% ages_sub),]

ggplot() +
  geom_point(data=foo_sub, aes(x=SP_ELEV_LC, y=value)) + 
  facet_grid(year~name)

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
