# 4_calibration_model.R: fit the models that turn land cover into albedo
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(gam)
library(mgcv)

alb_prod = "bluesky"

dir.create('output/calibration', recursive = TRUE, showWarnings = FALSE)
dir.create('figures', showWarnings = FALSE)

ctrl <- list(nthreads=8, maxit=500)

cal_interp_data =readRDS(paste0('data/calibration_modern_lct_interp_', alb_prod, '.RDS'))

cal_data2 = data.frame(site=seq(1,nrow(cal_interp_data)), cal_interp_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

# Month list from the shared calendar (R/calendar.R) rather than typed out in each script
# (issue I02, 2026-10-12).
source('R/calendar.R')
months = MONTHS

ctrl <- list(nthreads=8, maxit=500)

for (month in months) {

  print('Fitting model 1: (x,y)')
  mod1_interp = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod1_interp, paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 2: (x,y) + elevation')
  mod2_interp = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod2_interp, paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 3: (x,y) + elevation + OL')
  mod3_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod3_interp, paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 4: (x,y) + elevation + OL + ET')
  mod4_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod4_interp, paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 5: (x,y) + elevation + OL + ET + ST')
  mod5_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50) + s(ST, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod5_interp, paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 6: (x,y) + elevation + s(OL, ET, ST)')
  mod6_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, k=75),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod6_interp, paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 7: (x,y) + elevation + gp(OL, ET, ST)')
  mod7_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod7_interp, paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))

  print('Fitting model 8: (x,y) + elevation + tp(OL, ET, ST)')
  mod8_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod8_interp, paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))

}

months = MONTHS

ctrl <- list(nthreads=8, maxit=500)

AIC_list =  vector("list", length(months))
names(AIC_list) = months

AIC_df = data.frame(matrix(NA, nrow=8, ncol=1 + length(months)))
colnames(AIC_df) = c('model', months)
AIC_df$model = seq(1,8)

ANOVA_list = vector("list", length(months))
names(ANOVA_list) = months

for (month in months) {

  mod1_interp = readRDS(paste0('output/calibration/calibration_mod1_interp_', month, '_', alb_prod, '.RDS'))
  mod2_interp = readRDS(paste0('output/calibration/calibration_mod2_interp_', month, '_', alb_prod, '.RDS'))
  mod3_interp = readRDS(paste0('output/calibration/calibration_mod3_interp_', month, '_', alb_prod, '.RDS'))
  mod4_interp = readRDS(paste0('output/calibration/calibration_mod4_interp_', month, '_', alb_prod, '.RDS'))
  mod5_interp = readRDS(paste0('output/calibration/calibration_mod5_interp_', month, '_', alb_prod, '.RDS'))
  mod6_interp = readRDS(paste0('output/calibration/calibration_mod6_interp_', month, '_', alb_prod, '.RDS'))
  mod7_interp = readRDS(paste0('output/calibration/calibration_mod7_interp_', month, '_', alb_prod, '.RDS'))
  mod8_interp = readRDS(paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))

  this_AIC = AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp)

  AIC_list[[month]] = this_AIC

  AIC_df[,month] = round(this_AIC$AIC)

  ANOVA_list[[month]] = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp, test = "Chisq")
}

write.csv(AIC_df, 'output/calibration/AIC_table.csv', row.names = FALSE)

months = MONTHS

ctrl <- list(nthreads=8, maxit=500)

for (month in months) {

  print(paste0('Fitting calibration models for ', month, ' albedo'))

  print('Fitting model: (x,y) + elev + tp(OL, ET, ST)')
  mod_spatial_elev_cover = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_spatial_elev_cover, paste0('output/calibration/calibration_mod_spatial_elev_cover', month, '.RDS'))

  print('Fitting model: (x,y)')
  mod_spatial = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_spatial, paste0('output/calibration/calibration_mod_spatial_', month, '.RDS'))

  print('Fitting model: elevation')
  mod_elev = mgcv::bam(get(month) ~s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_elev, paste0('output/calibration/calibration_mod_elev_', month, '.RDS'))

  print('Fitting model: (x,y) + elevation')
  mod_spatial_elev = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_spatial_elev, paste0('output/calibration/calibration_mod_spatial_elev_', month, '.RDS'))

  print('Fitting model: (x,y) + tp(OL, ET, ST)')
  mod_spatial_cover = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) +  s(OL, ET, ST, bs='tp', k=200),
                               data=cal_interp_data,
                               family=betar(link="logit"),
                               method="REML",
                               na.action=na.omit,
                               control=ctrl)

  saveRDS(mod_spatial_cover, paste0('output/calibration/calibration_mod_spatial_cover_', month, '.RDS'))

  print('Fitting model: tp(OL, ET, ST)')
  mod_cover = mgcv::bam(get(month) ~ s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_cover, paste0('output/calibration/calibration_mod_cover_', month, '.RDS'))

  print('Fitting model: tp(OL, ET, ST) + elev')
  mod_cover_elev = mgcv::bam(get(month) ~ s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_cover_elev, paste0('output/calibration/calibration_mod_cover_elev_', month, '.RDS'))

}

months = MONTHS

ctrl <- list(nthreads=8, maxit=500)

for (month in months) {

  print(paste0('Fitting calibration model for: ', month))

  print('Fitting model 8: (x,y) + elevation + tp(OL, ET, ST)')
  mod8_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod8_interp, paste0('output/calibration/calibration_mod8_interp_', month, '_', alb_prod, '.RDS'))

}
