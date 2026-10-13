# 4_calibration_model.R: fit the models that turn land cover into albedo
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(gam)
library(mgcv)

# Settings and file locations: config.R, and R/paths.R for every file read or written. The
# directories products go to are created by pipeline_paths() (issue I07).
source('R/config.R')
source('R/paths.R')
cfg   = load_config()
paths = pipeline_paths(cfg)
alb_prod = paths$tag   # the albedo product in file names


ctrl <- list(nthreads=8, maxit=500)

cal_interp_data =readRDS(path_file(paths, 'calibration_table'))

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

  saveRDS(mod1_interp, path_calibration_model(paths, 'mod1', month))

  print('Fitting model 2: (x,y) + elevation')
  mod2_interp = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod2_interp, path_calibration_model(paths, 'mod2', month))

  print('Fitting model 3: (x,y) + elevation + OL')
  mod3_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod3_interp, path_calibration_model(paths, 'mod3', month))

  print('Fitting model 4: (x,y) + elevation + OL + ET')
  mod4_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod4_interp, path_calibration_model(paths, 'mod4', month))

  print('Fitting model 5: (x,y) + elevation + OL + ET + ST')
  mod5_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, k=50) + s(ET, k=50) + s(ST, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod5_interp, path_calibration_model(paths, 'mod5', month))

  print('Fitting model 6: (x,y) + elevation + s(OL, ET, ST)')
  mod6_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, k=75),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod6_interp, path_calibration_model(paths, 'mod6', month))

  print('Fitting model 7: (x,y) + elevation + gp(OL, ET, ST)')
  mod7_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='gp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod7_interp, path_calibration_model(paths, 'mod7', month))

  print('Fitting model 8: (x,y) + elevation + tp(OL, ET, ST)')
  mod8_interp = mgcv::bam(get(month) ~ s(x, y, bs='gp', k=500) + s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod8_interp, path_calibration_model(paths, 'mod8', month))

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

  mod1_interp = readRDS(path_calibration_model(paths, 'mod1', month))
  mod2_interp = readRDS(path_calibration_model(paths, 'mod2', month))
  mod3_interp = readRDS(path_calibration_model(paths, 'mod3', month))
  mod4_interp = readRDS(path_calibration_model(paths, 'mod4', month))
  mod5_interp = readRDS(path_calibration_model(paths, 'mod5', month))
  mod6_interp = readRDS(path_calibration_model(paths, 'mod6', month))
  mod7_interp = readRDS(path_calibration_model(paths, 'mod7', month))
  mod8_interp = readRDS(path_calibration_model(paths, 'mod8', month))

  this_AIC = AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp)

  AIC_list[[month]] = this_AIC

  AIC_df[,month] = round(this_AIC$AIC)

  ANOVA_list[[month]] = anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp, test = "Chisq")
}

write.csv(AIC_df, path_file(paths, 'aic_table'), row.names = FALSE)

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

  saveRDS(mod_spatial_elev_cover, path_calibration_model(paths, 'spatial_elev_cover', month))

  print('Fitting model: (x,y)')
  mod_spatial = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_spatial, path_calibration_model(paths, 'spatial', month))

  print('Fitting model: elevation')
  mod_elev = mgcv::bam(get(month) ~s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_elev, path_calibration_model(paths, 'elev', month))

  print('Fitting model: (x,y) + elevation')
  mod_spatial_elev = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) + s(elev, k=50),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_spatial_elev, path_calibration_model(paths, 'spatial_elev', month))

  print('Fitting model: (x,y) + tp(OL, ET, ST)')
  mod_spatial_cover = mgcv::bam(get(month) ~ s(x, y, bs="gp", k=500) +  s(OL, ET, ST, bs='tp', k=200),
                               data=cal_interp_data,
                               family=betar(link="logit"),
                               method="REML",
                               na.action=na.omit,
                               control=ctrl)

  saveRDS(mod_spatial_cover, path_calibration_model(paths, 'spatial_cover', month))

  print('Fitting model: tp(OL, ET, ST)')
  mod_cover = mgcv::bam(get(month) ~ s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_cover, path_calibration_model(paths, 'cover', month))

  print('Fitting model: tp(OL, ET, ST) + elev')
  mod_cover_elev = mgcv::bam(get(month) ~ s(elev, k=50) + s(OL, ET, ST, bs='tp', k=200),
                          data=cal_interp_data,
                          family=betar(link="logit"),
                          method="REML",
                          na.action=na.omit,
                          control=ctrl)

  saveRDS(mod_cover_elev, path_calibration_model(paths, 'cover_elev', month))

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

  saveRDS(mod8_interp, path_calibration_model(paths, 'mod8', month))

}
