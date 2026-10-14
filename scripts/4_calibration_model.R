# 4_calibration_model.R: fit the models that turn land cover into albedo
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. The fits are now made in loops over R/model_registry.R (issue I04a); the
# models, their order and the files written are unchanged.


library(gam)
library(mgcv)

# Settings and file locations: config.R, and R/paths.R for every file read or written. The
# directories products go to are created by pipeline_paths() (issue I07).
source('R/config.R')
source('R/paths.R')
cfg   = load_config()
paths = pipeline_paths(cfg)
alb_prod = paths$tag   # the albedo product in file names
# The model formulas and fitting controls (LADDER_MODELS, SPATIAL_MODELS,
# fit_calibration_model) come with R/paths.R from R/model_registry.R (issue I04a).

cal_interp_data =readRDS(path_file(paths, 'calibration_table'))

cal_data2 = data.frame(site=seq(1,nrow(cal_interp_data)), cal_interp_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

# The months analysed, from config.R (analysis$months, issue I04b); month names and
# numbers come from the shared calendar, R/calendar.R (issue I02).
source('R/calendar.R')
months = cfg$analysis$months

# The eight-model ladder for every month. The formulas are in R/model_registry.R
# (issue I04a); each was written out here in full before.
for (month in months) {

  for (model_id in LADDER_MODELS) {
    print(paste('Fitting', CALIBRATION_MODELS[[model_id]]$label))
    saveRDS(fit_calibration_model(model_id, month, cal_interp_data),
            path_calibration_model(paths, model_id, month))
  }

}

AIC_list =  vector("list", length(months))
names(AIC_list) = months

AIC_df = data.frame(matrix(NA, nrow=8, ncol=1 + length(months)))
colnames(AIC_df) = c('model', months)
AIC_df$model = seq(1,8)

ANOVA_list = vector("list", length(months))
names(ANOVA_list) = months

for (month in months) {

  # The ladder's fits for this month, named mod1_interp ... mod8_interp as before.
  ladder_fits = setNames(lapply(LADDER_MODELS, function(model_id) readRDS(path_calibration_model(paths, model_id, month))),
                         paste0(LADDER_MODELS, '_interp'))

  this_AIC = with(ladder_fits, AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp))

  AIC_list[[month]] = this_AIC

  AIC_df[,month] = round(this_AIC$AIC)

  ANOVA_list[[month]] = with(ladder_fits, anova.gam(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp, test = "Chisq"))
}

write.csv(AIC_df, path_file(paths, 'aic_table'), row.names = FALSE)

# The spatial experiments (R/model_registry.R), compared by script 5.
for (month in months) {

  print(paste0('Fitting calibration models for ', month, ' albedo'))

  for (model_id in SPATIAL_MODELS) {
    print(paste('Fitting', CALIBRATION_MODELS[[model_id]]$label))
    saveRDS(fit_calibration_model(model_id, month, cal_interp_data),
            path_calibration_model(paths, model_id, month))
  }

}

# Model 8 fitted a third time and saved over the ladder's fit, as in the original script
# (whether this and the spatial_elev_cover fit of the same formula can go is general
# question 7).
for (month in months) {

  print(paste0('Fitting calibration model for: ', month))

  print(paste('Fitting', CALIBRATION_MODELS[['mod8']]$label))
  saveRDS(fit_calibration_model('mod8', month, cal_interp_data),
          path_calibration_model(paths, 'mod8', month))

}
