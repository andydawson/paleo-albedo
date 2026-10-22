# 4_calibration_model.R: fit the models that turn land cover into albedo
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.
# The models are defined once, by id, in R/model_registry.R and fitted here in loops.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('4_calibration_model')
cfg   = run$cfg
paths = run$paths
alb_prod = paths$tag   # the albedo product in file names

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(gam)
library(mgcv)
# The model formulas and fitting controls (LADDER_MODELS, SPATIAL_MODELS,
# fit_calibration_model) come with R/paths.R from R/model_registry.R.

cal_interp_data = read_input(run, path_file(paths, 'calibration_table'))

cal_data2 = data.frame(site=seq(1,nrow(cal_interp_data)), cal_interp_data)
cal_data3 = cal_data2[,c('site', 'ET', 'OL', 'ST')]

# The months analysed, from config.R (analysis$months); month names and
# numbers come from the shared calendar, R/calendar.R.
source('R/calendar.R')
months = cfg$analysis$months

# The eight-model ladder for every month, fitted by config.R's comparison method ("ML" or
# the original "REML"; question B4) because the ladder is compared by AIC below
# and in script 5. The formulas are in R/model_registry.R.
comparison_method = cfg$analysis$calibration$comparison_method
for (month in months) {

  for (model_id in LADDER_MODELS) {
    print(paste('Fitting', CALIBRATION_MODELS[[model_id]]$label, 'by', comparison_method))
    fit = fit_calibration_model(model_id, month, cal_interp_data, method = comparison_method)
    write_output(run, fit, path_calibration_model(paths, model_id, month))
    print(fit_convergence(fit))
  }

}

AIC_list =  vector("list", length(months))
names(AIC_list) = months

# CM: the ladder is compared by AIC only. The original also ran an analysis of deviance
# (anova.gam, chi-square tests) on the eight models and kept it in a list nothing read; it is
# removed because models 5 to 8 are not nested (each replaces smooths or changes a basis), so
# those tests do not test what they appear to (Chris, 2026-10-16, question B4, for Andria's
# review).
AIC_df = data.frame(matrix(NA, nrow=8, ncol=1 + length(months)))
colnames(AIC_df) = c('model', months)
AIC_df$model = seq(1,8)

for (month in months) {

  # The ladder's fits for this month, named mod1_interp ... mod8_interp as before.
  ladder_fits = setNames(lapply(LADDER_MODELS, function(model_id) read_input(run, path_calibration_model(paths, model_id, month))),
                         paste0(LADDER_MODELS, '_interp'))

  this_AIC = with(ladder_fits, AIC(mod1_interp, mod2_interp, mod3_interp, mod4_interp, mod5_interp, mod6_interp, mod7_interp, mod8_interp))

  AIC_list[[month]] = this_AIC

  AIC_df[,month] = round(this_AIC$AIC)
}

write_output(run, AIC_df, path_file(paths, 'aic_table'), write.csv, row.names = FALSE)

# The spatial experiments (R/model_registry.R), compared by script 5.
for (month in months) {

  print(paste0('Fitting calibration models for ', month, ' albedo'))

  for (model_id in SPATIAL_MODELS) {
    print(paste('Fitting', CALIBRATION_MODELS[[model_id]]$label))
    write_output(run, fit_calibration_model(model_id, month, cal_interp_data),
                 path_calibration_model(paths, model_id, month))
  }

}

# The model used for prediction (config.R analysis$calibration$model_id), fitted by REML
# into its own file, which script 5 saves as the selected model for script 6. The original
# fitted model 8 a third time here and saved it over the ladder's fit; with an ML ladder the
# two must differ, so the prediction fit now has its own file (question B4).
# (Whether the spatial_elev_cover fit of the same formula can go is general question 7.)
prediction_model_id = cfg$analysis$calibration$model_id
for (month in months) {

  print(paste0('Fitting calibration model for: ', month))

  print(paste('Fitting', CALIBRATION_MODELS[[prediction_model_id]]$label, 'by REML, for prediction'))
  fit = fit_calibration_model(prediction_model_id, month, cal_interp_data, method = 'REML')
  write_output(run, fit, path_file(paths, 'calibration_prediction_fit', month = month))
  print(fit_convergence(fit))

}

run_finish(run)
