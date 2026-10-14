# The calibration models of script 4: one month's blue-sky albedo as a beta GAM (logit link,
# REML) of location (x, y), elevation and the land-cover fractions ET, OL and ST.
#
# Why: script 4 wrote out the same bam() call fifteen times, once per model, and script 5
# took "model 8" as the selected model in three separate places. Each model is now defined
# once here, by its id; script 4 fits them in a loop, and the model used for prediction is
# chosen by id in config.R (analysis$calibration$model_id, question B4). A formula changes
# only by a reviewed change to this file (issue I04a, 2026-10-14).
#
# The formulas, smoothing bases and basis sizes (k) are exactly script 4's. The fitted
# response is the month's column itself (jan ~ ...) rather than the original get(month) ~
# ..., so a fit no longer depends on a variable `month` existing where it is used; the
# predictions are the same (tools/check_calibration_month.R).

CALIBRATION_MODELS = list(
  # The ladder: each model adds to the one before. Script 5 compares them.
  mod1 = list(label = 'model 1: (x,y)',
              rhs = ~ s(x, y, bs = "gp", k = 500)),
  mod2 = list(label = 'model 2: (x,y) + elevation',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50)),
  mod3 = list(label = 'model 3: (x,y) + elevation + OL',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, k = 50)),
  mod4 = list(label = 'model 4: (x,y) + elevation + OL + ET',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, k = 50) + s(ET, k = 50)),
  mod5 = list(label = 'model 5: (x,y) + elevation + OL + ET + ST',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, k = 50) + s(ET, k = 50) + s(ST, k = 50)),
  mod6 = list(label = 'model 6: (x,y) + elevation + s(OL, ET, ST)',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, ET, ST, k = 75)),
  mod7 = list(label = 'model 7: (x,y) + elevation + gp(OL, ET, ST)',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, ET, ST, bs = "gp", k = 200)),
  mod8 = list(label = 'model 8: (x,y) + elevation + tp(OL, ET, ST)',
              rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, ET, ST, bs = "tp", k = 200)),
  # The spatial experiments: model 8's terms in other combinations, compared by script 5.
  # spatial_elev_cover is the same formula as mod8, fitted again (general question 7).
  spatial_elev_cover = list(label = 'model: (x,y) + elev + tp(OL, ET, ST)',
                            rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50) + s(OL, ET, ST, bs = "tp", k = 200)),
  spatial       = list(label = 'model: (x,y)',
                       rhs = ~ s(x, y, bs = "gp", k = 500)),
  elev          = list(label = 'model: elevation',
                       rhs = ~ s(elev, k = 50)),
  spatial_elev  = list(label = 'model: (x,y) + elevation',
                       rhs = ~ s(x, y, bs = "gp", k = 500) + s(elev, k = 50)),
  spatial_cover = list(label = 'model: (x,y) + tp(OL, ET, ST)',
                       rhs = ~ s(x, y, bs = "gp", k = 500) + s(OL, ET, ST, bs = "tp", k = 200)),
  cover         = list(label = 'model: tp(OL, ET, ST)',
                       rhs = ~ s(OL, ET, ST, bs = "tp", k = 200)),
  cover_elev    = list(label = 'model: tp(OL, ET, ST) + elev',
                       rhs = ~ s(elev, k = 50) + s(OL, ET, ST, bs = "tp", k = 200)))

LADDER_MODELS  = paste0('mod', 1:8)
SPATIAL_MODELS = setdiff(names(CALIBRATION_MODELS), LADDER_MODELS)

# Fitting controls for every model. Eight threads is not an analysis setting: it changes no
# result beyond the last bit, and it is the thread count the stored fits were made with.
CALIBRATION_CONTROL = list(nthreads = 8, maxit = 500)

# The formula of a model for one month: that month's albedo column on the model's terms.
calibration_formula = function(model_id, month) {
  if (!model_id %in% names(CALIBRATION_MODELS)) stop('calibration_formula: unknown model id ', model_id)
  eval(call('~', as.name(month), CALIBRATION_MODELS[[model_id]]$rhs[[2]]), globalenv())
}

# The model's terms as text, for the run manifest: "s(x, y, bs = \"gp\", k = 500) + ...".
calibration_terms = function(model_id) paste(deparse(CALIBRATION_MODELS[[model_id]]$rhs[[2]], width.cutoff = 500L), collapse = ' ')

# Fit one model for one month to the calibration table.
fit_calibration_model = function(model_id, month, data) {
  mgcv::bam(calibration_formula(model_id, month),
            data      = data,
            family    = mgcv::betar(link = "logit"),
            method    = "REML",
            na.action = na.omit,
            control   = CALIBRATION_CONTROL)
}
