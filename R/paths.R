# Every file the pipeline reads or writes, and where it is.
#
# Usage, at the top of a script:
#   source('R/config.R'); source('R/paths.R')
#   cfg   = load_config()
#   paths = pipeline_paths(cfg)
#   readRDS(path_file(paths, 'lct_paleo'))
#   saveRDS(x, path_prediction(paths, 'summary', month))
#   ggsave(path_figure(paths, 'forcing_barplot_slide18.pdf'))
#
# Why: the scripts built their file names by pasting strings, separately in the script that
# writes a file and in each script that reads it, and the two could disagree: script 5 read
# its spatial-experiment models from output/calibration/spatial_experiment/, a directory
# script 4 never writes to, so script 5 could not run past that point. Here each file is
# named once, in PATH_CATALOGUE, with the script that produces it, so a reader and its
# writer cannot drift apart, and I05's run manifests can ask which script made a file.
#
# Where files go is set by config.R, run$dir:
#   NULL         the baseline locations, exactly today's file names under data/, output/,
#                figures/ and runs/ in the repository.
#   a directory  everything a run produces (products, figures, run manifests) goes under
#                that directory with the same relative names, so an alternative run can
#                never overwrite the baseline. Original inputs are still read from the
#                repository. Products of earlier steps are read only from that directory;
#                seed_run_dir() puts there the baseline products a run should reuse.
# Single-config migration, issue I07, 2026-10-13. Plain R, no external tools, so it works
# the same on Linux, macOS and Windows.

source('R/calendar.R')

# The albedo product in the file names. Only the blue-sky product exists, so it is fixed
# here rather than offered as a setting.
ALBEDO_TAG = 'bluesky'

# The fits of script 4, by model id: the eight-model ladder, and the spatial experiments
# (which script 5 compares with the selected model, SP_ELEV_LC).
LADDER_MODELS  = paste0('mod', 1:8)
SPATIAL_MODELS = c('spatial', 'elev', 'spatial_elev', 'spatial_cover', 'cover', 'cover_elev',
                   'spatial_elev_cover')

# One row per file or family of files. `template` is the path relative to the repository
# (input) or to the run directory (product); {month} is a month name, {model} a model id,
# {tag} the albedo tag. Names are today's names exactly, oddities included
# (calibration_mod_spatial_elev_cover{month}.RDS has no underscore before the month).
PATH_CATALOGUE = data.frame(stringsAsFactors = FALSE, rbind(
  # id                          template                                                              kind       producer
  c('veg_posts',                'data/veg_posts_interp_ice.RDS',                                      'input',   NA),
  c('elevation_cache',          'data/elev_interp_cells.RDS',                                         'input',   NA),
  c('grid',                     'data/grid.RDS',                                                      'input',   NA),
  c('bluesky_raster',           'data/blue_sky_monthly_2000-2009.tif',                                'input',   NA),
  c('boundaries_ll',            'data/map-data/geographic/pbs_ll.RDS',                                'input',   NA),
  c('boundaries',               'data/map-data/geographic/pbs.RDS',                                   'input',   NA),
  c('glacier_shapes',           'data/map-data/ice/glacier_shapefiles_21-1k.RDS',                     'input',   NA),
  c('glacier_albedo',           'data/albedo_glacier_monthly.csv',                                    'input',   NA),
  c('dalton_ice',               'data/Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif',              'input',   NA),
  c('kernel_hadgem',            'data/radiative-kernels/HadGEM3-GA7.1_TOA_kernel_L19.nc',             'input',   NA),
  c('kernel_cam5',              'data/radiative-kernels/CAM5/alb.kernel.nc',                          'input',   NA),
  c('kernel_cack',              'data/radiative-kernels/CACKv1.0/CACKv1.0.nc',                        'input',   NA),
  c('ipcc_erf',                 'data/ipcc-ar6/AR6_ERF_1750-2019.csv',                                'input',   NA),
  c('ipcc_erf_pc05',            'data/ipcc-ar6/AR6_ERF_1750-2019_pc05.csv',                           'input',   NA),
  c('ipcc_erf_pc95',            'data/ipcc-ar6/AR6_ERF_1750-2019_pc95.csv',                           'input',   NA),
  c('lct_modern',               'data/lct_modern_reveals_interp.RDS',                                 'product', '1_veg_lct_prep'),
  c('lct_paleo',                'data/lct_paleo_reveals_interp.RDS',                                  'product', '1_veg_lct_prep'),
  c('calibration_table',        'data/calibration_modern_lct_interp_{tag}.RDS',                       'product', '2_calibration_lct_bluesky'),
  c('calibration_table_coarse', 'data/calibration_modern_lct_interp_{tag}_coarse.RDS',                'product', '2_calibration_lct_bluesky'),
  c('calibration_ladder',       'output/calibration/calibration_{model}_interp_{month}_{tag}.RDS',    'product', '4_calibration_model'),
  c('calibration_spatial',      'output/calibration/calibration_mod_{model}_{month}.RDS',             'product', '4_calibration_model'),
  c('calibration_spatial_elev_cover', 'output/calibration/calibration_mod_spatial_elev_cover{month}.RDS', 'product', '4_calibration_model'),
  c('aic_table',                'output/calibration/AIC_table.csv',                                   'product', '4_calibration_model'),
  c('calibration_selected',     'output/calibration/calibration_mod_interp_selected_{month}_{tag}.RDS', 'product', '5_calibration_eval'),
  c('calibration_stats',        'output/calibration/calibration_model_stats.csv',                     'product', '5_calibration_eval'),
  c('prediction_point_month',   'output/prediction/paleo_interp_predict_gam_{month}_{tag}.RDS',       'product', '6_prediction_model'),
  c('prediction_samples_month', 'output/prediction/paleo_interp_predict_gam_samps_{month}_{tag}.RDS', 'product', '6_prediction_model'),
  c('prediction_summary_month', 'output/prediction/paleo_interp_predict_gam_summary_{month}_{tag}.RDS', 'product', '6_prediction_model'),
  c('prediction_point',         'output/prediction/paleo_interp_predict_gam_{tag}.RDS',               'product', '6_prediction_model'),
  c('prediction_summary',       'output/prediction/paleo_interp_predict_gam_summary_{tag}.RDS',       'product', '6_prediction_model'),
  c('ice_fort',                 'data/ice_fort.RDS',                                                  'product', '7_plot_preds'),
  c('ice_fort_diff_young',      'data/ice_fort_diff_young.RDS',                                       'product', '7_plot_preds'),
  c('ice_fort_diff_old',        'data/ice_fort_diff_old.RDS',                                         'product', '7_plot_preds'),
  c('coarse_differences',       'data/alb_interp_preds_diffs_{tag}.RDS',                              'product', '7_plot_preds'),
  c('albedo_differences',       'data/ALB_diffs_{tag}.RDS',                                           'product', '7a_alb_diff_full'),
  c('forcing',                  'output/forcing/RF_holocene_all_cases.RDS',                           'product', '8_radiative'),
  c('forcing_by_period',        'output/forcing/forcing_by_period.csv',                               'product', '9_forcing_barplot'),
  c('forcing_variant_sensitivity', 'output/forcing/forcing_by_period_variant_sensitivity.csv',        'product', '9_forcing_barplot'),
  c('forcing_egu2024_recipe',   'output/forcing/forcing_by_period_egu2024recipe_vs_slide.csv',        'product', '9_forcing_barplot'),
  c('modern_erf',               'output/forcing/modern_ipcc_ar6_erf.csv',                             'product', '9_forcing_barplot'),
  # The model-structure sensitivity script is out of the migration's scope (question for
  # Andria, general 7) and reads its paths itself; its outputs are listed so the catalogue
  # is complete.
  c('spatial_eval_point_month', 'output/prediction/paleo_predict_spatial_eval_{month}.RDS',           'product', '6_prediction_model_spatial_eval'),
  c('spatial_eval_samples_month', 'output/prediction/paleo_predict_samps_spatial_eval_{month}_{tag}.RDS', 'product', '6_prediction_model_spatial_eval'),
  c('spatial_eval_summary_month', 'output/prediction/paleo_predict_samps_spatial_eval_summary_{month}_{tag}.RDS', 'product', '6_prediction_model_spatial_eval'),
  c('spatial_eval_summary',     'output/prediction/ALB_paleo_predict_spatial_eval_summary.RDS',       'product', '6_prediction_model_spatial_eval')
))
names(PATH_CATALOGUE) = c('id', 'template', 'kind', 'producer')
rownames(PATH_CATALOGUE) = PATH_CATALOGUE$id

# The model ids each model-family template accepts.
TEMPLATE_MODELS = list(calibration_ladder = LADDER_MODELS,
                       calibration_spatial = setdiff(SPATIAL_MODELS, 'spatial_elev_cover'))

# The directories a run writes into, created at the start of every run.
PRODUCT_DIRS = c('data', 'output/calibration', 'output/prediction', 'output/forcing',
                 'figures', 'figures/diagnostics', 'runs')

# The two path layouts. A profile says where products go; inputs always come from the
# repository (the working directory, from which every script is run).
baseline_paths   = function() list(profile = 'baseline', root = NULL)
redirected_paths = function(dir) list(profile = 'redirected', root = dir)

# The paths for this run: the profile the config asks for unless one is given. Creates the
# product directories and says where the run writes.
pipeline_paths = function(cfg, profile = if (is.null(cfg$run$dir)) baseline_paths() else redirected_paths(cfg$run$dir)) {
  paths = c(profile, list(tag = ALBEDO_TAG))
  for (d in PRODUCT_DIRS) dir.create(under_root(paths, d), recursive = TRUE, showWarnings = FALSE)
  # Plots printed with no device open go to Rplots.pdf in the working directory (scripts 3
  # and 5 rely on this); in a run directory they go to its own Rplots.pdf instead, so a
  # redirected run writes nothing into the repository.
  if (!is.null(paths$root)) {
    rplots = file.path(paths$root, 'Rplots.pdf')
    options(device = function(...) grDevices::pdf(file = rplots, ...))
  }
  message(if (is.null(paths$root)) 'paths: baseline; products are written to data/, output/, figures/ and runs/ in the repository'
          else sprintf('paths: run directory %s; every product of this run is written there', paths$root))
  paths
}

# A relative product path under the run's root ('data/x.RDS' itself for the baseline).
under_root = function(paths, rel) if (is.null(paths$root)) rel else file.path(paths$root, rel)

# The path of one catalogued file. `month` and `model` fill the template's placeholders and
# are checked against the calendar and the model ids the template accepts.
path_file = function(paths, id, month = NULL, model = NULL) {
  if (!id %in% PATH_CATALOGUE$id) stop('path_file: no catalogued file with id ', sQuote(id, FALSE))
  entry = PATH_CATALOGUE[id, ]
  rel = entry$template
  needs = function(key) grepl(sprintf('{%s}', key), rel, fixed = TRUE)
  if (needs('month') != !is.null(month))
    stop(sprintf('path_file: %s %s a month', id, if (needs('month')) 'needs' else 'takes no'))
  if (needs('model') != !is.null(model))
    stop(sprintf('path_file: %s %s a model id', id, if (needs('model')) 'needs' else 'takes no'))
  if (!is.null(month)) {
    if (length(month) != 1) stop('path_file: one month at a time')
    rel = sub('{month}', check_months(month), rel, fixed = TRUE)
  }
  if (!is.null(model)) {
    if (length(model) != 1 || !model %in% TEMPLATE_MODELS[[id]])
      stop(sprintf('path_file: %s takes model ids %s', id, paste(TEMPLATE_MODELS[[id]], collapse = ', ')))
    rel = sub('{model}', model, rel, fixed = TRUE)
  }
  rel = gsub('{tag}', paths$tag, rel, fixed = TRUE)
  if (entry$kind == 'input') rel else under_root(paths, rel)
}

# Script 6's prediction files: the point predictions, the simulation draws and their
# summary, per month, or (month = NULL) the all-month point and summary tables.
path_prediction = function(paths, kind = c('point', 'samples', 'summary'), month = NULL) {
  kind = match.arg(kind)
  if (is.null(month) && kind == 'samples') stop('path_prediction: draws are stored per month only')
  path_file(paths, paste0('prediction_', kind, if (!is.null(month)) '_month'), month = month)
}

# A calibration fit of script 4 (model_id 'mod1' ... 'mod8', or a spatial experiment:
# 'spatial', 'elev', 'spatial_elev', 'spatial_cover', 'cover', 'cover_elev',
# 'spatial_elev_cover'), or 'selected', the model script 5 selects.
path_calibration_model = function(paths, model_id, month) {
  if (identical(model_id, 'selected')) return(path_file(paths, 'calibration_selected', month = month))
  if (identical(model_id, 'spatial_elev_cover')) return(path_file(paths, 'calibration_spatial_elev_cover', month = month))
  if (model_id %in% LADDER_MODELS) return(path_file(paths, 'calibration_ladder', month = month, model = model_id))
  if (model_id %in% SPATIAL_MODELS) return(path_file(paths, 'calibration_spatial', month = month, model = model_id))
  stop('path_calibration_model: unknown model id ', sQuote(model_id, FALSE),
       '; known: ', paste(c(LADDER_MODELS, SPATIAL_MODELS, 'selected'), collapse = ', '))
}

# Script 8's per-cell forcing table. Its name has no albedo tag.
path_forcing = function(paths) path_file(paths, 'forcing')

# A figure file, under figures/ in the run's root.
path_figure = function(paths, ...) under_root(paths, file.path('figures', ...))

# Where run manifests are written.
path_runs = function(paths) under_root(paths, 'runs')

# A path as it is written in the catalogue ('data/x.RDS'): relative to the run's root,
# with / separators. Lets paths given under a run directory, or with \ separators, be
# matched to the catalogue and compared between runs (the run manifests, issue I05).
root_relative = function(paths, path) {
  rel = gsub('\\\\', '/', path)
  if (!is.null(paths$root)) {
    root = sub('/$', '', gsub('\\\\', '/', paths$root))
    if (startsWith(rel, paste0(root, '/'))) rel = substring(rel, nchar(root) + 2)
  }
  sub('^\\./', '', rel)
}

# The script that writes `path` (NA for an original input). Stops if the path is not in
# the catalogue. Accepts a path under any run root, with / or \ separators.
producer = function(paths, path) {
  rel = root_relative(paths, path)
  for (i in seq_len(nrow(PATH_CATALOGUE))) {
    if (grepl(template_regex(PATH_CATALOGUE$id[i], paths$tag), rel)) return(PATH_CATALOGUE$producer[i])
  }
  stop('producer: ', path, ' is not a catalogued file (R/paths.R, PATH_CATALOGUE)')
}

# A catalogue template as a regular expression matching the whole relative path.
template_regex = function(id, tag) {
  t = PATH_CATALOGUE[id, 'template']
  t = gsub('([.+()^$|\\[\\]\\\\])', '\\\\\\1', t)
  t = gsub('{tag}', tag, t, fixed = TRUE)
  t = gsub('{month}', paste0('(', paste(MONTHS, collapse = '|'), ')'), t, fixed = TRUE)
  if (!is.null(TEMPLATE_MODELS[[id]]))
    t = gsub('{model}', paste0('(', paste(TEMPLATE_MODELS[[id]], collapse = '|'), ')'), t, fixed = TRUE)
  paste0('^', t, '$')
}

# Every catalogued path for the given months, expanded over months and model ids. Used by
# the tests to compare the catalogue with the files on disk.
catalogue_paths = function(paths, months = MONTHS, include_spatial_eval = FALSE) {
  out = character()
  for (id in PATH_CATALOGUE$id) {
    if (!include_spatial_eval && startsWith(id, 'spatial_eval')) next
    t = PATH_CATALOGUE[id, 'template']
    ms = if (grepl('{month}', t, fixed = TRUE)) months else list(NULL)
    ks = if (grepl('{model}', t, fixed = TRUE)) TEMPLATE_MODELS[[id]] else list(NULL)
    for (m in ms) for (k in ks) out = c(out, path_file(paths, id, month = m, model = k))
  }
  out
}

# Make baseline products available in a run directory, for a run that reuses them (for
# example script 6 reading the selected models of the baseline run of script 5). `files`
# are baseline paths, e.g. path_calibration_model(baseline, 'selected', 'jan'). Each is
# linked where the system allows it, otherwise copied; a file already in the run
# directory is left alone. The seeded paths are listed in SEEDED.txt in the run
# directory, so checks can tell a reused file from one the run produced. Returns the
# run-directory paths.
seed_run_dir = function(paths, files) {
  if (is.null(paths$root)) stop('seed_run_dir: the baseline profile has no run directory to seed')
  record = file.path(paths$root, 'SEEDED.txt')
  done = if (file.exists(record)) readLines(record) else character()
  writeLines(sort(unique(c(done, files))), record)
  vapply(files, function(f) {
    if (!file.exists(f)) stop('seed_run_dir: no baseline file ', f)
    target = file.path(paths$root, f)
    if (!file.exists(target)) {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      linked = suppressWarnings(file.symlink(normalizePath(f), target))
      if (!linked && !file.copy(f, target)) stop('seed_run_dir: could not link or copy ', f)
    }
    target
  }, character(1), USE.NAMES = FALSE)
}
