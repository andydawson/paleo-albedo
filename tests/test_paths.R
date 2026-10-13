# Tests for R/paths.R. Run from the repository root: Rscript tests/test_paths.R
source("R/config.R")
source("R/paths.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

cfg  <- load_config()
base <- suppressMessages(pipeline_paths(cfg, baseline_paths()))
tmp  <- file.path(tempdir(), "paths_test_run")
alt  <- suppressMessages(pipeline_paths(cfg, redirected_paths(tmp)))

# The baseline catalogue against the files on disk when the migration began (I01): every
# catalogued file of the pipeline is one of them, under exactly the name it had.
frozen <- sub("^[0-9a-f]+ +", "", readLines("tests/frozen_inputs.md5"))
cat_files <- catalogue_paths(base)
missing <- setdiff(cat_files, frozen)
if (length(missing)) cat("catalogued but not in the frozen inventory:\n ", paste(missing, collapse = "\n  "), "\n")

# The anchors' map from anchor file name to the place the pipeline writes it.
live <- read.delim("tests/anchors/live_paths.tsv", comment.char = "#", header = FALSE,
                   col.names = c("anchor", "path"))

# Which catalogued paths are inputs and which products, in each layout; and the producer
# of every one of them (producer() stops on a path it does not know).
input_ids     <- PATH_CATALOGUE$id[PATH_CATALOGUE$kind == "input"]
input_files   <- vapply(input_ids, function(id) path_file(base, id), "", USE.NAMES = FALSE)
alt_files     <- catalogue_paths(alt)
alt_inputs    <- alt_files[alt_files %in% input_files]
alt_products  <- setdiff(alt_files, input_files)
base_producers <- vapply(cat_files, function(p) producer(base, p), "", USE.NAMES = FALSE)
alt_producers  <- vapply(alt_files, function(p) producer(alt, p), "", USE.NAMES = FALSE)

stopifnot(
  length(missing) == 0,
  all(live$path %in% cat_files),
  # every product family on disk is covered: 192 fits, 12 selected, 5 prediction families
  sum(grepl("^output/calibration/.*\\.RDS$", cat_files)) == 192,
  sum(grepl("^output/prediction/", cat_files)) == 12 * 3 + 2,

  # the accessors give today's names exactly
  identical(path_file(base, "lct_paleo"), "data/lct_paleo_reveals_interp.RDS"),
  identical(path_prediction(base, "summary"), "output/prediction/paleo_interp_predict_gam_summary_bluesky.RDS"),
  identical(path_prediction(base, "samples", "feb"), "output/prediction/paleo_interp_predict_gam_samps_feb_bluesky.RDS"),
  identical(path_calibration_model(base, "mod8", "nov"), "output/calibration/calibration_mod8_interp_nov_bluesky.RDS"),
  identical(path_calibration_model(base, "selected", "jan"), "output/calibration/calibration_mod_interp_selected_jan_bluesky.RDS"),
  identical(path_calibration_model(base, "cover_elev", "jan"), "output/calibration/calibration_mod_cover_elev_jan.RDS"),
  identical(path_calibration_model(base, "spatial_elev_cover", "jan"), "output/calibration/calibration_mod_spatial_elev_coverjan.RDS"),
  identical(path_forcing(base), "output/forcing/RF_holocene_all_cases.RDS"),
  identical(path_figure(base, "x.pdf"), "figures/x.pdf"),
  identical(path_figure(base, "diagnostics", "x.png"), "figures/diagnostics/x.png"),
  identical(path_runs(base), "runs"),

  # a run directory moves products, figures and manifests, never inputs
  identical(path_file(alt, "lct_paleo"), file.path(tmp, "data/lct_paleo_reveals_interp.RDS")),
  identical(path_forcing(alt), file.path(tmp, "output/forcing/RF_holocene_all_cases.RDS")),
  identical(path_figure(alt, "x.pdf"), file.path(tmp, "figures/x.pdf")),
  identical(path_runs(alt), file.path(tmp, "runs")),
  identical(path_file(alt, "kernel_cack"), "data/radiative-kernels/CACKv1.0/CACKv1.0.nc"),
  dir.exists(file.path(tmp, "output/calibration")), dir.exists(file.path(tmp, "figures")),
  all(startsWith(alt_products, tmp)), !any(startsWith(alt_inputs, tmp)),

  # producer() resolves every catalogued path, in both layouts, and knows inputs
  identical(is.na(base_producers), cat_files %in% input_files),
  identical(is.na(alt_producers), is.na(base_producers)),
  identical(producer(base, "output/forcing/RF_holocene_all_cases.RDS"), "8_radiative"),
  identical(producer(base, "output/calibration/calibration_mod_spatial_elev_coverjan.RDS"), "4_calibration_model"),
  identical(producer(base, "output/calibration/calibration_mod_interp_selected_jan_bluesky.RDS"), "5_calibration_eval"),
  identical(producer(alt, file.path(tmp, "data/ALB_diffs_bluesky.RDS")), "7a_alb_diff_full"),
  identical(producer(base, "./data/lct_modern_reveals_interp.RDS"), "1_veg_lct_prep"),
  is.na(producer(base, "data/grid.RDS")),
  fails(producer(base, "data/lct_paleo.RDS")),                 # an old file no script uses
  fails(producer(base, "output/calibration/calibration_mod9_interp_jan_bluesky.RDS")),

  # bad requests stop
  fails(path_file(base, "nonsense")),
  fails(path_file(base, "lct_paleo", month = "jan")),
  fails(path_prediction(base, "summary", "January")),
  fails(path_prediction(base, "samples")),
  fails(path_calibration_model(base, "mod9", "jan")),
  fails(path_calibration_model(base, "mod8")),
  fails(seed_run_dir(base, "data/grid.RDS"))
)

# In a run directory, plots printed with no device open go to its own Rplots.pdf.
local({
  plot(1); dev.off()
  stopifnot(file.exists(file.path(tmp, "Rplots.pdf")))
})

# seed_run_dir links or copies baseline products into a run directory, once.
seeded <- seed_run_dir(alt, path_file(base, "calibration_stats"))
stopifnot(identical(seeded, file.path(tmp, "output/calibration/calibration_model_stats.csv")),
          identical(unname(tools::md5sum(seeded)), unname(tools::md5sum(path_file(base, "calibration_stats")))),
          identical(seed_run_dir(alt, path_file(base, "calibration_stats")), seeded))
unlink(tmp, recursive = TRUE)
cat("test_paths: all tests passed\n")
