# Project configuration: every setting the pipeline scripts read, in one place.
#
# This file is ONE R list expression and nothing else. It is read by load_config() in
# R/config.R, which accepts only list(), c(), seq(), arithmetic and literal values (no
# variables, assignments or other functions) and checks every key against the schema in
# R/config_schema.R. Each run manifest records the settings it ran with.
#
# Three sections:
#   analysis      settings that change the numbers the pipeline produces
#   presentation  settings that change only how figures look
#   run           where this run writes its files (later also threads and a note)
#
# A key appears here only once a script reads it; the rest of the pipeline's settings
# are still in the scripts and move here issue by issue (single-config migration,
# GitHub issues #2 to #17). Where a value is a choice nobody has yet confirmed, the
# comment gives its question id in docs/cc/questions_for_andria_scientific.md (C*) or
# questions_for_andria_general.md (section number). Created 2026-10-13 (issue I03).

list(
  analysis = list(

    # The coarse time slices, years BP. Consecutive pairs bound the seven periods of
    # the forcing summary (script 9) and of script 7's coarse differences; period
    # labels such as '0.05 - 0.5 ka' are made from these numbers. The oldest is 12000
    # although the oldest slice in the data is 11500: script 7 relabels 11500 as 12000
    # to match it (question C7; the alias becomes its own setting in issue I06).
    coarse = list(ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),

    # Script 9: continental forcing by period, beside the modern IPCC agents.
    forcing = list(
      # Earth's surface area, for the global-equivalent forcing (script 9, A8).
      earth_area_m2 = 5.101e14,
      # The forcing variant reported: vegetation plus ice albedo change, threshold
      # method (script 9, A1). Which ice representation the paper reports is question
      # C1; the variant choice is C5(a). Legal: any of script 8's ten rf_* variants.
      variant = "veg_ice_thresh",
      # The kernels carried through, HadGEM3 first as the headline (A3; C3, C4).
      # Must include hadgem.
      report_kernels = c("hadgem", "cam5", "cack"),
      # The variants compared in the sensitivity table and figure, HadGEM3 only (A2).
      # veg_thresh and ice_thresh have missing values (no-ice cells); the
      # aggregation drops them.
      sensitivity_variants = c("veg_thresh", "ice_thresh", "veg_ice_thresh",
                               "veg_icesc_thresh", "veg_ice_parts", "veg_icesc_parts")
    )
  ),

  presentation = list(
    # Map windows, longitude (x) and latitude (y) in degrees. Three because three
    # were inherited (questions_for_andria_general.md, section 7):
    #   calibration  maps of the modern calibration data (scripts 2 and 3)
    #   tail         maps of the hindcast and its differences (script 7)
    #   diagnostics  script 1's ice-extent diagnostics (DIAGNOSTIC=TRUE)
    map_windows = list(calibration = list(x = c(-172, -50), y = c(15, 80)),
                       tail        = list(x = c(-166, -50), y = c(12, 82)),
                       diagnostics = list(x = c(-172, -50), y = c(17, 79)))
  ),

  run = list(
    # Where a run writes everything it produces: derived data, outputs, figures and its
    # run manifest. NULL means the usual places in the repository (data/, output/,
    # figures/, runs/). A directory, e.g. ".rundirs/median_test", keeps an alternative
    # run's products apart from the baseline, which it can then never overwrite; the
    # original inputs (albedo raster, kernels, boundaries ...) are still read from the
    # repository. A run in its own directory reads the products of earlier steps only
    # from that directory: put there first the ones it should reuse, with
    # seed_run_dir() in R/paths.R. Recorded in each run manifest; changing it does not
    # count as a change of analysis. An alternative run is best described by its own
    # copy of this file (e.g. configs/median.R, with its own dir), given to each script
    # as `Rscript scripts/<script>.R --config configs/median.R`.
    dir = NULL,

    # Before a script reads a file made by an earlier script, it checks (R/run_manifest.R)
    # that the file is the one that script's latest run wrote and that the run used the
    # same analysis settings as this one; if not, it stops and says what differs. TRUE
    # lets the run go on regardless, and every such file is listed in its run manifest.
    # Meant for checks against products made before run manifests existed (issue I05).
    allow_stale = FALSE
  )
)
