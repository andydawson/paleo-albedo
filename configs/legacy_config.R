# The ORIGINAL analysis, as Andria ran it, in this pipeline's settings (issue I16).
#
# A complete copy of config.R with every method choice set back to the original analysis,
# so that the pipeline can still reproduce it (and the regression anchors in tests/anchors).
# Use it as `Rscript scripts/<script>.R --config configs/legacy_config.R`; its products go to
# its own run directory. Differences from config.R, each a decision Chris took on
# 2026-10-14 for the new analysis (to be reviewed by Andria):
#   kernels$hadgem_variable   "albedo_sw_cs"  (clear-sky)          config.R: "albedo_sw"
#   kernels$cam5_variable     "FSNSC"  (clear-sky, surface)        config.R: "FSNT"
#   kernels$cack_variable     "CACK"   (the single year 2003)      config.R: "CACK CM"
#   ice$difference_method     "legacy_components"                  config.R: "endpoint"
#   draws$seed                1   (the original was unseeded; seed 1 reproduces the seeded
#                                  anchor and meets the unseeded anchor's tolerances; Chris)
#   run$dir                   ".rundirs/legacy"
# Keep it in step with config.R: a key added there must be added here at its original value.

list(
  analysis = list(

    # The months analysed, by their three-letter names. Any subset, e.g.
    # c("feb", "may", "aug", "nov"), runs scripts 4 to 8 on those months only (script 2
    # always builds the calibration table for all twelve, so it need not be rerun).
    months = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),

    # The time slices of the land-cover reconstruction, years BP, used by script 7a to
    # pair consecutive slices. They MUST be slices present in the prediction table (the
    # reconstruction has these 25); script 7a stops if one is not. A subset makes 7a
    # difference across wider gaps. The oldest slice is 11,500 BP (question C7).
    ages = c(50, 200, seq(500, 11500, by = 500)),

    # Script 1: how the 200 posterior draws of land cover per cell, slice and class are
    # summarised into one value. "mean" (the original choice) or "median"; the median is
    # rescaled so that ET + OL + ST = 1 in every cell and slice. Andria's note in script 1
    # ("not sure if should use median") is question A1.
    land_cover = list(reduction = "mean"),

    # Script 5: the model, by id, that is saved as the selected calibration model and used
    # by script 6 for prediction. One of the ladder mod1 ... mod8; their formulas are in
    # R/model_registry.R. Model 8 was chosen by hand (question B4).
    calibration = list(model_id = "mod8"),

    # Draws of simulated albedo from the calibration model (gratia's simulate()): per
    # evaluation in script 5, and per prediction in script 6, where they become the
    # posterior samples and the summaries' mean and intervals (questions A2, A3: 100 or
    # 1,000). Whole numbers, at least 2. `seed` is "random" (a new base seed each run,
    # recorded in the run manifest; copy it here to repeat a run exactly) or a whole
    # number, which makes the draws of scripts 5 and 6 repeatable; each month and each
    # simulate() call gets its own seed derived from it (R/random.R).
    draws = list(evaluation = 100, prediction = 100, seed = 1),

    # The coarse time slices, years BP. Consecutive pairs bound the seven periods of
    # the forcing summary (script 9) and of script 7's coarse differences; period
    # labels such as '0.05 - 0.5 ka' are made from these numbers. The oldest is 12000
    # although the oldest slice in the data is 11500: script 7 relabels 11500 as 12000
    # to match it, via slice_alias below (question C7).
    coarse = list(
      ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000),
      # c(data slice, coarse age it stands for): script 7 treats the 11,500 BP slice as
      # 12,000 BP. This also decides its ice outline: script 7 matches each age to the
      # nearest ice-sheet polygon (1, 2, ... 21 ka), so 12,000 gets the 12 ka polygon,
      # whereas 11,500 would get the 11 ka one. NULL is implemented, but is not a sensible
      # choice: no slice is then at 12,000 and the oldest period (10-12 ka) drops out.
      slice_alias = c(11500, 12000)),

    # The study domain, degrees. Script 7a drops cells west of lon_min; script 8 keeps
    # cells strictly between lat_min and lat_max before computing forcing; script 7 drops
    # cells east of lon_max_coarse before its coarse differences (which script 9 uses for
    # the EGU 2024 recipe). Inherited, not chosen (questions D3, C5).
    domain = list(lon_min = -170, lat_min = 27, lat_max = 74, lon_max_coarse = -60),

    # Ice. A cell counts as ice when its ice fraction exceeds `threshold` (script 7a, the
    # *_thresh forcing variants; questions C0, C2). The ice albedo per month comes from
    # data/albedo_glacier_monthly.csv; each role names the column it uses, one of
    # "ice_albedo", "ice_albedo_fixed" or "ice_albedo_sc". Script 7 uses `ice_albedo` and
    # script 7a the other two; the file's source and the meaning of the columns are
    # question C0a.
    ice = list(
      threshold = 0.5,
      # How script 7a turns each pair of slices into an albedo change, for the threshold
      # variants: "endpoint" takes each slice's albedo (ice or vegetation) and subtracts,
      # so an ice advance is counted (decided by Chris on 2026-10-14, question C2); the
      # original "legacy_components" reported an advance across the threshold as zero,
      # and counted a pair twice where the readvance smoothing moved the older slice across it.
      # The readvance smoothing is the same in both (Andria's call, C2).
      difference_method = "legacy_components",
      albedo_coarse = "ice_albedo",          # script 7: the coarse ice-albedo maps
      albedo_fixed  = "ice_albedo_fixed",    # script 7a: the *_ice_* forcing variants
      albedo_scaled = "ice_albedo_sc"),      # script 7a: the *_icesc_* forcing variants

    # Radiative kernels (script 8): the variable read from each kernel file, which decides
    # clear-sky or all-sky and top of atmosphere (TOA) or surface. The legal names and what
    # each is are in R/kernels.R. Decided by Chris on 2026-10-14, to be reviewed by Andria:
    # all-sky kernels at the TOA for the headline (C3a, C4) and CACK's 2001-2016
    # climatological mean (C3b). The original analysis read HadGEM3 "albedo_sw_cs"
    # (clear, TOA), CAM5 "FSNSC" (clear, surface) and CACK "CACK" for one year
    # (configs/legacy_config.R). cack_year is used only with cack_variable = "CACK"; the
    # file's Year dimension is "2001 = 1".
    kernels = list(hadgem_variable = "albedo_sw_cs",
                   cam5_variable   = "FSNSC",
                   cack_variable   = "CACK",
                   cack_year       = 2003),

    # Script 9: continental forcing by period, beside the modern IPCC agents.
    forcing = list(
      # Earth's surface area, for the global-equivalent forcing (script 9, A8). The headline
      # figure plots the global-equivalent forcing for comparison with the IPCC's global
      # means (decision C5, 2026-10-14); the domain mean stays as a separate figure.
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
    dir = ".rundirs/legacy",

    # Before a script reads a file made by an earlier script, it checks (R/run_manifest.R)
    # that the file is the one that script's latest run wrote and that the run used the
    # same analysis settings as this one; if not, it stops and says what differs. TRUE
    # lets the run go on regardless, and every such file is listed in its run manifest.
    # Meant for checks against products made before run manifests existed (issue I05).
    allow_stale = FALSE,

    # Script 1's diagnostic mode: TRUE adds checks and plots of the land-cover input (the
    # discarded ice flag, absent cell-slices, mean versus median) into figures/diagnostics/.
    # Changes no output (issue I06; was the DIAGNOSTIC environment variable).
    diagnostic = FALSE
  )
)
