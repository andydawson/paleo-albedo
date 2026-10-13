# Change log: the single-config migration

A record of every change made in the single-config migration, issue by issue: what changed,
why, what it does to the outputs, and how it was verified. Written for Andria and Chris, and
for the coauthor change log. Living document, updated as each issue merges.

- **What the migration is.** Every setting of the pipeline moves into one file, `config.R`
  at the repository root, with an `analysis` section (settings that change the numbers) and
  a `presentation` section (settings that change only the figures). Each run records the
  settings it used. Design: `2026-10-08_migration_issues_single_config_v2.md`. Issues: #2 to
  #17 on GitHub, label `single-config`. Decisions: CONTRIBUTING.md, 2026-10-08.
- **Branches.** All the work is on `feature/single-config`, made from `chris-dev` 1b9d5a9 on
  2026-10-08. Each issue has its own branch, merged back with `--no-ff`. Nothing is
  committed to `chris-dev`, `main` or `legacy`.
- **The standard.** Each issue must leave the pipeline's outputs unchanged at the default
  settings: byte-identical where the pipeline is deterministic, within measured tolerances
  where it is random (scripts 5 and 6). See `2026-10-08_single_config_regression_contract.md`.
- **Order.** I01, I02, I03, I07, I05, I04a, I04b, I06, I10, I11, I12, I08, I13a, I09, I13b;
  I14 (ensemble mode) is parked. I15 (#18, readability cleanup, added 2026-10-13) runs inside
  I13b: after its reconciliation with chris-dev, before its final verification; the extraction
  issues rename the code they move as they go.

| Issue | GitHub | Status | Merge commit |
|---|---|---|---|
| I01 Regression contract | #2 | merged 2026-10-09 | 85e22bd |
| I02 Month indexing | #3 | merged 2026-10-13 | d67587d |
| I03 Validated config; script 9; map windows | #4 | merged 2026-10-13 | 80644fc |
| I07 Paths; run directories; R-only tools | #5 | merged 2026-10-13 | 252c6b6 |
| I15 Readability cleanup | #18 | filed 2026-10-13; four figure slips marked in scripts 2 and 7 | df0d28f (comments only) |

---

## I01: the regression contract (#2), merged 2026-10-09

**Purpose.** Before changing anything, fix the standard every later change is checked
against, and build the tools to check it without disturbing the files the checks compare
with.

**Tools added** (`tools/`):

- `rscript.sh` runs R the way every reference run was made. It sets `PROJ_DATA` and
  `GDAL_DATA`, without which script 1's elevation lookup and the map helpers fail outside
  `micromamba activate`. It also fixes the BLAS/OpenMP threads at 8, because `predict.gam`
  reproduces byte for byte only at the same thread count; at 4 threads the last bits
  differ.
- `rundir.sh` builds an isolated run directory, `.rundirs/<name>/`. Its `data/` and
  `output/` are links to the real files, minus the outputs of the scripts to be run, so a
  verification run writes its outputs there, never over the reference files. Since
  2026-10-12 it copies the code rather than linking it (see I02).
- `frozen.sh` locks every file under `data/` and `output/` read-only for the duration of
  the migration, and records or verifies their checksums (331 files,
  `tests/frozen_inputs.md5`). A script that tries to overwrite one fails with "Permission
  denied" instead of silently replacing it. This replaced a snapshot-and-restore plan, at
  Chris's request.
- `check_anchors.sh` compares outputs with an anchor's checksums, byte for byte. Inputs
  that are only linked into a run directory are reported as such, not as a match.
- `compare_tables.R` compares random outputs column by column with four statistics: the
  maximum difference, the 99.9th percentile, the mean absolute difference and the bias.
  The maximum alone is a weak test when every cell's summary rests on 100 random draws.
- `check_calibration_month.R` refits the selected model for one month and compares it with
  the stored fit. Predictions must agree within 1e-6 and AIC must be equal.

**Reference data added** (`tests/anchors/`):

- Checksums for the all-months anchor.
- A new anchor, `interp-forcing-2026-10-08`, for script 9's four summary tables, which had
  none.
- `live_paths.tsv`, mapping anchor file names to the places the pipeline writes them.
- `tolerances.tsv`: measured, not chosen, from three reruns each of scripts 5 and 6.
  Script 5 allows twice the largest difference seen, with a floor of 0.01. Script 6 allows
  twice the largest seen for the maximum, the 99.9th percentile and the mean absolute
  difference, and four standard errors for the bias. Deliberately planted errors are all
  caught. Approved by Chris on 2026-10-09.

**Documents.** `2026-10-08_single_config_regression_contract.md`. The anchor READMEs were
corrected:
- script 1 is reproducible given the elevation cache;
- script 5's statistics are not reproducible;
- script 7 is deterministic.

CONTRIBUTING's list of what is at the repo root was brought up to date.

**What the baseline established.** Today's code reproduces every anchor:
- scripts 1, 2, 7, 7a, 8 and 9 byte for byte;
- script 6's point predictions byte for byte;
- script 6's summary and script 5's statistics within tolerance.

Script 6's summary is genuinely random between runs. The one-month calibration check takes
about 19 minutes, not the 1 to 5 the design assumed, and `bam` at 8 threads gave identical
results for January.

**Outputs changed.** None; no pipeline code was touched.

---

## I02: no script depends on the number or order of months (#3), merged 2026-10-13

**Purpose.** Prepare for the month selection to become a setting. Before this, a run on a
subset of months would have produced wrong numbers silently.

**The bug this prevents.** Script 8 took each month's radiative-kernel band from the month's
position in its month list. With the list `feb, may, aug, nov`, February would have read
January's kernel and November April's, with no error. Other scripts sized tables for
exactly 12 months, or picked particular months by position.

**Changes:**

- **`R/calendar.R`** (new). `MONTHS` is defined once. `check_months()` rejects unknown,
  duplicated or empty selections. `month_to_number()` maps a month to its calendar number
  by name. The month list had been typed out in seven scripts, four times in script 4.
  Tests are in `tests/test_calendar.R`.
- **Scripts 2, 4, 5, 6, 7, 7a, 8** take their months from the calendar. Script 2 was
  included by Chris's decision, although the issue had left it out.
- **Script 4.** The AIC table and the model lists are sized from the month list, not
  12/13.
- **Script 5.** The correlation vector is sized from the month list. The November-only
  block is skipped when November is not selected.
- **Script 7.**
  - The two February maps run only when February is selected.
  - The four-season panel shows whichever of its months are selected.
  - The coverage filter is commented out, at Chris's direction, with the reason: it
    counted rows per cell across ages and months, so its "fewer than 8" test removed no
    cells (0 of 2,870), and under a month subset it would have started removing cells for
    the wrong reason. The intended rule is question D4 for Andria.
- **Script 7a.** Both coverage checks compare with the number of ages, not a typed 25.
- **Script 8.** The kernel band is looked up by month name.
- **Script 9.** The EGU 2024 recipe, which needs February, May, August and November, is
  skipped with a message when any is missing, never computed on a subset.
- **`tools/rundir.sh`** copies `scripts/` and `R/` into the run directory instead of
  linking them. During I02's verification, a branch switch in the main checkout rewrote
  three scripts while runs of them were in progress. Git rewrites files in place and R
  reads a script progressively, so those runs were void and were redone. A copied run can
  no longer be affected by anything done in the checkout while it runs.

**Outputs changed.** None at twelve months.

**Verification:**
- **All twelve months:**
  - Scripts 2, 4 (AIC), 6 (point predictions), 7, 7a, 8 and 9 are byte-identical.
  - Script 5 and script 6's summary are within tolerance, and the twelve selected models
    are identical.
- **Month subsets:**
  - Script 4 with four months gives exactly the reference's four columns.
  - Script 8 with four months, with the months reversed and with July alone gives
    identical kernels for the chosen months and none for the others.
  - Script 9 without November skips the recipe and reproduces its other three tables.
- The frozen inputs were unchanged throughout.

---

## I03: a validated configuration file; script 9 and the map windows migrated (#4), merged 2026-10-13

**Purpose.** Create the configuration file and the machinery that reads and checks it, and
move the first settings into it. Script 9 goes first because its assumptions were already
explicit. The map windows go too, because they were defined in five places.

**New files:**

- **`config.R`** (repo root), a single R list with two sections:
  - `analysis$coarse$ages`: the eight coarse slices, 50 to 12000 years BP.
  - `analysis$forcing`:
    - `earth_area_m2` (5.101e14);
    - `variant` (`veg_ice_thresh`);
    - `report_kernels` (`hadgem`, `cam5`, `cack`);
    - `sensitivity_variants` (the six script 9 compares).
  - `presentation$map_windows`: the three windows, named for what they draw.

  Only keys that a script now reads are there; the rest arrive issue by issue. Each key's
  comment says where it came from and, for an unconfirmed choice, gives its question id:
  - the variant: C1 and C5(a);
  - 12000 as the oldest age: C7;
  - the three windows: general section 7.
- **`R/config.R`**, the loader, `load_config()`. The config file is read as a syntax tree,
  not run as R code. It may contain only `list()`, `c()`, `seq()`, arithmetic, parentheses
  and literal values. Everything else is refused before anything is evaluated:
  assignments, any other function, variables, `pkg::fun`, and therefore any file or
  environment read. Because of this a config depends on nothing but its own text, and the
  copy of it in a run manifest is a complete record of the settings.

  `config_json()` writes the config as one JSON string for the manifests. jsonlite's
  default rounds numbers to four significant digits and drops the names of a named vector
  without warning; this function keeps full precision and writes a named vector as an
  object.
- **`R/config_schema.R`**, the allowed keys and a check for each value (type, length,
  legal values, order, range, no names). Unknown, missing or duplicated keys are errors,
  and every problem is reported at once.

  One rule spans keys: `report_kernels` must include `hadgem`, because script 9's headline
  figure and its variant sensitivity use HadGEM3. The legal forcing variants are script
  8's ten.
- **`tests/test_config.R`** covers each rejection, the allowed constructors, and the JSON
  properties (precision, one-element vectors, names, stability on reading back).

**Changed:**

- **`R/calendar.R`.** `ages_ka()` and `period_labels()` build the slice and period labels
  ("0.05 - 0.5 ka" ...) from the ages. The labels had been typed out seven times across
  scripts 7, 8 and 9, and typed labels could silently disagree with configurable ages.
  Tests check they reproduce the typed strings exactly.
- **Script 9.**
  - It reads Earth's area, the variant, the kernels, the sensitivity variants and the
    coarse ages from the config. Its aggregation is unchanged.
  - Its manifest records the whole config as JSON.
  - The EGU 2024 recipe is now also skipped when the coarse ages are not slide 18's seven
    periods, since its comparison with the slide's numbers only means something for those.
  - The A2 note was wrong and is corrected. It said the six sensitivity variants were the
    ones with complete coverage, but `_veg_thresh` is missing in 58,416 of 764,640 rows and
    `_ice_thresh` in 706,188, while three complete variants are not among the six. The six
    are unchanged.
- **Map windows.** These are drawing bounds only. They were defined in five places:
  - `R/map_helpers.R` (used by scripts 2 and 3);
  - script 1's diagnostics, typed twice;
  - scripts 7, 7a and 8.

  There are three distinct windows, now each defined once in the config:
  - `calibration` (-172 to -50 E, 15 to 80 N), for scripts 2 and 3;
  - `tail` (-166 to -50 E, 12 to 82 N), for script 7;
  - `diagnostics` (-172 to -50 E, 17 to 79 N), for script 1 in diagnostic mode.

  The map helpers no longer have a window of their own. `prepare_boundaries()` and
  `boundary_layers()` require it as an argument, so each script states which window it
  draws with.
- **Script 7.**
  - It reads the coarse ages and the tail window from the config.
  - Its two typed copies of the ages and its slice and period labels are made from them.
  - Scripts 7 and 9 therefore use the same periods by construction.
  - The relabelling of 11500 as 12000 (C7) and the -60 longitude cut remain in the code
    until I06.
- **Scripts 7a and 8.** Their copies of the coarse ages, the tail window and (in script 8)
  the period labels were deleted, because neither script used them. A comment marks each
  place.
- **`tools/rundir.sh`.** It also copies `config.R`, for the same reason it copies the code.

**Outputs changed.** None at the default settings. The run manifests of scripts 7 and 9
now contain the configuration.

**Verification.** Code at aef8d33, every run in its own run directory, 8 threads, frozen
inputs locked and unchanged afterwards. Figures were compared byte for byte (PDFs with
their creation dates removed) against the same scripts run on the pre-I03 code (d67587d)
or, for the tail, against I02's run.

| Run | Numeric outputs | Figures |
|---|---|---|
| script 9 from the frozen inputs | four tables byte-identical | 7 identical |
| script 1, diagnostic mode | both land-cover tables byte-identical; diagnostic CSV identical | 28 identical |
| scripts 2 and 3 | calibration table byte-identical; coarse table identical | 62 identical |
| scripts 7 and 7a in parallel, then 8 and 9 | all six tail-anchor files and four forcing tables byte-identical | 23 identical |

The map windows therefore changed nothing visible. The config and calendar tests pass. The
manifests of scripts 7 and 9 record the configuration as JSON. Measured times with the
machine otherwise fully loaded: script 7 took 114 minutes and script 7a 72.

---

## I07: one catalogue of file paths; run directories; tools in plain R (#5), merged 2026-10-13

**Purpose.** Name every file the pipeline reads or writes once, keeping every existing file
name. Let an alternative run write all its products into a directory of its own, so it can
never overwrite the baseline. Fix the path disagreement that stopped script 5.

**Decisions taken while planning (Chris, 2026-10-13):**
- Run options are a third section of `config.R`, `run`, tracked in git, rather than an
  environment variable or an untracked file. The code ships with everything needed to run
  it.
- A run directory reads earlier products only from itself. Baseline products it should
  reuse are put there explicitly; nothing falls back silently to the baseline.
- A redirected run's figures are saved in its run directory.
- The pipeline and its checks must be plain R and work on Linux, macOS and Windows. The
  shell tools were Linux-only and not readable by an R user, so they are ported to R or
  retired.
- An alternative run is described by its own config file, given to each script with
  `--config <file>`. Several config files can be committed.
- Script 5's newly runnable section is to be run, measured and reported, and its `rbind`
  loop fixed if it is slow.

**New:**
- **`R/paths.R`.**
  - `PATH_CATALOGUE` lists every file of scripts 1–9 (about 45 families). For each it gives
    whether it is an original input or a product and, for products, the script that writes
    it. The outputs of the out-of-scope spatial-eval script are listed too.
  - Names are exactly today's, oddities included.
  - Accessors: `path_file()`, `path_calibration_model()`, `path_prediction()`,
    `path_forcing()`, `path_figure()`, `path_runs()`. `producer()` says which script wrote
    a path, which I05's manifests will need. `seed_run_dir()` puts baseline products into
    a run directory, linking where the system allows and copying otherwise, and lists
    them in its `SEEDED.txt`.
  - In a run directory, plots printed with no device open go to its own `Rplots.pdf`.
- **`config.R`.** A `run` section with `dir` (NULL means the baseline locations), checked
  by the schema. `--config <file>` chooses another config file.
- **`tools/frozen.R`** and **`tools/check_anchors.R`**, R replacements of the shell tools.
  A seeded file is never reported as a match.
- **Tests:** `tests/test_paths.R` and `tests/test_tools.R`. The path test checks the
  expanded catalogue against the 331-file inventory of I01 and the anchors' path map.

**Changed:**
- **Scripts 1–9.** About 140 path expressions (data reads and writes, fits, predictions,
  tail and forcing files, original inputs, figures) now come from the catalogue, and only
  the path argument of each call changed.
  - At the baseline every path is the string it was. This was checked by evaluating all 63
    distinct old path expressions next to their replacements for every month, with no
    differences.
  - Each script loads the config and the paths at the top. The scripts' own `dir.create()`
    calls are gone, and `alb_prod` comes from the path layer.
- **Script 5 (deliberate).** It read its seven spatial-experiment models from
  `output/calibration/spatial_experiment/`, a directory script 4 never writes, so it always
  stopped there (the known failure at line 218). It now reads them from where script 4
  writes them. `SP_ELEV_LC` is the selected model, not script 4's separate fit of the same
  formula. The rest of script 5 (the spatial experiment and the November block) runs for
  the first time.
- **`R/run_manifest.R`** writes the manifest into the run's `runs/` directory.
- **Retired:** `tools/rundir.sh`, `tools/frozen.sh`, `tools/check_anchors.sh`.
  `tools/rscript.sh` stays as an optional Linux helper for the byte-for-byte checks. The
  regression contract, CONTRIBUTING and the anchor READMEs are updated to match.

**Outputs changed.** None at the default settings, apart from script 5, which now produces
the outputs of its formerly unreachable sections.

**Verification.** Code at fcc6540. Every run was made in its own run directory, chosen
with `--config`, at 8 threads, with the upstream products seeded from the baseline. The
redirected runs are themselves the fixture the issue asks for. Figures were compared byte
for byte with the I03 runs of the pre-I07 code.

| Run | Result | Time, peak memory |
|---|---|---|
| script 1, diagnostic mode | both land-cover tables byte-identical; 28 figures identical | 1 min, 3.7 GB |
| scripts 2 and 3 | calibration table byte-identical; coarse table identical; 62 figures identical | 4 + 1 min |
| script 4, AIC section through the new paths | AIC table byte-identical | under 1 min |
| script 6 | point predictions byte-identical; summary within tolerance | 18 min, 3.4 GB |
| script 5, run to the end for the first time | statistics within tolerance; all 12 selected models byte-identical, including November's re-save | 10 min, 5.6 GB |
| scripts 7 and 7a in parallel, then 8 and 9 | all six tail-anchor files and four forcing tables byte-identical; 23 figures identical | 110, 100, <1, <1 min; 22 GB (script 7) |

Further checks:
- The repository's `figures/`, `runs/` and `Rplots.pdf`, and all 331 frozen files, were
  unchanged after every run, so a redirected run writes nothing into the baseline.
- `producer()` resolves every product in the tail's run directory.
- The manifests sit in the run directory and record `run$dir`.
- Seeded inputs are reported as seeded, not as matches.
- All four test files pass.
- Script 4's write paths were checked statically, since it cannot be refitted.

**Script 5's new sections (validation, not anchoring).**
- *Spatial experiment.* It runs and draws its six pages into the run's `Rplots.pdf`, and
  the plots look sane. The three models without a spatial term (ELEV, ELEV_LC, LC) show
  much wider credible intervals around the 1:1 line than the four spatial models, which is
  worth showing Andria.
- *Warnings.* The run's warnings come from `xlim` dropping points outside the axes and from
  a deprecated ggplot2 notation, not from errors.
- *Cost.* The whole script takes 10 minutes, so the `rbind` loop was left as it is.
- *Randomness.* Nothing here is seeded until I04a.

