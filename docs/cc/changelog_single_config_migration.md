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
| I05 Run manifests; consistency check | #6 | merged 2026-10-14 | f867506 |
| I04a Model, draw and reduction settings; seed | #7 | merged 2026-10-14 | 520b6e6 |
| I04b Months, ages, alias, domain, ice, CACK year | #8 | merged 2026-10-14 | 9210d04 |
| I06 Manifests in every script; plain config loader | #9 | merged 2026-10-14 | 3ea2624 |
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

---

## I05: one manifest per run, written from the start; the consistency check (#6), merged 2026-10-14

**Purpose.** Know, for every product, which code, settings and inputs made it, including
for runs that fail or are killed; and stop a script from silently reading a product made
under different analysis settings.

**Decisions taken while planning (Chris, 2026-10-13):**
- Scripts stay flat: one call at the top and one at the bottom, not the issue's design of
  wrapping each script in a function, so Andria can still run a script line by line.
- The override is one config setting, `run$allow_stale` (FALSE by default), not the
  `RUN_ALLOW_STALE` environment variable, and there is no per-run note (`RUN_NOTE` is
  gone): the config file and the commit say what a run was.

**New:**
- **`R/run_manifest.R`**, rewritten. `run_begin(script)` reads the config (or `--config`),
  sets up the paths and writes `runs/<script>/<UTC time to the ms>_<pid>.md` at once, status
  `started`. `read_input(run, path, reader = readRDS, ...)` hashes each input just before it
  is read; `write_output(run, object, path, writer = saveRDS, ...)` hashes each output just
  after it is written; both rewrite the manifest (temporary file, then rename).
  `run_finish(run)` closes the plot devices, lists the figures and sets `completed`. An error
  sets `failed` with the message; a killed run stays `started`; a config that cannot be read
  is recorded too. The manifest is a readable summary with the full record as JSON at the
  end: whole config, full commit, uncommitted changes to `config.R`, the config file used,
  `scripts/` and `R/` (untracked files included; "unknown" without git), inputs, outputs and
  figures with MD5s, R and package versions, RNG kind, thread settings.
- **The consistency check.** Reading a product of an earlier script consults that script's
  newest manifest only: it must have written this file with this MD5, and its `analysis`
  section must equal the current one. Otherwise the script stops and lists each differing
  setting (`forcing.earth_area_m2: [510100000000000] -> [520000000000000]`). Presentation
  and run settings may differ. With `run$allow_stale = TRUE` it goes on and the manifest
  lists the file under `stale_inputs`. Original inputs are hashed, not checked.
- **Figures** are the paths this run asked `path_figure()` for, modified since it started,
  plus `Rplots.pdf` only if the run plotted to the default device. (The first version took
  every file modified since the start; the verification below showed why that is wrong.)
- **`run$allow_stale`** in `config.R` and the schema; `root_relative()` in `R/paths.R`,
  split out of `producer()` for reuse.
- **`tests/test_run_manifest.R`**: 31 cases, each a real `Rscript` process in a scratch copy
  of `R/` and `config.R`, covering every behaviour above, including two runs started
  together, two runs writing figures side by side, `source()`, SIGKILL and a 2.1 GB file.

**Changed:**
- **Scripts 7, 7a, 8 and 9**, the four that used the old `run_start()`/`run_end()`, use the
  new calls; every read and write of a catalogued file goes through `read_input()` /
  `write_output()` with the same reader or writer and arguments, so the bytes are
  unchanged. Scripts 1-6 follow in I06.
- **Dropped:** script 8's hand-written note that CACK band 3 is the year 2003 (question C3);
  the band becomes a config setting in I04b and is then in every manifest. The hand-kept
  output lists, which missed script 9's EGU PNG and claimed stale files in script 7.
- **Behaviour worth knowing:** once an error handler is installed, `Rscript` carries on
  after an error and exits 0 (tested), so the handler ends a non-interactive run with
  status 1 itself.
- README (provenance section, launch example) and CONTRIBUTING (decisions on record).

**Outputs changed.** None.

**Verification.** Code at 4e8fef2, then the figure fix. Machine load about 48 on 48 cores
from another project throughout; this run used 16 cores.

| Run | Result | Time, peak memory |
|---|---|---|
| script 7 with `allow_stale = FALSE`, from the seeded baseline predictions | stopped at the predictions summary ("no run of 6_prediction_model is recorded"); manifest `failed` with the error and what it had read and written | 3 s |
| scripts 7 and 7a in parallel, then 8 and 9 (`allow_stale = TRUE`) | all six tail-anchor files and four forcing tables byte-identical; 23 figures identical to I07's; every recorded MD5 equals the file on disk; 8's four checks against 7 and 7a and 9's two against 8 and 7 pass; stale inputs only the two seeded predictions | 121 min, 23.3 GB (7); 80 min, 2.3 GB (7a); <1 min each (8, 9) |
| the same run's manifests | 7a had claimed 13 of script 7's figures and both claimed `Rplots.pdf`: fixed as above | - |
| scripts 8 and 9 rerun with the fix | anchors and figures still match; 9 lists exactly its seven figures; 8 lists only `Rplots.pdf`; checks pass against the new run of 8 | <1 min each |

The repository's `figures/`, `runs/` and `Rplots.pdf` were unchanged and the 331 frozen
inputs matched before and after. The figure attribution for 7 and 7a side by side is
covered by the new test, not by a second two-hour run.

**Left as found:** scripts 7 and 7a, when run side by side in one place, both print plots
to the same `Rplots.pdf`, so that file is unreliable then. It is not a product of the
pipeline; noted here, not fixed.

---

## I04a: the model, the draws, the land-cover summary and the random seed as settings (#7), merged 2026-10-14

**Purpose.** Make the analysis choices of the upper pipeline visible and checked in
`config.R`, keep the model formulas in one reviewed place, add the median as an
alternative land-cover summary, and make the simulated draws of scripts 5 and 6
repeatable.

**Decisions taken while planning (Chris, 2026-10-14):**
- Only the choice of model is a calibration setting (`calibration$model_id`). The issue's
  `maxit` and three `k` values stay in R: a single "cover" k would have covered models 7, 8
  and the experiments but not model 6 (k = 75) or the single-class terms of models 3-5
  (k = 50).
- Scripts 5 and 6 get their run manifests now rather than in I06, so the seed and the
  selected model are recorded from the start.

**New:**
- **`config.R`**, analysis section: `land_cover$reduction` ("mean" | "median", A1),
  `calibration$model_id` (mod1 ... mod8, B4), `draws$evaluation` and `draws$prediction`
  (whole numbers >= 2, A2/A3) and `draws$seed` ("random" or a whole number). All checked by
  the schema; the legal model ids come from the registry.
- **`R/model_registry.R`**: the eight ladder models and seven spatial experiments, each
  once, by id (label and formula terms, k included), with the shared controls (8 threads,
  maxit 500) and `fit_calibration_model()`. The fitted response is the month's column
  (`jan ~ ...`) instead of `get(month) ~ ...`, so a fit no longer depends on a global
  `month`. `R/paths.R` takes its model ids from it.
- **`R/preparation.R`**: `reduce_land_cover()` (the original mean, bit for bit and not
  rescaled; or the median, rescaled by `close_fractions()` to sum to 1, stopping on a
  missing, negative or all-zero cell-slice).
- **`R/random.R`**: `base_seed()` (the configured number, or for "random" one made from
  the clock and process id), `derive_seed()` (a base-R hash of base seed, script, month and
  call, so each `simulate()` call has its own seed and a month's draws do not depend on the
  other months), `draw_index()` (draw numbers past 999).
- **`run_record()`** in `R/run_manifest.R`: a named fact in the manifest (`details`).
- **Tests:** `tests/test_preparation.R`, `tests/test_random.R`, and new cases in
  `test_config.R` and `test_run_manifest.R`.
- **`tests/anchors/interp-seed1-2026-10-14/`**: checksums of the 26 draw-based outputs of
  scripts 5 and 6 with seed 1, the byte-for-byte reference the contract asked for.

**Changed:**
- **Script 1** reduces the draws with `reduce_land_cover()` and the configured method.
- **Script 4** fits the ladder, the experiments and the third fit of model 8 in loops over
  the registry, in the original order; the AIC section keeps its explicit `AIC(mod1_interp,
  ..., mod8_interp)` evaluated `with()` the loaded fits. The repeated fits of model 8 stay
  until Andria answers general question 7.
- **Script 5**: the selected model is `model_id` (was model 8, hard-wired three times);
  its id, terms, family, method and controls are in the manifest. All four `simulate()`
  calls use `draws$evaluation` and their own seed. The spatial comparison's SP_ELEV_LC is
  script 4's `spatial_elev_cover` fit (same formula as model 8) rather than the selected
  model, which another `model_id` would have mislabelled.
- **Script 6**: `draws$prediction` and a seed per month; draw numbers by `draw_index()`
  (`substr(iter, 2, 4)` read draw 1000 as 100).
- **Scripts 5 and 6** read and write through the run manifest (`run_begin`, `read_input`,
  `write_output`, `run_finish`), with the base seed recorded.

**Outputs changed.** None at the default settings beyond the run-to-run variation of
unseeded draws, which the default `seed = "random"` keeps. Script 5's spatial-comparison
figures use script 4's own fit of model 8's formula for SP_ELEV_LC.

**Verification.** Code at e612b69. Each run in its own run directory from the frozen
inputs, 8 threads, upstream products seeded with `allow_stale = TRUE`. Machine load 50-60
on 48 cores from another project.

| Check | Result | Time, peak memory |
|---|---|---|
| registry formulas vs old script 4 | all 16 calls identical (parse trees) | - |
| script 4 AIC section through the registry code, from the frozen fits | AIC table byte-identical | under 1 min |
| one-month refit of model 8 (January) through `fit_calibration_model()` | predictions identical to the stored fit (max difference 0); AIC -6669 equal to the anchor | 14 min, 0.9 GB |
| script 1, mean | both land-cover tables byte-identical | under 1 min, 3.1 GB |
| script 1, median | same columns, rows and keys as the mean tables; every row sums to 1 (max error 2.2e-16); equal to medians grouped independently and rescaled (2.2e-16); mean and median differ by 0.012 on average, at most 0.16; before rescaling the medians sum to 0.80-1.02 | under 1 min |
| script 2 on the median tables | runs (schema smoke test) | 4 min |
| scripts 5 then 6, default (random) and a second random run | point predictions byte-identical; statistics and summary within tolerance of the unseeded anchor; the two random runs differ in all 13 draw files and the statistics, with different base seeds recorded | 12 + 20 min; 5.7 / 3.7 GB |
| scripts 5 then 6, seed 1, twice | all 51 data files and 9 figures identical between the two; within tolerance of the unseeded anchor; selected models byte-identical to the frozen ones | 12 + 20 min |
| scripts 5 then 6, `model_id = "mod7"`, seed 1 | the selected model predicts exactly as the ladder's model 7; manifest records mod7 and its terms; predictions differ from model 8's by 0.005 on average | 11 + 23 min |
| manifests of all ten runs | completed; every output MD5 equals the file; script 6's 36 checks against script 5's selected models pass; stale inputs only the seeded baseline files | - |
| unit tests | all seven test files pass | - |

The repository's `figures/`, `runs/`, `output/` and `Rplots.pdf` were unchanged, and the
331 frozen inputs matched, before and after. The issue's four-month seeded run needs the
month selection setting of I04b; the property it tests (a month's draws do not depend on
the other months) is tested in `test_random.R` instead, and the 1000-draw run by
`draw_index()`'s test.

---

## I04b: months, ages, slice alias, domain, ice and the CACK year as settings (#8), merged 2026-10-14

**Purpose.** Make the analysis choices of the tail (scripts 7, 7a, 8) visible and checked
in `config.R`, with the inherited oddities exposed under their question ids, and let any
subset of months run through scripts 4 to 9.

**Decisions taken while planning (Chris, 2026-10-14):**
- The fine time slices are a config key (`analysis$ages`), with a comment that they must
  agree with the data; script 7a stops, naming them, if a slice is not in the prediction
  table.
- The glacier-albedo column for each of the three ice roles is its own setting, with the
  legal columns listed (question C0a).

**New keys** (all checked by the schema):
- `months` (written out; any subset);
- `ages` (the reconstruction's 25 slices);
- `coarse$slice_alias = c(11500, 12000)` or NULL (C7). The comment explains that it also
  picks the 11.5 ka slice's ice polygon, and that NULL is implemented but drops the
  oldest period;
- `domain`: `lon_min`, `lat_min`, `lat_max`, `lon_max_coarse`;
- `ice`: `threshold`, `albedo_coarse`, `albedo_fixed`, `albedo_scaled` (C0, C0a, C2);
- `kernels$cack_year` 2001-2016 (C3; band = year - 2000, checked against the file's
  "2001 = 1").

Combination rules: `lat_min < lat_max`; `lon_min` west of `lon_max_coarse`; the alias's
data slice is in `ages` and its coarse age is in `coarse$ages`.

**Changed:**
- **Scripts 4, 5, 6, 7, 7a, 8** use `months`; script 4's three redundant reassignments
  are removed. Script 2 deliberately keeps all twelve months, so the calibration table
  serves any selection.
- **Script 7a** pairs the configured `ages` and uses the threshold and domain keys.
- **Script 7** applies the alias and `lon_max_coarse`.
- **Script 8** uses `lat_min`/`lat_max` and the CACK band from `cack_year`.
- **Script 1's diagnostic** builds its zones from the domain keys, with the same text at
  the defaults.
- **Dead copies removed:** the fine `ages` and its relabelling in scripts 7 and 8, and
  7a's coarse `years` and `labels_year`.

**Outputs changed.** None at the default settings.

**Verification.** Code at b88813b. Each run was in its own run directory, at 8 threads,
starting from frozen inputs with `allow_stale = TRUE`. Load was 40-60 on 48 cores from
another project; this verification used three 8-core lanes.

| Check | Result | Time, peak memory |
|---|---|---|
| baseline tail: 7 ‖ 7a, then 8, 9 | all six tail-anchor files and four forcing tables byte-identical; 23 figures identical to I05's | 109 min, 23.3 GB (7); 77 min (7a) |
| script 1, diagnostic mode | both land-cover tables byte-identical; 28 figures and the diagnostic CSV identical to I07's | 1 min |
| four months (feb, may, aug, nov), seed 1: scripts 5, 6, 7a, 7, 8, 9 | the four months' draw and summary files byte-identical to the twelve-month seeded anchor (the check carried over from I04a); the forcing table has exactly those months | 2, 6, 17, 35 min |
| `slice_alias = NULL` (script 7, four months) | the 10-12 ka differences disappear; the younger periods are identical | 34 min |
| `ice$threshold = 0.6` (7a, four months) | the `*_part` variants are identical, the `*_thresh` variants differ (2,408 of 268,724 rows of `veg_ice_thresh`) | 17 min |
| `ice$albedo_fixed = "ice_albedo_sc"` (7a) | the fixed-ice variants equal the default run's scaled-ice ones exactly; the vegetation-only variants are unchanged | 17 min |
| `kernels$cack_year = 2010` (script 8) | every non-CACK column is identical to the baseline; the CACK kernel and forcings differ; `raster(band = 10)` holds the netCDF's Year 10 values (not Year 3 or 11) for two months; the Year attribute reads "2001 = 1" | under 1 min |
| `domain$lat_max = 70` (script 8) | identical to the baseline's rows below 70 N | under 1 min |
| literal equivalence before the runs | the config's `ages`, the diagnostic zone labels and the CACK band are `identical()` to the old literals | - |

The repository's figures, runs, data and outputs were unchanged, and the 331 frozen inputs
matched. Two first-pass failures were in my comparison harness, not the pipeline:
- a misread netCDF attribute;
- subsetting a table with a `units` column without loading `units`, which strips the class.

---

## I06: every script on the run manifest; plain config loader; session fixes (#9), merged 2026-10-14

**Purpose.** Every invocation of every numbered script leaves a provenance record, and
no environment variable steers the pipeline.

**Decisions taken while planning (Chris, 2026-10-14):**
- **The draw-identity helper (`lc_draw`) is parked**, as overcomplicated. Astra's redesign
  was adopted instead: no ensemble work during the migration; at I14, a plain loop over
  draws.
- **Readability before elegance**, recorded in AGENTS.md (chris-dev 85af40a).
- **Script 1's diagnostic switch** becomes `run$diagnostic` in the config. `RUN_CORES` is
  dropped. No environment variable is used as a setting; the three BLAS thread variables
  are recorded only.
- **The config loader** becomes plain `source()` plus the same validation, replacing the
  restricted parser.
- **The behaviour bugs** found by Astra's readability review are fixed simply; its other
  items go to I15.

**Changed:**
- **Scripts 1-4** now call `run_begin()`, read and write through the manifest, and end
  with `run_finish()`, as scripts 5-9 already did.
  - Script 4's fits and AIC table are recorded too.
  - Script 1's elevation cache is a recorded input. When absent, the AWS lookup is
    recorded and the new cache written through the manifest.
- **In all ten scripts** `run_begin()` precedes the `library()` calls, so a missing
  package is recorded as a failed run.
- **Script 8** no longer reads script 7's three ice tables, which it never used.
- **`config.R`** gains `run$diagnostic`, and its header explains that later issues add
  method options rather than closing the key set.
- **`R/run_manifest.R`:**
  - the error handler is installed for batch runs only;
  - the error and device options are restored at the end;
  - only the run's own plot devices are closed;
  - the "no run recorded" message explains that files made before run manifests existed
    have none.
- **`seed_run_dir()`** copies instead of linking, so a run can never write into the
  baseline through a link.
- **`R/config.R`** reads the config with `source()`.
- **Tests** updated and extended: session restoration, seeding by copy, the new
  loader's contract and `run$diagnostic`.
- **Docs:** Astra's ensemble redesign and readability review, and the two Fable reviews
  (extraction issues; completed work).

**Outputs changed.** None.

**Verification.** Code at 34365c1 (merged with feature/single-config). Each run was in its
own run directory at 8 threads.

| Check | Result | Time, peak memory |
|---|---|---|
| full chain at seed 1: 1, 2, 3, script 4's AIC section (from frozen fits), 5, 6, each checked against the previous script's real manifest | land-cover tables, calibration table, AIC table and point predictions byte-identical; all 26 files of the seed-1 anchor byte-identical; 62 figures of scripts 2-3 identical to I07's; manifests completed, with checks 2 vs 1, 3 vs 2, 5 vs 2 and 37 of 6 vs 1 and 5 all passing | 0, 4, 1, 0, 10, 17 min |
| tail from frozen predictions: 7 ‖ 7a, then 8, 9 | all ten tail and forcing anchors byte-identical; 23 figures identical; 8's check against 7a and 9's two checks pass; 8 now reads only 7a's product | 103 min, 23.2 GB (7); 70 min (7a) |
| script 1, `run$diagnostic = TRUE` | both tables byte-identical; 28 figures and the diagnostic CSV identical to I07's | 1 min |
| a copy of script 5 with an error injected after its first output | manifest `failed` with the error, the selected-model output written before it recorded; exit status 1 (rerun by hand) | under 1 min |
| unit tests | all seven test files pass | - |

The repository was unchanged and the frozen inputs matched before and after.

**A fault in my verification drivers, found here.** The I04a, I04b and I06 drivers logged
`exit=$?` after a `$(date)` in the same line, which resets `$?`, so every `exit=` in those
status logs is meaningless. No conclusion rested on them: every result above, and in
I04a and I04b, comes from the outputs and the manifests' recorded status. Driver template
fixed (capture the code first).

