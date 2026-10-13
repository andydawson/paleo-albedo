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
  I14 (ensemble mode) is parked.

| Issue | GitHub | Status | Merge commit |
|---|---|---|---|
| I01 Regression contract | #2 | merged 2026-10-09 | 85e22bd |
| I02 Month indexing | #3 | merged 2026-10-13 | d67587d |
| I03 Validated config; script 9; map windows | #4 | in verification | |

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

## I03: a validated configuration file; script 9 and the map windows migrated (#4)

*In verification; this section is completed when the issue merges.*

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

**Verification** (pending). Planned:
- script 9 from the frozen inputs: four tables byte-identical;
- scripts 1 (diagnostic mode), 2 and 3: numeric outputs byte-identical, and figures
  compared with the same scripts run on the pre-I03 code;
- scripts 7, 7a, 8 and 9 run in sequence: every tail anchor byte-identical, and script 7's
  maps compared with the I02 run;
- the config and calendar tests, and the frozen inputs unchanged.
