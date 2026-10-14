# Regression contract for the single-config migration

**What this is.** The standard every issue of the single-config migration (GitHub issues
#2 to #17, design in `2026-10-08_migration_issues_single_config_v2.md`) is verified
against: which artefacts must be byte-identical, which are compared within a tolerance
and what that tolerance is, which frozen inputs later issues verify from instead of
re-running the 27-hour calibration, and the commands that do the checking. Written for
issue I01 (#2), 2026-10-08. Nothing here may be loosened after a candidate result has been
seen; a change to this document is a decision for Chris, recorded in its own commit.

## 1. How every verification run is made

- **Environment.** Through `tools/rscript.sh` (an optional Linux helper; the pipeline
  itself needs only `Rscript`), which sets `PROJ_DATA` and `GDAL_DATA` (needed by script
  1's elevation lookup and the sf map helpers) and pins the BLAS/OpenMP thread count at
  **8**, the count of every anchored run. `predict.gam`
  reproduces byte for byte only at the same thread count; at 4 threads the predictions
  differ in the last bits. Thread count is run metadata, not an `analysis` setting, but
  comparison runs must hold it at 8.
- **Isolation.** Never in the repo's own output locations. Until I07 `tools/rundir.sh`
  built `.rundirs/<name>/` with links to the repo's files and a copy of the code. From
  I07 (2026-10-13) the run directory is set in `config.R`, `run = list(dir = ...)`: every
  product, figure and manifest of the run goes there, inputs are read from the
  repository, and the baseline products a run reuses are put there by `seed_run_dir()`
  in `R/paths.R`. `rundir.sh` was then retired, by Chris's decision that the pipeline and
  its checks be plain R and work on any system. A run reads the scripts from the
  checkout, so the checkout must not change (no edit, no branch switch) while it runs.
- **Frozen inputs are locked.** `Rscript tools/frozen.R lock` makes every file under
  `data/` and `output/` read-only for the duration of the migration, so any script that
  tries to overwrite one fails with "Permission denied" instead of replacing it silently.
  `Rscript tools/frozen.R verify` checks them against `tests/frozen_inputs.md5` (331
  files, recorded 2026-10-08). `unlock` exists for the end of the migration. (R ports of
  the shell tools, I07.)
- **Never edit a script while `Rscript` is running it** (AGENTS.md).

## 2. Artefact classes

| Class | Artefacts | Standard | Command |
|---|---|---|---|
| Deterministic | land-cover tables (script 1), calibration table (2), point predictions (6), ice and coarse-difference tables (7), consecutive differences (7a), forcing table (8), the four summary CSVs (9), AIC table (4) | **byte-identical** to the anchor, no tolerance | `Rscript tools/check_anchors.R <anchor-dir> [run-dir]` |
| Stochastic | script 5's statistics, script 6's simulation summary | within the **measured tolerances** in section 3, against the historical anchor | `tools/compare_tables.R <ref> <cand> tests/anchors/tolerances.tsv` |
| Fitted models | the `bam` objects of script 4 | predictions on the calibration table within 1e-6 of the stored fit, and integer AIC equal to the anchored table; never whole-object identity (a `bam` object carries its call and environment) | `tools/check_calibration_month.R <month>` |
| Seeded (after I04a) | scripts 5 and 6 run with a fixed configured seed | **byte-identical** between two runs, and between old and new code with the same seed | `tools/check_anchors.R tests/anchors/interp-seed1-2026-10-14 <run-dir>` (made in I04a: seed 1, checksums only) |
| Median path (I04a) | land-cover tables with `reduction = "median"` | the properties listed in I04a; no anchor exists and none is compared with the mean anchors | tests written in I04a |

## 3. Measured tolerances (approved by Chris, 2026-10-09)

The file is `tests/anchors/tolerances.tsv`; it applies only to comparisons with the
historical, unseeded anchor. Measured from three re-runs each of scripts 5 and 6 on
2026-10-08 plus the anchor itself: six pairs per artefact.

**Script 5, `calibration_model_stats.csv`** (12 rows, values rounded to 0.01). Every value
within twice the largest run-to-run difference seen, with a floor of one rounding step:
0.02 for the correlation, the share of data in the credible interval and the lower
difference quantile; 0.01 for the upper difference quantile, which never moved.

**Script 6, `paleo_interp_predict_gam_summary_bluesky.RDS`** (839,232 rows). Four
statistics per summary column, because each cell's summary rests on only 100 draws: the
mean albedo of a single cell moves by up to 0.04 between identical runs, while the
average move is 0.004 and the bias across all rows about 1e-5. A tolerance on the maximum
alone would pass an error spread over many cells.

| Column | max | 99.9th percentile | mean absolute | bias |
|---|---|---|---|---|
| `alb_mean` | 0.083 | 0.041 | 0.0077 | 2.2e-05 |
| `alb_mid` | 0.086 | 0.051 | 0.0097 | 2.8e-05 |
| `alb_sd` | 0.050 | 0.029 | 0.0056 | 1.6e-05 |
| `alb_lo` | 0.19 | 0.099 | 0.016 | 4.9e-05 |
| `alb_hi` | 0.19 | 0.10 | 0.020 | 5.7e-05 |

The first three statistics are twice the largest value seen. Bias is four standard errors
of the mean difference (the standard deviation of the per-row differences over the square
root of the row count): six pairs are too few to estimate it by doubling, and doubling
would have set `alb_hi` at 0.6 standard errors, failing most ordinary reruns.

All six re-runs pass. Deliberate errors are caught, each by a statistic other than the
maximum: every cell's mean albedo shifted by 0.001 (bias); 1% of cells shifted by 0.05
(99.9th percentile and bias); the standard deviation 20% too large everywhere (mean
absolute and bias). Identifier columns must be identical, and are, in every comparison.

## 4. Frozen inputs

Everything under `data/` and `output/` on this workstation as of 2026-10-08, 331 files,
with md5s in `tests/frozen_inputs.md5`; locked read-only with `Rscript tools/frozen.R lock`. The
ones later issues verify from:

| What | Path | md5 |
|---|---|---|
| calibration table | `data/calibration_modern_lct_interp_bluesky.RDS` | 289c0f8a |
| 192 calibration fits (4.0 GB): the 8-model ladder, 7 spatial experiments and the 12 selected models | `output/calibration/` | per file in `tests/frozen_inputs.md5` |
| point predictions | `output/prediction/paleo_interp_predict_gam_bluesky.RDS` | 647fbccf |
| prediction summary (= the anchor copy) | `output/prediction/paleo_interp_predict_gam_summary_bluesky.RDS` | 6bd0a8e5 |
| coarse differences (script 7) | `data/alb_interp_preds_diffs_bluesky.RDS` | 8ae11ad6 |
| consecutive differences (script 7a) | `data/ALB_diffs_bluesky.RDS` | 80398992 |
| forcing table (script 8) | `output/forcing/RF_holocene_all_cases.RDS` | a6705be3 |

Which scripts each issue may verify from frozen inputs: all of 5 to 9, always; script 4
is covered by the one-month check (section 5) and refitted in full only once, in I13b.
A parallel agent uses its own run directory (`run$dir` in `config.R`), never its own
copy of the fits.

## 5. The one-month calibration check

`tools/rscript.sh tools/check_calibration_month.R <month>` refits the selected model for
one month and compares it with the stored fit: predictions on the calibration table within
1e-6, integer AIC equal to `AIC_table.csv`. Later issues pass their own fitting function
(the model registry from I04a) instead of the default, which refits from the stored
model's own formula, family, method and controls.

Baseline result, January, 2026-10-08: **PASS**, prediction difference exactly 0, AIC
-6669 both, **18.9 minutes** at 8 threads. Two consequences for the issues: the check costs
about 19 minutes rather than the 1 to 5 the design assumed; and `bam` at 8 threads was
bit-reproducible for this month. The 1e-6 tolerance stays, since one month does not
establish it for every month.

## 6. Baseline evidence, 2026-10-08

Today's code (`feature/single-config` at 1b9d5a9, identical to `chris-dev`), clean tree,
8 threads, every run in its own run directory, frozen inputs locked throughout and
verified unchanged afterwards.

| Script | Runs | Result | Measured time |
|---|---|---|---|
| 1 | 1 | both land-cover tables byte-identical to the anchor (the elevation cache reproduces the original AWS lookup) | under 1 min |
| 2 | 1 | calibration table byte-identical; the coarse table identical to the copy on disk | 4 min |
| 4 | January check | as section 5 | 19 min |
| 5 | 3 | stops at its known late failure (`5:218`) after writing its outputs, every time; the 12 selected models byte-identical to the frozen ones; statistics within tolerance | 3 min each |
| 6 | 3 | point predictions byte-identical every time; summary differs between runs, within tolerance | 18-19 min each |
| 7, 7a, 8 | 1 | all six files of `interp-tail-2026-09-21` byte-identical | 120, 80, under 1 min |
| 9 | 1 | four CSVs byte-identical to the tracked copies; frozen as `interp-forcing-2026-10-08` | under 1 min |

Manifests of the tail runs: `runs/2026-10-08_1502_7a_alb_diff_full.md`,
`..._1517_7_plot_preds.md`, `..._1718_8_radiative.md`, `..._1719_9_forcing_barplot.md`.
Scripts 7 and 7a ran concurrently on separate cores, which is why both are slower than
their earlier single runs (62 and 52 min).

Settled by this run: script 6's summary really is random (the copy on disk had been
restored from the anchor in September); script 7 is deterministic, now shown on a clean
tree; script 1 is deterministic given the elevation cache.

## 7. Branches

Feature branch `feature/single-config`, created from `chris-dev` 1b9d5a9 on 2026-10-08.
One topic branch per issue off it, merged back with `--no-ff`. `chris-dev` is merged into
the feature branch at each stage boundary of the dependency map and once before the final
merge, never rebased (decision of 2026-10-08 in CONTRIBUTING). Nothing in the migration is
committed to `chris-dev`, `main` or `legacy`.
