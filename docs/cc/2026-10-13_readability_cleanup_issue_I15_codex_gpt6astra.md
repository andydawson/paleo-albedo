# Readability cleanup (I15): issue design

**What this is.** The design of migration issue I15 (#18), the readability
cleanup Chris asked for on 2026-10-13: "replace things like foo and bar etc
with interpretable variable names", plus other minor refactors that help
readability. Filed as https://github.com/andydawson/paleo-RF/issues/18.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning
effort `max`, driven by a Claude Code session on 2026-10-13 (145,630 tokens).
The codex sandbox cannot start here, so the repository files were inlined with
line numbers: AGENTS, CONTRIBUTING, config.R, every file in `R/` and
`scripts/`, the regression contract, the migration design (v2), the migration
changelog and issue #7 as a house-style example, all as of
`feature/single-config` c852b38.

**What was changed after the model wrote it** (Claude Code, with Chris's
decisions of 2026-10-13): the off-pipeline scripts are removed unless Andria
objects (the model had proposed excluding them); the "noticed" slips were
routed to general question 8 (chris-dev 9d6cfc9) and the four figure slips
marked with comments in scripts 2 and 7 (df0d28f); the model's two open
decisions were replaced by Chris's answers. Claims spot-checked against the
code before filing: `foo2` (7a), `bar` (8), `year == year` (7), the swapped
axis labels (2), the CV legend (7), the ice-outline matching (7), the coverage
denominator (5) and the unused complete-age filter (7a).

---

# [I15] Replace scratch names and clarify code without changing outputs

Readability companion to the **single-config migration**. Follow the [migration design and dependency map](https://github.com/andydawson/paleo-RF/blob/chris-dev/docs/cc/2026-10-08_migration_issues_single_config_v2.md) and [regression contract](https://github.com/andydawson/paleo-RF/blob/feature/single-config/docs/cc/2026-10-08_single_config_regression_contract.md). This issue adds a naming rule to work already being extracted, followed by a small reconciliation-aware cleanup. Code citations below refer to `feature/single-config` **c852b38**, not the older line numbers in the migration design. Andria’s review remains protected by `CONTRIBUTING.md:76–79,95–120`.

**Depends on / blocks:** Final cleanup depends on I08 (#10), I10 (#11), I11 (#12), I12 (#13), I13a (#14) and I09 (#15), with their existing prerequisites. It also requires I13b’s **reconciliation step**, but not I13b’s completion. It blocks I13b (#16)’s **final verification and merge**. Adopt the naming rule now; do not make extraction issues wait for I15 to close.

**Goal:** Replace scratch names and names that misdescribe their contents, so Andria can follow the R code without changing any analysis, numbers, saved schema or figure.

**Scope:** Scripts 1–9, including 7a; their existing and newly extracted functions in `R/`; and `make_grid()`, which script 3 actually sources (`scripts/3_plot_cal_lct_albedo.R:56–59`). Include a dated rename ledger under `docs/cc/`, following `AGENTS.md:20–23`. Scripts outside the pipeline are removed unless Andria wants them kept (see *Scripts outside the pipeline* below).

**Design:**

1. **Rename moved computations within their owning extraction issue.** I08 owns preparation names; I10 prediction names; I11 difference names; I12 kernel names; I13a aggregation names; I09 calibration/evaluation names. I05 owns the replacement manifest implementation. Keep movement and mechanical renaming distinguishable in atomic commits, and verify the final names in the owner’s already-required run. Do not rename the old script block in a separate preliminary pass.

2. **Delay remaining wrapper and plotting renames until reconciliation.** Use this order:

   `… → I10 → I11 → I12 → I08 → I13a → I09 → I13b: reconcile chris-dev → I15: remaining cleanup → I13b: verify and finish`

   This avoids changing Andria’s working vocabulary twice and lets conflict resolution compare her edits with familiar names. Preserve necessary wrapper bindings while computations move into functions. Continue the agreed checkpoint merges; never rebase the merge-built feature branch (`CONTRIBUTING.md:111–114`; regression contract `:143–147`). This is a sequence of milestones, not a circular dependency between completed issues.

3. **Keep a readable correspondence table.** Before each extraction, record old file/lines, old name, new name, what the object contains, destination function and any retained output name. Update it against Andria’s reconciled code before the final cleanup. Preserve her scientific comments and question references. Chris can use this table during her review; maintain both question files as required by `AGENTS.md:13–18`.

4. **Use a light naming convention.** Prefer existing `lower_snake_case`, naming the object’s role. Keep familiar domain abbreviations such as `alb`, `lct`, `rk`, `cfg`, `ET`, `OL`, `ST`, and ordinary local indices. Distinguish fitted predictions, response draws and response summaries. Do not standardise assignment operators, rewrite pipelines, rename every `_df`, or expand every abbreviation. Functions retain a stated purpose; comments explain why (`AGENTS.md:30–35`).

5. **Treat output names and R’s name lookup as part of the calculation.** A rename must account for `data.frame()`/`cbind()` names, named lists, formula capture, tidy evaluation, automatic axis/legend titles and factor levels. Preserve model IDs, file paths, configuration keys and seed-derivation identifiers. Use explicit existing output names where a constructor would otherwise derive a different name. A failed comparison means revise or withdraw the cleanup; never replace the anchor or widen its tolerance (regression contract `:3–9,38–43`).

6. **Keep cleanup bounded.** Share routines through the existing extraction work, without changing execution or RNG order. Removing repeated model fits or simulations is not deduplication authorised here. Keep every `library()` call; do not change a checkout while any script runs from it (`AGENTS.md:30–44`; regression contract `:20–34`).

**Concrete inventory**

Numbered filenames below are under `scripts/`. “No” means the proposed rename is a local binding change, not a change to a persisted name; its consumers still require verification. “Yes” identifies an output-facing construct requiring the stated protection. **No output changes are authorised.** “Retain” rows deliberately exclude unsafe renames.

| File:line(s) | Current name/construct | Proposed name/change | Category | Touches saved outputs? |
|---|---|---|---|---|
| `1_veg_lct_prep.R:31–56` | `lct_interp` first contains posterior draws, then their grouped means | Distinguish `cover_posts` and `cover_summary` inside I04a/I08 preparation; keep the mean calculation unchanged | misleading name | No; retain land-cover table schemas |
| `1_veg_lct_prep.R:69–74` | `ele_get_cells` | `cell_elevations`; retain explicit `elev = …$elevation` | inconsistent naming | No |
| `1_veg_lct_prep.R:125–126,161–192,206–223` | `present`, table `status`, `sums`, callback `d` | `present_cell_slice_keys`, `ice_status_table`, `cover_fraction_sums`, `class_draw_stats`; retain column `status` and all diagnostic text | other | No; diagnostic CSV/figures must remain identical |
| `2_calibration_lct_bluesky.R:48–54,134–147` | `bs_interp_df`, `bs_interp_df_coarse` | `albedo_native_by_cell`, `albedo_coarse_by_cell`, in I08; preserve month columns | inconsistent naming | No |
| `2_calibration_lct_bluesky.R:53–54,93–100,107,121` | Immediately overwritten column names; unused `cols_fill` definitions and `vname` assignments | Remove these specific unused assignments after checking the reconciled consumers | dead code / commented-out block | No; run script 2 because executable expressions are removed |
| `3_plot_cal_lct_albedo.R:45–50,152–156,184–206` | `cal_long` changes meaning after assignment from `cal_lct_melt` | Use `calibration_long` for the monthly calibration table and `cover_albedo_long` for the melted cover table; remove the misleading alias at `:184` | misleading name | No; preserve `variable`, `value`, factors and automatic plot labels |
| `3_plot_cal_lct_albedo.R:80–81,104` | Unused four-colour `color_values_four` beside the three-colour palette actually used | Remove only `color_values_four` | dead code / commented-out block | No |
| `3_plot_cal_lct_albedo.R:121–131,211–228` | Tutorial references to `euro_example`/`educ_rgb`; comments about snow and “month 5” | Describe the actual land-cover colours and the relationships plotted; retain scientific interpretation for Andria | misleading name | No; comments only |
| `4_calibration_model.R:22–23` | Unused `cal_data2`, `cal_data3` | Remove both scratch tables in the calibration owner’s cleanup | dead code / commented-out block | No numerical output; fitted-object environments remain subject to the model-specific contract |
| `4_calibration_model.R:18,30,118,154,234` | Five identical `ctrl` assignments | Let I04a’s controls/registry work establish one construction; I15 checks for leftovers rather than introducing another implementation | duplicated snippet | Yes—fit controls; values, fit calls and execution order stay unchanged |
| `5_calibration_eval.R:35–40,105–110,324–345` | `mod8_interp` also acts as the selected-model binding | I04a/I09 use `selected_fit` for the configured selection; retain `mod8_interp` wherever it specifically means ladder model 8 | misleading name | Yes—selected-model RDS files; retain IDs and all three selected-model write sites |
| `5_calibration_eval.R:230–255` | `model_name` contains a fitted model; `eval(as.name(...))` finds it | Explicit named `comparison_fits`, indexed by the existing `model` ID; local result `comparison_fit`. Preserve the seven reads and model order | misleading name | Yes—model facet values in `Rplots.pdf`; preserve `SP_ELEV_LC`, etc. |
| `5_calibration_eval.R:53–69,123–135,370–387` | `cal_interp_sim_gam` reused for draws and the table containing them | Separate `response_draws` and `response_table` in I09; name the melted result `response_long` | misleading name | No; preserve simulation column names and seed/call identity |
| `5_calibration_eval.R:243–255` | `sim`, `sim_df`, `sim_melt` | `comparison_draws`, `comparison_response_table`, `comparison_response_long` | placeholder name | No |
| `5_calibration_eval.R:192–204` | `cal_diff` contains correlations, coverage and difference quantiles | `calibration_stats`; retain the explicit CSV headers at `:202` | misleading name | No; `calibration_model_stats.csv` headers unchanged |
| `5_calibration_eval.R:206–217` | Unused `cal_sim_SPE` accumulator | Remove it; keep the `cal_sims` accumulator that the loop actually fills | dead code / commented-out block | No |
| `6_prediction_model.R:35–68` | Long object family obscures vector/table and point/simulation distinctions | I10 uses `fitted_albedo`, `point_predictions`, `response_draws`, `response_table`, `response_samples`, `response_summary`, respectively | inconsistent naming | No; preserve all prediction file schemas |
| `6_prediction_model.R:25–26` | Rename first column by position | Rename the verified `ages` column to the existing output name `year`; do not reorder columns | magic index | Yes—prediction tables’ `year` column; existing schema must match exactly |
| `7_plot_preds.R:158` | `paleo_interp_sim_gam` reads **point predictions**, not simulations | `point_predictions`. Keep the read here; dependency/manifest pruning is separate | misleading name | No |
| `7_plot_preds.R:36–38,98–122,436–485` | `breaks`; `labels` reused for bin IDs and period text | Distinguish `albedo_bin_breaks`, `albedo_bin_ids`, `coarse_period_labels`; reuse the existing `period_labels()` helper | misleading name | Yes—legend labels and saved ice-table `facets`; preserve their values and levels |
| `7_plot_preds.R:487–520` | `thresh`, `values` describe a colour scale, not an analysis threshold | `diff_colour_limit`, `diff_colour_positions` | misleading name | No; retain the exact scale limits and positions |
| `7_plot_preds.R:36–38,98–100,490–505,507–520` | Overwritten initial bin definitions, first `values`, first fill scale, first colour scale | Remove only these overwritten definitions; retain the final definitions, including their palettes | dead code / commented-out block | Yes—difference figures; final scale objects must be unchanged |
| `7a_alb_diff_full.R:53,67–77,144–154` | Callback `x`; `idx_ice` used for a raster band and later row indices; `ice_status_veg` contains extracted fractions | `layer_name`, `ice_layer_index`, `ice_rows`, `ice_fraction_samples`, in their separate scopes | misleading name | No; retain the extracted values and threshold masks |
| `7a_alb_diff_full.R:118–123` | `spdf_2`, `sf_data` | `grid_polygons_sp`, `grid_polygons_sf` | placeholder name | No; area values, units and column order unchanged |
| `7a_alb_diff_full.R:167–180,225–238` | `cell_id_missing` is a table; `_new` objects imply a replacement calculation | `incomplete_cells`, `complete_age_cells_diagnostic`, `complete_age_cell_counts`, `remaining_incomplete_cells`; **do not feed the diagnostic table into the difference loop** | misleading name | No |
| `7a_alb_diff_full.R:284–288` | Same two `rowSums()` assignments repeated immediately | Remove the second pair inside I11; preserve the first expressions and their NA behaviour | duplicated snippet | Yes—`ALB_diffs_bluesky.RDS`; byte identity required |
| `7a_alb_diff_full.R:333–339` | `foo2` | `higher_young_ice_fraction`, describing the actual comparison without changing its interpretation | placeholder name | No; retain both diagnostic plots and labels |
| `8_radiative.R:166–198` | `bar` | `june_ice_forcing_over_100`; retain the literal selection, inspections and plots | placeholder name | No; retain diagnostic figures/`Rplots.pdf` |
| `9_forcing_barplot.R:147–199,331–349` | `d`, `d7` | `forcing_by_cell`, `coarse_differences`; I13a passes data explicitly instead of retaining the global closure | placeholder name | No |
| `9_forcing_barplot.R:126–127,160–201` | `KERNELS` holds forcing-column names; `keep`, `main`, `sens`; `col`, callbacks `k`/`v` | `FORCING_COLUMNS`, `complete_cell_month_periods`, `forcing_by_period`, `variant_sensitivity`, `forcing_column`, `kernel_id`, `variant_id` | misleading name | No; preserve named kernel keys, `kernel`/`variant` columns and CSV ordering |
| `9_forcing_barplot.R:160–165` | Temporary counts `np`, `np_full` | `n_pairs`, `n_pairs_full`; retain the same join suffix and equality test | other | No; these columns are removed by the existing `select()` |
| `9_forcing_barplot.R:204,255–256` | `read_erf()` locals `p`, `x`; `panel_plot()` argument `dat` | `path`, `erf_history`, `panel_data`; update any named calls | placeholder name | No |
| `9_forcing_barplot.R:236–292,274,358` | `hol`, `both`, `hol3`, `rng` | `holocene_panel`, `forcing_panels`, `kernel_comparison`, `forcing_axis_limits`; `hol3` must not imply exactly three configured kernels | misleading name | No; preserve plot columns, strip labels and ranges |
| `1_veg_lct_prep.R:2–4`; scripts `2`, `4`, `5`, `6`, `7`, `8`: `2–3`; `7a_alb_diff_full.R:1–3` | Headers claiming every remaining line is unchanged; incomplete script 7a description | Update only inaccurate provenance statements and finish 7a’s purpose sentence | other | No; comments only |

For the abbreviated header row, the script basenames are those in the preceding rows.

| File:line(s) | Current name/construct | Proposed name/change | Category | Touches saved outputs? |
|---|---|---|---|---|
| `R/calendar.R:17–21` | `unknown`, `dup` | `unknown_months`, `duplicate_months`; retain diagnostic messages | other | No |
| `R/config.R:24–39` | Syntax-tree `e`, `f`, `what`, `args`, callback `a` | `expr`, `call_head`, `call_text`, `call_arguments`, `argument`; update named calls to this internal helper | placeholder name | No; accepted syntax and error text unchanged |
| `R/config.R:52–59,70–75` | `dup`, loop `k`, config-option index `i` | `duplicate_keys`, `key`, `config_arg_index`; retain public `config_path(args = …)` signature | other | No |
| `R/config_schema.R:39–42,56–62` | `bad`; `top` contains all path components | `invalid_choices`, `path_components` | misleading name | No |
| `R/map_helpers.R:38–39` | Geometry callback `g`, bounds `b`, `span` | `geometry`, `bounds`, `longitude_span`; retain the `300` rule and filtering order | placeholder name | No; scripts 2/3 figure checks required |
| `R/paths.R:115,135–152,187–194` | Directory `d`; local path `rel` | `product_dir`, `relative_path`; leave existing public argument names unchanged | placeholder name | No; all returned paths unchanged |
| `R/paths.R:200–221` | `t` means regex/template; `out`, `ms`, `ks`, `m`, `k` | `pattern` in `template_regex()`; `path_template`, `expanded_paths`, `template_months`, `template_models`, `month`, `model_id` in `catalogue_paths()` | placeholder name | No; catalogue expansion values, names and order unchanged |
| `R/paths.R:233–245` | `done` is a list of recorded paths; callback `f` | `recorded_files`, `source_file`; do not imply that the record proves copying succeeded | misleading name | No; `SEEDED.txt` contents unchanged |
| `R/run_manifest.R:22–26,33,48–90` | `p`, `sz`, `h`, `t0`/`t1`, `pkgs`, `pkgv`, `thr`, `L` | In **I05’s replacement only**, use role names such as `file_path`, `size_bytes`, `checksum`, `started_at`, `ended_at`, `package_names`, `package_versions`, `thread_settings`, `manifest_lines`. Do not separately polish the implementation being replaced | placeholder name | Yes—manifest construction; I15 adds no field or formatting changes |
| `scripts/make_grid.R:4–10` | `vars`, `xy`, `ext` | During I08’s move, use `coordinate_columns`, `coordinates`, `grid_extent`; preserve the function signature | other | No; grid structure and script 3 figures unchanged |

**Names deliberately retained**

These complete the inventory of ambiguous/output-sensitive cases; they are not permission for a global search-and-replace.

| File:line(s) | Current name/construct | Proposed name/change | Category | Touches saved outputs? |
|---|---|---|---|---|
| `5_calibration_eval.R:57–73` | First simulation summary’s `month` column actually contains observed albedo | Retain the column in this issue and add a short explanation at its construction. Keep it distinct from the loop’s month name in extracted function documentation | misleading name | **Yes**—grouping and monthly error plots; excluded from renaming |
| `6_prediction_model.R:38–68,72–94` | `alb_mean` means fitted mean in point files and simulated-response mean in summary files; `iter` identifies response draws | Keep on-disk names. I10 distinguishes internal results and uses its planned internal `resp_draw` mapping | misleading name | **Yes**—all prediction RDS schemas; excluded |
| `7_plot_preds.R:321–326` | Loop `year` collides with data column `year` in `subset(..., year == year)` | Retain both expression and binding here; route the possible correction separately | misleading name | **Yes**—`alb_interp_preds_month_binned_tile_pages_bluesky.pdf`; a “clarifying” rename can change selected rows |
| `3_plot_cal_lct_albedo.R:154–176,194–208`; `9_forcing_barplot.R:255–278` | `variable`, `value`, `ETS`/`STS`/`OVL`, `striplab` | Retain column names, factor labels and facet expressions, including `striplab`’s name lookup | other | **Yes**—facet, axis and legend text, including `Rplots.pdf` |
| `7a_alb_diff_full.R:207–223,310–331`; `8_radiative.R:68–81,130–164,200` | Coordinate, area, ice, albedo-difference and forcing column names | Retain all saved names, units, attributes, order and NA masks | other | **Yes**—difference and forcing RDS files |
| `R/paths.R:37–44,45–95`; `4_calibration_model.R:132–150`; `5_calibration_eval.R:214,250–252` | `mod1`…`mod8`, spatial model IDs, path oddities, comparison-model labels | Retain identifiers and filenames; document meanings through the registry and rename ledger | other | **Yes**—model files, AIC table and model facets |
| `4_calibration_model.R:147`; `5_calibration_eval.R:336–338` | `test = "Chisq"` | Retain: this is a statistical function argument, not a scratch variable | other | No proposed change |

No bare `tmp`, `df` or `x1` binding appears in the supplied scripts 1–9 or `R/`. Literal scratch names requiring action are `foo2`, `bar` and `dat`, plus the generic bindings enumerated above (`7a:333`, `8:166`, `9:255`). Keep meaningful `_df` names such as `AIC_df` and `rk_hadgem_df` (`4:123–150`; `8:94–100`). Also keep conventional coordinate names, short loop indices, plot handles and generic validator/error arguments where their small scope already makes the role clear—for example `R/config_schema.R:22–30`, `R/config.R:82–83`, and `1_veg_lct_prep.R:173–183`.

**Scripts outside the pipeline** (decision: Chris, 2026-10-13)

Only `make_grid.R` is a live dependency (script 3 sources it, `scripts/3_plot_cal_lct_albedo.R:56–61`); it is cleaned with I08. The others are not run by the pipeline and nothing reads their outputs, so I15 **removes them from the working tree unless Andria asks to keep one** (questions_for_andria_general.md sections 6 and 7, chris-dev 9d6cfc9). Every version stays in git history and on `legacy`. Any script she keeps is cleaned to the same standard and brought into the checks; if she keeps the model-structure sensitivity check it is rebuilt on the shared functions in I09.

| Script | What it is | If Andria keeps it: names to fix and how to verify |
|---|---|---|
| `scripts/archive/`, `GCM_snow_prob.R`, `thornthwaite.R` | the dropped snow / water-budget branch (`thornthwaite.R` is a locally modified copy of ClimClass) | `GCM_snow_prob.R`: commented `df_pm` scratch code, `tmax` names in the minimum-temperature/precipitation initialisers (`:46–84,92–102`); compare one month/slice on fixed NetCDF inputs. `thornthwaite.R`: `m`, `chg`, `zzz`, `exp`, `list_2.lists` (`:112–117,133–149,163–165,329–331`); before/after checks of the returned object on multi-year fixtures including freezing/thawing; preserve list names and S3 class |
| `beta_veg_lct_modern.R` | early modern-pollen to land-cover script, out of the queue since 2022 | `complete_dat`; `k` changes from country matches to a table; `prop_df` changes meaning; `dat_pollen`; two meanings of `foo` (`:22–69,86–99,130–142`). Compare old/new transformations on fixed pollen fixtures, preserving the implicit `k` column at `:35`. The apparent dangling assignment at `:13–16` may parse as chained assignment: check with the parser before calling it a fault |
| `6_prediction_model_spatial_eval.R` | the optional model-structure sensitivity check; cannot run today (missing packages, undefined `values` at `:278`, reads files that no longer exist from `:326`) | rebuilt in I09 on the shared functions rather than renamed in place: `alb_preds_diff_month` holds all months; first `foo`/`foo_sub` are differences, second pair predictions; commented `bar` at `:259–266` |

**Acceptance criteria:**

- [ ] Every inventory row is recorded as implemented, handled by its owning migration issue, or explicitly retained/deferred.
- [ ] Scripts outside the pipeline are removed, or (where Andria keeps them) cleaned and verified as above; README and CONTRIBUTING no longer list removed files.
- [ ] New extracted code uses meaningful names; existing function APIs, configuration keys, model IDs, RNG call identities and output paths remain compatible.
- [ ] Andria has an old-to-new table with current destinations; reconciliation preserves her edits and scientific comments (`CONTRIBUTING.md:76–79,111–114`).
- [ ] All changed R files parse. Every executable change has the applicable behavioural verification below; parse-only results are not presented as output verification.
- [ ] Deterministic artefacts remain byte-identical; fitted models and historical stochastic outputs meet the unchanged regression contract (`:38–79`).
- [ ] Fixed-seed old/new runs of scripts 5 and 6 are byte-identical, including affected figures. Renaming never changes the seed’s stage/month/call identity.
- [ ] Saved names, row order/names, classes, units, factor levels, grouping attributes, NA masks, figure labels and figure inventory remain unchanged.
- [ ] No library removal, scientific correction, new shell scaffolding, regenerated anchor or committed run output (`AGENTS.md:43–44`; `CONTRIBUTING.md:31–37`).
- [ ] Final checks run **after** reconciliation and remaining cleanup; earlier runs do not certify conflict resolutions.

**Verification:**

Use plain R and the existing R checking tools. Parse changed files, for example:

```r
invisible(lapply(changed_files, parse, keep.source = FALSE))
```

For a **comment-only** commit, also require identical parsed expressions before and after:

```r
stopifnot(identical(
  parse(before_file, keep.source = FALSE),
  parse(after_file, keep.source = FALSE)
))
```

Use separate configured run directories and seed **only upstream inputs**, never the output being checked (`R/paths.R:224–245`). Keep the verification environment and eight-thread setting fixed; check frozen inputs before and after (regression contract `:13–33`). Once manifests are enforced, record the existing stale-input override when using historical frozen products; do not fabricate provenance.

Compare deterministic files with `tools/check_anchors.R`; compare historical stochastic tables using `tests/anchors/tolerances.tsv`; compare fitted models through the one-month check (regression contract `:38–43,52–79,102–114`). Figures retain the I03/I07 byte-comparison standard, with only the already-established PDF creation-date treatment—not new image tolerances.

**Recommended atomic commits and cheapest sufficient checks**

Each row below is a commit family: keep separate logical renames/removals independently revertible. Several commits may share one recorded final owner-stage run; do not rerun script 7 after each spelling change.

| Commit | Verification |
|---|---|
| Record naming rule, ownership and rename ledger | Review citations and mappings; no pipeline run |
| Correct inaccurate comments only | Parse and identical parsed expressions; no pipeline run |
| Rename calendar/config/schema/path internals, one helper concern per commit | Existing `test_calendar.R`, `test_config.R`, `test_paths.R` and relevant `test_tools.R` checks; compare returned values, messages and path ordering. No whole-pipeline run solely for these local bindings |
| Rename boundary-helper locals | Existing helper checks plus scripts **2 and 3** from fixed inputs; compare figures (`R/map_helpers.R:31–41`; scripts `2:29–30`, `3:38–39`) |
| Apply readable names within **I05** | I05’s manifest tests; do not add a separate rewrite of the old helper |
| Preparation renames, then separately proven dead assignments, within **I08** | Scripts **1 with/without diagnostics, 2 and 3**. Check both land-cover tables, native/coarse calibration tables, diagnostic CSV and figures. For `make_grid()`, compare old/new extent, CRS, resolution and cell values on a small coordinate fixture before script 3 (`scripts/make_grid.R:4–10`) |
| Prediction names within **I10** | Script **6** from frozen selected fits: deterministic point-file hashes, historical summary tolerances, and fixed-seed old/new equality. Retain I10’s draw-identity checks (`6_prediction_model.R:32–68`) |
| Calibration/evaluation names and model lookup within **I09**, as separate logical commits | Script **4’s one-month selected-fit check**, AIC table from frozen fits, script **5** from frozen fits with fixed-seed old/new comparison, then **6** from the selected outputs. No extra full ladder refit; that belongs to I13b (regression contract `:97–114`) |
| Difference names and duplicate-assignment removal within **I11**, separately reviewable | Small pair/NA fixtures first, then required **7 and 7a** frozen-input runs and **8/9** on their regenerated differences. Preserve all five difference/ice artefacts and affected figures (`7:84,434,480–481`; `7a:331`) |
| Kernel diagnostic/local names within **I12** | **8**, then **9**, from frozen inputs; forcing hash, all four CSVs and affected figures (`8:166–202`; `9:366–372`) |
| Aggregation and panel names within **I13a** | **9** from frozen tail inputs; four CSVs and all seven saved figures (`9:285–286,303,313,323,364–372`) |
| Remaining wrapper names after reconciliation | Parse each commit; compare affected plot data/labels cheaply. Batch script 7 wrapper edits before **one final frozen-input 7/7a check** at the integration gate, followed by **8/9**. Other changed wrappers receive their corresponding runs above |

Script 7’s supplied I07 run cost was approximately **110 minutes and 22 GB**; script 7a was approximately **100 minutes**. That is why its wrapper changes are batched. A freshly simulated script 6 summary cannot substitute for the frozen summary when checking tail hashes: the contract identifies the frozen prediction summary explicitly (`:87–98`). I13b’s full end-to-end run remains a separate requirement.

Executable dead-code removal is **not parse-only** verification. Removing an assignment may affect captured model environments or graphics state; include it in its owner’s run. Likewise, retain printed diagnostics and implicit `Rplots.pdf` pages (`R/paths.R:116–121`; `scripts/3_plot_cal_lct_albedo.R:9–11`).

**Out of scope:**

- Scientific settings, numerical expressions, model formulas, simulation count/order, thresholds, masks, coordinate transformations and missing-data policy.
- Fixes that alter a figure, even when its current label or filtering appears wrong.
- Removing repeated fits/simulations, activating unused filtered tables, or combining scientifically different stages.
- Renaming saved columns, factor levels, model IDs, configuration keys, filenames or manifest fields for cosmetic consistency.
- Library removal, package upgrades, formatting sweeps, new dependencies or a general-purpose lint framework.
- Scientific repair of any off-pipeline script Andria keeps, beyond what makes it run and readable (the sensitivity check's rebuild belongs to I09).
- Work already owned elsewhere: draw-name parsing in I04a (`scripts/6_prediction_model.R:54`), script 8’s unused ice reads in I06 (`scripts/8_radiative.R:43–45`), and stage extraction/deduplication in I08–I13a.

## Noticed, not in scope

Routed 2026-10-13: the four figure slips (script 2's swapped axis labels; script 7's `year == year` filter, CV legend and missing young ice outlines), the coverage denominator and script 7a's unused complete-age filter are now general question 8 (chris-dev 9d6cfc9), and the four figure slips carry a `# CM:` comment beside the figure code (feature/single-config df0d28f). The rest were already recorded. Do not fix any of them inside I15. Scientific matters go to `questions_for_andria_scientific.md`; presentation, ownership and runtime matters go to `questions_for_andria_general.md` (`AGENTS.md:13–18`).

| File:line(s) | Observation to route; no correction in I15 |
|---|---|
| `scripts/1_veg_lct_prep.R:36–49,101–113` | Mean versus median and the discarded ice flag are scientific choices already discussed in the script. |
| `scripts/2_calibration_lct_bluesky.R:140–141` | Zero albedo is replaced by `1e-4`; retain this analysis choice during cleanup. |
| `scripts/2_calibration_lct_bluesky.R:149–158` | The merge makes `value.x` native and `value.y` coarse, while the axis labels describe the reverse. Correcting the labels changes saved figures. |
| `scripts/4_calibration_model.R:105–112,161–168,241–248` | The same model formula is fitted repeatedly, including a later overwrite of the mod8 path. General question 7 owns whether these calls can be removed. |
| `scripts/5_calibration_eval.R:175–198` | Credible-interval coverage drops missing values in the numerator but divides by `n()`. Preserve the denominator pending the existing coverage question. |
| `scripts/7_plot_preds.R:93,126,321–326` | The 11,500→12,000 alias is scientific bookkeeping; separately, `year == year` compares the data column with itself. A loop-variable cleanup must not silently correct that filter. |
| `scripts/7_plot_preds.R:197–202,372–377` | The CV plot uses `labels_sd` instead of `labels_cv`; correcting this changes its legend. |
| `scripts/7_plot_preds.R:63–73,449–454` | Ice rows store requested `ages` and matched `ice_year` separately, but later matching filters `ages` using matched ice years. Check the intended youngest-period overlays. |
| `scripts/7a_alb_diff_full.R:175–180,225–245` | A complete-age diagnostic table is constructed, but the loop uses the unfiltered table. Do not activate the diagnostic filter. Script 1’s claim that script 7a drops incomplete cells also needs review (`scripts/1_veg_lct_prep.R:145–151`). |
| `scripts/7a_alb_diff_full.R:144–154,247–260,293–305` | Threshold masks precede the one-pass ice adjustment. The first-pair neighbour indexing and multiple-readvance policy remain scientific/behavioural questions. |
| `scripts/7a_alb_diff_full.R:81`; `scripts/8_radiative.R:68–77,112–128,130–164` | Preserve strict domain cuts, CACK coordinate/band treatment, signs and factors of 100; these belong to the existing domain/kernel questions. |
| `scripts/9_forcing_barplot.R:160–182,331–354` | Completeness rules, period sums, monthly NA handling, area weighting and the recovered recipe’s different averaging remain analysis assumptions. |
| `scripts/beta_veg_lct_modern.R:13–16,71–73`; `scripts/GCM_snow_prob.R:92–102,129–132` | Investigate the chained assignment and initialiser names separately; the pollen threshold and climate-data interpolation buffer are scientific choices. |
| `scripts/6_prediction_model_spatial_eval.R:272–280,322–366` | `values` is used before its local definition; the active “OLD” tail reads/writes legacy products. These are revival/ownership questions, not dead code to delete on sight. |
| `scripts/thornthwaite.R:154–176` | The locally modified all-cold-temperature and thaw-month handling needs method review before any scientific refactor. |

**Decisions (Chris, 2026-10-13):**

1. Split ownership adopted: each extraction issue (I05, I08, I10, I11, I12, I13a, I09) renames the code it moves, following the naming rule above (a comment on each of those issues points here); I15 does the remaining renames after I13b's reconciliation with chris-dev and before I13b's final verification.
2. Scripts outside the pipeline are removed unless Andria objects; those she keeps are cleaned and checked.
3. Slips that change a figure or number go to the questions file and get a comment in the code; they are not fixed here.
