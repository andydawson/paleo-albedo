# Adversarial review of the extraction issues under the readability priority

**What this is.** Chris asked (2026-10-14) why the extraction issue I10 was needed, and
then asked for an adversarial review of all remaining extraction issues (I10, I11, I12,
I08, I13a, I09) based on the priority for readability and understanding (AGENTS.md,
2026-10-14). Verdict: drop I10, I12, I13a and I09, moving their small readability items
to I15; slim I11 to one tested function for script 7a's per-pair arithmetic; fold I08 into
I16 (new methods as tested functions, the original lines left inline); order I11-slim,
then I16, then I13b. Chris's decisions on it are in the change log.

**Who wrote it.** A Claude Code subagent (Fable), maximum effort, read-only, on branch
i06-wiring (34365c1), reading scripts 1-9, `R/`, `config.R`, the tests, issues #10-#19
with comments, the 2026-10-08 design, the three Astra documents of 2026-10-14, the change
log and the regression contract. It touched no file and ran no R.

---

## 1. Verdict on the programme as a whole

**The 2026-10-08 extraction design was built for an ensemble driver that no longer exists in that form, and for "callable stages" nobody has asked for. Under the 2026-10-14 priority it is mostly net-negative for Andria and should be cut to one real extraction plus two trivial moves.**

What it would buy Andria: nothing she has asked for. She reads R and steps through scripts. Every extraction replaces lines she wrote (`predict.gam(type='response')`, `simulate(nsim=, seed=)`, the `melt`/`summarize`, the kernel reads) with a call into `R/` whose body she must open in another file. The pipeline's science is *in* those lines; hiding them behind `predict_albedo()` is the opposite of the stated goal.

What it costs the project:
- **Reconciliation.** Every moved block turns a future edit of hers on `chris-dev` into a conflict that `git merge` cannot resolve. Today the risk is prospective (no script changes on `chris-dev` since the merge base except comments); the fewer lines move, the cheaper I13b's merge.
- **Verification runs with no readability return.** I11 alone costs about 110 + 80 min of 7/7a runs per attempt; I09 a 19-min refit plus 5 and 6.
- **Naming churn** (rename as you move) doubles the vocabulary change she must absorb.

What the programme genuinely buys, checked against the current code:
- **Tests.** Only one calculation is both bug-prone and unit-testable on a fixture: script 7a's per-pair albedo-difference arithmetic (`7a:256-317`), where C2's documented defects live (advance case gives both components NA, summed to a confident 0 at `7a:313-317`; a first-pair readvance indexes element 0 at `7a:263`). Everything else is already a tested function, trivial dplyr, or needs the real rasters.
- **I16's switches.** Only one (C2 `ice$difference_method`) lands inside a long inline block; every other switch is a short `if/else` at a visible read or write site (section 3).
- **I13b** needs nothing from the extractions.
- **The ensemble (I14, redesigned 2026-10-14)** can loop over the *scripts* per member into per-member run directories (`run$dir` + `--config`, built in I07). Stage functions are not required for that.

Astra's own readability review asks for comments, indentation, explicit `by=`, explicit arguments and reordering, not stage extraction; its "five best returns for Andria" contain no extraction.

## 2. Issue by issue

**I10 (#11), script 6: DROP.** `6:39-82` is one loop of four visible steps (read fit, `predict.gam`, `simulate` with the derived seed, melt + `draw_index`, summarise). No duplication, no I16 switch, no bug. A `summarise_responses()` shared with script 5 is false reuse: the five summarise blocks (`6:71-78`; `5:181-187`, `259-265`, `383-389`, `502-508`) differ in grouping columns, value-column names, `na.rm` and `.groups`. To I15: rename by name at `6:36-37`; split the seed from `run_record` (`6:27-28`); a "one row per cell, slice and draw" comment at `6:63`.

**I11 (#12), scripts 7 and 7a: SLIM to one function; do not vectorise.** The duplication between 7 and 7a (grid/area attach, sd/cv binning, a five-line pair-loop skeleton) is mostly dead code in 7a (`7a:167-188`, `190-211`, the repeated `rowSums` at `296-297`): deletion, not consolidation. The science differs (7: `-diff(alb_mean)` on coarse slices; 7a: twelve variants with threshold masks, the readvance adjustment and NA rules), so no shared `difference_*()`. Extract exactly one function in `R/differences.R`:

```r
# Albedo change for each consecutive pair of slices of one cell and month, younger minus
# older (sign: positive = albedo fell), from the slice-complete table of that cell-month.
pair_albedo_differences = function(alb_cell_filled, ice_threshold)
```

Body = `7a:256-317` moved verbatim (readvance adjustment included), duplicated `rowSums` removed in its own commit. Keep the `rbind` loop (vectorising changes row names, which the MD5 gate sees). Tests: retreat, advance, persistent ice, first-pair readvance, missing endpoint, threshold equality. Script 7's loop stays inline. Verification: `ALB_diffs_bluesky.RDS` byte-identical from frozen inputs, then 8 and 9.

**I12 (#13), script 8: DROP.** Kernel reads and sampling take seconds; the coordinate arithmetic needs the real NetCDFs to test, which a function does not change. Join-uniqueness checks are two `stopifnot()` lines (I15). I16's kernel switches land at `8:49`, `8:89-91`, `8:99-104` as a `varname` chosen by `if/else` or a short lookup table at the top.

**I08 (#10), scripts 1-3: DROP as designed.** `attach_elevation()` and `prepare_calibration()` would wrap two lines each. I16's B5/B8/B9 are **new** code: write them as tested functions in `R/preparation.R` (`land_mean_elevation()`, `land_weighted_albedo()`, `calibration_ice_mask()`) and leave the legacy lines inline under the `else`. Move `make_grid()` from `scripts/make_grid.R` into `R/preparation.R` (I15).

**I13a (#14), script 9: DROP; fix in place (I15).** Script 9 is the readable model already (local functions with stated purposes and an assumption header). The only defect: `aggregate_forcing(col)` closes over `d` and `EARTH_AREA` (`9:357`, `364`), `panel_plot` over `rng` (`9:445`, reassigned `9:541`); pass them as arguments. I16's C5 is an `if` at `9:423` choosing the plotted column.

**I09 (#15), scripts 4 and 5: DROP; I16 edits the registry and loops directly.** Fitting is extracted already (`R/model_registry.R`). B4 needs a `method` argument on `fit_calibration_model()`, script 4's ladder fitted by the comparison method and saved under its own ids, the mod8 overwrite loop replaced by one REML fit of the configured `model_id`, and script 5 handing that fit on. Script 5's readability items are I15 rows. The four simulate blocks must keep their seed labels or the seed-1 anchor breaks.

## 3. Where each I16 switch lands

| Key | Site | Form |
|---|---|---|
| B1 `land_cover$closure` | after `1:56` | `check_fractions()` in `R/preparation.R` (new, tested) |
| B9 `elevation$method` | `1:65-80` | `if/else`: legacy lines unchanged; new `land_mean_elevation()` |
| B5 `calibration$albedo_support` | `2:388-390` | `if/else`: legacy `terra::extract`; new `land_weighted_albedo()` |
| B8 `calibration$ice_mask` | before `2:400` | 3-line filter + recorded exclusions |
| B4 `comparison_method` | registry, `4:35-43`, `4:88-96`, `5:154-157` | `method` argument; loops edited in place |
| A3 `draws$prediction` | `config.R` | value only |
| C2 `ice$difference_method` | inside `pair_albedo_differences()` | the one branch inside arithmetic |
| C3a/C3b/C4 kernels | `8:49`, `8:89-91`, `8:99-104` | `varname` by `if/else`; CACK climatology read without `band` |
| C5 `forcing$aggregation` | `9:423` | selects the plotted column |

I16 still follows I11-slim (so C2's legacy branch is the anchored code). I13b verifies the legacy config (full run, anchors, tests, `legacy_config.R`) and, separately, the final config with each switch's own check; drop "every numerical stage is callable from R/".

## 4. Risks and containment

- **Byte identity of `ALB_diffs_bluesky.RDS`** after the 7a move: move verbatim, keep the `rbind`, separate commit for the duplicate `rowSums`, verify with the frozen-input run first.
- **Nine `if/else` sites across six scripts:** the manifest records the whole config; each branch carries the key, legacy value and question id in a comment; `config.R` says which script consumes each key; new methods are functions so a branch reads `if (legacy) <Andria's lines> else <one call>`.
- **Script 5's four seeded simulate calls stay separate:** each has its own purpose and seed label; merging would change outputs.
- **The ensemble later:** vectorise 7a's loop then, behind the tested function; run script 7 once, not per member.
- **Fixed-fit reuse** needs `allow_stale = TRUE` in run directories, recorded.

## 5. Plan

1. Close I10, I12, I13a, I09 as "not done: readability decision 2026-10-14", moving their non-extraction items to I15.
2. Rewrite I11 as "7a: one function for the per-pair arithmetic", with tests and a byte-identical 7a run, then 8 and 9.
3. Fold I08 into I16: legacy lines inline, new B5/B9/B8 methods as tested functions.
4. Amend I16 with the landing-site table and the rule "legacy inline, new methods as functions, one `if/else` per key"; add `fit_calibration_model(..., method)`.
5. Amend I13b: drop the "callable from R/" audit; add legacy-config and final-config run reports.
6. Order: I11-slim, I16 (calibration/data, then prediction/forcing), I13b. I14 stays parked.
