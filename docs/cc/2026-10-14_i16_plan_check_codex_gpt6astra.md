# Astra's check of Fable's I16 plan, 2026-10-14

**What this is.** Chris asked Astra to check Fable's revised I16 plan
(`2026-10-14_i16_plan_adversarial_review_fable.md`), saying he was inclined to accept it
unless there was a strong reason not to. Verdict: adopt with changes. Chris then asked for
the two reviewers to reach consensus before any implementation; the reconciliation follows
in the change log.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `max`,
2026-10-14, from Fable's review, issue #19 with comments, and the code of branch
i11-pair-differences inlined (no shell available to the model).

---

**Adopt with changes.**

- **B5’s formula mishandles missing albedo.** At the resampling step (`scripts/2_calibration_lct_bluesky.R:62–66`), averages exclude missing pixels separately in numerator and denominator. With equal-sized pixels, `albedo = c(0.4, NA)` and `land = c(1,1)`, the proposal returns `0.4 / 0.5 = 0.8`, rather than `0.4`. [GDAL average semantics](https://gdal.org/en/stable/programs/gdalwarp.html). Use `land * terra::ifel(observed, albedo, 0)` as the numerator input, retain `land * observed` as denominator input, and return `NA` when the resampled denominator is zero. Both inputs must have identical missing-land masks.

- **The geometry justification is false.** Across one degree centred at 70°N, `cos(latitude)` varies approximately **4.8%**, not under 1%; that latitude is within the configured domain (`config.R:73`). Only the averages’ common normalisation cancels—not their individual overlap weights. Furthermore, a quarter-degree land fraction cannot identify which side of a partially intersected pixel contains land. Correct the “exact cancellation” claim; use area weights or quantify the approximation. The supplied GeoTIFF origin establishes its centres, but checking its alignment with the target also requires `grid.RDS` geometry (`scripts/2_calibration_lct_bluesky.R:36,66`).

- **B4 needs a complete reader/checker handoff.** Change **all three** selected-model reads in `scripts/5_calibration_eval.R:56,126,364` to the new REML file. Otherwise November overwrites the selected model with the ladder fit at `:367`. Leave the comparison reads at `:346–353` pointing to the ladder. Also, `tools/check_calibration_month.R` does **not** load a config: its CLI hard-codes baseline data and the frozen AIC anchor (`:49–54`), and its default fitter reproduces the stored specification (`:19–33`). Invoke it with legacy-run inputs and an explicit registry-fitting callback; check the ML path separately. Minor factual correction: the ladder has eight models (`R/model_registry.R:50`), so contamination would be **seven ML plus one REML**, not eleven plus one.

- **Seed 1 alone does not establish reproduction of every anchor.** Scripts use derived per-call seeds (`config.R:49–53`; `scripts/5_calibration_eval.R:72–75,142–145`), while calibration refits explicitly have a tolerance contract (`tools/check_calibration_month.R:35–43`). Keep frozen-input byte checks, tolerated refit checks and seed-1 checks separate, with their respective inputs. Loading `legacy_config.R` proves none of those numerical guarantees. The regression contract, `R/random.R`, script 6 and anchor contents are absent here, so the all-anchor claim cannot be certified from these excerpts.

Implementation also needs to address these gaps:

- **C2’s proposed tests need the right scope.** Compare `_parts` against endpoints constructed from **adjusted fractions** (`R/differences.R:29–57`); script 7a’s existing mixed-albedo columns use raw fractions (`scripts/7a_alb_diff_full.R:141–147`). Test reversal/telescoping on fixed endpoint states. Reversing a two-row retreat through the whole function still throws at `R/differences.R:30` (`tests/test_differences.R:60–62`). Update `tests/test_differences.R:5–7`, which currently promises I16 fixes that fault too.

- **B5/B8 must reach the actual calibration table without misaligning rows.** B5 must populate `bs_interp_df`/`calibration_table`, not merely the unused coarse product (`scripts/2_calibration_lct_bluesky.R:138–151`; `scripts/4_calibration_model.R:23`). Apply B8 immediately after loading the modern table at `2:39`, before coordinates and albedo frames are constructed at `:41–58`.

- **B9 needs the fine water mask during preprocessing.** Mask the DEM before reducing it to the cache consumed at `scripts/1_veg_lct_prep.R:65–80`. Weighting already-averaged elevation by quarter-degree land fractions does not recover land-mean elevation.

- **“Rerun only 8 and 9” needs explicit input reuse.** A separate run directory has no fallback to baseline products, and changed analysis settings trigger stale-input checks (`config.R:128–140`). Seed the appropriate upstream products and explicitly permit recorded stale reuse, or rerun their producers. Script 9 also reads `coarse_differences` unconditionally at `scripts/9_forcing_barplot.R:324`, before deciding whether to skip the recovered recipe.