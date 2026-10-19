# Re-review of the variant-components fix: Opus, standing in for Fable

_2026-10-19. Re-review of branch `fix-variant-components` at 5224b78 after the revision, in the science-reviewer role (Fable's usage limit reached). Verdict: ready to merge. Chris's answers the same day: restrict the headline setting to totals (with the switch to partial ice); the changelog notes the stale-manifest point._

**Re-review of `fix-variant-components` at 5224b78 (base 8e715d2), standing in for Fable.**

**Verdict.** Ready to merge. Both blockers and all should-fix points from my review and Astra's are resolved. Five small points remain; none blocks the merge.

**Benefit.** In the new configuration, the sensitivity figure now shows vegetation and ice parts that add up to their totals, while the legacy configuration still reproduces the original outputs exactly.

**Hand check.**
- **My case, `endpoint`.** One cell has a retreat pair (ice part +72) and a vegetation pair (vegetation part +7.5). `complete_threshold_components()` gives vegetation 0 + 7.5 = 7.5 and ice 72 + 0 = 72, which sum to the total of 79.5. The code agrees.
- **Astra's two-cell case.** I re-worked it by hand and it holds:
  - vegetation -0.50, ice +0.86, total +0.36 W/m² (global equivalent);
  - area-share split for cell A: 11 + 77.4 = 88.4, which equals the change in mixed albedo from 0.642 to 0.20, × -200.
- **Under `legacy_components`.** The helper returns the table unchanged (`R/forcing_components.R:26`), so legacy script 9 is as before.

**My findings**
1. **Blocker (legacy run stops): resolved.**
   - The completion now runs only when `difference_method == "endpoint"`.
   - `configs/legacy_config.R` has no diff from the base: its six original variants and its comment are back.
   - Verification report `variant-components-legacy` (commit 70a30aa) shows script 9 exiting 0 and all four CSVs matching `interp-forcing-2026-10-08` byte for byte. That includes the variant-sensitivity CSV.
   - Only reports and review files were committed after 2bd4ab5 and 70a30aa, so both PASS runs used the final code.
2. **Should-fix (second pairing overwrote the vegetation column): resolved.** The pairing is removed, and the header of `R/forcing_components.R` says why.
3. **Should-fix (helper in `R/` with tests): resolved.** `tests/test_forcing_components.R` passes six cells (vegetation, retreat, still ice, advance, exactly at threshold, missing) through `pair_albedo_differences()`. It asserts:
   - exactly one kind of step per known pair, and the kind matches the adjusted ice fractions;
   - parts are 0 where they do not apply, and they sum to the total pair by pair and in total;
   - a missing pair stays NA;
   - the advance comes out as 0.68 - 0.35;
   - legacy anomalies (advance with both parts NA and total 0; double count) are present and left untouched;
   - the endpoint rule refuses the legacy table, and a bad sum stops.

   The test runs in the `-4` check list. It holds no hand-written period sum of several pairs: the "summed" assertion sums over all pairs. That is enough, because the identity holds per pair.
4. **Should-fix (comments): resolved.**
   - A2 now says the identity holds for `endpoint` only, uses the adjusted fractions, and calls the area-share ice change "signed".
   - Line 28 no longer says six variants.
   - The NA is now attributed to `R/differences.R`.
   - Labels and subtitles name totals and parts plainly, with a separate subtitle for the legacy configuration.
5. **Nit (area check proved nothing): resolved.** Check 3 now counts the cells with a known period value per variant and period.
6. **Nit (check depended on scratch files): partly resolved.**
   - The reference values are now written into the committed check, which says plainly that they came from the same session.
   - The headline comparison still reads the 18 October run from `/home/expert/.../paleo-RF-verify/4ba4d2a/...`, outside the repository.
   - Acceptable as a record; it cannot be rerun once that folder goes.

**Astra's findings**
1. **Blocker (legacy invariant): resolved**, as my point 1.
2. **Should-fix (overwrite on the second iteration): resolved**, as my point 2.
3. **Should-fix (legacy regression promise): resolved.** The legacy outputs are byte-identical, so the contract needs no exception.
4. **Nit (wording: adjusted fractions, signed change, "alternatives", six variants): resolved.**
5. **Astra's remark that a threshold component chosen as the headline keeps the old defect: not addressed.**
   - The schema still allows `variant = "veg_thresh"` or `"ice_thresh"`.
   - The headline aggregation would then drop cells with mixed steps, and disagree with the corrected A2 bar.
   - Nobody uses it today, but nothing is filed. This is Chris's question, below.
6. **Astra's remark on manifest staleness: still applies, and it is an operational point.**
   - `config.R`'s `sensitivity_variants` changed, so the next script 9 run on an existing run folder will stop in `read_input` (`R/run_manifest.R:177-186`). Script 8's manifest there records the old analysis settings.
   - Rerun script 8 there, or set `run$allow_stale`.
   - Worth one line in the changelog entry.

**New problems (all nits)**
- `configs/legacy_config.R:127-129` keeps its old comment: "veg_thresh and ice_thresh have missing values (no-ice cells); the aggregation drops them". The larger loss is cells with both kinds of step. Fixing the comment would not change any CSV, but leaving the file untouched is defensible, since A2 in script 9 now explains.
- Legacy figure: `forcing_barplot_variant_sensitivity.pdf` now has new labels and a new subtitle under the legacy configuration, so it is not identical to the old figure. Figures are not anchored, but Chris should know that "script 9 exactly as it was" holds for the CSVs, not for this figure.
- Figure label "month-by-month glacier albedo" for the `icesc` variants is accurate: the fixed column is 0.68 in every month, while `ice_albedo_sc` varies from 0.56 to 0.80. But Andria's name for it is "scaled", and the questions file asks "scaled by what?". Adding "(scaled)" would link the label to her code.
- Records: the changelog has no entry for this fix on the branch, and the questions file is unchanged.

**What the verification still misses.** Nothing material. Optional additions:
- a fixture with two pairs in one period, aggregated through `aggregate_forcing()`, to cover the period-sum step explicitly;
- an assertion that the input table is unchanged except for the two columns.

**Questions for Chris.**
1. Should the headline variant be restricted to totals (`veg_ice_thresh`, `veg_icesc_thresh`, `veg_ice_parts`, `veg_icesc_parts`), or should a component headline get the same completion? Meanwhile the default is `veg_ice_thresh`, so nothing is affected.
2. Should the changelog note that existing run folders need script 8 rerun (or `allow_stale`) before script 9, because a setting changed? Meanwhile, script 9 stops with a clear message.
3. For Andria, under C1: which split the paper shows remains open, and the code shows both. Astra's C2 point stands: Chris's choice of component definition does not settle the readvance question.

**Not checked.**
- I did not run R or open any RDS or figure.
- I took the anchor MATCH lines and the check's PASS from the reports. I did not re-derive the one-off reference values.
