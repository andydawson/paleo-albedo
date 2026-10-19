# Review of the variant-components fix: Opus, standing in for Fable

_2026-10-19. Independent review of branch `fix-variant-components` at 91102dc (first version), from the packet in the session scratch folder, acting in the science-reviewer role. Opus stood in for Fable because Fable's usage limit was reached. Reviewed in parallel with Astra (its review is filed beside this one). The main session confirmed finding 1 on the pre-I16 forcing table: 108 rows in 9 cells (72 with both parts, 36 with neither). Outcome: Chris chose to keep the legacy outputs as they were and to apply all the smaller points (commits from 1e816ea)._

**Verdict.** Do not merge yet. The fill-with-0 rule is right for the new configuration (`endpoint`), but under `configs/legacy_config.R` (`legacy_components`) script 9 will very probably stop at the new check. It would then write none of its four CSVs, and the legacy configuration could no longer reproduce `tests/anchors/interp-forcing-2026-10-08`.

**Benefit.** The sensitivity table and figure would show a vegetation part and an ice part that really add up to the headline total. Before the fix, the cells holding about 99% of the 8-10 ka total were missing from both bars, so a reader could have misjudged how much of the deglaciation forcing comes from ice.

**Hand check.**
- **One cell, one period, two pairs, `endpoint`.** HadGEM3 kernel k = -1.5 W/m² per 1% albedo; forcing = albedo change × 100 × k.
  - Pair 1, a retreat (ice at the older end): `vegetation` is NA, `ice` = 0.20 - 0.68 = -0.48, so forcing = -0.48 × 100 × -1.5 = +72.
  - Pair 2, vegetation at both ends: `vegetation` = -0.05, so forcing = +7.5, and `ice` is NA.
  - Total = +79.5. Sign is right: lower albedo gives positive, warming forcing.
  - Before the fix, the period sums of vegetation and ice were both NA (`sum(na.rm = FALSE)`, line 165), so the cell dropped out of both bars. After the fix: vegetation 0 + 7.5 = 7.5; ice 72 + 0 = 72; 7.5 + 72 = 79.5. The code agrees.
- **Exactness.** In `endpoint` the total equals `vegetation` or `ice` exactly (`R/differences.R:135-138`), and script 8 multiplies all three by the same kernel, so the 1e-9 sum check passes exactly.
- **Each case in `R/differences.R`, under `endpoint`:**
  - vegetation step: vegetation only.
  - retreat and advance: ice only.
  - still ice: ice = 0.
  - exactly at the threshold: counts as vegetation, because the test is strictly greater than (`>`).
  - missing ice fraction: total NA, so both components stay NA.
  - readvance adjustment: classified from the adjusted fraction in all three columns.
  - Result: every known pair has exactly one component. The stop cannot fire wrongly.
- **Two cases from `tests/test_differences.R`, under `legacy_components`:**
  - Case 7, ice advance across the threshold, `ice_frac = c(0.95, 0.95, 0.9, 0.1)`, pair 3: vegetation NA, ice NA, total 0 (from `rowSums(na.rm = TRUE)`). The total is known and neither component is set, so the stop fires.
  - Case 7b, the double count, `ice_frac = c(0.22, 0.60, 0.48)`: vegetation -0.05, ice -0.48, total -0.53. Both components are set, so the stop fires.

**Findings.**

1. **Blocker.** `scripts/9_forcing_barplot.R:199-202`, together with `configs/legacy_config.R:96` (`difference_method = "legacy_components"`).
   - **What:** the check "exactly one kind of step" does not hold for the original arithmetic.
     - Ice advances across the threshold give both components NA and a total of 0. That is 36 rows (3 cell-pairs × 12 months); scientific question C2 says these rows reach the kernel.
     - The readvance double counts give both components numeric. That is 72 rows (6 cell-pairs: cells 3077, 3693, 3966, 7551, 7554, 9086).
   - **Consequence:** `--config configs/legacy_config.R` stops at line 212. That is before any `write_output` call (lines 394-396), so even the headline `forcing_by_period.csv` is not written. The original analysis can no longer be reproduced, which breaks the regression contract's rule that the four script 9 CSVs are byte-identical.
   - **Evidence:**
     - The changelog's I16a entry (the ice-advance method change) says `endpoint` "changed 9 pairs of slices (108 cell-months)" and moved the headline at 10-12 ka by 4.7 mW/m². So at least some of these rows lie inside the domain and in period-complete cells.
     - The Fable crosstab in `2026-10-14_i16a_double_count_review_fable.md` gives the same counts (36 and 72).
     - HadGEM3 stores 0, not NA, where there is no sunlight (A6), so these totals stay known even at polar night.
     - I did not run R. This is inference, but the code path is unambiguous.
   - **Check that settles it:** the same verification spec with `config = 'configs/legacy_config.R'`, on the legacy forcing table.
   - **Options (Chris's choice, not mine):**
     - (a) Drop the "both or neither" stop and keep only the sum check. Legacy's both-numeric and 0-total rows do add up to their total, so legacy would run and the components would still sum.
     - (b) Apply the completion and the stop only when `difference_method == "endpoint"`.
     - (c) Keep the legacy configuration's old six variants and old aggregation, so its CSV stays byte-identical. This keeps a known bug.
   - Under every option, the legacy variant-sensitivity anchor changes, or the anchor is formally re-made. Under the contract that is Chris's decision, recorded in its own commit.

2. **Should-fix.** `scripts/9_forcing_barplot.R:191-208`.
   - **What:** the second pairing (`veg_thresh`, `icesc_thresh`, `veg_icesc_thresh`) overwrites `out$rf_hadgem_veg_thresh` with a value whose "known" mask comes from `veg_icesc_thresh`.
   - **Consequence:** on any ice-step row where `veg_icesc_thresh` is NA but `veg_ice_thresh` is known (for example, a month with a missing scaled glacier albedo), the final `veg_thresh` would be NA where its total is known. That brings back the original bug for that cell. The second pass's sum check compares against `veg_icesc_thresh`, so it would not catch this.
   - The second pairing produces nothing that is reported: `icesc_thresh` is in neither `sensitivity_variants` list. Its only effects are this risk and an extra stop.
   - **Evidence:** today the two glacier-albedo columns come from the same per-month CSV (`scripts/7a_alb_diff_full.R:110-111`), so the masks agree. The PASS confirms this for the current data only.
   - **Fix:** drop the second pairing until `icesc_thresh` is reported, or stop unless both pairings give an identical `veg_thresh`.

3. **Should-fix.** The new method is defined inside `scripts/9_forcing_barplot.R:190`, not in `R/`, and has no unit test. AGENTS.md says a new method is "one tested function in `R/`".
   - A five-row hand-made table covering a vegetation step, a retreat, still ice, an advance and a missing pair, under both difference methods, would have exposed Finding 1 at once. The pairs can be built with `pair_albedo_differences()` as `tests/test_differences.R` does.

4. **Should-fix (comments).** Several comments are true only under `endpoint`:
   - **A2 note (lines 57-59) and the comments in both configs:** they say a pair is an ice step if it is ice at either end, and that `veg_thresh + ice_thresh = veg_ice_thresh`. Under `legacy_components`, the ice component is chosen by the unadjusted older fraction and the vegetation component by the adjusted one. The identity fails on the 108 rows above. Each comment should say "with `difference_method = "endpoint"`".
   - **Line 63:** "Script 8 gives the threshold components NA". The NA comes from `R/differences.R` (script 7a); script 8 only multiplies.
   - **Line 28:** still says "6 variants"; there are now 8.
   - **Plot subtitle (line 343):** still says "these are the alternatives". The figure now mixes totals with their parts, and a reader may compare `veg_thresh` with `veg_ice_thresh` as rival estimates.
   - **Config wording:** "gives a threshold component that does not apply 0" reads awkwardly; "sets ... to 0" is clearer.
   - **Area-share description:** "the change on the share that melted" is wrong for an advance, where that share froze. The arithmetic is right; I checked by hand that `veg_part + ice_part` equals the exact area-mixed change.

5. **Nit (verification).** `docs/cc/verification/2026-10-19_variant-components_check.R:25`. The check "every variant covers the same area" proves nothing. `area_m2 = sum(area)` counts cells whose forcing is NA (Astra noted this on 18 October), and the pre-fix anchor already shows the same area for every variant. Count the cells with non-NA forcing per variant and period instead.

6. **Nit (reproducibility).** The check script hard-codes `/tmp/.../scratchpad/fullrun` for the one-off reference files, and both reports point to specs in the scratchpad. Once the scratchpad is cleared, the check cannot be rerun. Also, the "independent" one-off calculations were made in the same session that implemented the rule, so agreement shows the code implements that rule, not that the rule is right.

**Packet questions in brief.**
- **Q2 (can any other output change?):** under `config.R`, only the sensitivity CSV, its figure and the run record change. The headline and the EGU recipe read the untouched `forcing_by_cell` (lines 175, 359), and nothing else reads `forcing_variant_sensitivity`. Under the legacy configuration, everything from script 9 is lost (Finding 1).
- **Q3 (stop conditions):** under `endpoint` they cannot fire wrongly. Under `legacy_components` they fire on real data. They would miss the pairing problem in Finding 2. They also do not check that the label matches the ice fractions, although the table carries `ice_frac_young` and `ice_frac_old`.

**What the proposed verification misses.**
- A legacy-configuration run of script 9, with `tools/check_anchors.R` against `interp-forcing-2026-10-08`. The headline CSV and the EGU recipe CSV should stay byte-identical; the sensitivity CSV is the only intended change.
- A unit test of `complete_threshold_components()` with the five cases above, under both methods. For `endpoint`, assert:
  - the components sum to the total in every row;
  - every known row has exactly one non-zero-filled component;
  - the ice-step label matches `ice_frac_young > threshold | ice_frac_old > threshold` (adjusted fractions).
- An invariant on the real table:
  - for every cell, month and period, `sum(veg) + sum(ice) = sum(total)`, not only after aggregation;
  - for a cell that is vegetation throughout a period, the `veg_thresh` period sum equals the albedo difference between the period's two endpoints × 100 × k (telescoping), and the `ice_thresh` sum is 0.
- The cell count per variant (see Finding 5).
- A check that `veg_thresh` is the same from both pairings, if the second pairing stays.

**Questions for Chris or Andria.**
1. **(Chris)** What should the legacy configuration do in script 9? Options (a), (b) or (c) from Finding 1. Meanwhile, the code stops under the legacy configuration, I expect.
2. **(Chris)** Re-anchoring the legacy variant-sensitivity CSV is a change to the regression contract and needs his recorded decision. Meanwhile, the anchor check on `interp-forcing-2026-10-08` fails.
3. **(Andria, already covered by C1)** Which split the paper shows: by kind of step or by area share. Meanwhile, both are shown and the headline stays `veg_ice_thresh`. It may help to add a note under C1 that the two families also split vegetation and ice differently.

**Not checked.**
- I did not run R or read any RDS. The claim that the legacy rows reach `complete_threshold_components()` is inferred from the I16a changelog, the C2 counts and the crosstab review.
- I did not check that Chris's choice was worded as quoted (I saw only the commit message and the packet).
- I did not open the figures, and did not check the one-off reference CSVs or `tools/verify_change.R`.

Files read: about 14 (role file, packet, diff, `scripts/9_forcing_barplot.R`, `scripts/8_radiative.R`, part of `scripts/7a_alb_diff_full.R`, `R/differences.R`, `R/forcing_periods.R`, part of `R/config_schema.R`, `tests/test_differences.R`, the anchor README and CSV, the regression contract, the legacy config header, the changelog I16a entry, the 2026-10-14 Fable and 2026-10-18 Astra reviews, and C1/C2 of the questions file). Tool calls: 12.
