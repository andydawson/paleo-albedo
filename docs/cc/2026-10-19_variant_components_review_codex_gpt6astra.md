# Review of the variant-components fix: Astra (Codex, gpt-6-astra, max effort)

_2026-10-19. Independent review of branch `fix-variant-components` at 91102dc (first version), from the packet in the session scratch folder, with the science-reviewer role copied into the prompt. A first attempt read no files (it took the role's "never runs R" as forbidding any command); it is kept in the scratch folder only. This is the second attempt, told it may read files with read-only commands. Reviewed in parallel with Opus, standing in for Fable (Fable's usage limit was reached). Outcome: both found the same blocker; Chris chose to keep the legacy outputs as they were and to apply all the smaller points (commits from 1e816ea)._

**Verdict.** Block merge: the correction works for finite `endpoint` inputs, but its unconditional guard rejects supported `legacy_components` cases, and the verification does not address that incompatibility.

**Benefit.** This restores vegetation and ice contributions from cells that experience both kinds of step, so Andria can compare component bars that account for their corresponding total.

**Hand check.**

Use one month, three slices ordered **oldest → youngest**, threshold 0.5, fixed ice albedo 0.68, and HadGEM3 kernel −2 W m⁻² per 0.01 albedo. Thus forcing is −200 × albedo change.

- Cell A: ice fractions `(0.9, 0, 0)`; vegetation albedos `(0.30, 0.25, 0.20)`.
- Cell B: ice fractions `(0, 0, 0)`; vegetation albedos `(0.20, 0.25, 0.30)`.
- Their areas are respectively 1% and 3% of Earth.

All albedos are within [0, 1]; ice and non-ice fractions sum to one.

| Cell | Older step | Younger step | Vegetation forcing | Ice forcing | Total |
|---|---:|---:|---:|---:|---:|
| A | `(0.25 − 0.68) × −200 = 86` | `(0.20 − 0.25) × −200 = 10` | 10 | 86 | 96 |
| B | `(0.25 − 0.20) × −200 = −10` | `(0.30 − 0.25) × −200 = −10` | −20 | 0 | −20 |

Previously, A’s component sums both became NA. The proposed fill produces the table above. Global equivalents are:

- Vegetation: `0.01 × 10 + 0.03 × −20 = −0.50`.
- Ice: `0.01 × 86 = 0.86`.
- Total: `0.36 W m⁻²`.

Domain means divide these by 0.04: `−12.5 + 21.5 = 9 W m⁻²`. The code agrees.

For the area-share split, A’s components are `11` and `77.4`, totaling `88.4`. Its endpoint mixture changes from `0.9 × 0.68 + 0.1 × 0.30 = 0.642` to `0.20`, giving `(0.20 − 0.642) × −200 = 88.4`. This split correctly adds to a **different total**.

Under `endpoint`, retreat, retained advance, still ice, exactly-at-threshold vegetation, and missing ice fractions follow the proposed rule: equality with the threshold counts as vegetation; a missing ice state gives missing totals and components. The readvance adjustment remains upstream and unchanged.

**Findings.**

1. **Blocker — [scripts/9_forcing_barplot.R:199](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/scripts/9_forcing_barplot.R:199): exactly-one-component is not a legacy invariant.**

   `configs/legacy_config.R:96` selects `legacy_components`. Two explicit counterexamples already exist in `tests/test_differences.R:58–70`:

   - Fractions `(0.22, 0.60, 0.48)`, youngest first, become `(0.22, 0.35, 0.48)` after smoothing. For the first pair, legacy arithmetic gives vegetation `−0.05`, ice `0.20 − 0.68 = −0.48`, total `−0.53`. Both components are numeric, so the new guard stops.
   - The retained advance in test case 7 has adjusted younger fraction 0.525 and older fraction 0.1. Both legacy components are NA, while `rowSums(..., na.rm=TRUE)` produces total zero. The guard again stops.

   These follow directly from `R/differences.R:92–110`. They are inherited scientific faults, but they are also deliberately preserved behavior of the supported legacy method.

   **Consequence:** the unconditional call at line 212 can prevent **all script 9 outputs**, including the headline CSV, from being written. Removing threshold components from `sensitivity_variants` does not avoid it.

   **Check:** pass these existing fixtures through the new helper and period aggregation under both methods, then run script 9 against the frozen legacy forcing table. Define legacy handling explicitly; simply removing the guard would not establish the claimed split by kind of step.

2. **Should-fix — [scripts/9_forcing_barplot.R:207](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/scripts/9_forcing_barplot.R:207): the second iteration can invalidate the first split after it passed validation.**

   Both iterations write the same `rf_hadgem_veg_thresh` column, using different totals’ NA masks.

   Take a row with raw vegetation NA, fixed ice/total `4`, and scaled ice/total NA. The first iteration writes vegetation `0` and verifies `0 + 4 = 4`. The second overwrites vegetation with NA. The returned fixed split is now `(NA, 4)` against known total `4`, without a stop.

   Add another vegetation step of `10` in that period: vegetation aggregation becomes NA, ice remains `4`, and the known total is `14`. The original loss of contributions returns.

   **Scope:** the supplied glacier CSV is complete, so I found no evidence this affects the reported October run. It does violate the helper’s stated NA contract if fixed and scaled availability differ.

   **Check:** test both directions of differing total masks and validate the **returned** columns. With one shared vegetation column, either enforce matching masks explicitly or define how differing masks are represented.

3. **Should-fix — [configs/legacy_config.R:132](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/configs/legacy_config.R:132) and [verification spec:8](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/docs/cc/verification/2026-10-19_variant-components-2.R:8): the legacy regression promise is unresolved.**

   The legacy configuration promises reproduction of the anchors, but now requests eight variants instead of six. The regression contract, line 40, requires byte identity for all four script 9 CSVs; the sensitivity CSV is explicitly anchored.

   The PASS run uses `config.R` and current endpoint forcing. Its anchor section is empty. `tests/test_config.R` checks that legacy settings load; it does not execute legacy arithmetic.

   **Consequence:** this PASS cannot establish legacy compatibility, and a corrected sensitivity CSV cannot satisfy the existing whole-file anchor requirement.

   **Check:** retain the historical anchor and record the narrow intended exception through the contract’s decision process. Verify the other three CSVs byte-for-byte and compare the four existing total variants by period and variant. Separately verify corrected component values and the two added variants. Do not weaken unrelated anchors.

4. **Nit — [scripts/9_forcing_barplot.R:57](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/scripts/9_forcing_barplot.R:57): the explanatory text needs qualifications.**

   Threshold classification uses the **adjusted** fractions. The area-share ice term uses `ice_frac_old − ice_frac_young`, which can be negative during retained advances; “the share that melted” describes retreat only. “Every pair has both parts” also excludes missing-data cases.

   Line 28 still says six variants. The figure subtitle at line 343 calls all bars “alternatives,” although several are components of other bars.

   **Consequence:** readers may interpret components as competing estimates of total forcing or misunderstand advances.

   **Check:** reconcile the wording with `R/differences.R:51–75`; describe signed ice-area change, qualify known pairs, and label the figure as components and totals.

**What the proposed verification misses.**

The recorded PASS and its log support the reported current-run comparisons. They do not exercise the counterexamples above.

Add targeted checks for:

- Both difference methods, including the existing advance and smoothing fixtures; zero change; exactly 0.5; missing ice fraction; and scaled ice.
- The two-cell calculation above, asserting both domain and global additivity, telescoping totals, and preservation of the original input table.
- Missing pairs and differing fixed/scaled masks, plus deliberately invalid component counts and sums.
- Actual contributing cell/month masks and denominators. The current “every variant covers the same area” assertion checks `area_m2`, but script 9 line 171 sums area even for cells whose forcing became NaN. **The old defective anchor also has identical `area_m2` across variants.**
- Preservation of all unaffected CSVs and existing total variants, rather than the headline CSV alone.

A small helper in `R/`, directly sourced by tests, would suffice; no general framework is needed. The one-off reference files under `/tmp` are useful cross-checks but do not replace committed, hand-checkable fixtures.

On a successful run with identical inputs, I found no numerical path changing other outputs: the headline and recovered recipe use the original table. However, a legal component headline such as `variant = "veg_thresh"` retains the old aggregation defect and can disagree with its corrected A2 bar. Also, changing `analysis$forcing$sensitivity_variants` makes earlier producer manifests incompatible under the existing whole-analysis check; the verification runner explicitly permits stale copied inputs.

No random calls, seeds, or draw counts change. Seed-1 identity establishes reproducibility of that seeded path; historical tolerances assess unseeded variation. Neither establishes correctness of this partition. The script-9 rerun is conditional on one fixed upstream realization.

**Questions for Chris or Andria.**

- **Chris:** Should corrected A2 apply only to `endpoint`, preserving historical legacy output, or also to legacy with an explicit allocation rule for its anomalous totals and a narrow anchor exception? Meanwhile, the new helper stops on those anomalies.
- **Chris:** Should threshold components remain legal headline choices, requiring consistent aggregation, or should headline selection be restricted to totals? Meanwhile, component headlines retain the old behavior.
- **Andria, C1:** Which threshold/area-share and fixed/scaled combination should the paper report? Meanwhile, the headline remains fixed-albedo `veg_ice_thresh`; A2 displays both fixed-albedo component splits.
- **Andria, C2:** Retain real readvances or enforce monotone retreat? Meanwhile, the existing one-pass smoothing remains unchanged. Chris’s choice of component definition does not settle this question.

**Not checked.**

No R execution, binary forcing-table inspection, figure inspection, or independent kernel-file metadata check. I did not establish how many anomalous legacy rows survive script 9’s filtering, reproduce the claimed 343-cell count, or inspect the one-off calculations’ source code. No other reviewer’s conclusions were opened. Questions above are returned for filing; no files were changed.

Files read, including excerpts/search matches: packet diff; all eight changed files; `CONTRIBUTING.md`; regression contract; both questions files; `R/{differences,forcing_periods,random,kernels,config,config_schema,run_manifest,paths}.R`; scripts 7a/8 and legacy 7a; relevant tests, verification specs/reports, anchor README/MD5/CSV; glacier CSV; `tools/verify_change.R`; PASS check log. Tools: 7 batches containing 44 read-only `exec_command` calls; no R or writes.