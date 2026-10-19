# Re-review of the variant-components fix: Astra

_2026-10-19. Re-review of branch `fix-variant-components` at 5224b78 after the revision (Chris kept the legacy outputs as they were and applied all smaller points). Codex, gpt-6-astra, maximum effort, read-only. Verdict: ready to merge; one open question (component headlines), answered by Chris the same day: restrict the headline setting to totals, done with the switch to partial ice._

**Verdict.** Ready to merge at `5224b78` for Chris’s stated A2 scope; no remaining numerical blocker. The earlier component-headline question remains unresolved.

**Benefit.** Andria can compare vegetation and ice contributions that account for their corresponding totals, while reproducing the historical analysis unchanged with the legacy configuration.

**Hand check.**

One month, three slices ordered oldest → youngest, threshold 0.5, glacier albedo 0.68, and a toy HadGEM3 kernel of −2 W m⁻² per 0.01 albedo. Forcing is therefore −200 × albedo change.

| Cell | Fraction of Earth | Ice fractions | Vegetation albedos | Completed vegetation forcing | Completed ice forcing | Total |
|---|---:|---|---|---:|---:|---:|
| A | 0.01 | 0.9, 0, 0 | 0.30, 0.25, 0.20 | 10 | 86 | 96 |
| B | 0.03 | 0, 0, 0 | 0.20, 0.25, 0.30 | −20 | 0 | −20 |

A’s retreat contributes `(0.25 − 0.68) × −200 = 86`; its vegetation step contributes `(0.20 − 0.25) × −200 = 10`. Previously, each component’s period sum encountered an NA and lost A.

The revised global equivalents are:

- Vegetation: `0.01 × 10 + 0.03 × −20 = −0.50`.
- Ice: `0.01 × 86 = 0.86`.
- Total: `0.36 W m⁻²`.

The domain denominator is `0.04`, giving `−12.5 + 21.5 = 9 W m⁻²`. The revised helper and script 9 agree. All albedos are within [0, 1], and ice plus ice-free fractions sum to one.

For the area-share split, A instead gives `11 + 77.4 = 88.4`, matching its mixed-albedo endpoint change: `(0.20 − (0.9 × 0.68 + 0.1 × 0.30)) × −200`. The two splits correctly belong to different totals.

The retained-advance fixture gives `(0.68 − 0.35) × −200 = −66`: cooling, assigned to ice under `endpoint`. Legacy retains its historical zero. Missing totals remain missing in both completed components, preserving common month and cell denominators.

**Findings.**

No new numerical defect found in the revised A2 path. Two qualifications remain:

1. **Should-fix, carried forward — component headlines remain inconsistent with A2.**  
   [scripts/9_forcing_barplot.R:181](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/scripts/9_forcing_barplot.R:181), together with `R/config_schema.R:151`.

   The schema still permits `variant = "veg_thresh"` or `"ice_thresh"`. Headline aggregation uses the original table; completion occurs afterwards, exclusively for A2. In the hand case, a vegetation headline gives global equivalent **−0.60**, whereas its corrected A2 bar gives **−0.50**. The new subtitle at line 336 also incorrectly promises that every permitted headline is a total.

   This was an unnumbered question in my first review and remains unresolved; it does not affect the configured `veg_ice_thresh` headline. **Check:** exercise component headline selections with the hand fixture, or test their rejection if Chris restricts headlines to totals.

2. **Nit — integration verification still depends on a retained external run.**  
   [2026-10-19_variant-components_check.R:16](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/docs/cc/verification/2026-10-19_variant-components_check.R:16) and `2026-10-19_variant-components-4.R:10`.

   Committing the reference values and specs removes the scratch-file dependency. However, the headline comparison and copied inputs still require the absolute path to the 18 October run. This limits reproduction after that run directory disappears; it does not invalidate the present result. **Check:** provide documented, checksum-identified input fixtures and a committed expected headline checksum, then verify reproduction without the original directory.

The disposition of every earlier numbered finding is:

| Earlier finding | Status | Rechecked evidence |
|---|---|---|
| Astra 1; Opus 1: legacy guard failure | **AGREE — resolved** | `R/forcing_components.R:26` returns legacy input unchanged before inspecting components. `tests/test_forcing_components.R:60` covers both historical anomalies and pass-through. |
| Astra 3: legacy regression contract | **AGREE — resolved** | `configs/legacy_config.R` has no diff from base. All four saved legacy CSVs independently match the anchor checksums. |
| Astra 2; Opus 2: second pairing overwrites vegetation | **AGREE — resolved** | `R/forcing_components.R:28` reads only the fixed-albedo triplet and writes only its two components. Scaled availability cannot overwrite vegetation. |
| Opus 3: inline, untested method | **AGREE — resolved** | One small helper in `R/`; tests cover vegetation, retreat, still ice, retained advance, threshold equality, missing fractions, legacy identity and rejection of invalid components. |
| Astra 4; Opus 4: misleading comments and labels | **AGREE — resolved for the configured outputs** | Script 9 lines 53–77 qualify the method, adjusted fractions and signed area change. Lines 322–337 distinguish totals and parts. The supplied PNG is readable. The component-headline qualification remains above. |
| Opus 5: area equality proves nothing | **AGREE — resolved** | The committed check, lines 44–50, now counts finite cell-period forcings after period sums and monthly means. |
| Opus 6: reproducibility | **AGREE — partly resolved** | Reference values and specs are committed, and their same-session provenance is acknowledged. The external-run dependency remains as described above. |

**DISAGREE** with Opus’s earlier assertion that every legacy option requires changing or remaking the anchor: Chris’s chosen preservation option leaves all four anchored CSVs unchanged. No anchor exception is needed.

The recorded PASS results apply to the reviewed code: changes after the current verification commit `2bd4ab5` are review/report documents only. I independently checked the saved outputs:

- All four legacy CSVs match their anchor checksums.
- The current headline, recovered-recipe and IPCC CSVs match the 18 October outputs byte-for-byte.
- The four existing sensitivity totals retain their values; the threshold components change and the two area-share components are added.

**What the proposed verification misses.**

The remaining useful checks are narrow:

- Put the two-cell case through the actual period/month/area aggregation, asserting the numerical domain and global results, preserved input data and unchanged unrelated columns. The new test checks row additivity and a pooled sum; existing `test_differences.R:107` separately checks endpoint telescoping.
- Include a missing pair within a period and a wholly missing month. Assert identical contributing cell/month masks for the completed fixed components and their total. Equal cell counts alone cannot establish identical masks.
- Assert zero forcing for unchanged albedo and a zero kernel, and test the permitted component-headline behavior.
- Automate preservation of the other current CSVs and existing total variants; those comparisons were performed during this review but are not all in the committed check.

No seeds, draw counts or random calls change. The default remains 100 prediction draws with a random seed; legacy retains seed 1. These script-9 checks reuse fixed upstream realizations. Seed-1 identity establishes reproducibility of that seeded path, and historical tolerances assess stochastic variation; neither validates the component definition. Legacy byte identity preserves its known faults.

Kernel selection and scaling are unchanged: the current configuration selects all-sky TOA variables and CACK’s climatological mean; legacy retains its earlier selections. Script 8 still applies ×100 to HadGEM3/CAM5 and the negative unit-albedo CACK kernel.

**Questions for Chris or Andria.**

- **Chris — UNRESOLVED:** Should headlines be restricted to totals, or should component headlines receive consistent completion across headline and sensitivity outputs? Meanwhile, component headlines remain legal and retain the old aggregation. This earlier question still needs filing in the scientific questions document.
- **Andria, C1 — UNRESOLVED:** Which threshold/area-share and fixed/scaled combination should the paper report? Meanwhile, the headline remains fixed-albedo `veg_ice_thresh`, and current A2 shows both fixed-albedo splits.
- **Andria, C2 — UNRESOLVED:** Retain real readvances or enforce monotone retreat? Meanwhile, one-pass smoothing remains; `endpoint` counts advances that survive it. Chris’s component decision does not settle C2.

Chris’s legacy-preservation decision is explicit and implemented. I found no new instance of a question being presented as his decision.

**Not checked.**

No R execution, full-pipeline regeneration, binary RDS inspection or independent kernel-file metadata inspection. I did not reproduce the 343-cell attribution. I inspected the supplied sensitivity PNG, not other figures. Questions are returned for filing; no files were changed.

Files read, including excerpts: packet, revised diff, both reviews, AGENTS/CONTRIBUTING, regression contract, both questions files, both configs, relevant `R/` helpers/schema, scripts 7a/8/9, component/difference/config tests, verification specs/reports/check/log, anchor definitions and saved CSVs, glacier CSV, verification runner, sensitivity PNG. Tools: 13 batches containing 46 read-only shell calls and one image view; no R or writes.