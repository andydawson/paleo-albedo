# I16a review: the endpoint method also removes a double count (Fable)

**What this is.** Fable's adversarial review of the same question as
`2026-10-14_i16a_double_count_review_codex_gpt6astra.md` (which also holds the brief). Fable
read the code and ran read-only R analyses of the run outputs (a full cross-tabulation of
the adjusted and unadjusted ice classes over all 806,172 rows).

**Who wrote it.** A Claude Code subagent (Fable), maximum effort, read-only for the
repository, 2026-10-14. Scratch scripts were in the session scratchpad
(`i16a_review/fable/{crosstab,missing,handmade,na24}.R`).

---

## Verdicts

**1. Diagnosis: right.** Confirmed in code and data.
- Code: `R/differences.R` 43-45 classify from `ice_frac_adj`; 82-83 subtract `alb_ice_thresh[old]`, which `scripts/7a_alb_diff_full.R` 153-161 builds from the raw `ice_frac` before any adjustment; 93 sums the two with `na.rm=TRUE`.
- Data (crosstab over all 806,172 rows of the two `ALB_diffs_bluesky.RDS`): the 72 differing "both vegetation" rows all have a non-NA legacy ice component, which is only possible when the unadjusted older fraction was > 0.5, while the adjusted older fraction is 0.33-0.46. In all 72, legacy total − endpoint total equals the legacy ice component exactly, endpoint vegetation equals legacy vegetation, endpoint ice is NA. Hand-made cell `ice_frac = c(0.22, 0.60, 0.48)`, `alb_veg = c(0.20, 0.25, 0.30)` reproduces it: adjusted old = 0.35; legacy −0.05 + (0.20 − 0.68) = −0.53, endpoint −0.05.

**2. Removing the double count is correct, and it is not a separable choice.**
- Adding a vegetation change and an ice change for the same pair is wrong under any classification: they are two descriptions of one pair's change, not components.
- The original already classifies from the adjusted fraction in three of four places (vegetation mask, still-ice, the area-mixed variants); the unadjusted column is a by-product of per-slice work in 7a done before the per-cell adjustment exists. One definition of "ice" per output row is the readable choice.
- Classifying endpoint from the unadjusted fraction would make the threshold variants ignore the readvance rule while the mixed variants obey it, and would still not reproduce the legacy numbers (legacy is veg + ice, not either).
- If Andria drops the readvance rule, adjusted = unadjusted and the issue disappears; keeping adjusted-throughout leaves her question intact.

**3. Recommendation: (a) merge as is.** A separate switch would describe an inconsistency, not a method; it cannot reproduce the legacy value (which `legacy_components` already gives byte for byte); it is one more knob to explain to the PI. Condition: stop describing the endpoint change as "advances only".

**4. Other disagreements.** Exhaustive crosstab (adjusted young/old class × unadjusted old class), counts of rows / rows whose total differs:
- advance, unadjusted old vegetation: 36 / 36 (the known fault)
- both vegetation, unadjusted old ice: 72 / 72 (the double count)
- both vegetation, unadjusted old vegetation: 738,036 / 0
- retreat (all 10,392 have unadjusted old ice): 0
- both ice: 52,656 / 0; missing: 4,980 / 0

So nothing agrees "by accident" in this data. Two latent legacy cases, absent here but possible, both from the same root: (i) a retreat whose older slice was raised above the threshold by the adjustment gives a silent legacy zero (hand-made `c(0.3, 0.9, 0.4, 0.3)`: legacy 0 vs endpoint −0.46); (ii) an advance whose older slice was lowered gives legacy veg_young − glacier_old (about −0.5) vs endpoint about +0.4.

NA pattern: 24 rows (cell 692 year 6500, cell 83 year 5000, all months) where a readvance slice next to a missing slice gets adjusted fraction NA. Legacy leaves the vegetation component numeric, endpoint gives NA. Totals NA in both; both cells are north of 74°N and script 8 drops them, so no forcing effect. The ice-component NA pattern also differs by design on advance pairs (legacy NA/NA, endpoint ice = total), which is why the `ice_thresh` sensitivity variant moves at 0.5-2 ka (0 → −0.0002 mW/m² in the clear-sky run).

**5. Else.** No bug found in `endpoint_albedo_differences()`; tests pass. Three small points: (i) "still ice gives 0" is a rule in legacy but only a consequence in endpoint (glacier albedo varies by month only); say so. (ii) The docstring input list implies endpoint needs `alb_ice_thresh`/`alb_icesc_thresh`; it never reads them. (iii) 7a's per-slice `alb_veg_thresh`, `alb_ice_thresh`, `alb_veg_ice_thresh` columns are used by nothing but the legacy method; label them.

(Fable's exact proposed wording for the comments and the test case was adopted almost verbatim; see the commit.)
