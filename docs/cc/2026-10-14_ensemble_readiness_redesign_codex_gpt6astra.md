# Ensemble readiness: a simpler design than the lc_draw helper

**What this is.** Chris (2026-10-14) parked the draw-identity helper (`lc_draw`, planned for
I06 and carried through I08-I13a) as overcomplicated, and asked Astra for a simpler design,
judged first on readability for Andria and her colleagues. Recommendation: no ensemble work
during the migration; at I14, one ordinary `for` loop over the selected land-cover draws
runs each reconstruction through the normal pipeline functions, and results are pooled
only at the end.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `max`,
2026-10-14, from the current code (branch i04b-settings b88813b), the migration design v2,
AGENTS.md and questions A1-A3 inlined (no shell available to the model).

---

I recommend **one ordinary `for` loop over land-cover draws, running each reconstruction through the existing functions to its final period totals**. Combine those totals afterwards. This keeps the numerical functions readable and requires **no ensemble-specific implementation now**.

References below use the supplied current line numbers. The migration design and scientific-question excerpts are unnumbered, so references to them use issue or question identifiers.

An ensemble run must get these things right:

- **Select a reconstruction before reducing away its identity.** Today `reduce_land_cover()` averages or takes medians across posterior draws (`R/preparation.R:16–28`); script 1 then derives both modern and paleo tables from that reduction (`scripts/1_veg_lct_prep.R:75–89`). Under A1(ii), calibration remains fitted once to the mean modern reconstruction; selected paleo draws enter stage 6 separately. A1(iii), including calibration per draw, remains a separate scientific choice.
- **Keep land-cover draws separate through prediction and differences.** Stage 6 currently summarises simulated-response `iter` values (`scripts/6_prediction_model.R:53–76`). That within-reconstruction summary must not also pool land-cover draws. Stages 7 and 7a must pair younger and older slices from the **same** reconstruction: their present loops select only cell and month before ordering and differencing (`scripts/7_plot_preds.R:411–428`; `scripts/7a_alb_diff_full.R:245–252`). Fixed column selections also discard extra identities (`scripts/7a_alb_diff_full.R:116–117`). Changing `group_by()` alone is insufficient.
- **Apply kernels and aggregate separately for each reconstruction.** Kernel multiplication is rowwise and can reuse the same sampled kernels (`scripts/8_radiative.R:116–150`). Stage 9’s completeness checks, period sums, monthly means and spatial weighting must all operate within a reconstruction (`scripts/9_forcing_barplot.R:150–173`). Combining draws there could inflate sums as well as erase variation.
- **Pool completed results.** First produce each draw’s domain mean and global equivalent for each period, kernel and variant; then calculate the agreed ensemble summaries across those results. Any pooled albedo maps are reporting products and must not replace the separate inputs to differences.
- **Keep the scientific caveats explicit.** A1(d) asks whether a common raw `iter` denotes one joint realisation across cells, slices and the modern field. Complete labels and fractions summing to one do not establish that; combining independent local draws could give spuriously narrow continental uncertainty. A2 separately concerns response noise versus coefficient uncertainty. Current differences use `alb_mean`, not propagated response realisations (`scripts/7a_alb_diff_full.R:137`, `:286–315`). Neither an identity column nor a loop resolves these questions. See `docs/cc/questions_for_andria_scientific.md`, A1(d) and A2.

The options compare as follows, with readability taking precedence:

| Option | Readability for Andria | Risk of silently mixing draws | Work now → work at I14 |
|---|---|---|---|
| **Explicit branch on an ensemble setting** | Clear at the script: two named computations are visible. Repeated branches become harder to follow across stages. | Every affected reshape, pairing, completeness check and aggregation still needs the right implementation. | None required now → add branches and ensemble implementations at affected stages. |
| **An `ensemble` argument dispatching to separate functions** | Familiar R, but the reader follows another function call to discover the branch. | Same numerical risks; additionally the flag can disagree with the input. | None required now → extend interfaces and add separate implementations. |
| **S3 classes on tables** | Least suitable here: behaviour depends on class and method lookup rather than the visible call. | Classes do not automatically protect ordinary grouping, joins or differencing. Class preservation and dispatch need checking. | None required now → constructors, methods, validation and class-preservation tests. |
| **Outer loop, one reconstruction per call** | **Clearest:** repeat the familiar calculation for each selected draw. | **Lowest structural risk:** a numerical call receives only one reconstruction. Selection and identity bookkeeping still need validation. | **No ensemble work now** → selector, driver, final pooling and records. Existing numerical functions remain reusable. |

These are future sketches, not additions proposed for the migration. For the first three, `summarise_responses_single()` denotes I10’s unchanged extraction of `scripts/6_prediction_model.R:69–76`. The separate ensemble implementation would make the extra grouping visible:

```r
summarise_responses_ensemble = function(samples) {
  samples %>%
    group_by(lc_draw, year, x, y, elev, ET, OL, ST) %>%
    summarise(
      alb_mean = mean(value),
      alb_sd   = sd(value),
      alb_lo   = quantile(value, 0.025),
      alb_mid  = quantile(value, 0.5),
      alb_hi   = quantile(value, 0.975),
      .groups = "keep"
    )
}
```

The explicit branch sits in script 6:

```r
if (ensemble) {
  summary = summarise_responses_ensemble(samples)
} else {
  summary = summarise_responses_single(samples)
}
```

The argument-based version moves that branch inside the public function:

```r
summarise_responses = function(samples, ensemble = FALSE) {
  if (ensemble) return(summarise_responses_ensemble(samples))
  summarise_responses_single(samples)
}

summary = summarise_responses(samples, ensemble = ensemble)
```

The S3 version makes the class select the implementation:

```r
summarise_responses = function(samples) UseMethod("summarise_responses")
summarise_responses.data.frame  = summarise_responses_single
summarise_responses.lc_ensemble = summarise_responses_ensemble

class(samples) = c("lc_ensemble", class(samples))
summary = summarise_responses(samples)
```

All three need validation that the mode matches the input, plus corresponding changes outside response summarisation. A flag or class is not itself protection.

The recommended loop is shown here at stage 9. `forcing_by_draw` contains separate, ordinary forcing tables produced by running stages 6–8 separately for each member:

```r
period_results = list()

for (draw_id in draw_ids) {
  member_forcing = forcing_by_draw[[as.character(draw_id)]]
  totals = aggregate_forcing(member_forcing, column, cfg)

  totals$lc_draw = draw_id
  period_results[[as.character(draw_id)]] = totals
}

by_draw = bind_rows(period_results)
# Only now calculate the agreed ensemble summaries,
# separately for each period, kernel and variant.
```

In the actual driver, the loop should encompass selection, prediction, differences, kernels and aggregation, so it can process one member at a time. The identity belongs to the driver and its saved results; intermediate numerical tables retain their existing shape. The coarse-difference/EGU route follows the same rule.

For the remaining issues, the necessary work **now** is:

| Issue | Required now |
|---|---|
| **I06** | Remove the proposed identity helper and `R/stage_contracts.R`. Keep the manifest work. |
| **I10** | Extract the existing prediction and response-summary functions with explicit inputs and seeds. Keep response `iter`; no new draw identity or dispatch argument. |
| **I11** | Extract differences for one reconstruction per call, retaining explicit age ordering and existing missing-value behaviour. |
| **I12** | Keep the planned reusable kernel table and checked joins. No draw handling. |
| **I08** | Nothing additional. Keep reduction unchanged; a separate draw selector can be added at I14. |
| **I13a** | Include period preparation and completeness checking in the callable calculation on its supplied input; remove the globals as already planned. No ensemble pooling. |
| **I09** | Nothing additional. Keep models, data and seeds explicit as planned. |

Thus the only ensemble preparation is documenting **one reconstruction per call**. Remove the premature two-land-cover-draw tests from the extraction issues and move them to I14. Add no configuration key: the current policy already requires a consumer before adding one (`config.R:13–15`; `R/config_schema.R:3–7`).

This avoids rewriting the extracted calculations for A1(ii) with the current response treatment. A scientific change under A2 may require new numerical functions; that cannot be guaranteed away by interface design.

I14 also owns fixed-fit reuse and provenance. Redirected paths already exist, but `check_producer()` compares the whole `analysis` configuration, so introducing ensemble settings needs an explicit, recorded reuse policy (`R/paths.R:18–25`; `R/run_manifest.R:139–170`).

The following replaces only the ensemble-related passages; other designs and baseline verification gates stay.

For **I06**:

> **Title:** Wire manifests into every live pipeline script
>
> **Goal:** Give standalone `Rscript` and `source()` execution the same provenance behaviour, and record exactly what each invocation reads and writes.
>
> **Scope — replacement for the stage-contract sentence:** Ensemble orchestration is deferred to I14. This issue adds no draw-identity helper or `R/stage_contracts.R`.

For **I10**:

> **Design — replacement for the draw-identity paragraph:** Deterministic predictions, response samples and summaries are returned separately. Each call takes one land-cover reconstruction in the current table shape. The response-sample `iter` column keeps its existing meaning and on-disk representation; it is distinct from the raw land-cover posterior’s `iter`. No land-cover identity column or ensemble argument is added. I14 can call these functions once per selected reconstruction. Reads and writes remain in the wrapper.
>
> **Verification — draw checks:** A response label above 999 parses. Ensemble isolation is verified in I14.
>
> **Acceptance criteria — replacement bullet:**
>
> - [ ] One reconstruction per call; response `iter` preserved; no global or file discovery in functions.

Also remove I10’s stale “no production seed” phrase: explicit seeds are already implemented (`scripts/6_prediction_model.R:53–56`).

For **I11**:

> **Design — replacement grouping sentence:** Each call takes one reconstruction. Group by cell and month, with explicit age ordering; preserve the existing calendar completion and pair construction. I14 repeats the calculation for selected reconstructions.
>
> **Verification — replacement fixture sentence:** Test missing ages, threshold crossings and multiple readvances. Verification that separate reconstructions cannot supply one another’s rows belongs to I14.

For **I12**:

> **Design — replacement sampling/join paragraph:** Sample once per distinct cell and month and return a reusable table. Apply it to one reconstruction per call. Validate join uniqueness and coverage so the join introduces no duplicated or lost difference rows; retain the existing latitude trim and missing kernel values. The same sampled table can be passed to repeated calls.
>
> **Verification — replacement list:** Compare reused versus freshly sampled tables; four non-consecutive months; reordered months; run without script 7’s ice tables; check the CACK year mapping and all three conversions directly. Reuse across ensemble members is verified in I14.

For **I13a**:

> **Design — replacement paragraph:** Remove the closure over global `d` and `EARTH_AREA`. Each call prepares periods, checks completeness and aggregates one reconstruction. Preserve period sums → monthly means → area weighting, the NA behaviour and both normalisations. Keep the EGU recipe separate with its four-month requirement. Plots consume the returned tables. I14 labels the results by land-cover draw and pools them after aggregation.
>
> **Verification — replacement fixture sentence:** Test incomplete periods and missing kernel months. Ensemble isolation and pooling are verified in I14.

For **I14**, replace the goal and requirements list:

> **Goal:** Design a simple driver that repeats the extracted calculations for selected land-cover reconstructions, then summarises the completed results. No ensemble implementation is required during the migration.
>
> **Requirements on the future driver:**
>
> - Select each reconstruction by its original posterior `iter`, retaining the associated classes, cells and slices. Use a separately named selection function; leave `reduce_land_cover()`’s mean and median semantics unchanged.
> - Pass one reconstruction at a time through prediction, coarse and consecutive differences, kernel application and aggregation. Validate member identity and expected row keys before calculation; never combine members to satisfy coverage checks.
> - Keep land-cover identity separate from simulated-response identity. Record the selected source ids and the agreed seed policy; repeatable results must not depend on member processing order.
> - Prepare static geography, ice fractions and sampled kernels once and pass them to repeated calls.
> - Retain each member’s completed results. Pool only afterwards, separately by period, kernel and variant, using the summaries agreed with Andria.
> - Record settings, selected draws, calibration inputs, seeds and outputs. Keep member products distinct and baseline paths unchanged. Design fixed-fit reuse explicitly against the existing whole-`analysis` manifest check.
> - Verify a small two-member run against two separate calls of the ordinary pipeline functions. Include identical member values, contrasting values, incomplete coverage and reordered members; ensure no member is merged, lost or used to fill another’s gaps.
>
> **Open points:** Before ensemble numbers are quoted, Andria must confirm joint draw identity across cells and slices, including the modern field; the selected-draw count and calibration treatment under A1; and the uncertainty claim under A2. This design chooses none of them.

Remove the corresponding helper references from the v2 overview and dependency map. The extraction issues still follow I06 for the completed manifest wiring; they no longer depend on a draw-identity convention.