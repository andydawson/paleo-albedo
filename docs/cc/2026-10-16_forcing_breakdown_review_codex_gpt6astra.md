# Forcing by month and latitude (script 10): review by Astra (Codex gpt-6-astra, with reconciliation)

**Verdict.** Changes requested: the decomposition arithmetic is sound for pipeline inputs, but an accepted band width crashes and the output needs clearer scientific and missing-data labels.

**Benefit.** This lets Andria locate the months and latitudes contributing to the existing forcing estimates before testing their dependence on modern snow conditions.

Reviewed `fb333d4` → `2c7a0e4`; executable changes end at `d766260`.

**Hand check.**

Take two cells in one band, two months and two consecutive slice-pairs within one period. Let their areas be 2 and 1, and Earth’s area be 100 in the same area units.

| Cell | Area | January increments | February increments | Valid period-month sums |
|---|---:|---:|---:|---:|
| A | 2 | 1 + 2 = 3 | 2 + 4 = 6 | 2 |
| B | 1 | NA + NA = NA | 3 + 6 = 9 | 1 |

The four output columns should be:

| Month | `global_equiv_month` | `contribution_to_annual` | `band_mean` | `area_m2` |
|---|---:|---:|---:|---:|
| January | 6/100 = **0.06** | (6/2)/100 = **0.03** | 6/2 = **3** | **3** |
| February | 21/100 = **0.21** | (12/2 + 9/1)/100 = **0.15** | 21/3 = **7** | **3** |

Script 9 gives cell means 4.5 and 9, hence `(2 × 4.5 + 1 × 9)/100 = 0.18`. Script 10 gives `0.03 + 0.15 = 0.18`. **The source agrees.** January also demonstrates that `area_m2` is not necessarily the denominator of `band_mean`.

The identity survives the requested missing-data cases:

- If a forcing increment is NA, `sum(..., na.rm = FALSE)` invalidates that entire cell-month period sum in both scripts.
- If B’s February sum also becomes NA, B has no valid months. Both scripts omit its contribution; the total becomes **0.09**.
- If A lacks one January pair, the completeness filter removes A’s January rows. Its February value receives weight 1 rather than ½; both totals become **0.21**. The filter removes a **cell-month-period**, not necessarily the whole cell.

Generally, write \(S_{im}\) for a retained cell-month’s period sum, and \(n_i\) for its number of non-NA sums. Then

\[
\sum_{b,m} C_{bm}
=\frac{1}{A_E}\sum_{i:n_i>0}\frac{A_i}{n_i}
       \sum_{m:S_{im}\ne NA}S_{im},
\]

which is script 9’s global equivalent. This requires fixed area and latitude for each cell, as script 7a constructs from its grid. Script 10 groups its denominator by cell; script 9 also groups by area, so the equality is not unconditional for malformed tables with changing cell areas.

The sign and scaling also agree: albedo declining from 0.30 to 0.27 gives \( -0.03\times100\times(-1)=+3\) W/m² for a kernel of −1 W/m² per 0.01 albedo. CACK at +100 W/m² per unit albedo gives the same result through its negation. Script 10 introduces no further kernel scaling or regional normalization.

The supplied CSV independently sums to the repository’s script 9 CSV within **5.6 × 10⁻¹⁶ W/m²**. This corroborates the supplied log.

**Findings.**

1. **Should-fix — [scripts/10_forcing_breakdown.R:4](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/scripts/10_forcing_breakdown.R:4), also lines 60, 113 and 127: the snow interpretation exceeds what the selected quantity identifies.**

   `veg_ice_thresh` combines vegetation changes and ice transitions. Neither month nor latitude identifies snow cover, and the script reads no snow observations. It answers where the **combined forcing** occurs; it cannot identify the forcing caused by vegetation masking snow or establish insensitivity to past snow conditions.

   In the supplied 8–10 ka result, June–August contribute **0.19145 of 0.30280 W/m²**, or **63.2%** of the signed total; 50–60°N contributes **72.9%**. Those numbers are correct, but neither percentage measures the fraction unaffected by snow.

   The printed tables and figure title also omit the variant and kernel configuration. These supplied numbers use legacy clear-sky HadGEM3, whereas `config.R` now selects all-sky HadGEM3.

   **Proposed check/fix:** identify the variant, sky condition and inherited averaging convention in the output captions. Describe this as a seasonal/geographic diagnostic. Before drawing vegetation-specific snow conclusions, resolve the companion-variant question below. A vegetation-only companion would help, but would still not itself identify snow causation.

2. **Should-fix — [scripts/10_forcing_breakdown.R:68](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/scripts/10_forcing_breakdown.R:68); `R/config_schema.R:154`: valid configurations can fail before reading data.**

   Width 90 passes validation. With the default 27–74°N domain, the expression calls `seq(90, 74, by = 90)`. R rejects that with “wrong sign in 'by' argument”; the rejection is explicit in [R’s `seq.default` source](https://svn.r-project.org/R/trunk/src/library/base/R/seq.R). A narrowed domain wholly between two multiples produces the same failure.

   The ordinary edge cases are correct: width 10 yields `27,30,40,50,60,70,74`; width 7 yields `27,28,35,42,49,56,63,70,74`. With half-degree centres, the default bands assign every retained cell once. `cut()` is right-closed: an exact interior boundary belongs to the southern band, which becomes relevant for fractional widths.

   **Proposed check/fix:** handle an empty set of interior multiples with a simple guard. Test widths 7, 10 and 90, a narrow domain, and a fractional width whose edge coincides with a cell centre.

3. **Should-fix — [scripts/10_forcing_breakdown.R:35](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/scripts/10_forcing_breakdown.R:35), also lines 83–90: the descriptions obscure different missing-data denominators.**

   The code counts months with a **non-missing period forcing sum**, not simply months with a kernel value. Missing albedo or variant-specific NA values also remove months. `band_mean` excludes those cells’ weights, while `area_m2` includes their areas.

   This occurs in the supplied output. For CACK, December, 70–74°N, every period reports:

   - `global_equiv_month = 0`;
   - `contribution_to_annual = 0`;
   - `band_mean = NA`;
   - `area_m2 = 568,828,987,254.169`.

   In January of the youngest period, the area implied by the valid weighted mean is only **81.48%** of the reported area.

   The arithmetic preserves script 9, but an omitted contribution can look like a measured zero. The annual quantity also inherits averaging over available months; it is not necessarily a twelve-month mean.

   **Proposed check/fix:** state the precise denominators and distinguish retained area from valid-forcing area, preferably with a valid-area column. Test partial coverage, wholly missing bands and non-kernel NA values. Preserve the legacy numerical convention unless a separate scientific decision changes it.

4. **Nit — [docs/cc/verification/2026-10-16_forcing-breakdown.R:14](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/docs/cc/verification/2026-10-16_forcing-breakdown.R:14): the verification differs from the supplied contract.**

   The spec and report use four threads; the contract requires eight. The demonstrated CSV identity remains valid evidence, and these aggregation changes contain no simulations. Nevertheless, the report should record an approved exception or use the contracted setting.

I found **no unnecessary abstraction** here: the shared period helper prevents duplicated selection rules, and the aggregation function is small. A general reporting framework would not improve this change.

The “no new method” claim is defensible **as an algebraic decomposition of the existing estimator**. It does not validate that estimator’s scientific assumptions. Published precedent for the presentation exists:

- **Loranty, M. M., Webb, E. E., Talucci, A. C., and Alexander, H. D. (2026).** *Diminished Radiative Cooling With Northward Wildfire Advance in the East Siberian Boreal Region.* Geophysical Research Letters, **53**, e2026GL125497. DOI: **10.1029/2026GL125497**. Section 2.2 explicitly averages monthly albedo forcing within 55–60°, 60–65° and 65–70°N bins; Figure 3 presents March, July and seasonal forcing. This directly supports month/latitude stratification. [Publisher text](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2026GL125497).
- **Ghimire, B., Williams, C. A., Masek, J., Gao, F., Wang, Z., Schaaf, C., and He, T. (2014).** *Global albedo change and radiative cooling from anthropogenic land cover change, 1700 to 2005 based on MODIS, land use harmonization, radiative kernels, and reanalysis.* Geophysical Research Letters, **41**, 9087–9096. DOI: **10.1002/2014GL061671**. Sections 2.3–3 and Figure 3 support spatial and seasonal reporting of land-cover albedo forcing, and explain why large winter albedo changes need not produce the largest forcing. [Publisher text](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2014GL061671).

Neither establishes this pipeline’s particular missing-month weighting or validates its Holocene snow assumption. Record the references and these limited claims when filing the change. B2d’s decision is present on `chris-dev`, but absent from this branch’s scientific questions file.

**What the proposed verification misses.**

- **Independent expected values for all four columns.** The hand case above should become a targeted test through `tools/verify_change.R`, including missing pairs, non-kernel NAs and an entirely missing cell.
- **Correct allocation.** Swapping month labels or assigning every cell to the wrong latitude band can preserve the sum. Assert individual band/month membership and values, plus conservation when changing band width.
- **Coverage and signed cancellation.** Assert that all-missing forcing is distinguishable from valid zero forcing. Opposing contributions can cancel; a small signed contribution is not a bound on snow sensitivity.
- **The completeness assumption.** The helper compares against years observed somewhere in each period, not an independently supplied expected pair set. A pair missing everywhere is invisible to that check. This is inherited behavior, not an extraction regression.
- **Physical telescoping and ice advances.** Equality between scripts 9 and 10 cannot prove either. An old vegetation albedo of 0.2 followed by ice at 0.6 should give −40 W/m² with the example kernel. The endpoint path represents that change; the inherited legacy path can report zero for a surviving threshold advance. Script 10 faithfully decomposes whichever input it receives.
- **The current scientific configuration.** The provided run uses frozen legacy forcing. It does not demonstrate results from the newer all-sky, endpoint configuration.

There is **no random-stream change in the reviewed diff**. Scripts 9 and 10 introduce no draws; `R/random.R` and the configured 100 evaluation/prediction draws are unchanged. The seed-1 anchor establishes reproducibility at that seed, while historical tolerances concern unseeded variation. Neither validates the new allocation, and this downstream frozen-table run exercises neither simulation path.

**Questions for Chris or Andria.**

- **B2d:** Should this remain a labelled diagnostic of combined vegetation-and-ice forcing, or should a vegetation-only companion precede snow interpretation? If the latter, choose explicitly between `veg_thresh` and `veg_part`; they answer different questions. Meanwhile the code reports only the configured `veg_ice_thresh`.

  A crucial qualification: `veg_thresh` is not automatically an additive component of the reported period total. For one cell with an ice-to-vegetation transition of −0.3 followed by a vegetation change of −0.1, the combined forcing is **40 W/m²** with the example kernel. Each separate threshold component contains a structural NA in one pair, so its period sum becomes NA and contributes zero globally. A companion must disclose this selection.

- **B2b/C3:** Retain the inherited available-month average for this diagnostic, or separately review a complete-calendar treatment of confirmed polar-night gaps? Meanwhile the code omits missing months and redistributes their averaging weight across available months. Missing values should not be converted wholesale to physical zeros without that review.

Chris’s approval establishes the breakdown task. It does not settle these scientific interpretations; B2d explicitly leaves the paper’s final claim to Andria.

**Not checked.**

R execution and the verification suite were not available: `Rscript` was not installed. I checked source, hand arithmetic and the supplied CSV using Python. I did not independently validate raw cell-level RDS data, cover-fraction closure, fitted albedo bounds, NetCDF metadata/grid alignment, or regenerated current-configuration outputs. Kernel selections were traced through configuration and source, not re-read from NetCDF files. Dedicated reference checkers were unavailable and Crossref API access failed; publisher identities and supporting passages were checked. No other review of this change, `writing/` content, or excluded document formats were opened.

Files read/searched: `CONTRIBUTING.md`; packet diff/log/CSV; scripts `7a`, `8`, `9`, `10`; `R/{calendar,config_schema,differences,forcing_periods,kernels,paths,random,run_manifest}.R`; both configs; README diff; config/path tests; regression contract; both question files, including B2d on `chris-dev`; changelog search; verification spec/report; script 9 CSV. Tool calls: 15 `functions.exec` calls dispatching 48 read-only command attempts, including two Python checks and one failed Rscript invocation, plus eight web calls; no files modified.
## Reconciliation reply

**Verdict — DISAGREE with merging as submitted.** I retain “changes requested” for the accepted-width crash and misleading scientific and missing-data descriptions. **AGREE** that the decomposition arithmetic is sound for pipeline inputs. The main scientific disagreement is that ice-retreat forcing still depends on calibrated vegetation albedo.

**Hand check — AGREE.** I rechecked all four columns in Fable’s example: both scripts give **8.5/E**. Our original hand cases use different fixtures and are consistent. The algebra holds for fixed cell area and latitude, including all-missing cells and the shared completeness filter. Script 7a constructs area and coordinates from the grid, supporting those assumptions.

Re-summing the supplied CSV against the anchor gives a maximum discrepancy of **5.55 × 10⁻¹⁶ W/m²**. I also confirmed Fable’s CACK January ratios, the **81.48%** valid-area fraction, and the December distinction between HadGEM3’s valid zero and CACK’s missing mean.

Replies to Fable’s numbered findings follow. Line numbers refer to the current source, rather than packet-diff offsets.

1. **Scientific scope — AGREE on the interpretation problem; DISAGREE that the ice-retreat term is untouched by the calibration’s snow assumption.**

   The combined diagnostic cannot identify snow causation or bound snow sensitivity. I reconfirmed that 50–60°N contributes **0.220855** of **0.302796 W/m²** at 8–10 ka. That geographic contribution is not itself a measured ice-only contribution.

   Crucially, the retreat calculation contains **vegetation albedo at the newly exposed endpoint minus glacier albedo**. This is explicit in [R/differences.R:95](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/R/differences.R:95), and the endpoint implementation preserves that dependence. Fixed glacier albedo therefore does not make retreat forcing independent of the calibrated vegetation endpoint.

   **Settling test:** hold ice albedo at 0.6 and the kernel at −1 W/m² per 0.01 albedo. Retreat to vegetation albedo 0.3 gives +30 W/m²; changing that vegetation endpoint to 0.4 gives +20 W/m². The ice state and glacier albedo remain fixed.

   A vegetation-only companion is useful, but its header must not imply that it captures every part of the combined forcing potentially affected by past snow. This strengthens the qualification in my original report.

2. **Column documentation — AGREE, with a qualification to the multiplier.**

   Rechecking [script 10’s aggregation](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/scripts/10_forcing_breakdown.R:80) confirms both documentation gaps.

   Summed `global_equiv_month` is 12 times the annual result **when every contributing cell has 12 valid months**. With heterogeneous month counts there is no single general multiplier; the multiplication by each cell’s count occurs before spatial aggregation.

   Also, `band_mean` uses only cells with a non-missing period-month forcing sum, whereas `area_m2` includes all retained cells in that month-band. The month denominator is determined by **valid forcing**, not merely kernel availability. Missing albedo and structural variant NAs matter too.

   Explicit descriptions are necessary. A valid-forcing-area column would additionally make missing coverage visible without changing the inherited estimator.

3. **Band-width robustness — AGREE on the crash; DISAGREE that fractional widths inherently violate the stated assignment rule.**

   Widths 75–90 produce a starting multiple above 74 in [script 10:68](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/scripts/10_forcing_breakdown.R:68). The schema accepts them. R explicitly rejects the resulting negative `(to-from)/by`. [R’s `seq.default` source](https://svn.r-project.org/R/trunk/src/library/base/R/seq.R).

   However, the documented rule assigns **cell centres** to bands. Under right-closed intervals, assigning centre 32.5 to `(30,32.5]` is consistent with that rule; it does not claim the whole cell lies inside the band. Whole-degree validation is a reasonable simplification for this grid, but is not mathematically required.

   Fable’s proposed edge construction works for this positive-latitude domain. It is not a general replacement across the schema’s admitted negative latitudes. A guarded sequence of integer multiple indices handles both.

   **Checks:** accept the proposed widths 1, 3, 7, 47, 74 and 90; retain default 10, add a narrow domain without an interior multiple, and either reject fractional widths explicitly or assert their documented boundary membership. If adopting Fable’s constructor unchanged, test a negative-latitude domain too.

4. **Unused period levels — AGREE on the defect; DISAGREE with the `slice_alias = NULL` example for this path.**

   `nlevels()` can count an empty period and falsely report missing script 9 totals. Comparing the observed `(kernel, period)` keys is preferable; `n_distinct()` addresses the stated unused-level problem.

   But `slice_alias` acts in script 7’s coarse-difference path. Script 7a uses `analysis$ages`, and the principal script 9/10 aggregation reads script 8’s consecutive-pair table. Removing the alias does not itself remove its 10–12 ka period.

   **Settling checks:** use a fixture with a genuinely empty period to demonstrate the false stop; separately change only `slice_alias` and check that the principal forcing-period keys remain unchanged.

5. **Equal-height tiles — AGREE as a presentation clarification.**

   The categorical tiles represent contributions, which intentionally include area. I rechecked the CSV: the outer bands have roughly **12–16%** of the area of the interior bands.

   A caption explaining unequal band widths and retained areas is sufficient. A `band_mean` panel is optional and would need its own valid-area qualification. I have not inspected the rendered figure.

6. **Literal `11500` and unused `age_old` — AGREE.**

   I compared the extraction with the original script 9 block: these lines were moved unchanged. Neither aggregation uses `age_old`. Leave this outside the present arithmetic fix and record it for cleanup; if retained later, the configured ages should supply the endpoint.

7. **Records — AGREE on the packet omission and B2d follow-up.**

   The packet diff omits README, but the **actual repository diff includes it**, covering the script table, inputs/outputs and run order. This is a packet discrepancy, not an absent README change.

   I confirmed B2d is present on `chris-dev` and absent from this branch. The recorded result should include the legacy configuration, companion-variant limitations, and the vegetation-endpoint qualification above. No question files were edited in this read-only round.

For the verification proposals:

- **Independent expected values — AGREE.** The adding-up check cannot detect a shared error or incorrect month/band allocation. Add the hand fixture through `tools/verify_change.R`, asserting all four columns and actual labels. Include cells with different valid-month counts **within the same band**, non-kernel NAs, an entirely missing cell, and a dropped cell-month.

- **`global_equiv_month == months_with_value × contribution_to_annual` row by row — DISAGREE as a general invariant.** `months_with_value` belongs to cells, not aggregated output rows. The supplied CACK February, 70–74°N, youngest-period row already has a ratio of **10.778809**, not an integer month count. In my original positive-valued hand case, putting both cells in one band gives February values `21/E` and `15/E`, a ratio of 1.4. Test the cell-weighted expression directly. The 12-fold identity is appropriate for fixtures established to have 12 valid months everywhere; I confirmed it numerically for the supplied HadGEM3 and CAM5 rows.

- **Monthly band areas sum to script 9’s annual area — DISAGREE as an unconditional invariant; AGREE for this supplied run.** The [completeness filter](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/R/forcing_periods.R:31) removes cell-month-period combinations. If an area-2 cell loses January but retains February, alongside an area-1 cell, January’s retained area is 1 while script 9’s period area is 3. Assert monthly areas against retained monthly membership; assert equality to annual area only when membership is common across months.

- **`band_mean = global_equiv_month × E / valid_area` — AGREE when valid area is positive.** Add explicit checks for zero valid area and for valid zero forcing. They must remain distinguishable.

- **Band-edge tests — AGREE**, with the additions and fractional-width distinction in finding 3.

- **Current-configuration run and month-count distributions — AGREE.** The frozen legacy run does not establish the all-sky, endpoint results. Report counts per kernel and period, including zero-valid-month cells. The relevant input must actually have been produced under the current scientific settings; relabelling the frozen legacy table is insufficient.

- **Four versus eight threads — AGREE that the observed arithmetic evidence remains useful; UNRESOLVED as a contract exception.** These aggregations introduce no simulations, and the anchor equality is evidence for this run. Nevertheless, the contract explicitly requires eight threads. **Question for Chris:** “May this frozen-table aggregation verification use four threads as a recorded exception, or should it be repeated at eight?” Technical harmlessness alone does not establish that an exception was authorized.

For Fable’s questions:

1. **Vegetation-only variant — UNRESOLVED.**  
   **Question for Chris/Andria:** “Should the snow assessment cover only vegetation changes on persistently ice-free land, or also vegetation albedo on newly exposed land after ice retreat?”

   That scientific scope should precede the companion choice. `veg_thresh` excludes structural-NA transitions from period sums. `veg_part` weights vegetation change by the **older slice’s non-ice fraction**; it does not isolate all calibration dependence of ice transitions.

   Neither should be presented as an automatically additive component of `veg_ice_thresh`. The 8–10 ka anchor demonstrates this: separately aggregated `veg_thresh + ice_thresh` is **0.006488**, versus **0.302796 W/m²** for the combined variant.

2. **Shoulder-season versus northern-spring caveat — UNRESOLVED.**  
   **Question for Andria:** “Should the paper’s snow caveat and sensitivity test include shoulder-season vegetation signals at middle latitudes, as well as northern spring and newly deglaciated land?”

   I disagree with treating latitude and month alone as evidence that snow is marginal. The script contains no snow observations. The numerical premise also needs qualification: at 0.5–2 ka, 50–60°N contributes **+0.003030 W/m²**, not negative forcing; at 0.05–0.5 ka, 40–50°N contributes substantially more cooling than 50–60°N. Small signed totals can also conceal cancellation.

3. **Legacy labels — AGREE; no scientific choice is needed to identify the run accurately.**  
   Label the record as legacy `veg_ice_thresh`, clear-sky TOA HadGEM3, legacy ice differencing and CACK 2003; identify CAM5 as clear-sky surface where its results appear.

   **DISAGREE that C4 proves the current configuration roughly halves these particular results.** C4 compares legacy CACK with legacy HadGEM3 using unweighted slice-pair summaries. It does not measure the current HadGEM3 all-sky breakdown, which also accompanies other configuration changes. A matched current-configuration run settles that quantitative claim.

Two qualifications from my original report remain: the available-month convention needs a separate scientific decision before alteration, and the helper cannot detect a slice-pair absent **everywhere** in a period. Neither is a regression caused by this extraction. Likewise, agreement between scripts 9 and 10 does not establish physical telescoping of the inherited legacy ice calculation.

For the literature points, **AGREE** that this is an algebraic repartition of the existing estimator, conditional on the retained rows and missingness pattern. **DISAGREE** if “no new method” is taken to waive the repository’s requirement to record relevant precedent and its limits.

| Reference | Reconciliation |
|---|---|
| Bright & O’Halloran (2019), *GMD* 12, 3975–3990, DOI **10.5194/gmd-12-3975-2019** | **AGREE.** Rechecked §5.1/Fig. 1: seasonal and 1° latitude summaries of **kernels**. Useful presentation precedent, with that distinction. [Publisher text](https://gmd.copernicus.org/articles/12/3975/2019/) |
| Davin & de Noblet-Ducoudré (2010), *Journal of Climate* 23, 97–112, DOI **10.1175/2009JCLI3102.1** | **AGREE with limited relevance.** The abstract/conclusions distinguish tropical, temperate and boreal responses. This supports regional interpretation, not this missing-month weighting. [Publisher text](https://journals.ametsoc.org/view/journals/clim/23/1/2009jcli3102.1.xml) |
| Betts (2000), *Nature* 408, 187–190, DOI **10.1038/35041545** | **AGREE as mechanism background.** The accessible abstract supports snow-enhanced forest/open-land albedo differences. I did not verify a monthly-band presentation precedent. [Publisher abstract](https://www.nature.com/articles/35041545) |
| Bala et al. (2007), *PNAS* 104, 6550–6555, DOI **10.1073/pnas.0608998104** | **AGREE as latitude-stratification precedent.** Rechecked descriptions of tropical, temperate and boreal deforestation experiments; they are coupled climate experiments, not this diagnostic estimator. [Article text](https://pmc.ncbi.nlm.nih.gov/articles/PMC1871823/) |
| Ghimire et al. (2014), *GRL* 41, 9087–9096, DOI **10.1002/2014GL061671** | **AGREE; Fable’s access limitation is resolved here.** HTML §2.3 describes monthly calculations; Fig. 3’s caption explicitly identifies winter, summer and annual spatial forcing. [Publisher text](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2014GL061671) |
| Strandberg et al. (2014), *Climate of the Past* 10, 661–680, DOI **10.5194/cp-10-661-2014** | **AGREE as seasonal/regional background.** The abstract distinguishes winter albedo and summer evapotranspiration responses. It does not establish this monthly forcing partition. [Publisher abstract](https://cp.copernicus.org/articles/10/661/2014/) |
| Loranty et al. (2026), *GRL* 53, e2026GL125497, DOI **10.1029/2026GL125497**, from my report | **Retain.** Rechecked §2.2: monthly forcing is averaged within 55–60°, 60–65° and 65–70°N bins. This is the most direct month/latitude presentation precedent among these references. [Publisher text](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2026GL125497) |

**AGREE** with Fable’s remaining source-level checks: unchanged kernel scaling and sign, correct default bands, harmless `include.lowest`, explicit `tidyr::` usage, suitable adding-up tolerance, and no random-stream change. The shared helper remains a sensible way to avoid duplicated rules.

This reconciliation used source inspection, hand arithmetic, Python CSV checks and HTML literature passages. **R/Rscript are unavailable**, so no R tests or pipeline runs were executed. Raw RDS/NetCDF contents and rendered figures remain unchecked; dedicated reference checkers were unavailable. No files were modified, and no prohibited files were opened.