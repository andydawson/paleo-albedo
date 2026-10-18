# Vegetation and ice sensitivity variants, and coastal cell areas: code check by Astra (Codex gpt-6-astra, maximum effort)

_18 October 2026, at Chris's request: the two code problems Opus found in its normalisation review (findings 2 and 4 of `2026-10-18_forcing_normalisation_review_opus.md`), put to Astra to establish whether they are bugs and how large. Code: `feature/single-config` at 4ba4d2a and the original code on `legacy`; numbers from the saved tables of the 18 October full run (no R was run). Both confirmed. Not fixed: each fix needs a choice of definition from Chris or Andria (questions at the end)._

**Verdict.** The component comparison contains a confirmed bug; the coastal claim needs a decision because its premise that the calibration albedo excludes ocean is false.

**Benefit.** This review prevents the paper from misattributing deglaciation forcing and applying an area correction to an insufficiently defined albedo quantity.

I independently decoded the run’s per-cell `RF_holocene_all_cases.RDS` without running R. Reaggregating its 764,640 rows reproduced both forcing-summary CSVs, with a maximum numerical difference of \(1.6\times10^{-14}\).

**1. Vegetation and ice variants — verdict: bug in the period aggregation and sensitivity comparison; the current headline total is unaffected.**

**Should-fix.** [R/differences.R:132](/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/R/differences.R:132) assigns each known pair’s change to vegetation or ice, with NA in the other component. [scripts/9_forcing_barplot.R:151](/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/scripts/9_forcing_barplot.R:151) then requires every pair to be numeric. A cell with both kinds of pair disappears from both component totals.

The mechanism claimed in the packet is correct.

**Hand check.** Three slices are the minimum needed to expose this failure. Consider one month, two equal-area cells, and a kernel of −1 W/m² per percentage-point albedo change. Cell A retreats from ice to vegetation, then its vegetation brightens:

| Cell A, older → younger | Albedo change | Vegetation forcing | Ice forcing | Total forcing |
|---|---:|---:|---:|---:|
| Ice 0.60 → vegetation 0.20 | −0.40 | NA | +40 | +40 |
| Vegetation 0.20 → vegetation 0.30 | +0.10 | −10 | NA | −10 |
| Period sum in current code | −0.30 | NA | NA | +30 |

Cell B remains vegetation at albedo 0.20 and contributes zero. The two global-equivalent component bars become zero, although the total is \(30A/A_{\rm Earth}\). The total telescopes correctly; the components disappear.

The actual 8–10 ka HadGEM3 results confirm this:

| Cells retained by aggregation | Cell count | Valid area, m² | Global equivalent, W/m² |
|---|---:|---:|---:|
| `veg_thresh` | 2,027 | \(1.54269\times10^{13}\) | −0.036402 |
| `ice_thresh` | 285 | \(1.69066\times10^{12}\) | +0.038489 |
| Excluded from both | **343** | \(2.10630\times10^{12}\) | **+0.182684** |
| `veg_ice_thresh` total | 2,655 | \(1.92238\times10^{13}\) | +0.184771 |

Thus the omitted cells supply **98.87% of the net total**. The near cancellation of the two displayed components is misleading.

The denominator claim is also correct. The ice domain mean, 11.61287 W/m², describes \(1.69066\times10^{12}\) m². Script 9 nevertheless reports `area_m2 = 1.92238×10¹³`, because line 157 includes cells whose forcing is missing. The weighted mean is a conditional mean over the retained subset; its reported area does not identify that subset.

**Effect on the paper.** The current configuration selects `veg_ice_thresh`, calculated independently of these component sums. This defect affects `forcing_by_period_variant_sensitivity.csv` and `forcing_barplot_variant_sensitivity`; it does **not** remove the 343 cells from `forcing_by_period.csv`, the headline total, or its latitude/month breakdown. I did not inspect manuscript wording.

**Origin.** Andria’s original `legacy:scripts/7a_alb_diff_full.R:512–529` already used the complementary NAs. The hand case above behaves the same under `legacy_components`. However, `legacy` contains no script 9. The problematic period aggregation was introduced with script 9 in commit `e0ad29b`, dated 2026-09-21, **before the single-config migration**. I16’s `endpoint` method did not introduce this failure.

**Definition and precedent.** The comments describe pair-type components; C1 and C5 leave their intended scientific use open. They do not establish these masked subsets as alternative estimates of the same continental quantity.

Two simple, distinct definitions are available:

- **Additive pair-type contributions:** give a known non-applicable component zero, while retaining NA for genuinely unknown inputs. Sum both on the same domain. Vegetation + ice then equals total.
- **Period-cell classes:** retain the existing two subsets and explicitly add cells containing both pair types. Label them “only vegetation pairs”, “only ice-involving pairs”, and “both pair types”. Their global contributions sum to total; their separately normalised domain means do not.

The latter is an accounting classification, not a physical separation of vegetation and ice effects. I found no published precedent for this exact binary pair-classification rule. Chris should choose the intended interpretation before implementation.

**2. Coastal weighting — verdict: design choice needing a decision; whole-cell weighting is confirmed, but a standalone land-area bug is not established by these data.**

**Should-fix before interpreting the result as land-only forcing.** [scripts/7a_alb_diff_full.R:95](/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/scripts/7a_alb_diff_full.R:95) measures complete raster-cell polygons. There is no coastal clipping or land-fraction multiplier. The identical operation appears at `legacy:scripts/7a_alb_diff_full.R:218`.

But the packet’s assertion that the original albedo raster has ocean as NA is contradicted by the file:

- At 85°W, 58°N in Hudson Bay, January albedo is **0.719226**, and August is **0.009315**.
- At 60°W, 45°N, August is **0.008212**.
- The TIFF has the same Git blob on `legacy` and the current commit.

These are numeric water/ice values, not NA.

Moreover, [scripts/2_calibration_lct_bluesky.R:143](/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/scripts/2_calibration_lct_bluesky.R:143) samples the **native 0.25° centre pixel**. The separately calculated 1° average is not used for fitting. At 79.5°W, 54.5°N, I verified that the saved calibration table exactly matches the TIFF: July **0.0711319**, August **0.0736594**. That 1° cell’s estimated non-ocean share is only **0.3343**.

Consequently, the fitted response is neither an established land-only mean nor a measured whole-cell mean. Scripts 1 and 6 supply three terrestrial cover fractions summing to one, with no explicit ocean fraction or ocean-albedo mixture. B5 already identifies the spatial-support mismatch.

The comment at script 2:85—“crop albedo (I think this removes the ocean grid cells? not sure)”—records uncertainty, not a decision or an implemented land mask.

**Hand check, conditional on land-only albedo.** If a coastal cell has area \(A\), land fraction 0.25 and land forcing +10 W/m², its contribution is \(2.5A\) W. Using its full area gives \(10A\) W: four times too much. With an equal-area inland cell contributing zero, the land-domain mean is \(2.5/1.25=2\) W/m²; full-cell weighting gives 5 W/m².

That proves the error **if the forcing is land-only**. Applying the land fraction again to an already mixed whole-cell change would instead double-count the dilution.

**Measured size of an area-only sensitivity.** I joined the actual 2,655 forcing cells to `data/elev_land_mean_cells.csv` and multiplied their stored areas by `non_ocean_share`, holding every forcing value fixed.

| Quantity | Current weights | Non-ocean proxy weights |
|---|---:|---:|
| Domain area, m² | \(1.92238\times10^{13}\) | \(1.85183\times10^{13}\) |
| 8–10 ka HadGEM3 global equivalent | 0.184771 | 0.177867 |
| 8–10 ka HadGEM3 domain mean | 4.902867 | 4.899455 |
| 10–12 ka HadGEM3 global equivalent | 0.161084 | 0.153954 |

The proxy identifies **3.67% of the present weighted area as sea**: 543 cells have some estimated sea area, including 56 with more than half. Examples include 79.5°W, 51.5°N, with non-ocean share 0.4501, and 92.5°W, 57.5°N, with 0.4374.

For 8–10 ka, this reweighting lowers the global equivalent by **3.74%**, but the domain mean by only **0.07%**. The packet’s 50–60°N contribution is confirmed at **73.4%** of the net bar. Neither fact proves a corresponding percentage bias in the final scientifically corrected result.

**These are sensitivity results, not corrected estimates.** The proxy treats zero-elevation DEM pixels as ocean and retains lakes; its limitations are documented in `tools/prepare_land_elevation.R`. Recalibrating a properly defined land-only response could also change the forcing values themselves. Direction is not universal: this same reweighting increases the positive 2–4 and 6–8 ka totals.

**Effect and origin.** Any justified area correction would affect current area-weighted headline tables, kernel comparisons, sensitivity bars and latitude breakdowns. It would not directly change the stored per-cell W/m² values. The recovered EGU recipe’s unweighted mean does not use these area weights. The full-cell area assignment and centre-pixel calibration both predate the migration.

**Published practice.** The surface represented by the albedo change determines the appropriate area:

- Bright, R. M., Bogren, W., Bernier, P. & Astrup, R. (2016), *Carbon-equivalent metrics for albedo changes in land management contexts: relevance of the time dimension*, **Ecological Applications 26**, 1868–1880, DOI **10.1890/15-1597.1**. Equation 6 defines area as the surface over which the albedo change occurs. [Supporting passage](https://esajournals.onlinelibrary.wiley.com/doi/10.1890/15-1597.1).
- Zhang, R., Wang, H., Fu, Q., Rasch, P. J. & Wang, X. (2019), *Unraveling driving forces explaining significant reduction in satellite-inferred Arctic surface albedo since the 1980s*, **PNAS 116**, 23947–23953, DOI **10.1073/pnas.1915258116**. Equation 1 explicitly weights land, ocean and ice albedos by their surface fractions. This supports fractional surface accounting, not the pipeline’s particular vegetation/ice classification. [Equation and definitions](https://pmc.ncbi.nlm.nih.gov/articles/PMC6883849/).

**What verification misses.**

Legacy byte identity preserves both inherited conventions. [tests/test_differences.R:107](/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/tests/test_differences.R:107) checks telescoping of the total, but not component closure after script 9.

The settling checks should assert:

- The three-slice hand case survives period aggregation; chosen contributions close to total, with unknown inputs remaining unknown.
- Every conditional mean reports its actual valid area. Additive domain contributions use a common denominator.
- Under a defined land-only response, land fractions 1, 0.25 and 0 give unchanged, quarter and zero integrated forcing. Zero albedo change gives zero forcing.
- The calibration target and weighting area represent the same surface, verified against coastal pixels and the raster-construction provenance.

Both audit calculations held the saved stochastic predictions fixed. The run uses 100 prediction draws and recorded base seed 1549466984. Seed-1 anchors establish reproducibility, and historical tolerances describe stochastic variation; neither validates these scientific definitions.

**Questions for Chris or Andria.**

1. **C1/C5:** Are the component figures intended as additive pair-type contributions or as three explicit period-cell classes? Meanwhile, the code excludes 343 mixed-history cells from both component bars.
2. **B5/C5:** Should the response represent land-only albedo or whole-cell land/water albedo? The former requires a defensible land-only calibration target and matching land area; the latter requires a defined surface mixture and full-cell area. Meanwhile, the code combines centre-pixel responses, terrestrial cover fractions and full-cell weights. The existing elevation decision does not settle this forcing definition.

**Not checked.** No R execution, model refitting, validated coastline overlay, palaeoshoreline reconstruction, or independent kernel-file audit. No manuscript or reviewer reports were opened. Raster-construction provenance remains unresolved. Questions are left here for the main session to record; no files were changed.

Files read: AGENTS/CONTRIBUTING/README; configs; scripts 1/2/4/6/7a/8/9 and relevant legacy/history versions; referenced R helpers, two tests and two verification specs; regression contract and question files; run config/manifests, three forcing CSVs, forcing/calibration/modern-cover RDS files, elevation CSV and albedo TIFF. Commands/tools: read-only git, pwd/cat/nl/sed/rg/head/tail/type, Python standard-library decoding/arithmetic and failed availability probes, web search/open/find; no R.