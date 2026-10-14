# Sanity check of the methodology decisions of 2026-10-14

**What this is.** Chris took a set of methodology decisions on Andria's behalf so the
methods can be locked and a final run made (to be reviewed by Andria), and asked for an
independent sanity check of each. Outcome, as Chris then decided: B5, B8, B9, B4, C3a,
C3b, C4, C5 and A3 confirmed (with the refinements below); B1 turned into a check rather
than a correction; B2b (solar weighting) dropped, as it would double-count sunlight, and
the winter gap listed for Andria; the "central forcing from fitted means" suggestion left
to Andria; the ice-advance arithmetic error (C2) to be fixed as a code correction with a
legacy switch; B2a and the readvance smoothing (C2) left to Andria. The decisions and the
blocking questions were added at the start of the Methods section of the local write-up
(`writing/Draft manuscript (spatially complete) - CM methods comments.docx`).

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `max`,
2026-10-14, from the code of branch i04b-settings b88813b, both question files and README
inlined (no shell available to the model).

---

Most decisions are defensible, but I would **replace B2b, qualify B1 and B4, and resolve the uncertainty claim before the final run**. C2 also contains a directional error: changing the NA handling alone will not make ice advances work.

This is a review of the supplied code and published documentation, not a numerical rerun. Proposed configuration keys below sit under `analysis`; “legacy” means the supplied code’s behaviour unless explicitly distinguished from Andria’s original analysis.

1. **B5 — Land-average calibration albedo: retain the intent, qualify the implementation.**

   Matching the spatial support of response and predictors is sound. MOD44W is an appropriate water-mask product. However, a 250 m mask **cannot recover land-only albedo from an already mixed 0.25° albedo value**. Ideally, mask the source MODIS observations before aggregation. Otherwise, describe the result as an approximation and examine sensitivity to mixed pixels. Mask Hudson Bay and coastal water as well as inland lakes. [NASA’s MOD44W description](https://modis.gsfc.nasa.gov/data/dataprod/mod44w.php).

   **Implementation:** `calibration$albedo_support = "land_mean"`; legacy `"centre_pixel"`. Record the water-mask collection, year/composite and source resolution. Replace the unmasked averaging at `scripts/2_calibration_lct_bluesky.R:63` and write the chosen response to `calibration_table` at `:144–148`; merely selecting the existing coarse table does not implement land-only averaging.

   **Verify:** use physical land-area weights, report valid observed land coverage by month, and inspect Great Lakes, Hudson Bay and coastal cells. B5 changes the response—and therefore the fitted elevation, spatial and cover effects—not just measurement precision.

2. **B8 — Excluding modern ice-cap cells: sound, but the 1 ka mask is a proxy.**

   A vegetation–albedo calibration should exclude observations dominated by glacier ice when their predictors describe vegetation. But “48 present-day ice-cap cells” overstates the evidence: these are **48 cell centres inside 1 ka polygons**, not necessarily 48 wholly glaciated cells during 2000–2009. Other cells can contain substantial ice despite having an ice-free centre.

   **Implementation:** `calibration$ice_mask = "centre_in_1ka_polygon"`; legacy `"none"`. Filter the calibration rows around `scripts/2_calibration_lct_bluesky.R:144–147`, while retaining those cells in the paleo prediction domain. Do not remove them from script 1’s entire time series.

   **Verify:** map exclusions against contemporary glacier/satellite evidence; report retained observations by month and residual ice contamination in mixed cells. Retain the exclusion principle; have Andria review the proxy mask, particularly because it further reduces northern calibration support.

3. **B9 — Land-average elevation: retain.**

   This is a sensible support-matching correction alongside B5. Replacing offshore elevations with zero would not solve the problem; genuine below-sea-level land should also remain possible. A mean elevation is still a simplified predictor of heterogeneous terrain, and holding it fixed retains the modern-topography assumption.

   **Implementation:** `elevation$method = "land_mean"`; legacy `"centre_point"`. Replace the lookup at `scripts/1_veg_lct_prep.R:56–73` with an area-weighted DEM summary over the agreed non-water footprint. Use it consistently in modern and paleo tables. Keep the original point cache separately; identify the new cache by DEM, mask and grid versions.

   **Verify:** coastal and mountain-cell comparisons, DEM coverage, units and vertical datum. Record resolution and whether the DEM represents terrain or an ice surface.

4. **B1 — Closing cover fractions: retain as validation; the stated correction appears stale.**

   The current script does not drop taxa after normalization. It reads an already aggregated three-class posterior. Means already close if the input draws close, and I04a explicitly closes medians (`R/preparation.R:11–30`). Therefore, first establish whether any substantive closure problem remains.

   Closure is appropriate for exhaustive fractions of the same land surface. It is not appropriate to silently redistribute a genuine omitted class. Nor does closure remove the rank deficiency of a three-dimensional smooth fitted on a two-dimensional simplex.

   **Implementation:** an additional `land_cover$close_mean = TRUE`, legacy `FALSE`, applied in preparation before the modern/paleo split (`scripts/1_veg_lct_prep.R:52,81`). Keep I04a’s median closure regardless; avoid closing the same medians again.

   **Verify:** input draw completeness, finite non-negative fractions, sums and materially changed rows. Unexpected deficits require investigation. Leave any replacement of the cover smooth to Andria; B1 alone does not settle that model choice.

5. **B4 — ML comparison followed by REML prediction: reasonable, but correct the rationale.**

   Comparing **REML scores** across different unpenalized structures is problematic. However, `AIC()` here uses mgcv’s conditional likelihood criterion and effective degrees of freedom; it is not simply comparing REML scores. The existing AIC table is therefore not automatically invalid merely because smoothing parameters were estimated by REML. ML refits remain a defensible comparison strategy. [mgcv’s AIC documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/logLik.gam.html).

   Three corrections matter:

   - AIC can legitimately increase with complexity; monotonic improvement is not a validity requirement.
   - The entire ladder is not nested. Model 6 already uses the default thin-plate basis; model 8 increases its `k`, while model 7 changes basis (`R/model_registry.R:25–32`).
   - ML does not repair the compositional rank deficiency. Conversely, handled rank deficiency does not automatically make AIC meaningless: inspect the estimable rank, convergence and effective degrees of freedom.

   **Implementation:** `calibration$comparison_method = "ML"` versus legacy `"REML"`; `prediction_method = "REML"` in both. Add a method argument at `R/model_registry.R:67–73`, save comparison fits separately, and refit whichever model is actually selected with REML. Otherwise, the special overwrite of mod8 at `scripts/4_calibration_model.R:85–91` can leave another selected model fitted by ML.

   **Verify:** identical observations within each monthly comparison, unrounded AIC, convergence, rank and basis adequacy. Do not treat the omnibus ladder `anova.gam()` at `:64` as a sequence of valid nested tests. [mgcv’s comparison guidance](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/anova.gam.html). Andria must distinguish “AIC supports a common mod8” from automatic month-specific selection; `config.R:42` currently specifies a fixed model.

6. **C3a — TOA flux throughout: retain.**

   TOA net flux is the appropriate quantity for this planetary-energy comparison. CAM5 `FSNTC` means clear-sky net solar flux at the model top; `FSNSC` is the surface counterpart. **With C4 enabled, the headline variable must be `FSNT`, not `FSNTC`.** [CAM field definitions](https://www2.cesm.ucar.edu/models/cesm1.2/cam/docs/ug5_3/hist_flds_fv_cam5.html).

   **Implementation:** `kernels$cam5_level = "toa"`; legacy `"surface"`. Combine this with the sky setting to select `FSNT`, `FSNTC`, `FSNS` or `FSNSC` at `scripts/8_radiative.R:87–89`.

   **Verify:** assert the chosen variable exists and inspect its metadata. Preserve the percentage-point conversion: CAM5’s albedo kernel represents an additive 0.01 albedo perturbation. A positive albedo change should produce negative net-downward forcing. [Pendergrass et al., 2018](https://essd.copernicus.org/articles/10/317/2018/).

7. **C3b — CACK climatology: retain.**

   The published supplement explicitly names **`CACK CM`**. It is calculated using 2001–2016 climatological radiation inputs; because the calculation is nonlinear, it need not equal the arithmetic average of annual `CACK` values. It removes dependence on an arbitrary satellite year, but remains a modern reference-state kernel. [Bright and O’Halloran, Table S3](https://gmd.copernicus.org/articles/12/3975/2019/gmd-12-3975-2019-supplement.pdf).

   **Implementation:** `kernels$cack_mode = "climatology"`; legacy `"year"` with existing `cack_year = 2003`. Change `scripts/8_radiative.R:97–107`. The climatology has no year-selection operation: do not retain the four-dimensional `level=month, band=year` recipe blindly.

   **Verify:** actual NetCDF variable names, dimension order, month indexing and coordinates. Compare extracted values with direct array lookups at known locations. Preserve the per-unit-albedo conversion and sign reversal at `:140–150`.

8. **C4 — All-sky headline kernels: retain strongly.**

   All-sky kernels include cloud masking and are appropriate for the actual planetary radiative effect. Use HadGEM3 `albedo_sw`, CAM5 `FSNT`, and all-sky CACK. Clear-sky calculations remain useful sensitivity experiments. [Smith et al., 2020](https://essd.copernicus.org/articles/12/2157/2020/).

   **Implementation:** `kernels$hadgem_sky = "all"` and `cam5_sky = "all"`; legacy `"clear"` for both. CACK remains all-sky in either configuration. Change `scripts/8_radiative.R:47,87` and generate figure labels from the settings; `scripts/9_forcing_barplot.R:287–291` currently hard-codes obsolete descriptions.

   **Verify:** compare clear versus all sky **within each model**, holding everything else fixed. The old HadGEM–CACK ratio also confounds model and climatology differences; the HadGEM–CAM5 comparison additionally mixed TOA and surface fluxes.

   This does not reconstruct Holocene clouds, atmospheric transmission or orbital insolation. Blue-sky surface albedo and all-sky TOA kernels are compatible; they do not inherently double-count clouds.

9. **C5 — Global-equivalent comparison: retain, with a qualified interpretation.**

   The correct normalization is the affected area’s flux anomaly divided by Earth’s total area. For standardized kernels \(K\) in net-downward W m⁻² per unit albedo:

   \[
   F_{\mathrm{global}}=
   \frac{1}{A_{\mathrm{Earth}}}
   \sum_i A_i
   \frac{\sum_m d_m K_{im}\Delta\alpha_{im}}{\sum_m d_m}.
   \]

   Here \(A_i\) must match the surface represented by \(\Delta\alpha\). Current areas are **whole raster-cell areas** (`scripts/7a_alb_diff_full.R:119–124`). After B5, either convert land-only changes to whole-cell changes or use corresponding land areas—exactly once. Ice fractions must use the same denominator.

   This produces comparable spatial units, **not full equivalence to IPCC ERF**. The kernel calculation diagnoses a direct albedo radiative effect under a fixed atmosphere; AR6 ERF includes adjustments. Different reference periods and forcing patterns also preclude inferring equal temperature effects from equal numbers. [IPCC AR6, Chapter 7](https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-7/).

   **Implementation:** `forcing$aggregation = "global_equivalent"` at `scripts/9_forcing_barplot.R:164–173,231`. This is already the supplied branch’s headline behaviour. An Andria-original legacy setting must select the complete `"legacy_egu2024"` recipe, including its unweighted domain mean; changing only the denominator is insufficient.

   **Verify:** a uniform 1 W m⁻² perturbation integrates to affected area/Earth area; report valid area separately from nominal area. Label results as endpoint changes, not time-integrated forcing or rates.

10. **A3 — 1,000 draws: retain for numerical precision, not as an uncertainty correction.**

    Increasing independent draws from 100 to 1,000 reduces Monte Carlo standard errors by about \(\sqrt{10}\). It does not change what is sampled: the shown `simulate()` call generates conditional beta responses, holding the fitted relationship fixed. [gratia’s simulation documentation](https://gavinsimpson.github.io/gratia/reference/simulate.html).

    Two framing corrections are essential. Beta response variance is \(\mu(1-\mu)/(\phi+1)\), so observation scatter is not literally constant everywhere. Also, script 7a reads **summary means**, not draws (`scripts/7a_alb_diff_full.R:39`), so increasing draws does not create forcing uncertainty intervals.

    **Implementation:** existing `draws$prediction = 1000`, legacy `100`; change `evaluation` separately if intended (`config.R:51`; `scripts/6_prediction_model.R:53–76`). Record a fixed seed for the final run.

    Prefer deterministic fitted means for central forcing: they already exist at `:44–50`, whereas the downstream summary uses a noisy simulation mean at `:71`. Make this a separate option, such as `draws$mean_source = "fitted"`, legacy `"simulation"`.

    **Verify:** simulated means against fitted means and beta quantiles; check memory before expanding the long table to roughly 70 million rows per month. **A2 belongs on Andria’s review list:** coefficient uncertainty requires coefficient draws, not more response draws. [gratia’s fitted-sample documentation](https://gavinsimpson.github.io/gratia/reference/fitted_samples.html).

11. **B2b — Solar-weighting annual forcing: reconsider; do not implement as stated.**

    Monthly radiative kernels already contain the effect of incoming sunlight. Multiplying their resulting monthly forcing by another solar weight counts the seasonal illumination effect again. This applies to clear-sky kernels too; C4 does not cause the problem.

    Use a time average of monthly fluxes, as in the equation above, with an explicit calendar. Equal-month averaging is an approximation; month-duration weighting is the defensible refinement.

    The missing observations also extend beyond true polar night: December at 60°N still has sunlight. MODIS warns about high solar zenith angles, so retrieval gaps cannot all be treated as physically zero forcing. [MODIS albedo validation guidance](https://modis-land.gsfc.nasa.gov/ValStatus.php?ProductID=MOD43).

    **Implementation:** replace the proposed solar option with `forcing$annual_weighting = "month_length"`; legacy `"equal_months"`, at `scripts/9_forcing_barplot.R:169`. Separately distinguish verified zero-insolation kernel values from genuinely missing values. The current `na.rm=TRUE` silently averages CACK over fewer months.

    **Verify:** retain dark months as zero contributions where physically justified; do not renormalize onto illuminated months. Require twelve months for an annual headline. Quantify contributions from unsupported-but-illuminated cell-months and report a sensitivity bound rather than assuming they are negligible.

12. **B2a — Projected spatial coordinates: recommend to Andria, with a geometry correction.**

    Using a suitable projected coordinate system is preferable to treating latitude and longitude degrees as isotropic distances. However, **equal-area does not mean distance-preserving**. Albers is a reasonable regional choice, not an exact solution to distance distortion. The GP explicitly depends on distances. [mgcv’s GP documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.construct.gp.smooth.spec.html).

    **Implementation:** `calibration$spatial_coordinates = "projected"` plus an explicit CRS; legacy `"lonlat"`. Modify the spatial terms in `R/model_registry.R:18–48` and create identical model-coordinate columns for fitting and prediction.

    Keep geographic `x,y` unchanged for raster extraction. Prefer separate model-input frames: adding projected columns indiscriminately would also make `melt()` treat them as simulation columns at `scripts/6_prediction_model.R:61–62`.

    **Verify:** projection round trips, projected versus geodesic distances across the domain, consistent prediction coordinates, and changed cover responses/forcing. Settle this before the expensive model comparison.

13. **C2 — Readvances: recommend retaining supported events; fix the arithmetic regardless.**

    A non-monotone ice history is not intrinsically erroneous, but a readvance in an interpolated raster is not automatically real either. Inspect the chronology and interpolation before imposing monotonic retreat. The current neighbour averaging is neither a documented reconstruction method nor a reliable monotonic constraint (`scripts/7a_alb_diff_full.R:254–261`).

    The threshold calculation handles retreat but misses advance: when young is ice and old is vegetation, both components become NA and their sum becomes zero (`:297–315`). Merely replacing `na.rm=TRUE` with `FALSE` would also break legitimate vegetation-only cases.

    **Implementation:** `ice$readvances = "retain"`; legacy `"legacy_neighbour_average"`. Separately, `ice$difference_method = "endpoint"`; legacy `"legacy_components"`. After choosing the ice history, construct each endpoint:

    \[
    \alpha_{\mathrm{threshold}}=
    \begin{cases}
    \alpha_{\mathrm{ice}},&f_{\mathrm{ice}}>0.5\\
    \alpha_{\mathrm{veg}},&f_{\mathrm{ice}}\leq0.5
    \end{cases}
    \qquad
    \Delta\alpha=\alpha_{\mathrm{young}}-\alpha_{\mathrm{old}}.
    \]

    For fractional mixing, difference \((1-f)\alpha_{\mathrm{veg}}+f\alpha_{\mathrm{ice}}\) directly. Preserve missing required inputs. Recompute from the selected ice history; existing threshold fields were built before the adjustment.

    **Verify:** retreat, advance, unchanged cover, persistent ice, first-pair events and missing endpoints. Reversing endpoints must reverse the sign; summed total changes must equal the endpoint difference. The chronology choice belongs with Andria; the directional and missing-value errors are code corrections.

The decisions interact in four consequential ways:

- **B5/B8/B9/C5 require one surface definition.** Agree whether the analysis covers a fixed modern land footprint. If it includes deglaciation over present-day Hudson Bay or lakes, ice-to-water transitions need an explicit water state; a vegetation prediction cannot represent them. A MOD44W mask is also not a Holocene lake/coastline reconstruction.
- **B1/B2a/B4 depend on the final calibration inputs.** Cell averaging, ice exclusions and elevation changes can all change model rankings. B3’s cancellation argument is only valid on the link scale: \(\operatorname{logit}^{-1}(b+g_y)-\operatorname{logit}^{-1}(b+g_o)\) depends on the spatial/elevation contribution \(b\). These terms therefore affect response-scale changes even when held constant through time. Inspect spatial confounding and cover-response sensitivity; AIC alone does not validate temporal transfer.
- **A3 cannot substitute for uncertainty propagation.** For coefficient-based forcing intervals, each draw must use a common coefficient vector across cells and ages before differencing and aggregation. Land-cover draws additionally require confirmation of their joint structure. Kernel spread is a sensitivity result, not automatically a confidence interval.
- **C2/C5 require genuine telescoping and consistent coverage.** Summing consecutive changes is appropriate for fixed monthly kernels, without weighting by interval length. Check against direct endpoint differences. At `scripts/9_forcing_barplot.R:142–156`, derive expected pairs from configured ages, not merely observed rows; reject pairs crossing reporting boundaries. Report the oldest endpoint as 11.5 ka. Distinguish structural zero contributions from unknown values in the sensitivity variants.

For configuration and legacy reproduction, use simple named choices and short explicit branches. Add each key to the schema and manifest; retain separate output directories and provenance for new masks, DEMs and caches. `legacy_config.R` must be a complete literal `list(...)`, not a script that sources and modifies another configuration (`config.R:3–6`).

**“Exactly legacy” needs a named reference.** The frozen current pipeline and Andria’s EGU recipe are different analyses. Moreover, the purported recovered recipe still inherits the current trim and kernel values through its join (`scripts/9_forcing_barplot.R:323–328`). Exact historical reproduction requires the historical inputs, domain, kernels and aggregation—not just switches. Identical stochastic outputs also require the original RNG state and software environment; `"random"` cannot guarantee them. Keep known erroneous behaviour accessible only as clearly labelled compatibility behaviour.

I would implement in this order:

1. Freeze the legacy reference and settle the estimand, uncertainty claim, spatial-coordinate choice and ice-history policy.
2. Implement and check kernel selection/indexing, endpoint differencing, missing-value rules and annual aggregation on small synthetic cases.
3. Build the shared surface masks and cell summaries; implement B5, B8, B9, closure validation and B2a.
4. Fit the final-input ML comparison, inspect rank and diagnostics, then save the selected REML fits.
5. Generate predictions, differences and forcing; aggregate with consistent surface areas and month coverage.
6. Compare the legacy reproduction and targeted sensitivities, then update manuscript claims, figure labels and provenance.

A revised **“Decisions taken (to be reviewed by Andria)”** list could read as follows, after implementation and verification:

- **B5:** Calibration uses mean albedo over the land portion of each 1° cell to match the scale of the land-cover reconstruction. Water is excluded before aggregation wherever the source resolution permits.
- **B8:** Cells identified by the 1 ka ice mask are excluded from calibration to avoid attributing glacier albedo to vegetation. This mask is treated as a proxy for persistent recent ice.
- **B9:** Elevation is averaged over each cell’s land area, replacing the centre-point value and avoiding offshore seabed measurements.
- **B1:** Cover fractions are checked for completeness and closure before calibration and prediction. Median summaries retain their existing normalization; any additional corrections are documented.
- **B4:** Candidate models are compared after ML fitting, and the model used for prediction is fitted by REML. Model rank and fit diagnostics are checked before selection.
- **C3a:** All reported kernels describe changes in net shortwave radiation at the top of the atmosphere, ensuring a consistent flux definition.
- **C3b:** CACK uses its published monthly climatology based on 2001–2016 inputs instead of the single year 2003.
- **C4:** All-sky kernels provide the headline estimates because clouds affect how surface-albedo changes alter Earth’s radiation balance. Clear-sky calculations are retained as sensitivities.
- **C5:** Regional albedo effects are expressed as global-equivalent flux changes for comparison with IPCC global means. The comparison distinguishes the direct albedo effect from IPCC effective radiative forcing.
- **A3:** Conditional response simulations are increased to 1,000 to reduce Monte Carlo noise. These simulations do not, by themselves, represent uncertainty in the fitted relationship or land-cover reconstruction.
- **B2b, revised:** Annual forcing is averaged over time using month durations, because monthly kernels already account for sunlight. Winter extrapolation and missing kernel values are assessed separately.

The two pending choices should remain explicitly labelled:

- **B2a — For Andria:** Fit the spatial smooth using projected coordinates, while retaining the existing 1° reconstruction grid.
- **C2 — For Andria:** Retain readvances supported by the ice reconstruction. Calculate changes from complete endpoint states so that both ice advance and retreat are represented correctly.