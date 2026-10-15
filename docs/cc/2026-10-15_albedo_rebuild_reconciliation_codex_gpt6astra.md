# Albedo raster rebuild: reconciliation, Astra's pass

One reconciliation pass: Astra read Fable's design and conceded, held or proposed middles. Unedited.

---

I recommend keeping **AppEEARS native-grid subsets as the first route**, subject to comparison with original HDF files. I concede Fable’s case for using **full and magnitude inversions together**, with full-only results alongside. I retain **monthly means calculated separately by year, followed by equal weighting of years**. Neither twenty dates pooled across a decade nor 200 land pixels establishes adequate coverage.

Both designs already recommend a 70° local-noon solar-zenith limit. Its consequences need correcting: it also removes some April and September observations in the far north.

Fable’s GDAL and mask-offset claims are confirmed. The legacy raster contains values at lake and ocean locations; its Hudson Bay seasonal pattern is consistent with sea ice. The TIFF alone cannot establish the original retrieval classifications.

The merged recipe below distinguishes published methods, official product specifications and project judgements. The precise coverage thresholds, historical shoreline treatment and adequacy of noon albedo for the paper remain decisions for Chris.

I read [both designs](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/rebuild_design/astra.md) [in full](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/rebuild_design/fable.md). Nothing was changed, and no prohibited files were opened. Supporting passages were checked in public HTML. **The formal reference-checker requirement remains incomplete:** refaudit returned `UNVERIFIED`, and RefChecker’s DOI lookup failed because the sandbox could not resolve Crossref. The bibliographic checks below use publisher, author-institution and NASA records; they are not a claimed two-checker pass.

The factual checks give these results.

| Claim | Finding and consequence |
|---|---|
| **GDAL lacks HDF4 and netCDF** — Fable line 71 | **Confirmed for the environment’s GDAL library.** The requested R command failed before execution because R could not create its temporary file. Direct, read-only inspection of that environment’s `libgdal.so` returned GDAL **3.13.3**, **190 registered drivers**, and no HDF4, HDF5 or netCDF driver. Full-HDF processing therefore needs a driver fix. Installed `ncdf4` provides a separate R route for ERA5 NetCDF files. |
| **The legacy raster contains water values** — line 42 | **Confirmed geographically.** At Lake Superior, −87.5°, 47.5°, January/July/August are **0.021443 / 0.007843 / 0.007331**. At Hudson Bay, −85°, 60°, they are **0.744982 / 0.077772 / 0.007736**. MOD44W marks both locations as water; Hudson Bay has ocean-mask QA. The winter brightness is consistent with ice, but the TIFF does not prove its source snow flag or shallow-ocean classification. |
| **MOD44W has an odd crop offset** — line 57 | **Confirmed.** Its upper-left corner is **7,447 columns and 5,279 rows** from the standard MODIS global upper-left corner, at 231.6563582625 m spacing. The 250 m pixels are aligned, but the crop begins partway through 500 m blocks. Construct four-pixel groups using global grid indices. |
| **No NASA subsetting service exists for MCD43A3** — line 29 | **Incorrect.** AppEEARS explicitly lists **MCD43A2.061 and MCD43A3.061** in its cloud-migration changelog. Fable itself lists AppEEARS as route B. Its narrower observation about absent CMR service flags cannot establish that AppEEARS is unavailable. [NASA AppEEARS changelog](https://appeears.earthdatacloud.nasa.gov/changelog). |

I also reproduced Fable’s January, July and December valid-pixel counts. However, the **whole raster** reaches 64.75°N in January and 59.5°N in December. The calibration table has different spatial coverage. Neither boundary, nor small northern granule sizes, proves that the legacy processing applied no additional zenith filter.

The following is my proposed reconciliation, in processing order.

1. **Products, period and domain — hold the products and conservative start; narrow the inventory.**

   Use MCD43A3/A2 Collection 6.1 and the supplied shortwave broadband. Retain **5 March 2000–31 December 2009** for the primary recipe. The product-team introduction identifies validated coverage from 5 March. Earlier availability, including Fable’s 24 February date, is a separate question from the primary inclusion date. Inventory earlier files, but do not infer target dates from the beginning of their 16-day acquisition windows. January and February consequently have at most nine contributing years. [MODIS introduction](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/introduction/).

   Accept Fable’s tighter **actual-cell footprint** in principle. Its 38 tiles and my 56 bounding-box tiles describe different scopes; I have not independently reproduced the 38-tile intersection. Recalculate it from densified target polygons. A quarter-degree comparison raster covering only those cells must identify the unprocessed remainder of the legacy rectangle.

   **Basis:** published MODIS specifications support the products, broadband and date convention. Choosing the validated start and the study footprint is a **project judgement**.

2. **Acquisition — hold AppEEARS first, with full HDF as reference and fallback.**

   Request native-projection GeoTIFF subsets in bounded batches. Verify grid boundaries, decoded albedo values, QA, fills, dates and completeness against original A3/A2 HDF files. Include tile boundaries and entirely missing dates: the existing AppEEARS delivery README says all-fill outputs can be omitted.

   Native projection is an available API option; collection-specific fidelity and useful batch sizes remain untested. The odd MOD44W crop offset is something to handle explicitly, not evidence that its native pixels were resampled. [AppEEARS API](https://appeears.earthdatacloud.nasa.gov/api/).

   Keep these planning distinctions:

   | Route | Reconciled assessment |
   |---|---|
   | Full A3/A2 HDF | Approximately **10 TB** under Fable’s narrower inventory; approximately **13 TB** under the brief’s broader inventory. These are estimates requiring a final granule inventory. |
   | AppEEARS subsets | Transfer volume remains **unmeasured**. Neither design’s compression estimate should become a promised saving. Measure the pilot. |
   | Harmony/OPeNDAP | No usable collection-specific route was established here. Capability requests were inaccessible; that is not proof of absence. |
   | MCD43C3 | Keep as an optional comparison. Its coastal averages can contain shallow-water inputs, which cannot subsequently be separated with MOD44W. [C3 guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43c3-cmg-albedo-product/). |

   **Basis:** acquisition is an **engineering choice**, supported by service documentation. Published scientific precedent is not needed to choose between byte-equivalent delivery routes.

3. **Water screening — hold four land subpixels; concede simpler default QA; withdraw automatic historical intersection.**

   Keep a 500 m pixel only when all four correctly aligned MOD44W subpixels are classified as land and none is missing. A three-of-four rule admits mixed retrievals; weighting their albedo by land fraction cannot recover the unknown land component.

   I concede Fable’s simpler starting policy: **use the delivered land classification**, record its QA, and test exclusion of low-confidence land separately. My particular acceptance list of QA 1/6/7/9 was an additional judgement without demonstrated benefit. Likewise, use A2 land/water type as a disagreement diagnostic initially, rather than automatically vetoing MOD44W land. Check its stability before reading it only once per tile.

   Retain **2015 as the declared reference footprint**, while comparing it with the available annual 2000–2009 masks. I withdraw automatically excluding everything mapped as water in any year: that rule can accumulate classification errors and preferentially remove changing landscapes. Annual MOD44W availability is documented. [MOD44W.061 catalogue](https://doi.org/10.5067/MODIS/MOD44W.061).

   **Published basis:** Carroll et al. (2017), §2, documents the MOD44W-derived operational mask and shoreline treatment. It does **not** establish our four-of-four rule, QA policy or reference year. Those remain **judgements**. Fable’s specific attribution of its masking procedure to Moody et al. (2005) was not verified from an allowed full-text passage; do not present it as established precedent.

   Both designs must describe the result accurately: a mean over retained, apparently unmixed land pixels. Some land in mixed pixels is lost. Fable’s assertion that this fringe is necessarily small needs measurement in lake-rich cells.

4. **Retrieval quality — concede a broader primary product, with a required quality comparison.**

   Recommend accepting recognised mandatory shortwave QA codes **0–7**, after removing fills and invalid physical values. Report full-inversion and missing-band shares separately. Produce the **full-only alternative using 0, 2, 4 and 6**.

   There is a published methodological precedent: **Oleson et al. (2003), §2.1**, explicitly retained all qualities because a high-quality restriction removed too much coverage. **Wang et al. (2018)** reports broadband validation across quality classes, with RMSE below 0.0318 and bias within ±0.0076; that numerical claim is verified in the published abstract. Neither establishes that every magnitude inversion is reliable in our northern winter sample.

   The product team still recommends band-specific QA for highest-quality work. Therefore, inspect the seven A2 band-quality fields in the pilot and a stricter comparison. I concede that imposing seven extra primary filters immediately would be premature. [A3 QA guidance](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a3-albedo-product/).

   Correct Fable’s shorthand: **codes 2–7 do not all mean missing Aqua band 6**. Codes 4/5 concern band 5; 6/7 concern both.

   **Basis:** published precedent exists for including both inversion types. Applying that policy to this Collection 6.1 calibration is still a **judgement**, requiring comparisons of both values and coverage.

5. **Snow products — retain all three states, but distinguish the meanings of snow fractions.**

   Produce all-conditions, snow-free and snowy albedo from the same screened dates, with known snow status. Record otherwise usable dates lost through unknown snow status. Keep counts before and after quality screening.

   Use **snowy-date share** for the primary frequency diagnostic. Fable’s radiation-weighted share describes the share of sampled incoming energy associated with snowy retrievals; it is a different quantity. It can accompany the weighted sensitivity, under that explicit name.

   **Published basis:** Jia et al. (2022), §§2.1.1 and 2.2.2, separates snow-free retrievals with A2 snow flags. Hasler et al. (2024), Methods and equation 1, combines snow states and illumination fractions in albedo-change estimates. Neither establishes our sampling thresholds or makes a retrieval-date share equivalent to fractional ground snow cover.

   Hold **no gap filling in the primary product**. Filling winter from neighbouring snow-free months would create a modelled counterfactual and needs a separate method decision. Any snow-mixture calculation must use common dates, years and spatial coverage; separately averaged fractions and albedos need not reconstruct the all-conditions mean.

6. **ERA5 illumination — hold a centred noon interval; concede nearest-grid spatial assignment as the simpler default.**

   Use hourly `ssrd` and `fdir`, both for total-sky conditions. The ERA5 convention is accumulation over the preceding hour; `fdir` is radiation through a horizontal plane. Thus no additional cosine conversion is needed. [ECMWF time convention](https://confluence.ecmwf.int/pages/viewpage.action?pageId=462529323), [parameter 228021](https://codes.ecmwf.int/grib/param-db/228021).

   Calculate apparent solar noon, including the equation of time, and combine the overlapping hourly flux intervals into an hour centred on noon. Form the diffuse fraction **after** combining the energies:

   \[
   f=(D-B)/D,\qquad
   \alpha=(1-f)\alpha_{\mathrm{BSA}}+f\alpha_{\mathrm{WSA}}.
   \]

   Fable’s omitted correction can move noon across an hourly boundary and change the selected hour completely. It is not automatically negligible because the input is hourly.

   For spatial assignment, accept the nearest ERA5 grid point initially, with a bilinear-flux comparison in the pilot. Hasler et al. provides a precedent for nearest-neighbour assignment of reanalysis illumination, although with another reanalysis and temporal treatment.

   **Basis:** the albedo mixture and radiation definitions are established; the precise centred-hour approximation and spatial assignment are **judgements**. Reject missing, non-positive denominators and materially inconsistent fluxes. I withdraw my automatic **10 W m⁻²** floor pending evidence that an additional numerical floor is needed.

   Call the result **local-noon albedo under ERA5 all-sky illumination**. Agreement with the manuscript’s intended atmospheric convention remains to be confirmed.

7. **Solar zenith — hold the shared 70° primary rule; correct its interpretation.**

   Both designs already choose **local-noon SZA ≤70°**. NASA’s validation guidance explicitly treats larger angles as suspect. Keep 75° as a labelled sensitivity, and an unrestricted version only for diagnosing legacy differences. [NASA validation guidance](https://modis-land.gsfc.nasa.gov/ValStatus.php?ProductID=MOD43).

   Fable’s mid-December limit near 47°N is reasonable illustrative arithmetic. Its statement that nothing is lost from April through September is incorrect. At 78.5°N, illustrative noon SZA is approximately **74.3° on 1 April** and **75.2° on 15 September**. Apply the rule to each date, not to a month’s midpoint.

   Separate polar night, excessive SZA and failed retrievals. Missing observations around 60°N in winter cannot all be attributed to polar night. Low illumination also does not, by itself, justify accepting unsupported calibration extrapolation.

   **Basis:** 70° has **published product-team guidance**. The sensitivity limits and how to handle lost calibration coverage are **judgements**.

8. **Monthly and decadal reduction — hold equal-year monthly means; retain weighting as a sensitivity.**

   Recommend calculating each native pixel’s arithmetic monthly mean within each year, then averaging eligible years equally. Retain annual counts and means. This gives a clear definition and avoids allowing a year to dominate merely because it supplied more retrievals.

   Fable’s pooled radiation-weighted calculation is a valid definition of a different summary. Its claim that this is necessarily the albedo required by monthly kernels is too strong. Algebraically, the underlying forcing involves

   \[
   \overline{K_d\,\Delta\alpha_d},
   \]

   which is not generally recovered by multiplying a monthly kernel by an SSRD-weighted albedo difference. That requires additional assumptions about the relationship between the kernel, radiation and albedo variation.

   **Published evidence:** Bright and O’Halloran (2019), §4.4, applies CACK to monthly climatological white-sky albedo differences. It does not prescribe Fable’s pooled SSRD reducer. Jia et al. uses a different, day-of-year climatology with trimming. **Neither exact reducer proposed here has been verified as the published recipe for this application.** The primary averaging hierarchy is therefore a **judgement for Chris**, not a literature requirement.

   Keep Fable’s weighted result as a clearly named sensitivity. Weighting A3 noon albedo by daily radiation does not produce a true daily flux-ratio albedo. That needs time-varying BSA, obtainable from A1 parameters. A small A1 comparison remains useful; I would not build a second continental product now. [A1 calculation](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a1-brdfalbedo-model-parameters-product/).

   Concede a **pilot-only median check**. Exact medians can be calculated in pixel blocks; they are not fundamentally prevented by streaming.

9. **Temporal and spatial coverage — reject the two small absolute minima as sufficient tests.**

   Fable’s **20 pooled dates** could come from one year and a short run of overlapping retrieval windows. Its **200 pixels** cover approximately **42.93 km²**, irrespective of how much land the target cell contains.

   Retain the structure of my coverage checks, with these numbers presented explicitly for Chris’s decision:

   | Check | Proposed starting rule |
   |---|---|
   | All-conditions pixel-month-year | At least `max(15, ceiling(0.5 × calendar_days))` accepted dates, represented in each third of the month |
   | All-conditions climatology | At least eight eligible years |
   | Conditional snow-state estimate | At least five dates in that state per eligible year, in at least three years |
   | Land retained after water screening | At least 95% of reference land area |
   | Supported climatology within that retained land | At least 90% |

   **All five thresholds are judgements.** I found no verified publication establishing them, and they should not be described as MODIS requirements. Their consequences must be shown before adoption. Keep diagnostic estimates and counts below the thresholds, while withholding them from the accepted calibration response.

   The two area percentages have different denominators. Report both, alongside actual land area and discarded-land area. Counts measure coverage, not independent observations.

10. **Spatial aggregation and elevation — hold fractional pixel overlap.**

   Aggregate native pixel climatologies directly to the actual 1° polygons using pixel–polygon intersection areas. Sum numerators and denominators across tiles. Construct the quarter-degree comparison independently on the legacy lattice.

   Fable’s centre assignment is an approximation at cell boundaries. Equal-area pixels justify equal weights for whole pixels; they do not justify assigning every boundary pixel wholly to one cell. The approximation deserves particular attention at 0.25°, where boundary pixels form a larger share.

   **Published basis:** Oleson et al. documents area-weighted aggregation and the need to reconcile land/water definitions. The exact intersection implementation is a **numerical choice**: verify constant fields, area conservation and convergence with polygon-edge densification.

   Use the same declared reference land definition for elevation, returning to the DEM. A shared cell lookup cannot convert cached point elevations into land means. Geometrically precise averaging also cannot restore land omitted by mixed-pixel screening.

11. **Storage, tools, configuration and legacy behaviour — take the simpler interface, with stronger provenance.**

   These are **engineering judgements**.

   Accept Fable’s small `calibration$albedo` interface, adding an explicit **recipe identifier** to product and conditions. Store preparation settings in a versioned recipe and require the pipeline to check that identity against the preparation manifest. Changing a preparation argument must produce a distinguishable product. Keep snowy albedo as an output even if it is not a calibration selector.

   Use explicit R preparation steps with shared functions. Prefer `ecmwfr` over a new CDS REST client, subject to an authentication/download pilot; its current documentation supports the newer token-based API. Use portable PSOCK workers, count all library threads against the 24-core limit, and use `ncdf4` for ERA5. [ecmwfr documentation](https://bluegreen-labs.github.io/ecmwfr/).

   Keep **Float32 derived albedo**, rather than introducing an additional 0.001 rounding step. Use separate integer count layers, provider checksums and SHA-256 provenance. Commit the recipe, manifest and small authoritative cell table; handle rasters according to repository data/LFS policy.

   Correct the storage assumption: **4,560 checkpoints containing eight full-tile Float32 layers occupy about 840 GB uncompressed**. Fable’s 0.3 TB assumes compression. Finish and release intermediate checkpoints by tile under an enforced cache limit. Its 35 MB input estimate also does not represent peak R memory.

   The ERA5 estimates differ mainly because the domains differ: approximately **84 GB versus 99 GB uncompressed**, before padding. Both are compatible with bounded monthly processing.

   Preserve the complete legacy branch, including extraction, zero handling and output names. Do not repurpose `_coarse` to mean “unweighted temporal alternative”; give that alternative its own name. Verify changes through `tools/verify_change.R` and retain existing legacy anchors.

12. **Validation and stopping rules — combine the useful comparisons; remove unsupported numerical expectations.**

   Accept Fable’s diagnostic “legacy-like” calculation, alongside comparisons that separate water screening, quality, time reduction and centre-versus-cell averaging. It can help explain differences; the unknown legacy recipe cannot serve as ground truth.

   Correct the expected sign: removing dark summer water from a mixture generally **raises** the remaining land mean; removing bright ice can **lower** it. A wholly water cell becomes missing. Fable’s summer/winter wording is reversed if it describes rebuilt minus legacy values. Other methodological differences can change either sign.

   Prioritise **Jia’s available 6 GB comparison archive**, followed by representative radiometer sites. Its period, atmospheric treatment and screening differ. It is independently constructed but shares MODIS observations. [Published data](https://zenodo.org/records/6350821). Keep MCD43GF optional; its gap-filled values and unverified version/DOI details need checking before use.

   C3 `Percent_Snow` is a spatial input percentage, whereas our snowy-date share is temporal. Compare compatible quantities rather than treating them as interchangeable.

   **Published basis:** Jia et al., §2.2.3, supports bias/RMSE assessment and explicit treatment of footprint mismatch. Numerical release limits remain **judgements**.

   Stop for wrong grids, dates, scaling, QA decoding, incomplete downloads or failed area conservation. Investigate unexplained regional bias and changes in the paper’s conclusions. Withdraw fixed expectations such as “water removal must change at least 1% of cells”, universal legacy correlation targets, and **0.02–0.95 as a hard physical range**. My earlier numerical bias and missingness gates also remain proposals, not validated acceptance standards.

The principal references and their precise roles are:

| Reference | Support established here |
|---|---|
| **Lucht, W., Schaaf, C. B. and Strahler, A. H. (2000).** *An algorithm for the retrieval of albedo from space using semiempirical BRDF models.* IEEE TGRS **38**, 977–998. DOI **10.1109/36.841980**. [Author-institution record](https://collaborate.umb.edu/en/publications/an-algorithm-for-the-retrieval-of-albedo-from-space-using-semiemp/) | Foundational identity and abstract checked; operational calculation passages were read in the product guides. |
| **Oleson, K. W., Bonan, G. B., Schaaf, C., Gao, F., Jin, Y. and Strahler, A. (2003).** *Assessment of global climate model land surface albedo using MODIS data.* GRL **30**, 1443. DOI **10.1029/2002GL016749**. [Full HTML](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2002GL016749) | Including lower-quality retrievals to preserve coverage; area weighting; land/water reconciliation. Its coastal exclusions differ from our proposed treatment. |
| **Wang, Z., Schaaf, C. B., Sun, Q., Shuai, Y. and Román, M. O. (2018).** *Capturing rapid land surface dynamics with Collection V006 MODIS BRDF/NBAR/Albedo (MCD43) products.* RSE **207**, 50–64. DOI **10.1016/j.rse.2018.02.001**. [NASA record](https://modis.gsfc.nasa.gov/sci_team/pubs/abstract.php?id=27680) | Daily retrieval behaviour and reported all-quality validation; abstract-level evidence here. |
| **Carroll, M. L. et al. (2017).** *Development of an operational land water mask for MODIS Collection 6, and influence on downstream data products.* IJDE **10**, 207–218. DOI **10.1080/17538947.2016.1232756**. [Methods](https://www.tandfonline.com/doi/full/10.1080/17538947.2016.1232756) | Mask construction and shoreline distinctions; not our four-subpixel acceptance rule. |
| **Hersbach, H. et al. (2020).** *The ERA5 global reanalysis.* QJRMS **146**, 1999–2049. DOI **10.1002/qj.3803**. [Article](https://rmets.onlinelibrary.wiley.com/doi/10.1002/qj.3803) | ERA5 provenance; current ECMWF specifications establish the selected variables and time convention. |
| **Jia, A., Wang, D., Liang, S., Peng, J. and Yu, Y. (2022).** *Global daily actual and snow-free blue-sky land surface albedo climatology from 20-year MODIS products.* JGR Atmospheres **127**, e2021JD035987. DOI **10.1029/2021JD035987**. [Methods](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021JD035987) | Snow-state separation, a published alternative climatology, and validation methods; not our thresholds or exact averaging hierarchy. |
| **Hasler, N. et al. (2024).** *Accounting for albedo change to identify climate-positive tree cover restoration.* Nature Communications **15**, 2275. DOI **10.1038/s41467-024-46577-1**. [Methods](https://www.nature.com/articles/s41467-024-46577-1) | Combining snow and illumination states; nearest-neighbour reanalysis assignment. |
| **Bright, R. M. and O’Halloran, T. L. (2019).** *Developing a monthly radiative kernel for surface albedo change from satellite climatologies of Earth’s shortwave radiation budget: CACK v1.0.* GMD **12**, 3975–3990. DOI **10.5194/gmd-12-3975-2019**. [Methods](https://gmd.copernicus.org/articles/12/3975/2019/) | Monthly kernel construction and application; no prescription of Fable’s pooled radiation-weighted reducer. |

Because this round is read-only, the question logs remain unchanged. Their next update should retain the original-raster provenance question, record the land-mean decision against B5, correct B2b’s blanket polar-night explanation, and add the treatment of unsupported winter snow-free conditions.

The merged decisions for Chris are:

- Use A3/A2 Collection 6.1 from **5 March 2000**, with actual contributing years reported.
- Try **native AppEEARS subsets**, checked against HDF files.
- Use **all processed qualities**, with full-only results alongside.
- Keep **four-land-subpixel screening** and a **70° noon limit**.
- Use **monthly means, then equal year weights**; retain radiation weighting as a sensitivity.
- Approve the coverage cut-offs, retain gaps, and require direct land-area averaging.
- Keep the legacy mode unchanged and identify every rebuilt recipe explicitly.

The genuinely open points are:

- Does the paper require noon albedo, or a daily energy-based quantity?
- How much temporal and land-area coverage is sufficient for calibration?
- Do historical shoreline changes materially alter the 2015-footprint result?
- How should unsupported northern winter and snow-free conditions enter the scientific conclusions?
- Does the AppEEARS pilot preserve every required value and provide a practical download route? The formal reference-checker audit also remains pending.