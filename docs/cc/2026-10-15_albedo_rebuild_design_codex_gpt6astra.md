# Albedo raster rebuild: independent design by Astra (Codex gpt-6-astra, max effort)

From the brief in 2026-10-15_albedo_rebuild_design_brief.md, before seeing Fable's design. Astra's text, unedited. Nothing here is a decision.

---

I recommend rebuilding a **monthly climatology of local-solar-noon shortwave albedo**, using MCD43A3/A2 Collection 6.1, ERA5 radiation and MOD44W screening before spatial averaging. This gives the paper a reproducible calibration response and supplies observed snow-free and snowy alternatives.

Use native-grid layer subsets, downloaded in bounded batches and processed locally in R. AppEEARS is the preferred acquisition route **provided a pilot proves that its outputs preserve the required native pixels and QA values**. Full HDF streaming is the dependable fallback. MCD43C3 is unsuitable for the primary rebuild because its coastal averages can already contain shallow water.

Two limitations must be explicit. Rejecting every mixed 500 m pixel removes detected water contamination but also discards some land: it does not recover the exact mean of every land fragment within a 1° cell. Likewise, snow-free observations cannot establish winter snow-free albedo where that state was never observed.

Produce all-conditions, snow-free and snowy albedo, with separate spatial coverage, valid-date counts, snow fractions and missing-data reasons. Keep the existing raster and its existing extraction behaviour under the legacy configuration.

The thresholds below are proposed judgements for a pilot, not published MODIS requirements. No bulk download or numerical validation has been performed.

I used repository text, public HTML documentation and public methods pages. I did not open `writing/` or any PDF, DOCX or PPTX. The workspace is read-only, so this design and the proposed question-log updates are supplied here.

**The methodological basis is established, but the complete processing recipe is a new project decision.** [Lucht et al. (2000)](https://collaborate.umb.edu/en/publications/an-algorithm-for-the-retrieval-of-albedo-from-space-using-semiemp/) and [Schaaf et al. (2002)](https://modis.gsfc.nasa.gov/sci_team/pubs/abstract_new.php?id=03391) establish the BRDF/albedo framework; current product guides determine Collection 6.1 details. Román et al. (2010), cited by the [MCD43A3 guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a3-albedo-product/), addresses coupling to diffuse illumination. I have not consulted the PDF ATBD. The precise water-screening procedure attributed to Moody et al. (2005) remains **unverified here**; its [NASA bibliographic record](https://esdpubs.nasa.gov/sgpubs/person/Eric_G_Moody) is verified. The four-subpixel rule below is therefore explicitly a recommendation, not something attributed to Moody.

1. **Define the quantity and inventory the inputs.**

   **Evidence.** MCD43 products are retrieved daily using overlapping 16-day observation windows. A3 black-sky albedo refers to local solar noon. The shortwave broadband covers approximately 0.3–5 μm. The guide identifies validated coverage from 5 March 2000. Daily products consequently do not constitute independent daily observations, and January–February 2000 cannot provide complete monthly samples. See the [MCD43 introduction](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/introduction/).

   **Recommendation.** Define the primary product as:

   > The area-weighted land mean of a 2000–2009 climatology of monthly mean local-noon shortwave albedo, under ERA5 ambient illumination, conditional on documented retrieval and coverage requirements.

   Use the official **shortwave broadband**, rather than averaging the seven spectral bands or averaging visible and near-infrared albedo.

   Retrieve these layers:

   | Product | Required layers |
   |---|---|
   | MCD43A3.061 | `Albedo_BSA_shortwave`, `Albedo_WSA_shortwave`, `BRDF_Albedo_Band_Mandatory_Quality_shortwave` |
   | MCD43A2.061 | Snow status, local-noon solar zenith angle, land/water type, and `BRDF_Albedo_Band_Quality_Band1` through `Band7` |
   | MOD44W.061 | Water mask and its QA |
   | ERA5 hourly single levels | `surface_solar_radiation_downwards` and `total_sky_direct_solar_radiation_at_surface` |

   Also obtain A2 platform and valid-observation layers for the pilot. Preserve their diagnostics if they prove useful. Match actual SDS names against metadata: the A2 file specification uses `SNOW_BRDF_Albedo`, while some interfaces use different capitalisation.

   Inventory actual granules before submitting jobs. Intersect tile footprints with the required cell polygons, remove spurious bounding-box matches, and pair A3/A2 by collection, tile and target date. Use the product’s target date without imposing an extra eight-day shift.

   My conservative starting rule is 5 March 2000–31 December 2009. Earlier available retrievals can be examined separately. Report the actual years contributing to every month; never describe January as a ten-year average.

2. **Remove water on the native grid and quantify the land discarded.**

   **Verified locally.** The existing AppEEARS bundle contains two native-projection GeoTIFFs and supporting files. Its QA lookup includes classes 1, 2, 3, 4, 5, 6, 7 and 9; this differs from some older online tables. In particular, class 9 records a water prediction overridden by DEM slope.

   **Recommendation.** First verify the CRS, origins and pixel boundaries. Nominal 500 m MODIS pixels are approximately 463.31 m across; each should contain exactly four aligned MOD44W pixels of approximately 231.66 m. Do not resample the binary mask with bilinear interpolation.

   Retain an albedo pixel only when:

   - All four mask pixels are land.
   - None has missing or unresolved mask information.
   - Their QA passes the declared mask policy.
   - A2 land/water type is land; retain disagreement counts separately.

   For the initial MOD44W QA policy, accept land classified with QA 1 and the documented ancillary corrections 6, 7 and 9; exclude low-confidence land, QA 3. Compare against both “QA 1 only” and “all valid land classifications” in the pilot. These are **judgements**, particularly the treatment of ancillary corrections.

   I recommend retaining 2015 as the reference land footprint, but obtaining the annual 2000–2009 MOD44W masks before production. Exclude from the primary fixed footprint any 500 m pixel with mapped water in any of those years. This avoids assuming that a shoreline observed in 2015 remained unchanged throughout the calibration period. Compare this conservative footprint with year-specific masks.

   **Why four out of four?** A three-land/one-water pixel still has a mixed albedo retrieval. Multiplying that value by 0.75 does not recover its land component. A majority-land rule improves coverage but accepts residual contamination; use it only as a labelled sensitivity.

   However, four-out-of-four screening sacrifices the land inside mixed pixels. Publish, for each target cell:

   - Total reference land area from the 250 m mask.
   - Area retained as unmixed 500 m land.
   - Land area lost to mixed pixels, uncertain classification and changing water.
   - Area lost subsequently to albedo QA.

   A provisional requirement is that retained unmixed pixels represent **at least 95% of reference land area** before treating their mean as representative of the whole land portion. Cells failing this requirement need an explicit exclusion or a different observation/model strategy. Even passing this threshold does not make subpixel shorelines or the satellite footprint exact.

3. **Screen retrieval quality and classify snow separately.**

   **Evidence.** Collection 6.1 mandatory broadband QA distinguishes full inversions from magnitude inversions and also flags unavailable bands 5/6:

   | Mandatory QA | Interpretation |
   |---|---|
   | 0 / 1 | Full / magnitude inversion |
   | 2 / 3 | Full / magnitude; band 6 unavailable |
   | 4 / 5 | Full / magnitude; band 5 unavailable |
   | 6 / 7 | Full / magnitude; bands 5 and 6 unavailable |

   These definitions come from the [Collection 6.1 A3 guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a3-albedo-product/).

   **Recommendation.**

   - Primary: accept full inversions, codes **0, 2, 4 and 6**.
   - Check A2 quality for the contributing spectral bands: accept classes 0 or 1. Do not reject an explicitly unavailable band 5/6 as though it contributed.
   - Sensitivity: admit magnitude inversions, keeping their contributions identifiable.
   - A stricter sensitivity should retain only mandatory QA 0 and the best A2 retrievals.

   Decode recognised values before testing bits. For recognised A3 codes, bit 0 distinguishes inversion type; bits 1 and 2 describe band-6 and band-5 availability. A blanket `QA == 0` filter silently changes the spectral-availability policy.

   A2 per-band quality classes 0/1 are full inversions; 2/3 are magnitude inversions. Snow status 0 means snow-free and 1 snowy; fill values must be removed first. Its valid-observation fields contain 16 bits describing observations used in the retrieval window, **not monthly valid-day counts**. See the [A2 file specification](https://ladsweb.modaps.eosdis.nasa.gov/filespec/MODIS/61/MCD43A2).

   Apply the 0.001 albedo scale once, respecting any automatic reader scaling. Reject fill values before scaling and flag resulting values outside 0–1.

   Make three streams from the same screened retrievals: all conditions, snow-free and snowy. Require a known snow flag for this partition and count otherwise-valid retrievals excluded because the flag is missing.

   Strict QA can preferentially remove transitions. NASA warns that rapid surface changes may receive lower-quality flags; [Wang et al. (2018)](https://modis.gsfc.nasa.gov/sci_team/pubs/abstract.php?id=27680) evaluates the daily retrievals’ ability to capture such changes. This is why the magnitude-inversion sensitivity is necessary.

4. **Calculate the diffuse fraction at the time represented by A3.**

   **Evidence.** Use ERA5:

   | Variable | Short name | Meaning |
   |---|---|---|
   | Surface solar radiation downwards | `ssrd`, parameter 169 | Total downward shortwave energy |
   | Total-sky direct solar radiation at surface | `fdir`, parameter 228021 | Direct downward shortwave energy |

   The hourly reanalysis accumulations are in J m⁻² over the hour **ending at the timestamp**. They should not be differenced again as though accumulated from midnight. See [ERA5 documentation](https://confluence.ecmwf.int/spaces/CKB/pages/76414402/ERA5+data+documentation). The direct component is horizontal-plane radiation, so no additional cosine conversion is needed; this convention is also stated in [Casado-Rubio et al. (2017)](https://journals.ametsoc.org/view/journals/apme/56/6/jamc-d-16-0297.1.xml).

   **Recommendation.** Calculate apparent solar noon from longitude and date, including the equation of time. Use UTC throughout. A tested astronomical calculation can be checked against the [NOAA solar calculator](https://gml.noaa.gov/grad/solcalc/).

   Estimate radiation over a one-hour interval centred on that noon:

   - Weight the overlapping ERA5 hourly intervals by their overlap.
   - Interpolate total and direct radiation spatially using identical weights.
   - Form the ratio afterwards.

   This centred-hour treatment is a **judgement**: hourly ERA5 cannot supply an instantaneous measurement at noon.

   With total energy \(D\) and direct energy \(B\),

   \[
   f_{\mathrm{diffuse}}=\frac{D-B}{D},
   \qquad
   \alpha=(1-f_{\mathrm{diffuse}})\alpha_{\mathrm{BSA}}
          +f_{\mathrm{diffuse}}\alpha_{\mathrm{WSA}}.
   \]

   Reject missing inputs and materially negative radiation or \(B>D\). Permit only a documented numerical tolerance for tiny encoding errors, recording any corrections. Do not silently clip substantial inconsistencies.

   Use total-sky variables for the primary product. A clear-sky alternative would require **both** clear-sky total and clear-sky direct radiation. Do not combine one clear-sky variable with one total-sky variable.

   This calculation approximates diffuse illumination as isotropic. The A3 guide identifies limitations for bright surfaces and large solar angles. Call the output “ambient-illumination local-noon albedo” in the technical documentation so its meaning is unambiguous.

   **Alternatives considered:**

   - A fixed UTC hour gives different local times across the continent.
   - Terra/Aqua overpass times do not match A3’s noon reference.
   - A whole-day diffuse fraction combined with noon BSA is a hybrid approximation.
   - A genuine daily energy-weighted product requires MCD43A1 BRDF parameters, allowing BSA to vary with solar zenith angle. The [A1 guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a1-brdfalbedo-model-parameters-product/) provides that calculation.

   For the last alternative, the appropriate daily quantity is

   \[
   \alpha_{\mathrm{daily}}=
   \frac{\sum_h[B_h\alpha_{\mathrm{BSA}}(\theta_h)
              +(D_h-B_h)\alpha_{\mathrm{WSA}}]}
        {\sum_h D_h}.
   \]

   Evaluate this on representative pilot areas before deciding whether the noon approximation is adequate for the paper. A daily mean cannot be obtained merely by changing A3’s temporal weights.

5. **Treat darkness and low sun as different missing-data causes.**

   **Evidence.** NASA advises treating retrievals above 70° solar zenith angle as suspect for research. See its [current albedo validation guidance](https://modis-land.gsfc.nasa.gov/ValStatus.php?ProductID=MOD43).

   **Recommendation.** Use noon SZA ≤70° for the primary product, with a ≤75° sensitivity. Also require noon-window downward irradiance above a provisional **10 W m⁻²** equivalent; this numerical floor is a judgement to avoid unstable ratios.

   Distinguish:

   - Polar night: no daylight.
   - Daylight with excessive SZA.
   - Insufficient radiation for a stable ratio.
   - Missing or rejected MODIS retrievals.
   - Missing ERA5 inputs.

   Leave albedo missing in polar night. Neither zero albedo nor white-sky albedo is a substitute. Where later forcing calculations establish genuinely zero illumination, zero forcing may be appropriate; missing illuminated months must remain distinguishable.

6. **Average time explicitly, preserving snow and sampling information.**

   **Recommendation.** Use this order:

   1. Calculate screened daily blue-sky albedo at native pixels.
   2. Calculate each pixel’s monthly mean separately for each year.
   3. Average eligible monthly means across years with equal year weights.
   4. Aggregate those pixel climatologies spatially.

   This avoids giving a pixel or year more influence simply because it supplied more retrievals. Means retain intermittent snowy conditions. Medians answer a different question and can suppress brief snow events; retain them as a sensitivity, not the primary reducer.

   For state \(s\), including “all”:

   \[
   M_{iym,s}=
   \frac{\sum_{d\in(y,m)}I_{id,s}\alpha_{id}}
        {\sum_{d\in(y,m)}I_{id,s}},
   \qquad
   C_{im,s}=\frac{1}{Y_{im,s}}\sum_{\text{eligible }y}M_{iym,s}.
   \]

   Here \(I\) identifies accepted retrievals in the chosen state.

   **Provisional eligibility rules — all judgements:**

   | Quantity | Initial rule |
   |---|---|
   | All-conditions pixel-month-year | At least `max(15, ceiling(0.5 × calendar_days))` accepted target dates, with representation in each third of the month |
   | All-conditions pixel-month climatology | At least eight eligible years |
   | Snow-free or snowy pixel-month-year | At least five accepted dates in that state |
   | Conditional state climatology | At least three eligible years, explicitly flagged as less supported than the primary climatology |
   | Published whole-land cell-month response | Supported pixels cover at least 90% of the retained land footprint |

   Repeat the pilot with stricter thresholds. The overlapping retrieval windows mean these counts measure **coverage**, not independent sample size.

   Publish snow frequency as the proportion of accepted retrieval dates classified snowy, first per pixel-year-month, then summarised with the declared year and area weights. Also publish known-snow-status counts before the albedo QA filter, so differential rejection of snowy retrievals is visible.

   Call this **snow fraction among sampled retrieval dates**. It is not an unbiased estimate of calendar-day snow duration or fractional ground snow cover.

   Preserve native monthly counts for all, snow-free and snowy retrievals, eligible-year counts, and missing reasons. At coarse resolution, distinguish mean valid days per land pixel from total valid pixel-days or area-days.

   Two cautions follow:

   - A snow-free climatology describes observed snow-free conditions. Do not interpolate summer albedo into a permanently snowy winter and call it observed.
   - Independently averaged snow fractions and state albedos generally do not satisfy a coarse-cell mixing identity. For snow-mixture experiments, retain the joint sums and calculate mixtures on common pixel/year support before aggregation.

   Do not gap-fill the primary release. Gap-filled snow-free products have published precedents, including [Moody et al. (2008)](https://journals.ametsoc.org/view/journals/apme/47/11/2008jamc1795.1.xml), but filling this response from land-cover classes risks imposing the relationship the calibration is supposed to estimate.

7. **Aggregate directly to the actual 1° polygons.**

   **Recommendation.** Construct target polygons from `grid.RDS`, retaining its cell identifiers. Transform sufficiently densified polygon edges into MODIS sinusoidal coordinates, and calculate overlap areas with the retained native pixels.

   For a cell \(c\),

   \[
   A_{cm,s}=
   \frac{\sum_i a_{ic}C_{im,s}}
        {\sum_{\text{supported }i}a_{ic}},
   \]

   where \(a_{ic}\) is the retained pixel area inside the cell. Keep the denominator and compare it with the complete retained-land area before applying the coverage threshold.

   Use `sf` and `exactextractr`, or an equivalently checked `terra` implementation. Fractional polygon overlap is supported by [exactextractr](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html). Document the MODIS spherical Earth model and check convergence as polygon edges are densified.

   Sum numerators and denominators across tiles; do not average tile means. Do not use bilinear reprojection to construct the land mean.

   Also produce a 0.25° diagnostic raster on the **exact legacy lattice**, including its origin. Calculate it independently from native pixels. The quarter-degree centres and one-degree boundaries need not form nested cells, so a second average of quarter-degree means can introduce another approximation.

   Carry the same reference-land definition into elevation preparation. The current elevation cache contains point elevations, so it cannot supply the required land-mean elevation without returning to a documented DEM.

**For local acquisition, I recommend AppEEARS native-grid subsets with streaming reduction, subject to a fidelity pilot.** Its API supports spatial, temporal and layer selection, native projection, GeoTIFF/NetCDF output, and R requests. All scientific screening, mixing and averaging would happen locally. See the [AppEEARS API documentation](https://appeears.earthdatacloud.nasa.gov/api/).

The exact requested A3/A2 layer availability, job limits, output compression and preservation of native pixels have **not been tested here**. Full HDF files provide the reference against which the pilot must compare.

The following calculations use the supplied approximately 56 tiles and approximately 3,600 target dates. They are planning quantities, not a fresh CMR inventory.

| Route | Download volume | Peak local storage and processing |
|---|---|---|
| Full A3+A2 HDF streaming | Approximately **13 TB**, using the supplied measurements; roughly **400,000 files** | A tile-month is about **2 GB** of compressed inputs. Delete only after a verified checkpoint. Roughly **1.2 trillion native pixel-date positions** before cropping; HDF decoding and I/O are substantial. |
| Native layer subsets | **Unmeasured.** The required layers occupy about **15 bytes per native pixel** before compression: roughly **17 TB** across the full tile envelopes, multiplied by the retained spatial fraction and compression ratio | Smaller spatial requests and fewer decoded layers can reduce transfer and local work. The same native pixels still require scientific processing. Apply a fixed cache budget; subsets may still total terabytes. |
| MCD43C3 global daily HDF | Supplied estimate: approximately **0.65 TB**, around **3,600 files** | Stream individual files rather than retain all globals. Approximately 93 billion global pixel-date positions; the legacy bounding rectangle contains about 13 billion. Much lighter spatial processing, but unsuitable for the primary land-only response. |

For subsets, an illustrative retained fraction of 0.3 and compressed/raw ratio of 0.2 would give about **1 TB**. That is an example calculation, **not a measured forecast**. Adding platform and seven valid-observation fields approximately doubles the selected uncompressed payload. Reading selected SDS locally reduces decoding and memory; it does not by itself reduce the bytes downloaded in a full HDF.

Harmony and OPeNDAP should remain alternatives pending a successful collection-specific capability query and native-value comparison. General service support does not establish support for these particular collections; [Harmony documents a capability endpoint](https://harmony.earthdata.nasa.gov/docs).

MCD43C3 is explicitly unsuitable for exact native water screening. Its [guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43c3-cmg-albedo-product/) says shallow-water inputs can enter coastal CMG averages. A 0.05° grid is useful for comparison, but cannot separate those contributions afterwards. Its percentage-snow and percentage-input layers also cannot recover separate snow-state albedos or a MOD44W-defined land denominator; their definitions are in the [C3 specification](https://ladsweb.modaps.eosdis.nasa.gov/filespec/MODIS/61/MCD43C3).

Budget ERA5 explicitly: approximately 501 × 281 grid points, two variables and 87,672 hours give **about 99 GB as uncompressed Float32**, before modest boundary padding. Packed downloads may be smaller. Request monthly chunks, including neighbouring timestamps needed around midnight and year boundaries.

A practical workstation allocation is:

- **100 GB maximum transient input/cache space**, enforced before downloads.
- Approximately **100–150 GB uncompressed** for useful native monthly climatologies and count layers, rather than daily native archives.
- Around **100 GB uncompressed** for ERA5; much smaller coarse outputs.
- Four processing workers initially, with an **80 GB total RAM budget**. Increase only after measuring memory and I/O. Count worker, GDAL and BLAS threads together against the 24-core limit.

Process one tile and calendar month across years, retain monthly summaries, and release daily files after atomic, checksummed checkpoints. This fits comfortably within 1.3 TB without retaining the raw MODIS archive.

**Pipeline integration should preserve the existing behaviour as a complete legacy mode.** The current script samples the quarter-degree raster at cell centres for the main calibration table; its averaged raster supplies a separate table. See [script 2, line 143](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/2_calibration_lct_bluesky.R:143). Replacing only the TIFF while retaining that extraction would not implement the land-mean decision.

Use three small R entry points:

| Proposed tool | Purpose |
|---|---|
| `tools/download_albedo_inputs.R` | Inventory inputs, submit bounded requests, download, verify and resume |
| `tools/prepare_albedo.R` | Apply the recipe and produce native/coarse albedo, counts and coverage |
| `tools/check_albedo.R` | Generate validation tables and plots from a declared verification spec |

Shared functions belong in `R/`, with one implementation of QA decoding, averaging and area weighting.

Proposed analysis selectors:

| Key | Legacy value | Rebuilt value |
|---|---|---|
| `analysis$albedo$source` | `"legacy_raster"` | `"modis061_era5"` |
| `analysis$albedo$state` | `"all"` | `"all"`, `"snow_free"` or `"snowy"` |
| `analysis$albedo$spatial_support` | `"legacy_point"` | `"land_mean_1deg"` |
| `analysis$albedo$recipe_id` | `"legacy_unknown"` | A versioned, explicit recipe |
| `analysis$elevation$source` | `"legacy_cache"` | `"land_mean"` |

Put period, mask years and QA policy, inversion policy, SZA limit, radiation convention, reducers and coverage thresholds in the recipe’s validated analysis settings. Worker counts, cache limits and download concurrency belong under `run`. Reject incompatible combinations.

Use these R packages:

- `httr2`, `curl`, `jsonlite` and `yaml` for requests and metadata.
- `terra`, `sf` and `exactextractr` for spatial processing.
- `ncdf4` for ERA5 arrays and metadata.
- `digest` for SHA-256, and base `parallel` PSOCK workers for portability.
- `testthat` for the substantive numerical checks.

NASA authentication should use `~/.netrc`; the CDS wrapper should read `~/.cdsapirc` into memory without logging its contents. CDS exposes a REST interface usable from R, although ECMWF labels direct REST usage unsupported; authentication and retrieval therefore need an early integration check. Dataset terms must first be accepted through the CDS account. See [CDS access documentation](https://cds.climate.copernicus.eu/how-to-api).

The full-HDF fallback requires a GDAL build with HDF4 support. This is a preflight requirement, not something guaranteed by having an HDF5 reader. [GDAL documents the distinction](https://gdal.org/en/stable/drivers/raster/hdf4.html). Native GeoTIFF subsets make the ordinary processing path easier to run across operating systems. No Bash, Python, `gdalwarp` executable or shell download wrapper is required.

Write outputs under a new versioned preparation directory:

- Twelve-band Float32 GeoTIFFs for all, snow-free and snowy albedo, with explicit month names, units and NoData.
- Native sinusoidal climatologies and count layers for audit.
- Authoritative 1° products and a cell-ID table for calibration.
- The matching 0.25° diagnostic products.
- Coverage, mask-loss, snow fraction, eligible-year counts and missing-reason layers.
- Annual monthly summaries for diagnosing interannual variability.
- A machine-readable manifest, readable recipe and checksums.

The manifest should contain collection versions, granule identifiers and production dates, request definitions, provider checksums where supplied, SHA-256 hashes, software versions, code commit, grids, mask provenance, every scientific setting, rejected-input counts and output hashes. Record source hashes before deletion; never retain credentials or temporary authenticated URLs.

Existing run manifests hash original inputs but do not establish that a prepared input matches the requested scientific recipe. Add an explicit preparation-manifest check. A raster with the wrong mask, state or recipe must fail before calibration.

For legacy regression, preserve the original file, centre extraction, zero replacement, missing-value handling and output names. Add selectors to both configurations and the schema, run verification through `tools/verify_change.R` in its separate worktree, and compare against the existing anchors. Do not update anchors to accommodate numerical drift. Scientific implementation requires the independent reviews specified in `AGENTS.md`; this document does not substitute for them.

**Validation should separate expected methodological differences from implementation errors.**

Start with three representative tiles covering lake/coastal margins, boreal snow transitions and high latitudes. January, April and July in 2001 and 2008 would cover contrasting conditions and early versus later platform sampling. This is about 550 tile-dates, or approximately 36 GB of A3+A2 HDF reference inputs under the supplied size assumptions.

The executable checks should include:

| Check | What it establishes |
|---|---|
| Native subset versus original HDF | Identical retained grid, integer albedo values, QA codes and target dates |
| Synthetic four-subpixel masks | Mixed, uncertain and missing mask pixels cannot enter strict land averages |
| All mandatory QA codes and fills | Correct inversion and missing-band interpretation |
| Known radiation examples | Correct direct/diffuse weighting, zero-denominator handling and preceding-hour convention |
| Midnight and year-boundary cases | Correct solar-noon/date matching |
| Constant fields and partial polygon overlaps | Area conservation, tile merging and absence of grid shifts |
| Unequal valid-date counts between pixels/years | Correct averaging hierarchy |
| Interrupted and resumed batches | No omitted or double-counted granules |
| Serial versus parallel processing | Deterministic results within the declared numerical tolerance |

Compare with the existing raster in stages:

1. Compare matching quarter-degree pixels on the legacy lattice.
2. Compare rebuilt centre samples with existing centre samples.
3. Compare the rebuilt 1° land mean with those centre samples.
4. Attribute differences using mask, QA, snow, reducer and noon/daily sensitivity outputs.

Expect the closest agreement over well-observed, homogeneous inland areas in snow-free months. Expect larger differences at lakes and coasts, in heterogeneous cells, during snowmelt, and at high latitude. There is no justified universal expectation for the sign. The legacy raster’s unknown construction means numerical disagreement alone is not evidence that the rebuild is wrong.

For an external climatology, use [Jia et al. (2022)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021JD035987). Their product uses 2001–2020 MODIS Collection 6, aerosol-based diffuse fractions, a 75° SZA screen and a trimmed day-of-year averaging procedure. Those differences must accompany the comparison. It is independently constructed, but **not independent of MODIS observations**.

The [published Zenodo collection](https://zenodo.org/records/6350821) supplies actual and snow-free climatologies at 0.05° and 0.5°; the 0.05° archive is about 6 GB. Aggregate its daily climatology to months and compare common spatial support. Report bias, MAE/RMSE, seasonal amplitude, missingness and regional patterns. Correlation alone is insufficient.

Add genuinely independent radiometer checks where footprints are representative, using [SURFRAD](https://gml.noaa.gov/grad/surfrad/) and suitable northern stations. Compare both albedo and ERA5’s diffuse fraction with observations at the corresponding local time.

Inspect maps and time series around the Great Lakes, Canadian lake districts, Hudson Bay, Atlantic/Pacific coasts, boreal forest–open-land transitions and mountain snow margins. Include areas with substantial MOD44W QA-3/QA-9 classification and changing water.

Provisional stopping rules, all **project judgements**, are:

- **Stop processing** for grid misalignment, wrong dates/scaling, unexplained QA decoding, checksum failures or non-conserving aggregation.
- **Withhold affected cell-months** when the 95% static-land or 90% observational-coverage requirements fail.
- **Pause adoption of a complete calibration product** if failures remove more than 10% of reference land area in important spring months.
- **Investigate before release** if representative independent comparisons show unexplained mean bias above 0.02 or RMSE above 0.05.
- **Reconsider the noon convention** if the A1 pilot changes important monthly albedos systematically by more than 0.01.
- Revisit the scientific interpretation if plausible QA, mask or snow choices change the sign or principal conclusions of the forcing analysis.

These are investigation gates, not targets to achieve by tuning the product towards the legacy raster.

**The main remaining risks are sampling and representativeness.** Water screening leaves unsampled land; clouds and low sun preferentially remove some snow states; strict full-inversion filtering can miss transitions; the early record has different platform sampling; and coarse ERA5 illumination does not resolve local cloud or terrain conditions. The snow-free product also retains its observed atmospheric and seasonal sampling. None of these uncertainties is represented adequately by treating thousands of overlapping retrieval dates as independent observations.

For Andria, retain and extend the existing provenance question: whether any original script, export definition, granule list or processing notes survive. The scientific question is how the paper should handle regions/months with inadequate snow-free support: exclusion, a narrower sensitivity claim, or a separately justified modelled counterfactual.

The proposed question-log entries are:

- **General, existing §4:** “Request any surviving construction code or metadata for the blue-sky raster, including reducers, QA/snow/water treatment, ERA5 timing and spatial aggregation. Source: rebuild design, 2026-10-15.”
- **Scientific:** “How should the snow sensitivity be presented where winter snow-free albedo has insufficient observational support? Source: rebuild design, 2026-10-15.”
- Record Chris’s 2026-10-14 land-mean decision against B5, while preserving the distinction between that target and the coverage achievable after strict native-pixel screening.

Chris’s decisions can be kept to five:

1. Adopt **local-noon monthly albedo**, conditional on the A1 daily-energy pilot.
2. Adopt **four-out-of-four land screening**, the period-water check and explicit coverage losses.
3. Accept **full inversions as primary**, with magnitude inversions as a required sensitivity.
4. Approve or revise the proposed **coverage thresholds and no-gap-filling policy**.
5. Use **validated native AppEEARS subsets**, with full-HDF streaming as fallback, and require the stated validation gates before changing the main calibration.