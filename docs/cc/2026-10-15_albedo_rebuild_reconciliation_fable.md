# Albedo raster rebuild: reconciliation, Fable's pass

One reconciliation pass: Fable read Astra's design and conceded, held or proposed middles; includes the step-by-step precedent-or-judgement table and the merged decision list. Unedited.

---

# Reconciliation of the two rebuild designs (Fable, 2026-10-15, one pass)

Read in full: `astra.md` (Astra's design) and my own `fable.md`. Read-only as before; the
checks below were made on 2026-10-15 against public HTML pages, NASA CMR, Harmony, AppEEARS's
public API endpoints, Zenodo and Crossref; no PDF opened, nothing downloaded. For each point of
difference: **concede**, **hold** or **middle**, with the evidence. Chris's standing rule is
applied in section 3: every recipe step is labelled with a verified published precedent or as a
judgement.

## 1. Where the designs agree (no reconciliation needed)

Products and layers (MCD43A3 shortwave BSA, WSA, mandatory QA; MCD43A2 snow flag, local-noon
zenith, land/water type); the shortwave broadband; the blue-sky formula with ERA5 ssrd and fdir
in total-sky form; the diffuse fraction at local solar noon; water removed at 500 m with all four
MOD44W sub-pixels land, and the loss quantified; the 70-degree noon zenith rule as primary;
missing values in polar night, no gap filling; three streams (all, snow-free, snowy) with counts;
mean rather than median; direct aggregation from native pixels to the 1-degree cells plus a
0.25-degree diagnostic on the exact legacy lattice; MCD43C3 unsuitable for the land-only
response; the legacy configuration keeps the raster and the centre-pixel extraction
byte-identical; one tile and month at a time with checksummed checkpoints; tools in `tools/`
with functions in `R/`; manifest with hashes; `verify_change.R` for the legacy regression;
Jia et al. 2022, SURFRAD and lake, coast and boreal spot checks for validation.

## 2. Points of difference

### 2.1 Acquisition route

Astra: AppEEARS native-grid layer subsets after a fidelity pilot, full HDF streaming as
fallback. Fable: full HDF streaming, AppEEARS as fallback.

**Correction of my wording.** My design said "no NASA subsetting service exists for MCD43A3";
that was about OPeNDAP and Harmony, and it is now verified twice: the CMR collection record
C2278860820-LPCLOUD lists no OPeNDAP, ESI or Harmony service features, and Harmony's own
capability endpoint for the collection returns `subsetting: bbox false, variable false,
temporal false ... services: []`. AppEEARS *is* a subsetting service and I listed it as route B;
I should have said "no on-the-fly subsetting service other than AppEEARS".

**New evidence.** AppEEARS's public product endpoint lists all 30 MCD43A3.061 layers and all 19
MCD43A2.061 layers, including `Albedo_BSA_shortwave` (float32 output, fill 32767, scale
0.001, QA layer attached), `BRDF_Albedo_Band_Mandatory_Quality_shortwave` (byte, fill 255),
`Snow_BRDF_Albedo` and `BRDF_Albedo_LocalSolarNoon` (byte, fill 255). Its API documentation
states no request-size limits (only: Bearer tokens expire after about 48 hours; download URLs
are valid 48 hours; task status carries a `user_consider_limit` flag whose meaning is not
documented [unverified]). Astra's doubt about layer availability is therefore settled in
AppEEARS's favour; its doubt about job limits remains. The MOD44W delivery proves AppEEARS
preserves the native grid (same projection and pixel size), with the mosaic origin at an odd
250 m offset that must be handled.

**My environment finding, which Astra did not have:** the `paleo-rf` GDAL (conda-forge
`libgdal-core` 3.13.3) has no HDF4 and no netCDF driver. Full-HDF streaming cannot start until
`libgdal-hdf4` is added; AppEEARS GeoTIFFs need nothing new. Astra's "preflight requirement" is
exactly the condition that fails today.

**Middle (proposed).** Run Astra's fidelity pilot as the first step, but as a decision
experiment with two arms rather than a presumption: (a) one AppEEARS area request for three
tiles, three months, two years, five layers, compared pixel for pixel with the same HDF files
read through a GDAL with HDF4 (which must be installed for the pilot anyway); (b) the measured
AppEEARS bundle size per layer-day and the queue time. Choose AppEEARS if the pilot shows
identical retained values and QA codes and a bundle size well below the HDF volume; otherwise
full-HDF streaming over the 38 tiles (my CMR-based inventory: about 2.4 GB per July day and
1.6 GB per January day for MCD43A3, about a third again for MCD43A2, roughly 10 TB in all; Astra's
13 TB and 400,000 files use the 56-tile bounding box, which includes empty Pacific tiles and
spurious high-latitude ones). Either way the processing code is the same from the point where a
tile-day's five layers are in memory, so the route choice does not touch the recipe.

### 2.2 Quality filter

Astra: full inversions primary (codes 0, 2, 4, 6), magnitude inversions as a required
sensitivity, plus the MCD43A2 per-band quality of the contributing bands. Fable: all processed
retrievals primary (codes 0-7), full-only as the sensitivity.

**Evidence.** The user guide urges users "to use the band specific quality flags to isolate the
highest quality full inversion results" (verified); NASA's MOD43 validation page says users must
refer to the quality flags for research (verified, 2026-10-15). Against that, Wang et al. 2018
report shortwave blue-sky RMSE below 0.032 and bias within 0.008 "for all quality results"
(verified abstract), and the user guide's own gap-filled product warns that high latitudes are
"dominated by very low illumination" with "primarily poor quality BRDF retrievals" (verified),
i.e. full-only removes most of the winter north.

**Concede, with a condition.** The product team's recommendation is the published practice
Chris's rule asks for, so full inversions become the primary stream. Both streams come from the
same pass at the cost of one more accumulator, so the sensitivity is free. The condition: the
pilot must report, per month and latitude band, the pixel-months that full-only loses relative to
all-processed; if full-only empties the calibration north of 50 N in November to February beyond
what the zenith rule already removes, the choice goes back to Chris with those numbers rather
than being settled here. On Astra's extra check of the MCD43A2 per-band quality for the
contributing bands: hold as optional. The broadband's own mandatory QA already states full or
magnitude; the per-band layers add seven byte layers per tile-day for a refinement whose effect
is unmeasured. Record them in the pilot only.

### 2.3 Monthly reducer, weighting and the A1 "daily-energy" pilot

Astra: per pixel, mean over accepted dates within each month-year (equal date weights), then
equal-weight mean over eligible years; the quantity is explicitly "monthly mean local-noon
shortwave albedo"; a pilot with MCD43A1 BRDF parameters to test whether a daily energy-weighted
albedo differs materially from the noon albedo. Fable: one pooled mean over all valid days of the
calendar month in 2000-2009, weighted by each day's ERA5 insolation.

**Evidence on precedent.** Monthly and climatological albedo from MCD43 in the published
climatologies is a plain average of retrievals (Moody et al. 2008 zonal and temporal averages;
Jia et al. 2022's day-of-year averaging as described by Astra [the trimming detail unverified]).
I found no published MODIS climatology that weights daily values by insolation; my argument came
from the kernel definition (the kernel's monthly flux response belongs with a flux-weighted
albedo), which is physical reasoning, not a precedent. Within a month and at local noon the
weighting matters only where insolation changes strongly across the month (high-latitude spring
and autumn) and the albedo changes with it (snowmelt).

**Concede the primary, keep mine as a labelled alternative.** Primary stream = Astra's
hierarchy (plain mean per pixel-month-year, equal years). The insolation-weighted pooled mean is
produced in the same pass as a second stream, labelled a judgement, and the difference is
reported by month and latitude; it informs the snow sensitivity, where the timing of melt within
the month is the whole point. On the noon definition itself: hold with Astra; both designs use the
noon black-sky albedo, which is what MCD43A3 provides and what Roman et al. 2010 and the user
guide describe (verified).

**The A1 pilot: middle.** The daily energy-weighted albedo needs the three BRDF kernel weights
from MCD43A1 and the black-sky polynomial of Lucht et al. 2000 (Astra cites the A1 guide for the
calculation; verified that the guide page exists, calculation not re-read). It is the right test
of the noon approximation, it is small on three tiles, and it should be run only after the
primary product exists, so that it cannot delay the calibration. Its outcome changes the
definition of the product only if Chris accepts Astra's gate (systematic monthly change above
0.01). I would add that the result should be judged on the forest-versus-open-land contrast,
not only on the albedo level, because the level is absorbed by the calibration while the
contrast drives the forcing.

### 2.4 Solar-zenith rule

Astra: noon SZA at most 70 degrees primary, 75 as sensitivity, plus a 10 W m-2 irradiance floor
on the noon window. Fable: 70 primary, "none" as the comparison setting.

**Evidence.** "Data with solar zenith angles greater than 70 degrees should be considered
suspect for research efforts" (NASA MOD43 validation page, verified); the user guide's MCD43GF
page says the same (verified). Jia et al. 2022 used 75 degrees according to Astra [unverified].
My arithmetic on the consequence stands: 70 degrees removes December north of about 47 N,
January north of 49 N, November north of 51.5 N, February north of 57 N; the legacy raster kept
values to about 60-62.5 N in January, so it applied no rule.

**Middle.** 70 primary (both); sensitivities 75 (Astra, matching Jia if confirmed) and "none"
(Fable, needed for the legacy comparison). Astra's irradiance floor: concede; it is a numerical
guard on the ratio, a judgement, harmless, and it separates polar night from low sun in the
missing-reason counts.

### 2.5 Start date

Astra: 5 March 2000 (day 2000065) to 31 December 2009. Fable: "2000-02-24, first date to be
confirmed". **Concede.** The user guide states that the reprocessed V006/V006.1 record runs
"from Day 2000065 (5 March 2000 to present)" and that only Terra data exist before mid-2002
(verified). Start the inventory at 2000-065, and report the years contributing to each month
(January and February have nine years, and 2000-2002 are Terra-only; both go in the README and
the table's year counts).

### 2.6 Coverage thresholds and no gap filling

Astra (all labelled judgements): pixel-month-year needs at least max(15, half the days) accepted
dates with representation in each third of the month; climatology at least eight eligible years;
conditional states at least five dates and three years; a published cell-month needs supported
pixels covering at least 90 % of the retained land footprint; and retained unmixed pixels must
be at least 95 % of the reference land area for the cell mean to stand for the land portion.
Fable: at least 20 pooled days per pixel-month; at least 200 valid land pixels per cell-month.

**Evidence.** No published threshold exists for either set; both are judgements and Astra says
so. Two facts bear on the numbers: MCD43 produces a value on most days where there is light,
because each day draws on 16 days of observations, so date counts are coverage, not sample size
(both designs say this); and in the Canadian Shield lake districts the one-pixel fringe around
water is a large share of the land, so a 95 % unmixed-land rule would exclude many cells
wholesale.

**Middle.** Adopt Astra's hierarchy and its *structure* of thresholds (per month-year, per
climatology, per state, per cell), with looser starting values and all of them as recorded tool
settings: at least 10 accepted dates per pixel-month-year (no thirds rule, which is hard to
read and to justify), at least 5 eligible years for the all-conditions climatology, at least 5
dates and 3 years for a state stream, and a cell-month published when supported pixels cover at
least 50 % of the retained land footprint, with the 90 % level reported as "well supported".
The 95 % unmixed-land rule becomes a reported quantity and a flag, not an exclusion; whether to
exclude cells below some level is a decision for Chris after the pilot shows the distribution.
No gap filling: agreed by both; the precedent for *filled* products (Moody et al. 2008; MCD43GF)
is the reason the unfilled product must be labelled as such.

### 2.7 MOD44W policy: QA classes and the annual masks

Astra: exclude low-confidence land (QA 3) from the land footprint; accept QA 1, 6, 7, 9; fetch
the annual 2000-2009 MOD44W masks and exclude any pixel that was water in any year. Fable: use
the 2015 `water_mask` as delivered; record QA class counts.

**Evidence.** The QA classes are from the delivered lookup (both verified it). The catalogue
entry says v6.1 is "a time series rather than a simple static representation" with masks for
terrain shadow, burn scars, cloud and ocean ice (verified); Astra's earlier review noted the
guide's caution against reading year-to-year differences as real change [that caution not
re-verified here].

**Middle.** Keep 2015 as the primary footprint (Chris's decision, on disk, verified grid). Fetch
the annual masks (ten small AppEEARS requests like the one already made) and compute the
"water in any year" union as a sensitivity footprint with its area difference reported; adopt it
only if Chris chooses, because the annual products carry classification noise that a union
would propagate as lost land. On QA 3 (low-confidence land): report its share; do not exclude it
by default (judgement: excluding it removes land for a confidence label, and its effect is
measurable in the pilot).

### 2.8 Equation of time and the noon window

Astra: apparent solar noon with the equation of time, a one-hour window centred on noon with
overlap weights across the two ERA5 accumulations, spatial interpolation of total and direct with
identical weights, ratio formed last. Fable: the accumulation hour containing mean solar noon,
nearest ERA5 point, equation of time ignored.

**Concede the equation of time** (a tested astronomical function, checked against the NOAA
calculator; trivial cost). **Middle on the window:** the overlap-weighted centred hour is a
judgement that adds code for an effect bounded by the sub-hourly variation of the diffuse
fraction; the nearest-ERA5-point lookup is simpler than interpolation and the 0.25-degree field
is smooth relative to a 500 m pixel. Propose the simple forms (containing hour, nearest point) as
the implementation, with Astra's forms tested on the pilot tiles; if the monthly albedo differs
by less than 0.002 anywhere, the simple forms stay. Astra's consistency rule (never mix a
clear-sky variable with a total-sky one; reject B greater than D beyond an encoding tolerance)
is conceded as written.

### 2.9 Aggregation geometry

Astra: exact polygon overlap of densified 1-degree cells with native pixels (`exactextractr`
or an equivalent `terra` implementation). Fable: assign each equal-area pixel to the cell
containing its centre; no resampling.

**Evidence.** Both are area-weighted because sinusoidal pixels are equal in area; the
difference is the roughly 2 % of a cell's pixels that straddle its edge. `exactextractr` is not
installed in `paleo-rf` (checked); `sf` 1.1.3 is.

**Hold, with a check.** Centre assignment is the readable implementation and exact up to the
edge pixels; the pilot computes the exact-overlap mean for a dozen cells (lake, coast, interior)
and reports the difference, which I expect below 0.001 except where the albedo changes sharply
at a cell edge. Astra's rules "sum numerators and denominators across tiles" and "do not
re-average quarter-degree means into one-degree means" are conceded; both designs compute the
0.25-degree product independently from native pixels.

### 2.10 Configuration keys and the preparation-manifest check

Astra: `analysis$albedo$source`, `state`, `spatial_support`, `recipe_id`,
`analysis$elevation$source`, with the recipe's scientific settings in the validated config; a
check that the prepared product matches the configured recipe. Fable: `calibration$albedo =
list(product, conditions)`, recipe settings only in the tool's manifest.

**Middle.** Two keys plus Astra's `recipe_id`: the pipeline names the product and its state;
`recipe_id` names the manifest the product must have been made by, and script 2 stops if the
product's manifest id differs (Astra's preparation-manifest check, conceded: the run manifests
hash files but cannot say which recipe made them). The recipe settings themselves stay in the
tool's manifest, not duplicated into `config.R`, because the pipeline does not read them and a
second copy can drift. Key names are Chris's to choose; `spatial_support` is I16c's
`albedo_support` under another name and should be one key.

### 2.11 Packages and tools

Astra names `httr2`, `exactextractr`, `testthat`, `yaml`, SHA-256 via `digest`, PSOCK workers.
None of `httr2`, `exactextractr`, `testthat`, `yaml` is installed in `paleo-rf` (checked);
`httr` 1.4.9, `curl`, `jsonlite`, `digest`, `sf`, `terra`, `ncdf4`, `future` and `data.table`
are. **Hold:** use what is installed plus `ecmwfr` and the GDAL drivers; the project's tests
are plain `stopifnot` scripts (`tests/test_*.R`), so `testthat` would be a new convention for
one tool. Astra's three entry points versus my five: naming only; the five map onto Astra's
three (download; prepare; check) and I would accept Astra's split with the inventory and the
mask preparation as sub-steps of "download" and "prepare".

### 2.12 Snow fraction and the mixing identity

Astra adds: publish snow status counts before the albedo QA filter (so differential rejection
of snowy retrievals is visible); keep joint sums so that mixtures are computed on common
support; call the quantity "snow fraction among sampled retrieval dates". **Concede all
three;** I had the counts but not the pre-filter counts nor the joint-support caution, which
matters for the sensitivity formula alpha = (1 - s) alpha_snowfree + s alpha_snowy.

## 3. Each recipe step against Chris's rule: precedent or judgement

| Step | Verified published precedent (what it supports) | Judgement (what remains ours) |
|---|---|---|
| MCD43A3/A2 v061 daily 500 m, shortwave broadband, noon black-sky and white-sky | Lucht et al. 2000 (doi:10.1109/36.841980), Schaaf et al. 2002 (doi:10.1016/S0034-4257(02)00091-3): the algorithm and broadbands; user guide: layers, scale, fill, noon reference | none |
| Blue-sky = (1 - f) BSA + f WSA | Lewis and Barnsley 1994 (cited by the guide; no DOI); user guide ("interpolated ... as a function of the fraction of diffuse skylight"); Roman et al. 2010 (doi:10.1016/j.rse.2009.11.014 per NASA record): isotropic form adequate to 0.004-0.013 | none |
| f from ERA5 ssrd and fdir, total sky, at local solar noon | ERA5 documentation: variables, units, accumulation convention (verified); the manuscript's own statement; Hasler et al. 2024 (doi:10.1038/s41467-024-46577-1) use a reanalysis direct/diffuse split with MODIS albedos (verified Methods) | the hour window, equation of time, nearest-point lookup; total-sky rather than aerosol-only f (Jia et al. 2022 used aerosol-based f per Astra [unverified]) |
| Quality: full inversions primary | User guide and NASA validation page recommend full inversions and use of the flags (verified) | accepting codes 2-7 (band 5/6 fill) as full inversions; the all-processed sensitivity (supported by Wang et al. 2018's all-quality accuracy, doi:10.1016/j.rse.2018.02.001) |
| Snow partition from the A2 flag | Wang et al. 2018: day-of-interest snow state in Collection 6 (verified); MCD43GF uses the snow flags to make a snow-free product (user guide, verified); Hasler et al. 2024: snowy and snow-free class albedos combined with snow fraction (verified) | minimum dates and years for each state; no gap filling (the filled precedents are Moody et al. 2008, doi:10.1175/2008JAMC1795.1, and MCD43GF) |
| Noon zenith at most 70 degrees | NASA validation page and user guide (verified); Wang and Zender 2010 (doi:10.1016/j.rse.2009.10.014) and Stroeve et al. 2005 (doi:10.1016/j.rse.2004.09.001) on high-SZA snow bias | the 75 and "none" sensitivities; the irradiance floor |
| Water screened at the native scale before averaging, with MOD44W | Moody et al. 2005 (doi:10.1109/TGRS.2004.838359): water screened with the MODIS land/water mask before spatial aggregation [the exact screening rule in that paper not re-read; both designs say so]; Carroll et al. 2009 and 2017 (doi:10.1080/17538940902951401, doi:10.1080/17538947.2016.1232756): the mask and its use in MODIS Collection 6 | the all-four-sub-pixels rule, the 2015 footprint, QA-3 treatment, coastal-cell minimum |
| Monthly mean per year, equal years, mean not median | Moody et al. 2008 (monthly and zonal averages of MCD43 retrievals); Jia et al. 2022 (daily climatology by averaging over years; details per Astra [unverified]) | the thresholds; the insolation-weighted alternative (physical argument from the kernel definition, no MODIS precedent found) |
| Equal-area aggregation from sinusoidal pixels to cells | Oleson et al. 2003 (doi:10.1029/2002GL016749): area-weighted aggregation of MODIS albedo to a model grid | centre assignment versus exact overlap |
| Start 5 March 2000; Terra-only before mid-2002 | User guide (verified) | none |
| Outputs with manifest and checksums | project practice (`R/run_manifest.R`) | none |

## 4. What each design found that the other missed

**Astra found, I missed:** the validated record starts 5 March 2000 and is Terra-only before
mid-2002 (verified); the MCD43A2 field name is `SNOW_BRDF_Albedo` in the file specification
with long name `Snow_BRDF_Albedo` (verified on the LAADS spec page; AppEEARS uses the latter);
the snow-status counts before the QA filter and the joint-support caution for mixtures; the
preparation-manifest check; the serial-versus-parallel determinism check and the
interrupted-batch check; the equation of time; the irradiance floor and the separate
missing-data reasons; the annual MOD44W masks as a shoreline-change check; Jia et al. 2022's
Zenodo deposit (record 6350821, 0.05-degree archive 6.0 GB and 0.5-degree 79 MB, daily actual and
snow-free, CC-BY-4.0: verified) with its stated differences (aerosol-based f, 75-degree screen,
2001-2020 [unverified]); SURFRAD as a genuinely independent check of both albedo and the ERA5
diffuse fraction; the A1 daily-energy pilot and its formula; Casado-Rubio et al. 2017
(doi:10.1175/JAMC-D-16-0297.1, resolved on Crossref; a DNI forecasting paper, its statement of
the horizontal-plane convention not checked).

**I found, Astra missed:** the `paleo-rf` GDAL lacks both the HDF4 and the netCDF drivers, so
the full-HDF route is blocked today and `ncdf4` is the only ERA5 reader; the legacy raster
contains water and sea ice with numbers (Lake Superior 0.008 in August, Hudson Bay 0.75 in
January and 0.008 in August, Great Salt Lake 0.05-0.12), sits on the ERA5 lattice, and its
January coverage edge coincides with empty MCD43 granules, so it applied no zenith rule; the
exact 38-tile set from the land-cover cells (Astra budgets 56 tiles, 13 TB, 400,000 files); the
measured per-day volumes by season; CMR and Harmony both saying no OPeNDAP/Harmony subsetting
exists for MCD43A3.061 (Astra left this pending); AppEEARS's full layer lists for both products
and the absence of documented size limits; the MOD44W mosaic's odd 250 m offset from the tile
origin; the C6.1 mandatory QA table in full with the even/odd rule; the arithmetic of what the
70-degree rule removes by month and latitude; the kernel-consistency argument for insolation
weighting (as a labelled alternative); AppEEARS's 48-hour URL and token validity.

## 5. Merged list of decisions for Chris (plain words)

1. Install `libgdal-hdf4` and `libgdal-netcdf` (and `ecmwfr`) into `paleo-rf` now; needed for
   the pilot whichever route wins.
2. Run the two-arm pilot (three tiles, three months, two years): AppEEARS subsets against HDF
   files; choose the route on identical values plus measured volume and queue time.
3. Quality: full inversions primary (codes 0, 2, 4, 6), all-processed as the sensitivity,
   both from one pass; revisit if full-only empties the winter north.
4. Reducer: plain monthly mean per year, equal years (primary); insolation-weighted pooled mean
   as a labelled alternative; no median.
5. Noon zenith 70 primary; 75 and "none" as sensitivities; irradiance floor on.
6. Record starts 5 March 2000; year counts published per month.
7. Thresholds: 10 dates per pixel-month-year, 5 years, 5 dates and 3 years per snow state,
   cell published at 50 % supported land with 90 % flagged as well supported; unmixed-land
   share reported, not used to exclude, until the pilot shows the distribution.
8. Water: 2015 footprint primary; fetch the annual masks and report the "water in any year"
   union as a sensitivity; QA-3 share reported, not excluded.
9. Equation of time in; containing hour and nearest ERA5 point as the implementation, Astra's
   centred-window form tested in the pilot.
10. Aggregation by pixel centre, checked against exact overlap on a dozen cells in the pilot.
11. Config: product, state and `recipe_id` keys; script 2 stops on a recipe mismatch; one
    spatial-support key shared with I16c.
12. The A1 daily-energy pilot: run after the primary product, judged on the forest-open
    contrast as well as the level; it changes the definition only on Chris's say.
13. What to commit: the cell table and the 0.25-degree rasters; native tiles and ERA5
    derivatives outside git with checksums.
14. Validation set: legacy comparison in Astra's four stages; Jia et al. 2022 from Zenodo
    (0.5-degree file first, 79 MB); SURFRAD; MCD43GF sample optional.

## 6. Genuinely open

- Whether Andria's raster was a mean or a median, and what it did with quality, snow, water
  and the ERA5 hour: only her surviving code can answer; both comparisons are diagnostic.
- Whether full-inversion-only leaves enough winter coverage north of 50 N for the calibration:
  a pilot measurement, then a decision.
- Whether the noon albedo or a daily energy-weighted albedo is the right calibration response
  for forcing: the A1 pilot informs, Andria and Chris decide; no published precedent for this
  project's use was found either way.
- Whether cells with much mixed-pixel land loss (Shield lake districts, coasts) should be
  excluded, flagged or kept: depends on the pilot's distribution and on how the forcing's
  area weights are reconciled (Astra's earlier review, outside this design).
- How the paper presents the snow sensitivity where the snow-free state is never observed
  (question for Andria, both designs).
- Jia et al. 2022's exact filters and diffuse-fraction source (needed to interpret any
  disagreement; the paper could not be read here).
