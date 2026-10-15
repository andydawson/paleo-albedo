# Albedo raster rebuild: independent design by Fable

From the same brief, before seeing Astra's design. Fable's text, unedited. Nothing here is a decision.

---

# Rebuilding the monthly blue-sky albedo raster: design (Fable, 2026-10-15)

Independent design, read-only. Repository read: worktree `wt-feature` (feature/single-config at
3bcf725). Data inspected: `data/blue_sky_monthly_2000-2009.tif`, `data/grid.RDS`,
`data/lct_modern_reveals_interp.RDS`, `data/mod44w/2015/*`. Product documentation, NASA CMR
metadata and Crossref were queried on 2026-10-15; nothing was downloaded and no PDF was opened.
Every claim below is labelled **Evidence** (verified, with its source), **Inference** (mine, from
evidence) or **Judgement** (a choice to be approved); **[unverified]** marks what I could not check.

## Summary

Rebuild the calibration albedo from MODIS MCD43A3 and MCD43A2 Collection 6.1 (daily, 500 m,
sinusoidal) and ERA5, processed locally one tile-month at a time (download, reduce, delete) for the
38 tiles covering the 2,860 land-cover cells: about 10 TB downloaded, nothing raw retained, about
0.3 TB of checkpoints. Recipe: daily blue-sky albedo per 500 m pixel as
(1 - f) x black-sky + f x white-sky, with f the ERA5 diffuse fraction ((ssrd - fdir)/ssrd) in the
hour containing local solar noon; keep processed retrievals (full and magnitude inversions,
counted separately); drop days whose local-noon solar zenith exceeds 70 degrees; remove every
500 m pixel not land in all four of its 250 m MOD44W 2015 sub-pixels; reduce to a monthly
climatology as the insolation-weighted mean over all valid days of that calendar month in
2000-2009 (minimum 20 days), in three versions: all conditions, snow-free days, and the snowy
share with day counts; then average the 500 m pixels (equal area in the sinusoidal grid) into
each 1-degree cell, and onto the legacy 0.25-degree lattice for comparison. Outputs: a small
per-cell table (committed), 0.25-degree rasters, 500 m tile climatologies (checksummed, outside
git), a manifest. Two new keys, `calibration$albedo$product` ("rebuilt" or "legacy") and
`conditions` ("all" or "snow_free"); the legacy configuration keeps the existing raster,
byte-identical. That raster demonstrably contains lake, sea-ice and shallow-ocean values, so
land-only agreement is expected only over interior land. The MCD43C3 climate-modelling grid is
rejected because it averages shallow water into its cells; no NASA subsetting service exists for
MCD43A3.

## 0. What the inspection of the existing files shows (Evidence)

The existing raster, `data/blue_sky_monthly_2000-2009.tif` (terra 1.9.50):

- 500 x 280 x 12, 0.25 degree, extent -175.125 to -50.125 E, 10.125 to 80.125 N; pixel centres
  on whole quarter degrees, i.e. on the ERA5 0.25-degree grid points. Twelve bands all named
  `median`, Float32, NoData NaN, LZW.
- Missing values per band: January 95,476 of 140,000 cells (valid 44,524), July 78,966 (valid
  61,034), November 89,171, December 104,318 (valid 35,682). 78,878 cells are missing in every
  month (open ocean, no MODIS land tile), 35,653 are valid in all twelve.
- It contains water. Sampled values (Jan, Jul, Aug): Lake Superior centre (-87.5, 47.5) 0.021,
  0.008, 0.007; Hudson Bay (-85, 60) 0.745 in Jan-Apr, 0.62 May, 0.42 Jun, 0.078 Jul, 0.008 Aug,
  missing Dec (sea ice then open water); Great Salt Lake (-112.5, 41) 0.117 Jan, 0.05 Aug; open
  Pacific (-140, 40) missing. Interior prairie (-100.5, 45.5) 0.41 Jan, 0.14 Jul; Alaska North
  Slope (-160, 70) missing Jan and Dec, 0.66 Feb, 0.18 Jul.
- In January the valid cells stop at about 62.5 N (question B2b in the scientific question file);
  this is where the MCD43 granules themselves become empty in mid-January (CMR granule sizes for
  2005-01-15: tiles v02, 60-70 N, 0.9-1.0 MB against 78-130 MB for v03-v05), so the legacy
  raster applied no solar-zenith rule of its own (Inference).

The MOD44W delivery, `data/mod44w/2015/`: one AppEEARS mosaic (request e44f2b75, AppEEARS 3.131,
layers `water_mask` and `water_mask_QA`, native sinusoidal, 231.656 m, 29,761 x 74,191 pixels),
379,133,327 land and 448,023,863 water pixels (the mosaic covers much ocean); QA classes from the
delivered lookup: 1 high-confidence observation, 2 low-confidence water, 3 low-confidence land, 4
ocean mask, 5 ocean mask but no water detected, 6 burned scar (MCD64A1), 7 urban/impervious, 9
model-predicted water where DEM slope indicates no water. The mosaic's origin sits an odd number
of 250 m pixels from the sinusoidal grid origin (7,447 columns, 5,279 rows), so 2 x 2 blocks for
the 500 m grid must be formed from the global tile grid, not from the mosaic's first pixel
(section 2.6). The product's user guide and the catalogue entry (doi:10.5067/MODIS/MOD44W.061,
Carroll et al. 2024) describe v6.1 as an annual time series from a decision-tree classifier with
masks for terrain shadow, burn scars, cloud and ocean ice.

The 1-degree target: `lct_modern_reveals_interp.RDS` has 2,860 cells, centres -171.5 to -53.5 E,
18.5 to 78.5 N. Projecting their edges to the sinusoidal grid gives 38 MODIS tiles: h07v05 h07v06
h08v03-h08v07 h09v02-h09v07 h10v02-h10v06 h11v02-h11v05 h12v01-h12v05 h13v01-h13v04 h14v01-h14v04
h15v01 h15v02 h16v01. (The bounding-box CMR query returns 56, including empty Pacific tiles and
spurious high-latitude ones.) A 1-degree cell holds about 50,000 500 m pixels at 30 N, 41,000 at
45 N, 29,000 at 60 N and 20,000 at 70 N.

The R environment (`paleo-rf`): terra 1.9.50 on GDAL 3.13.3 built as conda-forge `libgdal-core`,
which has **no HDF4 and no netCDF driver** (190 drivers listed; HDF4/HDF5/netCDF absent although
the `hdf4` and `libnetcdf` libraries are installed as dependencies); `ncdf4` 1.24, `httr` 1.4.9,
`curl` 8.0, `jsonlite` 2.0, `sf` 1.1.3, `future` 1.75, `data.table` 1.18 present; `ecmwfr`,
`httr2`, `earthdatalogin`, `rstac` absent. Workstation: 48 cores, 125 GB RAM, 1.3 TB free.

## 1. The recipe, step by step

### 1.1 Products and layers

**Evidence.** MCD43A3 v061 (doi:10.5067/MODIS/MCD43A3.061): daily, 500 m, sinusoidal tiles, one
file per tile and day of interest, built from 16 days of Terra and Aqua observations with the day
of interest weighted most (user guide introduction; Wang et al. 2018, doi:10.1016/j.rse.2018.02.001);
the CMR record of granule MCD43A3.A2005189 spans 2005-06-30 to 2005-07-15, confirming the file
date is the day of interest (ninth day). Layers (names as in the GEE catalogue copy of the
specification): `Albedo_BSA_shortwave`, `Albedo_WSA_shortwave` (black-sky at local solar noon and
white-sky, 0.3-5.0 um; int16, scale 0.001, valid 0-32766, fill 32767) and
`BRDF_Albedo_Band_Mandatory_Quality_shortwave`. MCD43A2 v061 (doi:10.5067/MODIS/MCD43A2.061)
carries `Snow_BRDF_Albedo` (0 snow-free retrieval, 1 snow retrieval), `BRDF_Albedo_LocalSolarNoon`
(solar zenith angle at local solar noon, degrees), `BRDF_Albedo_LandWaterType` (0 shallow ocean, 1
land, 2 ocean coastlines and lake shorelines, 3 shallow inland water, 4 ephemeral water, 5 deep
inland water, 6 moderate or continental ocean, 7 deep ocean), per-spectral-band quality and
valid-observation bit fields. Granule sizes on 2005-07-08: MCD43A3 median 53 MB, up to 110 MB
per tile; MCD43A2 median 18 MB, up to 41 MB. The algorithm processes "all land and coastal areas
and shallow water regions (pixels within 5 km of land or less than 50 m deep)" (user guide
introduction), which is why the legacy raster holds Hudson Bay and lake values.

**Recommendation.** Read from MCD43A3: BSA shortwave, WSA shortwave, mandatory quality
shortwave. From MCD43A2: snow flag, local-solar-noon zenith angle, and (once per tile, as a
diagnostic) the land/water type. Nothing else; the spectral bands are not needed.

### 1.2 Shortwave broadband

**Evidence.** The 0.3-5.0 um broadband is the quantity radiative kernels are defined for (the
kernels in `R/kernels.R` are shortwave surface-albedo kernels; CACK is built from CERES shortwave
fluxes, Bright and O'Halloran 2019, doi:10.5194/gmd-12-3975-2019). The narrow-to-broadband
conversion is part of the product (Lucht et al. 2000, doi:10.1109/36.841980; Schaaf et al. 2002,
doi:10.1016/S0034-4257(02)00091-3). **Recommendation.** Shortwave broadband only, as the
manuscript says. Alternative considered and rejected: computing the broadband from the seven
spectral albedos ourselves (no gain, more volume).

### 1.3 Quality filter

**Evidence.** The mandatory QA for Collection 6.1 (user guide, MCD43A3 page, quoted): 0 full BRDF
inversion; 1 magnitude inversion; 2 full inversion with band 6 fill; 3 magnitude with band 6 fill;
4 full with band 5 fill; 5 magnitude with band 5 fill; 6 full with bands 5 and 6 fill; 7 magnitude
with bands 5 and 6 fill ("due to non-functional or noisy detectors", Aqua band 6); 255 fill. So
"full inversion" = even code, "magnitude inversion" = odd code. The team "urge[s] users to use the
band specific quality flags to isolate the highest quality full inversion results" (user guide).
Wang et al. 2018 report, for Collection 6 at tower sites, shortwave blue-sky RMSE below 0.032 and
bias within 0.008 "for all quality results", i.e. including magnitude inversions. Wang et al. 2012
(tundra, doi:10.1016/j.rse.2011.10.002) and 2014 (grassland, cropland, forest in dormant and
snow-covered periods, doi:10.1016/j.rse.2013.08.025) evaluate MCD43A under snow and find the
product usable with larger errors for magnitude inversions [the numbers in those two papers were
not re-read here: unverified]. Magnitude inversions dominate at high latitude in winter and under
rapidly changing snow (user guide, MCD43GF page: "in the high latitudes ... primarily poor quality
BRDF retrievals, usually dominated by very low illumination").

**Recommendation (Judgement).** Default: keep every processed retrieval (codes 0-7; fill
excluded), and record per pixel-month the share of full inversions, so the quality of every cell
value is known. Reason: the climatology needs winter and shoulder-season days at high latitude,
where full-only would leave few days and bias the month towards clear, stable weather; the
product team's own accuracy statement covers all qualities. Option `quality = "full_only"` (even
codes) as a sensitivity. Codes 2-7 (band 5/6 fill) are accepted by default because the broadband
is still produced from the remaining bands and they are frequent for Aqua; `quality =
"full_only_complete"` (code 0 only) is the strictest option. Decision for Chris (section 8).

### 1.4 Snow handling and the snow-free version

**Evidence.** Collection 6 "utilizes the day of interest snow/snow-free status to capture snow
albedo", replacing Collection 5's majority-of-16-days rule (Wang et al. 2018, abstract); the
MCD43A2 snow flag states which state the retrieval represents. NASA's own snow-free product,
MCD43GF v061 ("Gap-Filled Snow-Free Daily L3 Global 30ArcSec CMG", doi:10.5067/MODIS/MCD43GF.061
[DOI inferred from the catalogue naming pattern: unverified]), removes "pixels with ephemeral snow
... using the MCD43D41 snow flags" and fills the gaps by temporal then spatial fitting (user guide,
MCD43GF page). Jia et al. 2022 (doi:10.1029/2021JD035987) built a 500 m daily "actual and
snow-free" blue-sky climatology from 20 years of MODIS in Google Earth Engine, validated at 38
sites (RMSE 0.031, bias -0.003, r 0.96; abstract) [their snow-free definition and diffuse-fraction
source could not be read: unverified]. Hasler et al. 2024 (doi:10.1038/s41467-024-46577-1, Methods
via Europe PMC) combine snow-covered and snow-free class albedos with monthly snow fractions and
direct/diffuse fractions, exactly the structure Astra proposed for the sensitivity test.

**Recommendation.** Per pixel and calendar month, from the same daily values, produce (a)
all-conditions albedo (every valid day), (b) snow-free albedo (valid days with snow flag 0), (c)
snowy albedo (snow flag 1; cheap and useful for the mixing formula), (d) the snowy share =
insolation-weighted share of valid days flagged snow, and (e) the counts: valid days, snow-free
days, full-inversion days. Where a pixel-month has fewer than the minimum of snow-free days the
snow-free value is missing; **no gap filling** in the tool (Judgement: a filled value is a model,
and the sensitivity test should see where the data end). If Andria wants a spatially complete
snow-free field, fill in a later step from the pixel's nearest snow-free months, documented as a
fill, or use MCD43GF. A frozen lake stays excluded in every month (Astra's point: the target is
terrestrial albedo).

### 1.5 Polar night and low sun

**Evidence.** "The MCD43 product is not recommended for solar zenith angles beyond 70 degrees"
(user guide, MCD43GF page; the gap-filled product's own QA has "7 = SZA extension between 70 and
82 degrees" for values it fills in). Snow albedo retrievals at high solar zenith are biased low
relative to theory and Greenland measurements (Wang and Zender 2010, doi:10.1016/j.rse.2009.10.014;
reply by Schaaf et al. 2011, doi:10.1016/j.rse.2011.01.002; Stroeve et al. 2005,
doi:10.1016/j.rse.2004.09.001). MCD43A2 gives the local-noon zenith angle per pixel and day. The
granules themselves become empty when the noon zenith angle reaches about 80 degrees (section 0).

**Consequence of a 70-degree rule (arithmetic, mid-month declination):** December values vanish
north of about 46.8 N, January north of 48.8 N, November north of 51.5 N, February north of 57 N,
March north of 67.5 N; nothing is lost from April to September. The legacy raster has values up to
60-62.5 N in December and January, i.e. it kept everything MODIS produced. Under the 70-degree
rule the December calibration model would be fitted only south of about 47 N, a larger gap than
today's (question B2b).

**Recommendation (Judgement).** Default `max_noon_zenith = 70`, the product team's recommendation,
with the counts recorded; a configured alternative (`none`) reproduces the legacy coverage for the
comparison. The winter months beyond 70 degrees receive little insolation and small kernel
values, and the pipeline already extrapolates there; the snow sensitivity test is where winter is
examined. This is a decision for Chris because it changes the calibration's winter coverage.

### 1.6 Water screening at 500 m, and coastal pixels

**Evidence.** Moody et al. 2005 (doi:10.1109/TGRS.2004.838359) screened water with the MODIS
land/water mask before aggregating albedo to coarser grids; Oleson et al. 2003
(doi:10.1029/2002GL016749) reconciled land/water definitions with area weighting. The MCD43
algorithm's own land/water type (MCD43A2) comes from the MODIS Collection 6 land-water mask, which
was derived from MOD44W (Carroll et al. 2017, doi:10.1080/17538947.2016.1232756; Carroll et al.
2009, doi:10.1080/17538940902951401). MOD44W and MCD43 share the sinusoidal tile grid: 4,800 x
4,800 and 2,400 x 2,400 pixels per 1,111,950.52 m tile, so each 500 m pixel contains exactly four
250 m pixels when blocks are counted from the tile origin.

**Recommendation.** For each 500 m pixel count the water sub-pixels (0-4) from `water_mask` (0
land, 1 water). Default rule `water_rule = "all_land"`: a pixel enters the land albedo only if all
four sub-pixels are land (Judgement: a retrieval with any water in it has a mixed BRDF and the
MCD43 shoreline class is exactly that; the loss is a one-pixel fringe around every water body,
small against a 1-degree cell). Alternative `"majority"` (at least three land). Record, per tile
and per 1-degree cell, the pixels removed by the rule and the cross-tabulation against
`BRDF_Albedo_LandWaterType` (1 = land); large disagreement would mean a grid misalignment or a
real 2015-versus-2000s change, and both must be looked at. MOD44W QA is not used in the rule
(the mask already embodies it) but its class counts per tile are recorded. Coastal 1-degree cells
are kept (Astra's point 4): a cell-month gets a value when it has at least `min_land_pixels`
(default 200, about 0.5 % of a mid-latitude cell; Judgement) valid land pixels, and the table
carries land-pixel and valid-pixel counts so stricter rules can be applied downstream.

### 1.7 The diffuse fraction from ERA5

**Evidence.** The blue-sky (actual) albedo is the interpolation between black-sky and white-sky
by the fraction of diffuse skylight (Lewis and Barnsley 1994; Lucht et al. 2000; Schaaf et al.
2002; user guide: "actual albedo is a value which is interpolated between these two as a function
of the fraction of diffuse skylight", assuming isotropic diffuse light; Roman et al. 2010,
doi:10.1016/j.rse.2009.11.014, examine the anisotropic correction and find biases improve by only
0.004-0.013). MCD43 black-sky albedo is given at local solar noon (user guide), so the matching
diffuse fraction is the one at local solar noon. ERA5 single levels (doi:10.24381/cds.adbb2d47;
Hersbach et al. 2020, doi:10.1002/qj.3803) provide `surface_solar_radiation_downwards` (ssrd,
parameter 169) and `total_sky_direct_solar_radiation_at_surface` (fdir, 228021), both J m-2
accumulated "over the hour ending at the validity date/time" (ECMWF ERA5 documentation), on a
regular 0.25-degree grid, 1940 to present, CC-BY. fdir is the direct component on a horizontal
plane, so diffuse = ssrd - fdir [standard ECMWF definition; the parameter-database page could not
be fetched: unverified wording]. ERA5-Land has ssrd but no direct component (ECMWF forum, as
summarised in search results [unverified]). "Total sky" means clouds are included: this f is the
all-sky diffuse fraction, larger than the clear-sky, aerosol-only fraction of the MODIS team's
blue-sky tool.

**Recommendation.** For every ERA5 grid point and day, f = (ssrd - fdir)/ssrd for the
accumulation hour that contains local solar noon, noon taken as 12 h - longitude/15 UTC (equation
of time, up to 16 min, ignored: Judgement, negligible against the hourly step); f missing where
ssrd is zero. Each 500 m pixel takes the f of the ERA5 point whose cell contains the pixel centre
(a lookup table per tile, computed once). Also keep each day's ssrd total as the insolation weight
(1.8). Using the all-sky f is consistent with the all-sky kernels chosen on 2026-10-14 and with the
manuscript's wording ("fraction of direct and diffuse shortwave from ERA5"); the clear-sky
alternative (ssrdc with fdir's clear-sky twin) is not offered. Alternative considered: the
monthly-mean-by-hour-of-day ERA5 product (tiny download) applied to monthly mean BSA and WSA;
rejected as default because it cannot follow snow days and departs from the manuscript's daily
formula, but it is the fallback if the hourly download is refused.

### 1.8 From days to a month, and from ten years to a climatology

**Evidence.** A radiative kernel gives the monthly flux response to a uniform albedo change; the
GCM kernels are built by perturbing albedo at every time step and averaging the flux response
over the month, and CACK from CERES monthly fluxes (Bright and O'Halloran 2019), so the albedo
that belongs with them is the month's insolation-weighted (flux-ratio) albedo, not the plain mean
of daily values. The legacy band names say `median`; its reducer is unknown. Consecutive MCD43
daily values share up to 15 days of observations, so day counts overstate independence.

**Recommendation (Judgement).** Per pixel and calendar month, the climatology is the mean over
all valid days of that month in 2000-2009, pooled across years, weighted by the day's ERA5
insolation (ssrd); a pixel-month needs at least `min_valid_days = 20` pooled days (about two per
year) or it is missing; the year coverage is recorded. Pooling rather than year-then-climatology
keeps the rule readable and copes with missing years without a second threshold. The plain
(unweighted) mean is produced alongside for the legacy comparison. A median is not produced by the
tool (it needs all 300-odd daily values of a pixel-month in memory, which forces a different loop
order); if the legacy comparison suggests the legacy was a median, a one-off check on a few
tiles can be made with the alternative loop order (section 8, decision 8).

### 1.9 From sinusoidal pixels to the 1-degree land mean (and to 0.25 degree)

**Evidence.** The sinusoidal projection is equal-area: every 500 m pixel has the same area
(463.3127 m square, 0.2147 km2), so a plain mean over pixels is an area-weighted mean. The
land-cover cells are 1-degree cells with centres at x.5 (section 0). The legacy raster's 0.25-degree
lattice has centres at multiples of 0.25 degree.

**Recommendation.** Compute, once per tile, the longitude and latitude of every pixel centre;
assign each pixel to the 1-degree cell (floor of lon and lat) and to the 0.25-degree legacy cell
containing its centre; then the cell value is the mean of the valid land pixels' climatology, with
counts. No reprojection and no resampling anywhere, so the result is exact up to pixels straddling
a cell edge (about 2 % of a cell's pixels, assigned by their centre: Inference). The 0.25-degree
product on the legacy lattice serves the comparison in section 4 and the maps in script 2. The
same land mask and the same assignment serve the land-mean elevation of I16c, which removes the
"quantify the resample() approximation" step of the I16 plan.

### 1.10 Outputs, format and provenance

- `data/albedo_rebuild/bluesky_cells_1deg.RDS` (and `.csv`): one row per cell and month
  (2,860 x 12): x, y, month, albedo_all, albedo_all_unweighted, albedo_snowfree, albedo_snow,
  snow_share, n_days, n_days_snowfree, share_full, land_pixels, valid_pixels. Small; committed.
- `data/albedo_rebuild/bluesky_quarter_degree_<variant>.tif`: 12-band Float32 on the legacy
  lattice, variants all, snowfree, snow_share, n_days; LZW; about 7 MB each; committed (or LFS).
- `data/albedo_rebuild/tiles/<tile>_climatology.tif`: 500 m, 12 months x the same layers,
  int16-scaled; not in git; md5 in the manifest.
- `data/albedo_rebuild/era5/diffuse_fraction_noon_daily.nc`, `insolation_daily.nc` (0.25 degree,
  3,653 days; about 1.7 GB each); not in git; md5 in the manifest.
- `data/albedo_rebuild/MANIFEST.json` and `README.md`: product versions and DOIs; the granule
  inventory (ids, LP DAAC production stamps from the file names, sizes, the CMR checksum where
  given), ERA5 request parameters, the MOD44W request id, every setting of the tool, code version
  (commit), R and package versions, md5 of every output, counts of pixels removed by each rule,
  and the per-tile cross-tabulation of 1.6.

## 2. Getting the data locally: routes compared

Volumes are from CMR granule metadata for one July day (2005-07-08) and one January day
(2005-01-15); counts are of files. Ten years = 3,653 days.

| Route | Download volume | Disk kept | Processing | Verdict |
|---|---|---|---|---|
| **A. Full HDF files, streamed per tile-month** (38 tiles) | MCD43A3 about 2.4 GB per July day over the 38 tiles, 1.6 GB per January day; roughly 7 TB for ten years; MCD43A2 about a third of that (0.95 GB per day for 56 tiles): about 2.4 TB; total about 10 TB, in about 278,000 files | nothing raw; checkpoints 4,560 tile-months x about 60 MB = about 0.3 TB, deletable after step 5 | read 3 + 2 layers per tile-day (about 35 MB in memory), vectorised arithmetic; the bottleneck is bandwidth, not CPU; 4-8 workers each on one tile-month | **Recommended** |
| B. AppEEARS area requests (layer subsets as GeoTIFF/NetCDF) | the same pixels compressed: estimate 0.4-0.7 GB per day for 5 layers over the domain, 1.5-2.5 TB total, through many queued requests; limits on request size not found in the documentation [unverified] | same as A | avoids HDF4 and tile handling; adds request management, 48-hour download URLs, and the mosaic's odd-pixel offset (section 0) | fallback if the HDF4 driver cannot be installed |
| C. OPeNDAP / Harmony variable subsetting | not available: the CMR collection record for MCD43A3.061 (C2278860820-LPCLOUD) lists no OPeNDAP, ESI or Harmony service features and granules carry only HTTPS and S3 links; LAADS OPeNDAP exists for the 30 arc-second MCD43D layer files (1 km, lat/lon) [coverage of all years unverified] | - | would give about 0.6 GB per day at 1 km | not available at 500 m; the MCD43D route is a coarser substitute, not pursued |
| D. MCD43C3 0.05-degree CMG | 181 MB per day global, 0.65 TB | same | trivial | **rejected for land-only**: the user guide states that "along coastlines ... the 30 arcsecond pixels that lie over shallow water will be averaged into the 0.05 degree CMG pixel", and 5.6 km cells cannot be screened with a 250 m mask; usable only as a cheap cross-check (Percent_Snow, Percent_Inputs) |
| E. MCD43GF gap-filled snow-free (30 arc-second, daily) | about 1.9 GB uncompressed per layer-day globally; a validation sample of 2 days per month for 3 years, two layers, is of order 0.2 TB [compression unverified] | sample only | subset to the domain | optional, for validating the snow-free version (section 4) |
| ERA5 hourly ssrd and fdir, 2000-2009, domain -172..-52 E, 17..79 N | 119,769 points x 87,672 hours x 2 variables: about 84 GB as NetCDF float32, half that packed; 120 monthly CDS requests via `ecmwfr` | the two derived daily files (3.4 GB) kept; hourly files deletable | ratio and hour selection | required for A and B |

**Route A in detail.** Inventory: one CMR query per product, year and tile (JSON; `jsonlite`),
filtered to the 38 tiles and to titles whose date is the day of interest (CMR's temporal filter
returns every granule whose 16-day window overlaps the day: 896 for one day). Download: HTTPS from
`data.lpdaac.earthdatacloud.nasa.gov` with the Earthdata login read from `~/.netrc` by libcurl
(`curl::curl_download` with the `netrc` handle option, or `httr::GET(config(netrc = TRUE))`),
following the URS redirect; retries and a size check against the CMR size; the S3 links work only
inside AWS us-west-2 and are ignored. Reading: terra via GDAL's HDF4 driver, subdataset names of
the form `HDF4_EOS:EOS_GRID:"<file>":MOD_Grid_BRDF:Albedo_BSA_shortwave` [name pattern standard,
to be confirmed on the first file]. This needs the conda-forge package `libgdal-hdf4` (and
`libgdal-netcdf` if terra rather than `ncdf4` is to read ERA5) added to the `paleo-rf`
environment: one line in the README's environment section, no scaffolding. Loop order: tile, then
year, then month; one tile-month per worker (download about 2.5 GB, process, write one checkpoint
raster of the eight monthly accumulators, delete the files); restart-safe because a finished
tile-month is skipped. Then per tile: the twelve climatologies from the 120 checkpoints; then the
cell table and the 0.25-degree rasters.

## 3. How it fits the pipeline

**Benefit to the paper and to Andria's use** (one sentence): the calibration response becomes a
documented, land-only, reproducible product with a snow-free twin, so the methods section can
state the recipe and the snow sensitivity test can be run from config alone.

**Tools (`tools/`, run once, in order; plain `Rscript`, any OS; functions in `R/albedo_rebuild.R`
so nothing is pasted between tools):**

1. `tools/albedo_inventory.R`: CMR query, the granule table (`data/albedo_rebuild/inventory.csv`,
   committed; about 278,000 rows, some MB).
2. `tools/albedo_water_mask.R`: MOD44W 2015 mosaic to one 500 m land mask per tile (water
   sub-pixel count 0-4, aligned to the tile grid) plus the pixel-to-cell lookup (1-degree id,
   0.25-degree id, ERA5 point id) per tile.
3. `tools/albedo_era5.R`: 120 monthly CDS requests through `ecmwfr` (the key read from
   `~/.cdsapirc` at run time and handed to `wf_set_key()`; nothing stored in the repository),
   then the two derived daily files; hourly files deleted.
4. `tools/albedo_tile_months.R [--workers N]`: the streaming step of section 2.
5. `tools/albedo_climatology.R`: per-tile climatologies, the cell table, the 0.25-degree rasters,
   the manifest and README.

Each tool checks its inputs' md5 against the manifest before using them and appends its own
record; the manifest format follows `R/run_manifest.R`'s style (readable Markdown header, JSON
body) without reusing `run_begin()`, which is tied to the numbered scripts and the path catalogue.

**Config keys** (`config.R`, `configs/legacy_config.R`, `R/config_schema.R`):

```r
calibration = list(model_id = "mod8", comparison_method = "ML", ice_mask = "centre_in_1ka_polygon",
                   albedo = list(product = "rebuilt",      # "rebuilt" | "legacy" (data/blue_sky_monthly_2000-2009.tif)
                                 conditions = "all"))      # "all" | "snow_free"; snow_free needs product = "rebuilt"
```

Legacy config: `product = "legacy", conditions = "all"`. The I16c key `albedo_support`
("land_mean" | "centre_pixel") applies to the legacy product only (schema combination rule: the
rebuilt product is land-mean by construction). Tool settings (quality rule, zenith limit, water
rule, minimum days, minimum pixels) are the tool's arguments, recorded in the manifest, not
pipeline keys: the pipeline reads a product, and a different product is a different manifest.

**Path catalogue** (`R/paths.R`): inputs `bluesky_raster` (unchanged), `albedo_cells`
(`data/albedo_rebuild/bluesky_cells_1deg.RDS`), `albedo_quarter_all`, `albedo_quarter_snowfree`.
`ALBEDO_TAG` stays `bluesky`: file names do not change, so the anchors keep working.

**Script 2.** `if (cfg$analysis$calibration$albedo$product == 'legacy')` keeps today's code path
exactly (resample, maps, extract, the 1e-4 floor); otherwise it reads the cell table, picks the
column by `conditions`, joins on (x, y) to the modern land-cover table (stopping if any cell is
unmatched), and writes the calibration table in the same shape (x, y, elev, ET, OL, ST, jan ...
dec). The `_coarse` table, a diagnostic, is written with the unweighted column for the rebuilt
product (documented) so script 3 still runs. Maps come from the chosen 0.25-degree raster. The
run manifest records the product, the column and the manifest id of the rebuild.

**Regression contract.** Under `configs/legacy_config.R` the calibration table must stay
byte-identical to `interp-allmonths-2026-09-20` (the branch is skipped entirely), checked with a
`tools/verify_change.R` spec running scripts 1-2 with anchors. The rebuilt product gets its own
anchor once Chris accepts it (its table checksum), and a spec running script 2 under `config.R`
with `checks` that count cells, missing cell-months per month, and value ranges.

**Tests** (`tests/test_albedo_rebuild.R`, synthetic rasters, seconds): blue-sky arithmetic on
known numbers; QA classification (even = full, 255 = missing); the 2 x 2 water rule including the
odd-pixel offset of the mosaic; the noon-hour choice for sample longitudes (e.g. -75 E gives the
accumulation ending 17:00 UTC); the weighted monthly mean with missing days and the minimum-day
rule; the pixel-to-cell assignment at a cell edge; a cell with land but no valid pixels gives
missing, not zero; the manifest's md5 check fails on a changed file.

**Packages.** `terra` (with the HDF4 driver), `ncdf4` or `terra` for ERA5, `curl`/`httr` for
downloads, `jsonlite` for CMR and the manifest, `ecmwfr` (CRAN) for CDS, `parallel::mclapply`
(Linux) or `future` for the workers, `tools::md5sum`. No `rstac`, `earthdatalogin` or AppEEARS
client is needed.

## 4. Validation

1. **Internal.** Per month: maps of valid-pixel share, snowy share, full-inversion share; value
   ranges 0.02-0.95; cells with fewer than `min_land_pixels`; the land-pixel count against the
   land fraction implied by `cell_area` in the posterior file if Andria explains it.
2. **Against the legacy raster, on its own lattice.** First a "legacy-like" variant of the rebuild
   (all qualities, no zenith rule, unweighted mean, water not removed), band by band. Expect over
   interior all-land 0.25-degree cells, April to October: r above 0.95, |bias| below 0.02
   (Inference: same product, same formula). Expect larger differences November to February
   (reducer, snow days, low sun) and wherever the legacy sampled water. Then the land-only variant:
   differences must concentrate at lakes and coasts, darker in summer where open water was
   averaged in (Lake Superior cell: legacy 0.008 in August against a land value near 0.1-0.15),
   brighter in winter and spring where lake and sea ice were (Hudson Bay 0.75 in January), and
   missing where a cell has no land.
3. **Independent.** The snow-free version against MCD43GF (NASA's snow-free, gap-filled product,
   route E sample) in May-September where both are observation-based; the all-conditions version
   against Jia et al. 2022 if their data can be obtained [availability unverified]; MCD43C3's
   Percent_Snow against our snowy share on a few months (cheap); optionally monthly albedo at a
   handful of SURFRAD/AmeriFlux towers (Jia used 38 sites; RMSE about 0.03 is the published
   benchmark).
4. **Spot checks** (cells): Lake Superior (-87.5, 47.5); Hudson Bay (-85, 60); Great Salt Lake
   (-112.5, 41); Canadian Shield lake district (e.g. -95.5, 55.5: expect several percent of
   pixels removed and a visible change); a prairie cell (-100.5, 45.5: expect near-identity with
   the legacy); a Baffin ice-cap cell (albedo 0.6-0.8 all year, snow share near 1); Alaska North
   Slope in November-January (missing under the zenith rule, present without it); a Pacific
   coast cell with little land (count and value).
5. **Stop rules** (decided before the run). Stop and reconcile with Andria if: the legacy-like
   variant has r below 0.9 or |bias| above 0.03 in any of May-September over interior land (the
   recipe then differs from hers in a way we have not understood); water removal changes fewer
   than 1 % of cells by more than 0.01 (mask misaligned or not applied); the snow-free version
   against MCD43GF has |bias| above 0.03 or r below 0.9 in summer; or more than 10 % of cell-months
   south of 60 N are missing in March-October (rules too strict).

## 5. Risks

- GDAL in `paleo-rf` lacks the HDF4 and netCDF drivers; the fix is a conda package, but it must be
  installed and the environment line in the README updated before anything else. Fallback: route B.
- About 10 TB through one Earthdata login: throttling, transient failures; the inventory with
  sizes and retries makes the run resumable, but a service interruption stalls it.
- The legacy recipe stays unknown; the comparison diagnoses, it cannot certify.
- Quality codes 2-7 (Aqua band 6 missing): the broadband with one spectral band missing is of
  lower quality; the share is recorded, the choice is explicit (decision 3).
- The 70-degree rule removes most of Canada in December and January from the calibration
  (section 1.5); the choice is explicit (decision 4).
- MOD44W is 2015; reservoirs, drained lakes and shoreline changes since 2000-2009 are mislabelled
  locally; ephemeral water is classified by that year. The cross-tabulation against MCD43A2's
  land/water type (Collection 6 mask, early-2000s inputs) measures this.
- Blue-sky at local solar noon is lower than the day's flux-weighted albedo because albedo rises
  with solar zenith angle; computing a daily-integrated albedo would need the BRDF parameters
  (MCD43A1, about three times the volume). Stated as a limitation, consistently applied to every
  cell, so it affects the calibration's level more than its land-cover contrast.
- The 16-day window makes neighbouring days dependent; counts are of days, not of observations.
- A 1-degree cell with very little land is noisy; the table carries counts so the calibration
  can drop or weight such cells (a decision outside this design, with Astra's area-weighting
  point for the forcing).
- Processing order tile-year-month cannot produce a median; if the median matters, a second pass
  is needed (decision 8).

## 6. Open questions for Andria

1. Any surviving code or notes for the existing raster: the reducer (mean or median, over days
   and over years), the quality filter, snow handling, water handling, the ERA5 variables and hour.
2. Is the all-sky diffuse fraction (clouds included, ERA5 ssrd and fdir) what the methods section
   means, as opposed to the clear-sky, aerosol-only fraction of the MODIS team's tool?
3. For the snow-free version, leave winter pixel-months without snow-free days missing (proposed)
   or fill them, and if so how?
4. Does the paper define the monthly albedo as the insolation-weighted mean (proposed, matching
   the kernels) or the plain mean of daily values?
5. Keep or drop retrievals whose broadband was made without Aqua band 6 (quality codes 2-7)?

## 7. Decisions for Chris

1. Route A (full HDF files streamed per tile-month, 38 tiles, about 10 TB downloaded, nothing
   raw kept) rather than AppEEARS; the CMG product is rejected for land-only work.
2. Add `libgdal-hdf4` (and `libgdal-netcdf`) plus `r-ecmwfr` to the `paleo-rf` environment and
   record the line in the README.
3. Quality default: all processed retrievals (codes 0-7) with full-inversion share recorded;
   `full_only` as the sensitivity.
4. Zenith rule default 70 degrees at local solar noon (loses December north of about 47 N), with
   `none` as the comparison setting.
5. Water rule: all four 250 m sub-pixels land; coastal cells kept with at least 200 valid land
   pixels.
6. Diffuse fraction: ERA5 hourly ssrd and fdir at the hour containing local solar noon, daily
   (about 84 GB download), rather than the monthly-by-hour product.
7. Reducer: insolation-weighted mean pooled over all valid days of the month in 2000-2009,
   minimum 20 days; unweighted mean alongside; no median.
8. Whether a one-off median check on a few tiles is wanted for the legacy comparison.
9. Config shape: `calibration$albedo = list(product, conditions)` with the legacy file as
   `product = "legacy"`, and I16c's `albedo_support` restricted to the legacy product.
10. What to commit: the cell table and the four 0.25-degree rasters (about 30 MB) in git; the
    500 m tiles and ERA5 derivatives outside git with checksums.
11. Whether to fetch the MCD43GF validation sample (about 0.2 TB) now or after the first
    comparison with the legacy raster.

## 8. The recipe in order (one page, for the methods section)

1. Inputs: MCD43A3 v061 and MCD43A2 v061, 2000-02-24 to 2009-12-31 (first MCD43 date to be
   confirmed from the inventory), 38 sinusoidal tiles; MOD44W v061 water mask, 2015; ERA5 hourly
   ssrd and fdir, 2000-2009, 0.25 degree.
2. Per 500 m pixel and day: keep if the shortwave mandatory quality is 0-7 (full or magnitude
   inversion), the local-noon solar zenith angle is at most 70 degrees, and all four MOD44W
   sub-pixels are land.
3. Blue-sky albedo = (1 - f) x BSA_shortwave + f x WSA_shortwave, with f = (ssrd - fdir)/ssrd of
   the ERA5 hour containing local solar noon at the pixel's ERA5 point.
4. Per pixel and calendar month, pooled over 2000-2009 and weighted by the day's ssrd: the mean
   blue-sky albedo over all valid days (all conditions), over snow-free days, over snow days; the
   snowy share; the day and quality counts; missing below 20 valid days.
5. Per 1-degree land-cover cell (and per 0.25-degree legacy cell): the mean over its land pixels
   with a value, with pixel counts; missing below 200 pixels.
6. Outputs with checksums and a manifest; the legacy raster retained as the legacy setting.

## References (DOIs resolved on Crossref 2026-10-15 unless marked)

- Bright, R. M. and O'Halloran, T. L. (2019). Developing a monthly radiative kernel for surface
  albedo change from satellite climatologies of Earth's shortwave radiation budget: CACK v1.0.
  Geosci. Model Dev. 12, 3975-3990. doi:10.5194/gmd-12-3975-2019
- Carroll, M. L. et al. (2009). A new global raster water mask at 250 m resolution. Int. J.
  Digital Earth 2, 291-308. doi:10.1080/17538940902951401
- Carroll, M. et al. (2017). Development of an operational land water mask for MODIS Collection 6,
  and influence on downstream data products. Int. J. Digital Earth 10, 207-218.
  doi:10.1080/17538947.2016.1232756
- Carroll, M. et al. (2024). MODIS/Terra Land Water Mask ... V061 [dataset]. NASA LP DAAC.
  doi:10.5067/MODIS/MOD44W.061 (DOI from the Earthdata catalogue page; not on Crossref)
- Hasler, N. et al. (2024). Accounting for albedo change to identify climate-positive tree cover
  restoration. Nat. Commun. 15, 2275. doi:10.1038/s41467-024-46577-1 (Methods read via Europe PMC)
- Hersbach, H. et al. (2020). The ERA5 global reanalysis. Q. J. R. Meteorol. Soc. 146, 1999-2049.
  doi:10.1002/qj.3803; dataset doi:10.24381/cds.adbb2d47 (CDS page; not on Crossref)
- Jia, A., Wang, D., Liang, S., Peng, J., Yu, Y. (2022). Global daily actual and snow-free
  blue-sky land surface albedo climatology from 20-year MODIS products. J. Geophys. Res. Atmos.
  127, e2021JD035987. doi:10.1029/2021JD035987 (abstract only; methods unverified)
- Lewis, P. and Barnsley, M. J. (1994). Influence of the sky radiance distribution on various
  formulations of the earth surface albedo. Proc. 6th Int. Symp. Physical Measurements and
  Signatures in Remote Sensing, Val d'Isere, 707-715 (no DOI; cited by the MCD43 user guide)
- Lucht, W., Schaaf, C. B., Strahler, A. H. (2000). An algorithm for the retrieval of albedo from
  space using semiempirical BRDF models. IEEE TGRS 38, 977-998. doi:10.1109/36.841980
- Moody, E. G., King, M. D., Platnick, S., Schaaf, C. B., Gao, F. (2005). Spatially complete
  global spectral surface albedos: value-added datasets derived from Terra MODIS land products.
  IEEE TGRS 43, 144-158. doi:10.1109/TGRS.2004.838359
- Moody, E. G., King, M. D., Schaaf, C. B., Platnick, S. (2008). MODIS-derived spatially complete
  surface albedo products: spatial and temporal pixel distribution and zonal averages. J. Appl.
  Meteorol. Climatol. 47, 2879-2894. doi:10.1175/2008JAMC1795.1
- Oleson, K. W. et al. (2003). Assessment of global climate model land surface albedo using MODIS
  data. Geophys. Res. Lett. 30, 1443. doi:10.1029/2002GL016749
- Roman, M. O. et al. (2010). Assessing the coupling between surface albedo derived from MODIS
  and the fraction of diffuse skylight over spatially-characterized landscapes. Remote Sens.
  Environ. 114, 738-760. doi:10.1016/j.rse.2009.11.014 (DOI from the NASA MODIS publication
  record; the journal page was not reachable; not re-resolved on Crossref)
- Schaaf, C. B. et al. (2002). First operational BRDF, albedo nadir reflectance products from
  MODIS. Remote Sens. Environ. 83, 135-148. doi:10.1016/S0034-4257(02)00091-3
- Schaaf, C. B., Wang, Z., Strahler, A. H. (2011). Commentary on Wang and Zender: MODIS snow
  albedo bias at high solar zenith angles relative to theory and to in situ observations in
  Greenland. Remote Sens. Environ. 115, 1296-1300. doi:10.1016/j.rse.2011.01.002
- Stroeve, J. et al. (2005). Accuracy assessment of the MODIS 16-day albedo product for snow:
  comparisons with Greenland in situ measurements. Remote Sens. Environ. 94, 46-60.
  doi:10.1016/j.rse.2004.09.001
- Wang, Z. and Zender, C. S. (2010). MODIS snow albedo bias at high solar zenith angles relative
  to theory and to in situ observations in Greenland. Remote Sens. Environ. 114, 563-575.
  doi:10.1016/j.rse.2009.10.014
- Wang, Z. et al. (2012). Evaluation of MODIS snow albedo product (MCD43A) over tundra. Remote
  Sens. Environ. 117, 264-280. doi:10.1016/j.rse.2011.10.002
- Wang, Z. et al. (2014). Evaluation of MODIS albedo product (MCD43A) over grassland, agriculture
  and forest surface types during dormant and snow-covered periods. Remote Sens. Environ. 140,
  60-77. doi:10.1016/j.rse.2013.08.025
- Wang, Z., Schaaf, C. B., Sun, Q., Shuai, Y., Roman, M. O. (2018). Capturing rapid land surface
  dynamics with Collection V006 MODIS BRDF/NBAR/Albedo (MCD43) products. Remote Sens. Environ.
  207, 50-64. doi:10.1016/j.rse.2018.02.001
- MODIS BRDF/Albedo user guide V006 and V006.1 (Schaaf and Wang, UMass Boston):
  https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/ (pages: introduction,
  mcd43a3-albedo-product, mcd43c3-cmg-albedo-product, mcd43gf-cmg-gap-filled-snow-free-products;
  read 2026-10-15). Product DOIs: 10.5067/MODIS/MCD43A3.061, 10.5067/MODIS/MCD43A2.061,
  10.5067/MODIS/MCD43C3.061 (from CMR records; not on Crossref).
- MCD43A2 and MCD43C3 layer tables: Google Earth Engine catalogue copies of the LP DAAC
  specification, https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD43A2
  and .../MODIS_061_MCD43C3 (the LP DAAC pages themselves returned 403 to the fetch tool).
- ERA5 data documentation (ECMWF Confluence, "ERA5: data documentation"): ssrd 169, fdir 228021,
  ssrdc 228129 in J m-2, accumulations over the hour ending at the validity time; mean-rate
  equivalents msdwswrf 235035 and msdrswrf 235058 in W m-2.
- ecmwfr (CRAN; GitHub bluegreen-labs/ecmwfr): version 2 supports the new CDS with a personal
  access token stored in the system keyring (`wf_set_key()`, `options(keyring_backend = "file")`
  on Linux); recommends month-by-month requests for large downloads.
- NASA CMR granule and collection records (cmr.earthdata.nasa.gov/search, 2026-10-15): tile
  lists, sizes, time ranges, service features, download links.
- Astra's review of snow and water masking, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md,
  and the I16 consensus plan, docs/cc/2026-10-14_i16_plan_consensus.md.
