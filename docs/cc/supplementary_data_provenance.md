# Data provenance for the supplementary material

**What this is.** A living record of exactly how each external data set was obtained and
prepared, at the level of detail needed for someone else to repeat it: source, version,
DOI, the request or download parameters, dates, the files received and their checksums, and
any step applied before the pipeline reads them. It feeds the supplementary methods of the
paper. Chris asked for it on 2026-10-15, because this detail was missing for some of the
original data sets (see "Gaps" below).

Each entry is completed as the steps happen; nothing here is reconstructed from memory.

---

## 1. Land/water mask: MODIS MOD44W version 6.1 (for the land-mean albedo and elevation)

**Purpose.** To compute the albedo and elevation of each 1-degree cell over its land part
only, excluding inland and coastal water (lakes, Hudson Bay, coastal sea), so that they
describe the same surface as the land-cover fractions (decided by Chris 2026-10-14, to be
reviewed by Andria; scientific questions B5 and B9). The version and year are a proposal
for Andria to confirm.

**Product.** MOD44W MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m
SIN Grid, Version 6.1 (`MOD44W.061`), annual, 250 m, sinusoidal grid. Layers `water_mask`
(land / water) and `water_mask_QA` (per-pixel quality).
Citation: Carroll, M., Gill, R.L., Strong, S.L., Burke, A.L., Blanco-Rojas, M., Frost, M.J.
(2024). MOD44W MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN
Grid V061. NASA EOSDIS Land Processes Distributed Active Archive Center.
https://doi.org/10.5067/MODIS/MOD44W.061
Algorithm and purpose (masking water in downstream MODIS land products): MOD44W user guide
and ATBD, https://lpdaac.usgs.gov/documents/1915/MOD44W_User_Guide_ATBD_V61.pdf

**Access route.** NASA AppEEARS (Application for Extracting and Exploring Analysis Ready
Samples), https://appeears.earthdatacloud.nasa.gov/, with a NASA Earthdata Login account
(https://urs.earthdata.nasa.gov/users/new; free). AppEEARS version and its recommended
citation: to be copied from the README delivered with the request.

**Request** (submitted by Chris McWilliams from the AppEEARS web page, 2026-10-15 09:04 BST):

| Field | Value |
|---|---|
| Request type | Extract > Area |
| Request name | `paleo-rf-mod44w-2015` |
| Area | uploaded vector file `data/mod44w/paleo_rf_area.geojson` (below) |
| Start / end date | 2015-01-01 / 2015-12-31 (one annual layer) |
| Product, layers | `MOD44W.061`: `water_mask`, `water_mask_QA` |
| Output format | GeoTIFF |
| Projection | Native Projection (MODIS sinusoidal), so the 250 m pixels are not resampled |
| Calendar-date file names | as submitted (to be read from the request's `*-request.json`) |

**The area file.** `data/mod44w/paleo_rf_area.geojson` (committed; MD5
`270cf1d829ac389658e4759a684aaa71`): one GeoJSON polygon (RFC 7946, WGS84 longitude and
latitude), the rectangle longitude -172 to -52, latitude 17 to 79. This encloses all 2,870
cells of the pipeline's 1-degree grid (cell edges -172 to -53, 17 to 79; one degree of margin
to the east), checked by point-in-polygon (`data/mod44w/paleo_rf_area_check.png`). The
edges carry a vertex every 0.5 degree, counter-clockwise, so that any viewer or service
draws them along the parallels and meridians rather than as great circles (a first version
with only the four corners was replaced before submission for this reason). Made with:

```python
import json
W, E, S, N, step = -172.0, -52.0, 17.0, 79.0, 0.5
def seq(a, b):
    n = int(round(abs(b - a) / step)); return [a + (b - a) * i / n for i in range(n + 1)]
ring = ([[x, S] for x in seq(W, E)] + [[E, y] for y in seq(S, N)[1:]] +
        [[x, N] for x in seq(E, W)[1:]] + [[W, y] for y in seq(N, S)[1:]])
json.dump({"type": "FeatureCollection", "features": [{"type": "Feature",
           "properties": {"name": "paleo-RF study area: lon -172 to -52, lat 17 to 79"},
           "geometry": {"type": "Polygon", "coordinates": [ring]}}]},
          open("paleo_rf_area.geojson", "w"))
```

**Delivery** (to be completed): date the request finished (the measured turnaround), the
files received with their sizes and MD5 checksums, where they were put
(`data/mod44w/2015/`, not committed because of size; the checksums identify them), the
AppEEARS version and citation from the README.

**Preparation before the pipeline** (to be completed with issue I16c): how the 250 m mask
is turned into each cell's land fraction and land-mean albedo and elevation, in a committed
tool with its own README; the comparison with an exact area-weighted calculation.

**Methodological precedent** (requested by Chris 2026-10-15). Found by Astra (Codex,
`gpt-6-astra`, live web search); each identity checked with RefChecker and refaudit (all
pass, none retracted) and each quotation checked by Claude Code in the full text where it
could be reached.

| Reference | What it supports | Verbatim (methods) | Checked |
|---|---|---|---|
| Moody, E.G., King, M.D., Platnick, S., Schaaf, C.B., Gao, F. (2005). Spatially complete global spectral surface albedos: value-added datasets derived from Terra MODIS land products. *IEEE TGRS* 43(1), 144-158. doi:10.1109/TGRS.2004.838359 | MODIS albedo with water screened out, aggregated as land-only means onto coarse climate-model grids | "Pixels flagged as seasonally snow-covered or over ephemeral waters were also replaced with fill-values" (Sec. III-A); "the 1-min resolution (land-only) products have been aggregated onto climate modeling grids" (Sec. IV) | Both found in the full text (NASA-hosted PDF) |
| Helbig, M., Wischnewski, K., Kljun, N., Chasmer, L.E., Quinton, W.L., Detto, M., Sonnentag, O. (2016). Regional atmospheric cooling and wetting effect of permafrost thaw-induced boreal forest loss. *Global Change Biology* 22(12), 4048-4066. doi:10.1111/gcb.13348 | MOD44W used to exclude water from a MODIS albedo analysis of vegetation change, in North America | "Pixels classified as open water bodies by the MODIS Land Water Mask (MOD44W) were discarded from the analysis." (Materials and methods) | Found in the full text (author PDF) |
| Oleson, K.W., Bonan, G.B., Schaaf, C., Gao, F., Jin, Y., Strahler, A. (2003). Assessment of global climate model land surface albedo using MODIS data. *GRL* 30(8), 1443. doi:10.1029/2002GL016749 | Land-only MODIS albedo by land fraction, aggregated to a model grid, for comparison with a land-surface model | "We used MODIS land fraction to obtain MODIS albedo for the land only." (Sec. 2.2, per Astra) | **Not yet confirmed**: the publisher blocks automated access and no open copy exists; to be read by a person |

Limits, as Astra noted: none of these uses MOD44W v6.1 with blue-sky albedo at 1 degree,
and none addresses land-only *elevation*; together they support the principle (albedo
representing the land surface only, water masked, averaged to the coarse grid), which is
the defensible claim. Moody and Helbig are in the Zotero collection `paleo-RF`.

---

## Gaps in the provenance of the original data sets

From the README's data table and general question 4, still to be obtained from Andria:
the construction of `blue_sky_monthly_2000-2009.tif` (MODIS MCD43A3 v061 plus ERA5: code
and who ran it); the origin and version of `grid.RDS`, the political boundary polygons and
`taxon2LCT_translation_v2.0.csv`; the source of the glacier polygons
(`glacier_shapefiles_21-1k.RDS`) and of the glacier albedo values; who interpolated the
Dalton et al. (2020) ice fractions and how; the R and package versions of the original
results. The kernels and IPCC data are documented in the README (source, DOI, download date).
