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
Citation (as given in the README AppEEARS delivered with the data): Carroll, M., DiMiceli, C.,
Townshend, J., Sohlberg, R., Hubbard, A., Wooten, M., Spradlin, C., Gill, R., Strong, S.,
Burke, A., Blanco-Rojas, M., & Frost, M. (2024). MODIS/Terra Land Water Mask Derived from
MODIS and SRTM L3 Global 250m SIN Grid V061. NASA Land Processes Distributed Active Archive
Center. https://doi.org/10.5067/MODIS/MOD44W.061. Accessed October 15, 2026.
Algorithm and purpose (masking water in downstream MODIS land products): MOD44W user guide
and ATBD, https://lpdaac.usgs.gov/documents/1915/MOD44W_User_Guide_ATBD_V61.pdf

**Access route.** NASA AppEEARS (Application for Extracting and Exploring Analysis Ready
Samples), https://appeears.earthdatacloud.nasa.gov/, with a NASA Earthdata Login account
(https://urs.earthdata.nasa.gov/users/new; free). The request was processed by AppEEARS
version 3.131; its citation (from the README): AppEEARS Team. (2026). Application for
Extracting and Exploring Analysis Ready Samples (AppEEARS). Ver. 3.131. NASA EOSDIS Land
Processes Distributed Active Archive Center (LP DAAC), USGS/Earth Resources Observation and
Science (EROS) Center, Sioux Falls, South Dakota, USA. Accessed October 15, 2026.
https://appeears.earthdatacloud.nasa.gov

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
| Calendar-date file names | not used: the files are named by year and day of year (`doy2015001`) |

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

**Delivery.** Downloaded by Chris from the request's page and copied to the workstation on
2026-10-15 (files time-stamped 11:13 BST; submitted 09:04 BST, so the request took at most
about two hours). Unzipped unrenamed into `data/mod44w/2015/` (not committed because of
size; the checksums identify them). AppEEARS records the request itself in
`paleo-rf-mod44w-2015-request.json` and the 52 source tiles (MODIS sinusoidal grid,
MOD44W.A2015001, collection 6.1 files of 2024) in `paleo-rf-mod44w-2015-granule-list.txt`.

| File | Bytes | MD5 |
|---|---|---|
| `MOD44W.061_water_mask_doy2015001000000_aid0001.tif` | 21,368,608 | `40093e9d2eee5557a2e4843306f7616e` |
| `MOD44W.061_water_mask_QA_doy2015001000000_aid0001.tif` | 28,351,072 | `0ced9907316fdf74e36a32b4ab4a1c2f` |
| `README.md` (AppEEARS) | 36,696 | `e86a9f9bb6fc02330e9b92a18737efe9` |
| `paleo-rf-mod44w-2015-request.json` | 12,479 | `327084034e4b2833dd1eaec7a8546e7a` |
| `paleo-rf-mod44w-2015-granule-list.txt` | 8,427 | `f042911ac2df83a18483b22e485e1b34` |
| `MOD44W-061-water-mask-Statistics.csv` | 114 | `3d303ae10049ecf70b1617c0090e29f0` |
| `MOD44W-061-water-mask-QA-lookup.csv` | 244 | `8a360cb836e81dc94f04eef027caa240` |

The water mask as received: one layer, 29,761 x 74,191 pixels of 231.66 m in the MODIS
sinusoidal projection (`+proj=sinu +R=6371007.181`), value 0 land, 1 water (379,133,327 land
and 448,023,863 water pixels within the rectangle, which includes ocean); the QA layer's
codes are listed in the QA lookup file (1 high-confidence observation ... 9 model-predicted
water where the DEM slope says no water).

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
| Oleson, K.W., Bonan, G.B., Schaaf, C., Gao, F., Jin, Y., Strahler, A. (2003). Assessment of global climate model land surface albedo using MODIS data. *GRL* 30(8), 1443. doi:10.1029/2002GL016749 | Land-only MODIS albedo by land fraction, aggregated to a model grid, for comparison with a land-surface model | "We used MODIS land fraction to obtain MODIS albedo for the land only." (Sec. 2.2, per Astra) | Confirmed by Chris in the publisher's full text, 2026-10-15 (automated access is blocked); the rest of its methods section to be checked |

Limits, as Astra noted: none of these uses MOD44W v6.1 with blue-sky albedo at 1 degree,
and none addresses land-only *elevation*; together they support the principle (albedo
representing the land surface only, water masked, averaged to the coarse grid), which is
the defensible claim. Moody and Helbig are in the Zotero collection `paleo-RF`; Oleson to be added once its methods are checked.

---

## Gaps in the provenance of the original data sets

From the README's data table and general question 4, still to be obtained from Andria:
the construction of `blue_sky_monthly_2000-2009.tif` (MODIS MCD43A3 v061 plus ERA5: code
and who ran it); the origin and version of `grid.RDS`, the political boundary polygons and
`taxon2LCT_translation_v2.0.csv`; the source of the glacier polygons
(`glacier_shapefiles_21-1k.RDS`) and of the glacier albedo values; who interpolated the
Dalton et al. (2020) ice fractions and how; the R and package versions of the original
results. The kernels and IPCC data are documented in the README (source, DOI, download date).
