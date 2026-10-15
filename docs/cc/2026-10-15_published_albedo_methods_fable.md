# Published methods for gridded albedo rasters: review by Fable

Independent of Astra's review. Unedited; the source copies it read are kept in the session scratchpad only (copyright). Nothing here is a decision.

---

# Published methods for gridded albedo rasters: products, precedents and what to adopt (Fable, 2026-10-15)

Independent literature research for paleo-RF, from the brief `albedo_lit/brief.md`. Read-only in both
checkouts and in `data/`; nothing under `writing/` and no .docx/.pptx/.pdf inside the repository was
opened. The two rebuild designs and the two reconciliations were read only at the end, for section 5.
Every DOI quoted below was resolved through the Crossref API (or doi.org for DataCite DOIs) on
2026-10-15; "verified" means I read the passage in the paper, user guide or data record itself
(downloaded copies are in `fable_sources/`, file names given in brackets); **[unverified]** marks what
I could not read at source. Evidence and recommendation are kept apart: sections 1 to 3 are evidence,
sections 4 to 6 are my recommendation.

## Summary

There is no single "standard" recipe for a monthly, land-only, blue-sky albedo climatology at coarse
resolution, but there is a well-trodden published path with few forks. Every study close to ours starts
from the NASA MCD43 BRDF/albedo suite (500 m tiles, 0.05-degree CMG, or 30-arcsecond CMG), takes the
shortwave broadband black-sky (local solar noon) and white-sky albedo, forms a blue-sky value by a linear
mix weighted by the diffuse fraction (sources vary: AOD look-up table, reanalysis fluxes, CERES, or the
plain mean of the two), separates snow from snow-free with the MCD43A2 snow flag or a MODIS snow product,
screens quality with the mandatory QA, reduces days to months and years to a climatology by a plain
mean or median, and treats water by a land/water mask or a land-fraction threshold before or at
aggregation. The published choices differ, each defensibly, on: the diffuse-fraction source; full-only
versus all-quality retrievals; mean versus median; the solar-zenith cut (70 or 75 degrees); and whether
gaps are filled.

Two published products can serve the calibration response almost directly. Jia et al. (2022, JGR
Atmospheres; Zenodo, CC-BY-4.0) provide a 500 m and 0.05-degree **daily blue-sky albedo climatology
in both actual and snow-free conditions** from MCD43A3/A2 Collection 6 (2001-2020), validated at 38
sites (RMSE 0.031); its 500 m tiles for North America are about 60 GB. MCD43C3 v061 at 0.05 degree,
reduced as in Duveiller, Hooker and Cescatti (2018), is the other. A full 500 m rebuild (the two
designs) is the most faithful to the manuscript's stated 2000-2009 MODIS-plus-ERA5 construction but
carries the most unpublished judgements.

**Recommendation (ranked):** (1) adopt Jia et al. 2022 at 500 m, screen water with MOD44W, average
daily-climatology values to months and pixels to the land part of each 1-degree cell; (2) MCD43C3
v061 monthly, Duveiller-style, with a 0.05-degree land-fraction screen; (3) the 500 m rebuild; (4)
CLARA-A3 blue-sky albedo (0.25 degree, monthly) as an independent check, not the response.

---

## 0. How the evidence was gathered

- Product documentation read at source: the MCD43 V006/V006.1 user guide pages at UMass Boston
  (introduction, MCD43A3, A2, C2, C3, D, GF, file specification) [umb_*.txt]; NASA Earthdata catalogue
  entries for MCD43A2/C3/GF v061 and VNP43C3 v002 [earthdata_*.html, earthdata_vnp43c3_002.txt]; the
  NASA CMR API for temporal coverage, granule counts and sizes; the GEE catalogue page for MCD43A2
  (class codes) [gee_mcd43a2.txt]; the ORNL DAAC guides for the ABoVE blue-sky albedo and the Zhao and
  Jackson dataset [ornl_*.txt]; the Copernicus C3S catalogue entry and the CGLS albedo PUM/ATBD v1
  [c3s_albedo.txt, cgls_albedo_pum_v1.txt]; the CEDA record for QA4ECV [qa4ecv_ceda.txt]; the ECMWF page
  for the MCD43GF-based BRDF climatology [ecmwf_brdf_climatology.txt]; the Zenodo record and the UMD
  archive listings for Jia et al. 2022 [jia2022_zenodo.json, jia_*_index.html].
- Papers read in full (PDF, public copies): Duveiller et al. 2018 (both papers), Hasler et al. 2024,
  Bright et al. 2015, Lejeune et al. 2020, Li et al. 2015, Bright and O'Halloran 2019, Karlsson et al.
  2023 (CLARA-A3), Jia et al. 2022 (author copy at glass.hku.hk), Jiao et al. 2017, Zhao and Jackson
  2014 (author copy at Stanford), Hopcroft et al. 2026, Dawson et al. 2025, Liang et al. 2021 (GLASS,
  HTML full text).
- Abstract only (publisher paywall or anti-bot block for curl): Moody et al. 2005 (NTRS version of the
  abstract), Moody et al. 2008 (AMS page), Sun et al. 2017, Wang et al. 2018 (NASA MODIS pubs page),
  Gao et al. 2014 (Crossref abstract), Ghimire et al. 2014 (Crossref abstract; its method is described
  in the companion Jiao et al. 2017, read in full), Myhre et al. 2005, Barnes and Roy 2008, Oleson et
  al. 2003, Potter et al. 2019, Liu et al. 2017 (VIIRS). Moody et al. 2007 and Román et al. 2010:
  title and DOI only.
- Could not be read at all: the AGU 2023 abstract of the project's own method (ADS, CAPTCHA); any
  MuSyQ albedo product paper (none found on Crossref under that name).

---

## 1. Published gridded albedo products and climatologies (as of October 2026)

### 1.1 NASA MODIS MCD43 suite, Collection 6.1 (Schaaf and Wang, LP DAAC)

**What it provides (verified, user guide and catalogue).** Every day, a BRDF is fitted per pixel to 16
days of Terra+Aqua directional reflectances (Terra only before mid-2002), the day of interest weighted
most, with the RossThick-LiSparseReciprocal kernels (Lucht et al. 2000; Schaaf et al. 2002). From the
BRDF: black-sky albedo at local solar noon and white-sky albedo for MODIS bands 1-7 and three broadbands
(0.3-0.7, 0.7-5.0 and 0.3-5.0 um). Scale factor 0.001, fill 32767, valid 0-32766. Collection 6 and 6.1
are "Validated (Stage 3)" from day 2000065 (5 March 2000); the C6.1 mandatory QA codes are 0 (full
inversion) and 1 (magnitude inversion), with 2-7 marking the same two cases when band 6, band 5 or both
are fill because of dead detectors (user guide introduction). The quality note the product team repeats
on every page: "Users are urged to use the band specific quality flags to isolate the highest quality
full inversion results for their own science applications."

**Water (verified, introduction page).** "Note that the per-pixel data and the quality information
are computed for all land and coastal areas and for shallow water regions (pixels that are within 5 km
of land OR are less than 50 meters deep). The products and quality flags are not computed for moderate
or deep water regions." So inland lakes and near-shore ocean carry albedo values; the EOS land/water
mask is stored per pixel in MCD43A2 as `BRDF_Albedo_LandWaterType` with classes 0 shallow ocean, 1 land,
2 ocean coastlines and lake shorelines, 3 shallow inland water, 4 ephemeral water, 5 deep inland water,
6 moderate or continental ocean, 7 deep ocean (verified on the GEE catalogue page for MCD43A2.061;
VNP43 v002 adopted the same seven-class MODIS heritage mask, verified on its catalogue page).

**Snow (verified).** MCD43A2 `Snow_BRDF_Albedo`: 0 snow-free albedo retrieved, 1 snow albedo
retrieved. In Collection 6 the retrieval uses the day-of-interest snow status rather than the majority
over 16 days (Wang et al. 2018 abstract, verified).

**Blue-sky (verified, MCD43A3 page).** "Actual albedo is a value which is interpolated between these two
as a function of the fraction of diffuse skylight which is itself a function of the aerosol optical
depth ... The underlying assumption of an isotropic distribution of the diffuse skylight is approximate
but avoids the expense of an exact calculation while capturing the major part of the phenomenon.
However, for large angles and bright surfaces it's more appropriate to use the full anisotropic
expression [Román et al. 2010]."

**Members of the suite.**

| Product | Grid | Content | Temporal coverage (CMR, 2026-10-15) | Size |
|---|---|---|---|---|
| MCD43A3 v061 | 500 m sinusoidal tiles | BSA, WSA, 10 bands + mandatory QA | 2000-02-16 to present | ~50 MB per full tile-day (brief's measurement); 0.56 MB for an edge tile (CMR) |
| MCD43A2 v061 | 500 m tiles | band quality, valid-obs days, land/water type, snow flag, local solar noon, platform, uncertainty | same | ~15 MB per tile-day (brief) |
| MCD43C3 v061 | 0.05 deg global CMG | BSA, WSA 10 bands; Quality (majority of underlying 30" pixels), Local_Solar_Noon, Percent_Inputs, Percent_Snow, BRDF_Albedo_Uncertainty | same | 182 MB per day (CMR example 2005-175) |
| MCD43C1/C2 v061 | 0.05 deg | BRDF parameters; C2 = snow-free parameters, from which snow-free BSA/WSA follow by the published polynomial (coefficients on the C2 page) | same | similar |
| MCD43D31-D68 v061 | 30 arcsec global, one layer per file | D31 QA, D32 local solar noon, D41 snow status, D42-51 BSA, D52-61 WSA, D62-68 NBAR | 2000-02-16 to present | D51 (BSA shortwave) 223 MB per day |
| MCD43GF v006 | 30 arcsec global | gap-filled, snow-free BSA, WSA, BRDF parameters, NBAR, QA | 2000-03-03 to 2017-12-31 (6,513 granules) | ~13.5 GB per daily granule bundle |
| MCD43GF v061 | 30 arcsec global | as above, C6.1 inputs | **2013-01-01 to 2021-12-31 only** (3,287 granules; zero in 2000-2012; LP DAAC: "remaining historic data will be added at a later date") | ~11.7 GB per day |

The MCD43C3 page carries two cautions that matter here (verified): "along coastlines, this means that
the 30 arcsecond pixels that lie over shallow water will be averaged into the 0.05 degree CMG pixel",
and "We recommend that users use the MCD43D 30 arcsecond products (or the 30 arcsecond MCD43GF
gapfilled products) instead of the MCD43C products as much as possible, since the MCD43C products merely
represent averages of the direct BRDF retrievals obtained with the MCD43D processing."

**MCD43GF method (verified, GF page).** Four tiers: high-quality retrievals; else a phenological temporal
fit; else a spatial fit "borrowed from pixels within 1 degree of latitude"; else spatial smoothing;
"pixels with ephemeral snow were removed using the MCD43D41 snow flags"; QA 0-3 observed, 4-7 filled
(7 = solar-zenith extension between 70 and 82 degrees, added because "The MCD43 product is not
recommended for solar zenith angles beyond 70 degrees"). Evaluation: Sun et al. 2017 (abstract
verified): RMSE 0.027 (NIR) and 0.020 (red) against original high-quality data, record 2001-2015.

**Licence and access.** NASA LP DAAC, free with an Earthdata login; AppEEARS, LAADS, Data Pool and
Earthdata Search. DOIs (all resolve): 10.5067/MODIS/MCD43A3.061, 10.5067/MODIS/MCD43A2.061,
10.5067/MODIS/MCD43C3.061, 10.5067/MODIS/MCD43D42.061 (and D51 etc.), 10.5067/MODIS/MCD43GF.061.

**Validation of the suite.** Wang et al. 2018 (RSE 207, doi:10.1016/j.rse.2018.02.001; abstract
verified): daily C6 captures more rapid dynamics than C5; the reconciliation documents quote its
"RMSE below 0.032 and bias within 0.008 for all quality results" [that figure not re-read by me].
Stroeve et al. 2005 (doi:10.1016/j.rse.2004.09.001) for snow over Greenland; Wang and Zender 2010
(doi:10.1016/j.rse.2009.10.014) on the high-solar-zenith snow bias (titles verified).

### 1.2 Moody et al. 2005, 2007, 2008: NASA "spatially complete" snow-free albedo (Collection 4/5 era)

- Moody, King, Schaaf, Hall, Platnick 2005, IEEE TGRS 43, doi:10.1109/TGRS.2004.838359 (verified
  resolves). From the NTRS version's abstract (verified): about 30 % of global land is obscured by
  cloud and another large share by ephemeral and seasonal snow, so "an ecosystem-dependent temporal
  interpolation technique was developed" to fill the Terra MOD43B3 snow-free record.
- Moody et al. 2008, J. Appl. Meteor. Climatol. 47, doi:10.1175/2008JAMC1795.1 (abstract verified at
  AMS): "Five years (2000-04) of spatially complete snow-free land surface albedo data ... (MOD43B3,
  collection 4). In addition, a spatially complete snow-free aggregate albedo climatological product
  was generated ... retrieves missing data within 3 %-8 % error"; the paper reports zonal averages and
  phenology of filled versus original data.
- Moody et al. 2007, RSE 111, doi:10.1016/j.rse.2007.03.026 (title verified): Northern Hemisphere
  2000-2004 spectral albedo statistics of surfaces "in the presence of snow", by land-cover class.
- Status: superseded by MCD43GF (same lineage); resolution and distribution page **[unverified]**.
  The designs' attribution of a specific water-screening rule to Moody 2005 could not be checked by
  me either (the paper is paywalled) and should not be cited for that until someone reads it.

### 1.3 Jia, Wang, Liang, Peng and Yu 2022: daily actual and snow-free blue-sky climatology

JGR Atmospheres 127, e2021JD035987, doi:10.1029/2021JD035987 (resolves; read in full from the author's
public copy, [jia2022_jgr.txt]).

- **Inputs (sect. 2.1.1, verified):** MCD43A3 **C06** 500 m daily shortwave BSA and WSA, 2001-2020;
  MCD43A2 C06 snow flag and solar zenith; AOD at 0.55 um from MOD08 C61 (1 degree monthly) for the
  diffuse fraction.
- **Blue-sky (sect. 2.2.1, verified):** alpha = (1 - S) BSA + S WSA with S the diffuse skylight ratio
  "retrieved from a LUT based on the solar zenith angle and AOD (Schaaf et al. 2002)", i.e. the
  clear-sky, aerosol-only diffuse fraction, following Wang et al. 2017 and Chrysoulakis et al. 2019.
- **Climatology (sect. 2.2.2, verified):** processed on Google Earth Engine; "The daily climatology
  was calculated by averaging available albedo values of each DOY from 2001 to 2020 ... the maximum
  and minimum ones were neglected ... Pixels with a large SZA (>75 degrees) were also eliminated ...
  The snow-free albedo climatology was obtained by ignoring all snow pixels marked by MCD43A2." Outputs
  at 500 m, then "upscaled ... from 500 m to 0.05 degree by calculating the mean value", and 0.5
  degree; a land-cover and PFT climatology (mode of MCD12Q1 2001-2020) is attached.
- **Water:** not screened; MCD43A3 carries values over shallow inland water (sect. 1.1), so the 0.05
  degree means can include lake pixels. The attached MCD12Q1 climatology gives the water class per
  500 m pixel (mode over 20 years).
- **Validation (sect. 3, verified):** 38 long-term sites: RMSE 0.031, bias -0.003, R 0.96 overall;
  snow-free summer RMSE below 0.03; snow cases RMSE 0.055, bias -0.007. Also compared GLASS,
  GlobAlbedo, ERA5, MERRA-2 and 14 CMIP6 models (ERA5 best of the models, positive bias in the
  snow-free season).
- **Known gaps (sect. 3.3, verified):** "The snow-free map in winter left blanks over high latitudes
  owing to a lack of available valid snow-free albedo values"; AOD below 0 over Western Australia
  breaks the LUT there.
- **Access (verified):** Zenodo 10.5281/zenodo.6359686 (500 m, CC-BY-4.0; only the first days of each
  month, 36 zips of 1.35-1.7 GB) and 10.5281/zenodo.6350821 (CMG); the full set at
  http://glass.umd.edu/archive/Albedo/ABI/: `0.05D/` 365 daily NetCDF files of 14 MB (about 5 GB);
  `0.5D/` 365 files; `500M/DOY/` one NetCDF per MODIS tile (for DOY 180, 326 tiles; 41 tiles in the
  h06-h15, v01-v06 box, 72 KB to 10 MB each, so roughly 60 GB for North America over 365 days).
  Whether "actual" and "snow-free" are separate variables in each file is stated in the Zenodo
  description ("It includes historical and snow-free blue-sky albedo climatology data") but the
  variable layout was not inspected (no data downloaded).

### 1.4 GLASS albedo (Liang, Qu, Liu and colleagues)

- Liang et al. 2021, BAMS 102, doi:10.1175/BAMS-D-18-0341.1 (full text read, [liang2021_glass_bams.txt]):
  "Unlike the MODIS albedo products, which are based on inversions of the BRDF model parameters, the
  GLASS albedo products are based on the direct-estimation method and represent surface albedo under
  general clear-sky atmospheric conditions. The GLASS albedo algorithm consists of two steps. The first
  step adopts the direct-estimation algorithm (Qu et al. 2014) ... the second step adopts the
  spatiotemporal filtering algorithm (N. Liu et al. 2013) to generate a smoothed and gap-free product."
  Suite-wide: 1 km and 0.05 degree, AVHRR from 1981 and MODIS from 2000, 8-day, gap-free.
- Algorithm and evaluation: Qu et al. 2014, IEEE TGRS 52, doi:10.1109/TGRS.2013.2245670; Liu et al.
  2013, Int. J. Digital Earth 6, doi:10.1080/17538947.2013.804601; Liang et al. 2013,
  doi:10.1080/17538947.2013.805262 (all resolve).
- Quantities: black-sky and white-sky shortwave with a quality flag (BNU product page, Chinese; and
  the BAMS text); no blue-sky layer. Jia et al. 2022 found GLASS "comparable" to their climatology in
  snow-free summer (RMSE below 0.03) but its 8-day step "may not be sufficient to characterize the
  impact of snow cover change" (verified).
- Access: glass.umd.edu/archive/Albedo/MODIS/ (0.5D, 250M, 500M folders listed 2025) and
  glass-product.bnu.edu.cn; free. Water handling, licence text and the exact current version
  **[unverified]**.

### 1.5 Copernicus surface albedo (CGLS and C3S)

- C3S catalogue "Surface albedo 10-daily gridded data from 1981 to present" (verified,
  [c3s_albedo.txt]): black-sky ("directional-hemispherical", at local solar noon) and white-sky
  ("bi-hemispherical"), broadband and spectral; AVHRR 1981-2005 at ~4 km, SPOT-VGT 1998-2014 and
  PROBA-V 2013-2020 at ~1 km, Sentinel-3 2018-2024 at 300 m; 10-day frequency from a 20-day moving
  window; variables include an error layer, QFLAG, NMOD (valid observations) and AGE. Earlier versions
  brokered from CGLS. No blue-sky.
- CGLS 1 km v1 PUM/ATBD (verified, [cgls_albedo_pum_v1.txt]): AL-DH and AL-BH for visible, NIR and
  broadband; QFLAG bit 2 is snow status; validation against MODIS C5 at 6 km gives RMSD about 0.03 for
  shortwave black-sky, with "Boreal and Polar areas (bias up to 0.1) where the impact of snow/ice is
  more important" and "larger fraction of missing values over snow".
- Access: Copernicus Climate Data Store (free registration) and CGLS; licence: Copernicus open.
  Fitness for 2000-2009: SPOT-VGT 1 km covers the period; it is not MODIS and has no blue-sky.

### 1.6 CLARA-A3 surface albedo (CM SAF)

Karlsson et al. 2023, ESSD 15, 4901-4926, doi:10.5194/essd-15-4901-2023 (read in full,
[karlsson2023_clara_a3_essd.txt]).

- Sect. 3.3 (verified): black-sky (SAL), white-sky (WAL) and **blue-sky (BAL)** albedo, "provided in
  the standard CLARA spatial grid resolution of 0.25 x 0.25 degrees"; pentad and monthly means; AVHRR,
  1979 onwards (CDR to 2020, interim CDR after). "BAL is derived as the direct irradiance
  fraction-weighted mean of SAL and WAL (e.g., Pinty et al., 2005). The direct irradiance fraction is
  derived with a statistical relationship that links cloud probability (CMAPROB) with the clearness
  index." "Snow-free and snow/ice AVHRR observations are combined in the final temporal averages by
  weighting their respective observation counts." "The albedo data do not contain uncertainty estimates
  per grid cell. However, a wide variety of parameters (e.g., skewness, kurtosis, and number of valid
  observations per grid cell) ... are provided in the data files." Observations at sun zenith above
  70 degrees or view zenith above 60 degrees are discarded.
- Validation (sect. 3.3.2, verified): BSRN (13 non-coastal land sites), PROMICE (25 ice-sheet
  stations), SHEBA/Tara sea ice; "the mean relative bias was < 10 %", point-to-pixel representativeness
  at 25 km is the main limitation.
- Access: CM SAF, free registration; data DOI 10.5676/EUM_SAF_CM/CLARA_AVHRR/V003 (resolves to the CM
  SAF record). Land/water: the grid covers everything; land-only values are not separable at 0.25
  degree.

### 1.7 GlobAlbedo and QA4ECV

- GlobAlbedo (ESA, Lewis, Muller and colleagues): 1998-2011, 0.05 degree (and 1 km) monthly BSA and
  WSA from AATSR, SPOT-VGT and MERIS by optimal estimation with MODIS-anisotropy gap filling; "global
  R2 of 0.85" against MODIS; "problems associated with snow detection ... at very high latitudes
  (> 70 degrees)". All of this as described in Lejeune et al. 2020, sect. 2.1 (verified there);
  GlobAlbedo's own papers (Lewis et al. 2012; Muller et al. 2013) **[not read, no DOI verified]**.
- QA4ECV broadband albedo 1982-2016 (CEDA record, verified): AVHRR + geostationary, 0.05 and 0.5
  degree, daily and monthly, BSA and WSA for vis, NIR and shortwave, per-pixel uncertainties, CC-BY
  4.0; the CEDA record is still marked "Preview / THIS RECORD HAS NOT BEEN PUBLISHED YET", citation
  "[PROVISIONAL] Muller et al. (9999)". Hence not a citable, archived product today.

### 1.8 Gao et al. 2014: climatological albedo look-up maps by land-cover class

Gao, He, Wang, Ghimire, Shuai, Masek, Schaaf, Williams 2014, J. Appl. Remote Sens. 8, 083532,
doi:10.1117/1.JRS.8.083532 (Crossref abstract verified; SPIE full text blocked). "Ten years (2001 to
2011) of MODIS BRDF/albedo products were used to generate global albedo climatology. Albedo LUMs of
land cover classes defined by the IGBP at multiple spatial resolutions were generated ... monthly
statistics of white-sky (diffuse) and black-sky (direct) albedo for each IGBP class for visible,
near-infrared, and shortwave broadband under both snow-free and snow-covered conditions." Jiao et al.
2017 (sect. 2.2, verified) describe the construction: intersection of the monthly median MODIS
broadband albedo at 30 arcsec with the majority MCD12 IGBP class, reported "at different spatial
resolutions from 0.05 degree to 1 degree". Used by Ghimire et al. 2014, Jiao et al. 2017, Bright and
O'Halloran 2019 and Hasler et al. 2024. Access: a data.gov record points to Globus **[not verified]**.
This is albedo *by class*, not the albedo of each cell's actual surface, so it answers a different
question from ours (section 4).

### 1.9 Regional blue-sky and class products for North America (ORNL DAAC)

- **ABoVE daily mean blue-sky albedo 2000-2017** (Solvik, Potter, Erb, Román, Schaaf, Sun, Wang,
  Rogers 2019, doi:10.3334/ORNLDAAC/1605, resolves; guide read, [ornl_above_bluesky_albedo_guide.txt]).
  500 m, Canada and Alaska (41.7-79 N, -173 to -52.6 E), daily 2000-02-24 to 2017-04-22; from
  MCD43A1 v006 BRDF parameters; "Daily mean albedo was calculated by averaging hourly instantaneous
  blue sky albedo values weighted by the solar insolation" (CERES SYN1deg); multiple scattering
  included ("blue sky albedo was considerably higher than both inputs"); AOD from MOD08 daily, else
  monthly, else MISR, else 0.2; SZA above 70 degrees used only when nothing else exists; quality flags
  for SZA, snow, MCD43 QA and AOD source. Companion paper Potter et al. 2019, GCB,
  doi:10.1111/gcb.14888 (resolves; abstract only). 12,532 GeoTIFFs; size not stated.
- **Zhao and Jackson 2014 dataset** (doi:10.3334/ORNLDAAC/1210, resolves; guide read,
  [ornl_na_modis_surface_biophysics.txt]): 0.05 degree, 20-60 N, 8-day climatology 2000-2012 of
  BRDF parameters (iso/vol/geo for three broadbands) **per IGBP land-cover class**, plus a monthly
  diffuse-fraction file (WRF-CLM, 2001-2005) and the blue-sky formula in the guide
  (alpha_act = alpha_bs(theta)(1 - f) + alpha_ws f, with the Lucht polynomial). Class-specific, so
  the same caveat as Gao 2014.

### 1.10 Newer and other products

- **VIIRS VNP43/VJ143/VJ243 v002** (catalogue verified): same RTLSR algorithm and 16-day window as
  MCD43; VNP43C3 at 0.05 degree with percent-snow and percent-inputs layers; Suomi-NPP products cease
  2 November 2026, users directed to NOAA-21/NOAA-20 equivalents. Continuity with MODIS: Liu et al.
  2017, RSE, doi:10.1016/j.rse.2017.09.020 (abstract verified: shortwave WSA absolute bias 0.0069).
  Period starts 2012, so not for 2000-2009.
- **Landsat and Sentinel-2 albedo**: algorithms rather than archived climatologies: Shuai et al.
  2011 (30 m snow-free from Landsat + MODIS BRDF, doi:10.1016/j.rse.2011.04.019) and Li et al. 2018
  (20 m from Sentinel-2A + MODIS/VIIRS anisotropy, doi:10.1016/j.rse.2018.08.025); both resolve.
  Irrelevant at 1 degree except as a validation idea.
- **ECMWF land-surface BRDF climatology v2.0** (verified page): a 2017-2021 average of MCD43GF v061,
  seven spectral bands, 30 arcsec and 0.025/0.05/0.1 degree, freely downloadable. Spectral only (no
  broadband), snow-cleared, wrong period for us; useful as an example of a published MCD43GF-based
  climatology.
- **ERA5 "forecast albedo"**: the IFS prescribes a MODIS-derived snow-free climatology and modifies it
  with modelled snow; monthly 0.25 degree. Documentation page content **[unverified; the ECMWF
  Forecast User Guide page I fetched had been deleted]**. Jia et al. 2022 found ERA5 albedo biased
  high in the snow-free season (verified). A model product, not an observation; not recommended.
- **MuSyQ**: cited in the brief; I found no Crossref record of a MuSyQ global albedo product paper
  (a 2026 book chapter on NOAA VIIRS gap-free albedo and MuSyQ chlorophyll appear, not albedo)
  **[unverified; treat as not available]**.
- **CERES EBAF-Surface / SYN1deg**: 1-degree monthly all-sky surface fluxes from which a surface
  albedo (upwelling/downwelling) can be formed; used by Duveiller 2018 for the energy balance and by
  Bright and O'Halloran 2019 for the kernel. Radiative-transfer product, coarse; not an observed
  surface albedo. DOI not verified.

---

## 2. Methods in studies closest to ours

Summary table (all entries verified from the paper unless marked):

| Study | Albedo product and quantity | Water | Snow | Blue-sky mixing | Quality and compositing | Spatial support |
|---|---|---|---|---|---|---|
| Duveiller, Hooker, Cescatti 2018, Nat. Commun. 9:679, doi:10.1038/s41467-017-02810-8; dataset Sci. Data 5:180014, doi:10.1038/sdata.2018.14 | MCD43C3 (Collection 5), shortwave BSA and WSA, 0.05 deg | land-cover fractions (ESA CCI 300 m, aggregated) include water as a non-vegetated class; windows need vegetation co-occurrence; topography mask | not separated (climatology of actual conditions) | "we took the mean of both values" (no diffuse-fraction data) | "the median value for each month ... from the years 2008 to 2012"; one 16-day product per month ("correspond best with the 15th of each month"); no QA filter stated | 5x5 windows of 0.05 deg (~25 km) unmixed by local regression; results aggregated to 1 deg |
| Lejeune et al. 2020, Earth Syst. Dynam. 11, 1209, doi:10.5194/esd-11-1209-2020 | GlobAlbedo 0.05 deg monthly 1998-2011, mean of WSA and BSA | GlobCover classes; cells with >= 95 % trees or crops-grasses | snow-free (snc < 0.1) vs snow-covered (snc > 0.9) regressions, separately | mean of the two | monthly climatology over the full GlobAlbedo period | pure 0.05 deg cells, aggregated to 2 deg; D18 gap-filled by random forest for the RF estimate |
| Bright et al. 2015, Biogeosciences 12, 2195, doi:10.5194/bg-12-2195-2015 | MCD43A v006 daily 500 m, **black-sky** VIS and NIR at local noon | n/a (forest and adjacent open pixels in Norway) | snow seasons only (Nov-May 2007-2009) | none (black-sky compared with model direct-beam albedo) | daily retrievals, monthly means | pixel pairs |
| Bright and O'Halloran 2019, GMD 12, 3975, doi:10.5194/gmd-12-3975-2019 | CERES EBAF v4 1 deg for the kernel; example with Gao 2014 **white-sky** class climatology 2001-2011 | n/a | per Gao LUM (snow-free/snow-covered) | none | monthly climatology | 1 deg CERES cells |
| Zhao and Jackson 2014, Ecol. Monogr. 84, 329, doi:10.1890/12-1705.1 | MCD43A1/A2 Collection 5, 8-day 500 m, 2001-2011; **white-sky** for pair comparisons; for RF, "actual surface albedo ... average of MODIS black-sky and white-sky albedos weighted by direct and diffuse downward fluxes from the regional climate simulation of WRF-CLM" | land-cover map of pure classes (9-year majority, 500 m and 1 km); only four vegetated classes | not separated; AMSR-E SWE shown alongside | WRF-CLM flux-weighted | 11 years averaged per 8-day period with QA-based year weighting (lower-quality years at half weight) | paired pixels within 15 km, elevation difference < 10 m, slope < 15 deg; RF at 1 deg from >= 3 pairs per cell |
| Li et al. 2015, Nat. Commun. 6:6603, doi:10.1038/ncomms7603 | MCD43C3 0.05 deg, 2002-2013 climatology; GLASS as a check | forest vs open-land pixels by land cover | snow frequency computed from the albedo product's snow information | "We simply assume the blue-sky albedo to be the average of black-sky and white-sky albedo because their difference is very small and they are highly correlated (R = 0.9992)" | "only data with a QC flag of 0 (best quality), 1 (good quality), or 2 (mixed quality) are used" | 9x5 window search for forest/open pairs |
| Gao et al. 2014 (sect. 1.8); Ghimire et al. 2014, GRL 41, 9087, doi:10.1002/2014GL061671 (abstract only); Jiao et al. 2017, Climatic Change, doi:10.1007/s10584-017-1962-8 (read) | MCD43 30 arcsec monthly **median**, 2001-2010/11, BSA and WSA by IGBP class | class maps (majority MCD12); water a class | snow-covered vs snow-free LUMs from the MCD43 snow flag; monthly snow fraction from MOD10 CMG 2001-2011 at 0.05 deg, resampled to 1 deg | f_r (white/black fractions) from NCEP/NCAR reanalysis diffuse and direct surface radiation 1981-2010; kernels (Shell et al. 2008) applied separately to BSA and WSA | monthly climatology | 1 deg for the forcing |
| Hasler et al. 2024, Nat. Commun. 15:2275, doi:10.1038/s41467-024-46577-1 | Gao 2014 LUMs (per month, CMG pixel, IGBP class, snow/snow-free); MOD10C1 v6.1 monthly snow 2000-2021; NCEP/NCAR diffuse and beam radiation 1981-2010, nearest-neighbour to 0.05 deg | "land as all pixels with >= 1 % of their area covered by any land cover type other than water (MCD12C1, 2016)" | snow and snow-free albedo combined with monthly snow fraction (Eq. 1) | reanalysis direct/diffuse fractions | monthly | 0.05 deg CMG, nominally 500 m output |
| Myhre et al. 2005, GRL 32, doi:10.1029/2005GL024004 (abstract) | MODIS surface albedo by MODIS vegetation type; potential natural vegetation map | n/a | n/a | n/a | n/a | global |
| Barnes and Roy 2008, GRL 35, doi:10.1029/2008GL033567 (abstract) | MODIS **snow-free** broadband albedo sampled by Landsat LCLU classes within ecoregions | n/a | snow-free only | n/a | n/a | 36 ecoregions of CONUS |
| Oleson et al. 2003, GRL 30, doi:10.1029/2002GL016749 (abstract) | MODIS WSA and BSA (noon) compared with CLM; "[retained all qualities]" per the Astra reconciliation **[unverified by me]** | | | | | model grid |
| Potter et al. 2019 / Solvik et al. 2019 (sect. 1.9) | MCD43A1 v006 daily blue-sky with multiple scattering, insolation-weighted daily mean | n/a | snow flag kept as QA | AOD LUT (MOD08/MISR) | daily | 500 m, Canada/Alaska |
| Hopcroft et al. 2026, GRL 53, e2025GL118518, doi:10.1029/2025GL118518 (read) | **no satellite albedo**: HadCM3B-M2.1d with dynamic vegetation; albedo is a simulated anomaly (Figs S7-S8) | | | | | model |
| Dawson et al. 2025, Clim. Past 21, 2031, doi:10.5194/cp-21-2031-2025 (read) | no albedo calculation; discussion of snow masking and summergreen/evergreen albedo (Hollinger et al. 2010) | | | | | |
| This project's method, AGU Fall Meeting 2023 abstract B21J-2087 "Mapping Holocene Radiative Forcing from Land Cover Change" (ADS 2023AGUFM.B21J2087D) | per the search-engine summary: "average monthly blue sky albedo over the 2000-2009 period from MODIS satellite reflectance data products and ERA5 atmospheric reanalysis diffuse and direct radiation", statistical model pollen-albedo, pre-industrial kernel **[abstract page could not be read: CAPTCHA]** | | | | | |

**Other Holocene or palaeo studies.** I found none that calibrated albedo from MODIS against
pollen-based cover in the way paleo-RF does. The Holocene land-cover forcing literature is
model-based (Hopcroft et al. 2026; Thompson et al. 2022 and Otto et al. 2011 as discussed by Dawson
et al. 2025; He et al. 2014 GRL per search results). The closest observational lineage is the
historical one: Myhre et al. 2005 (MODIS class albedos, potential natural vegetation), Ghimire et al.
2014 and Jiao et al. 2017 (MODIS look-up maps x LUH x kernels, 1700-2005), and Hasler et al. 2024
(the same machinery for restoration scenarios). The pollen-MODIS calibration literature that exists is
for *land cover*, not albedo (e.g. modern pollen against MODIS woody cover in 21 x 21 km windows, per
the search summary; not read).

---

## 3. The state of the art: where published methods agree and where they fork

**Agreed in practice (every study above that builds its own albedo):**

1. Source: MCD43 BSA at local solar noon and WSA, shortwave broadband 0.3-5.0 um (GlobAlbedo in
   Lejeune is the exception, chosen for 1998-2011 coverage).
2. Blue-sky by a linear mix of BSA and WSA with a diffuse weight; the product team's own definition.
3. Snow separated with the MCD43A2 snow flag (Jia 2022; Gao 2014; MCD43GF) or a MODIS snow-cover
   climatology applied to snow/snow-free class albedos (Jiao 2017; Hasler 2024), and snow-free
   versions published as such.
4. Water handled by class or mask, not by albedo value: land-cover fractions (Duveiller, Hasler's
   >= 1 % non-water rule), pure-class cells (Lejeune's 95 %), class maps (Zhao and Jackson), or the
   product's own land/water type (available in MCD43A2; used by MCD43 processing itself).
5. Reduction to a climatology by a plain summary of retrievals (mean: Jia, Zhao and Jackson, Li;
   median: Duveiller, Gao/Jiao), over 5 to 20 years; no insolation weighting of the noon albedo in
   any study found (the ABoVE product weights *hourly* albedo within a day, which is a different
   quantity).
6. Area-weighted aggregation from the native grid to the model grid (Jia: mean of 500 m to 0.05
   degree; Oleson 2003 per the designs; Duveiller to 1 degree).

**Forks, each with a published precedent:**

| Choice | Option A (precedent) | Option B (precedent) |
|---|---|---|
| Diffuse fraction | reanalysis total-sky direct/diffuse (NCEP/NCAR: Jiao 2017, Hasler 2024; WRF-CLM: Zhao and Jackson; ERA5: this project's AGU abstract) | aerosol-only clear-sky LUT (Jia 2022; ABoVE) or simply the mean of BSA and WSA (Duveiller 2018; Li 2015; Lejeune 2020) |
| Quality | full inversions / "highest quality" (user guide; Jia's SZA cut and trimming) | all processed retrievals (Li 2015 QC 0-2; Wang 2018 all-quality validation; Zhao and Jackson weight low-quality years by half) |
| Mean vs median | mean over years (Jia 2022 with min/max trimmed; Zhao and Jackson) | median (Duveiller 2018, five years; Gao 2014 monthly median) |
| Solar zenith | 70 degrees (MCD43 guidance; ABoVE; CLARA) | 75 degrees (Jia 2022) |
| Gaps | left as missing (Duveiller, Jia, Li) | filled (MCD43GF temporal/spatial fit; Moody 2005/2008 ecosystem-dependent interpolation; Lejeune's random-forest extension of D18) |
| Period | matched to the study's land-cover epoch (Duveiller 2008-2012; Zhao 2001-2011) | the longest available (Jia 2001-2020) |
| Support | native pixel then area mean (Jia; Oleson) | window regression / paired pixels to isolate class contrasts (Duveiller; Li; Zhao and Jackson) |

**Is there a "standard way"?** For a *climatology of the actual surface* at coarse resolution: the
closest thing to a standard is "MCD43 (A3 or C3) daily BSA/WSA, blue-sky mix, snow flag, QA, plain
multi-year mean or median per month, area aggregation" — exactly the lineage of Jia et al. 2022 and of
Duveiller et al. 2018. For a *land-cover-specific* albedo, the standard is the look-up-map lineage
(Gao 2014; Ghimire 2014; Jiao 2017; Hasler 2024). Our regression of cell albedo on cover fractions is
closer to Duveiller's unmixing than to the look-up maps, and its response should be the first kind.

---

## 4. Adopt or build? Options for the calibration response

The response we need: monthly broadband blue-sky albedo of the land part of each 1-degree cell,
2000s climatology, in all-conditions and snow-free versions. Ranked by fitness, defensibility,
effort, volume and residual unpublished judgements.

### Option 1. Adopt Jia et al. 2022 (500 m daily climatology), screen water with MOD44W, aggregate

- **Method sentence:** "Monthly blue-sky albedo is the mean over the calendar month of the daily
  actual (and snow-free) blue-sky albedo climatology of Jia et al. (2022; MCD43A3/A2 C6, 2001-2020,
  AOD-based diffuse fraction, SZA <= 75 degrees), averaged over the 500 m land pixels of each
  1-degree cell, land defined by the MOD44W v6.1 mask (Carroll et al. 2017)."
- **Fitness:** actual and snow-free in one product; 500 m allows exact water screening (the Jia
  files keep the MODIS sinusoidal tiles, so the 250 m MOD44W mask nests as in the designs); the
  attached MCD12Q1 climatology is a second water screen. Differences from the manuscript's stated
  construction: period 2001-2020 (not 2000-2009), Collection 6 (not 6.1), diffuse fraction from AOD
  (clear-sky) rather than ERA5 total-sky, min/max trimming, SZA 75 not 70. Winter snow-free blanks in
  the north are documented by the authors.
- **Defensibility:** high: peer-reviewed product with site validation, CC-BY-4.0 archive, used as the
  benchmark in its own paper against ERA5, MERRA-2, GLASS and GlobAlbedo.
- **Effort and volume:** about 60 GB of NetCDF for 41 tiles x 365 days (measured listing, not
  downloaded), plus the 5 GB 0.05-degree set as a check; R with `terra`/`ncdf4`; the processing is
  "mask, monthly mean, cell mean" with no retrieval logic.
- **Residual judgements:** the MOD44W rule (all four sub-pixels land, or a fraction), the reference
  year of the mask (2015 on disk), the number of valid daily-climatology values required per month,
  and how to form a "snowy" albedo (not provided; derive from all-conditions, snow-free and a snow
  fraction from MCD43C3 Percent_Snow or MOD10C1 monthly, Hasler-style, which is itself published).
- **Verify before adopting:** the variable layout of the 500 m files; whether the 0.05-degree means
  already contain lakes (expected: yes); and agreement with the legacy raster over interior land.

### Option 2. MCD43C3 v061, Duveiller-style monthly reduction, 0.05-degree water screen

- **Method sentence:** "Monthly albedo for 2000-2009 is the median (or mean) over years of the MCD43C3
  v061 daily shortwave albedo for the 16-day retrieval centred nearest the 15th of each month, as in
  Duveiller et al. (2018); blue-sky from BSA and WSA with the ERA5 diffuse fraction (or their mean, as
  in Duveiller 2018 and Li et al. 2015); 0.05-degree cells with Percent_Snow > 0 excluded for the
  snow-free version; cells with a MOD44W water fraction above x % excluded; land mean per 1-degree
  cell."
- **Fitness:** matches 2000-2009 and v061; one file per month gives 120 files x 182 MB = 22 GB, or the
  full daily set 0.65 TB if a true monthly mean is wanted; Percent_Snow, Percent_Inputs and Quality
  layers give the snow and coverage diagnostics directly. Weakness (verified from the product page):
  coastal and lake-shore CMG cells already average shallow-water 30-arcsecond pixels, which no later
  mask can undo; the product team recommends D or GF over C. A 0.05-degree water-fraction screen
  (Hasler's threshold, inverted) removes most but not all contamination.
- **Defensibility:** good (Duveiller 2018 and Li 2015 both built from MCD43C3 and both are
  Nature-family papers); reviewers may ask about the coastal issue.
- **Residual judgements:** the water-fraction threshold at 0.05 degree; the choice between "one
  retrieval per month" and "all days"; mean vs median; Percent_Snow threshold; the ERA5 hour.

### Option 3. Full rebuild from MCD43A3/A2 at 500 m with ERA5 (the two designs)

- **Fitness:** the only option that reproduces the manuscript's stated construction (v061, 2000-2009,
  ERA5 direct/diffuse), with exact water screening and all three snow streams. ~10 TB download by
  streaming or an AppEEARS subset pilot; the environment's GDAL currently lacks HDF4 (per the
  reconciliations).
- **Defensibility:** every step has a precedent (section 5) but the *combination* is ours, and the
  thresholds (dates per month, years, land share, zenith sensitivities) have no published values.
- **Residual judgements:** the most of any option: QA policy, reducer hierarchy, coverage thresholds,
  noon window, MOD44W rule, aggregation geometry (all listed in section 5).

### Option 4. CLARA-A3 blue-sky albedo (BAL), 0.25 degree monthly

- **Fitness:** a published, validated blue-sky monthly climatology on exactly the legacy lattice, with
  per-cell observation counts, 1979-2023; but AVHRR (not MODIS), 25 km cells that mix land and water
  with no way to separate them, snow and snow-free merged by observation count, and the direct fraction
  from a cloud-probability relation. Best used as the independent check of the chosen response (and
  of the legacy raster) rather than as the response.

### Option 5. Look-up-map products (Gao et al. 2014; Zhao and Jackson 2014 ORNL)

- Albedo *by class*, not of the actual surface. They could reconstruct a cell albedo from our
  three-class fractions, but that would replace the regression with a prescribed class albedo, i.e. a
  different method. Useful as a sanity check on the fitted class contrasts (forest vs open, snow and
  snow-free), as Bright and O'Halloran 2019 and Hasler 2024 use them.

### Option 6. MCD43GF (gap-filled snow-free)

- v061 does not yet cover 2000-2012 (CMR, verified today); v006 covers 2000-2017 but each daily global
  bundle is about 13.5 GB (about 48 TB for ten years) and only a snow-free product results. The ECMWF
  2017-2021 climatology is spectral only. Not recommended for the response; a sample would serve as a
  snow-free cross-check.

### Option 7. GlobAlbedo / QA4ECV, Copernicus, ERA5 albedo

- GlobAlbedo (used by Lejeune 2020) is MERIS/AATSR/VGT at 0.05 degree, no blue-sky, snow artefacts
  above 70 N; QA4ECV is still unpublished on CEDA; Copernicus has no blue-sky and is not MODIS; ERA5
  albedo is a model field. None fits better than options 1-3.

---

## 5. The two rebuild designs: what a published method settles, what remains a judgement

Read after the above: `2026-10-15_albedo_rebuild_design_{fable,codex_gpt6astra}.md` and the two
reconciliations. Their merged recipe, step by step, against the evidence in sections 1-3:

| Design step (merged) | Settled by published practice | Still a judgement (no published value) |
|---|---|---|
| MCD43A3/A2 v061, shortwave broadband, noon BSA and WSA | yes: user guide; Schaaf 2002; Lucht 2000; every study in section 2 | none |
| Blue-sky = (1 - f) BSA + f WSA | yes: user guide; Jia 2022 eq. 1; Zhao and Jackson eq. 1; CLARA-A3 (Pinty 2005, doi:10.1175/jas3479.1) | none (ignoring multiple scattering is the common simplification; ABoVE is the exception) |
| f from ERA5 ssrd and fdir, total sky, at local noon | the *kind* of source is precedented (reanalysis direct/diffuse: Jiao 2017, Hasler 2024 with NCEP/NCAR; WRF-CLM in Zhao and Jackson); the manuscript and the AGU abstract state ERA5 | ERA5 specifically (no published MODIS blue-sky climatology with ERA5 found); the noon hour window; equation of time; nearest-point lookup. Note the alternative precedents: AOD LUT (Jia 2022; ABoVE), plain mean (Duveiller; Li; Lejeune) |
| Quality: full inversions primary, all-processed sensitivity (or the reverse, as Astra conceded) | both have precedent: "highest quality full inversion" (user guide), QC 0-2 (Li 2015), all qualities with year weighting (Zhao and Jackson), all-quality accuracy (Wang 2018) | which is primary; both reconciliations differ, so this is Chris's call with pilot numbers |
| Snow partition from the A2 flag; three streams | yes: Jia 2022 (snow-free by ignoring flagged pixels); Gao 2014 (snow-covered/snow-free LUMs); MCD43GF | minimum dates and years per state; the "snowy share" definition (Hasler's monthly snow fraction from MOD10 is the published route) |
| No gap filling | precedent both ways (Jia and Duveiller leave gaps; MCD43GF, Moody, Lejeune fill) | the choice itself, and how the unfilled winter north enters the calibration |
| Noon SZA <= 70 degrees | yes: product guidance; ABoVE; CLARA (70 sun zenith); Jia used 75 | the 75 and "none" sensitivities; the irradiance floor |
| Water: MOD44W at 500 m before averaging, all four sub-pixels land | the *principle* is precedented (land defined by a class map or mask before aggregation: Hasler; Lejeune; Zhao and Jackson; MCD43's own land/water type); MOD44W is the operational MODIS mask (Carroll et al. 2017, doi:10.1080/17538947.2016.1232756, resolves) | the four-of-four rule, the 2015 reference year, QA-3 treatment, any minimum land share per cell. The Moody 2005 attribution is unverified. |
| Monthly mean per year, then equal years; mean not median | mean over years: Jia 2022 (trimmed), Zhao and Jackson, Li; the two-level hierarchy is not stated by any of them (Jia pools DOYs across years in one mean) | the hierarchy; the trimming (Jia) or not; the insolation-weighted alternative (no precedent) |
| Coverage thresholds (dates, years, land share) | none found in any study; Jia and Duveiller publish whatever has data; Zhao and Jackson require >= 3 pairs per cell | all thresholds |
| Area-weighted aggregation from equal-area pixels to 1-degree cells | yes: Jia (mean to 0.05 degree); Duveiller (to 1 degree); Oleson 2003 per the designs | centre assignment vs exact overlap |
| Start 5 March 2000; 2000-2009 | user guide for the validated start; the period is the manuscript's | the period, given Jia's 2001-2020 alternative and B10 |
| Outputs, manifest, checksums | project practice | none |

Two things the designs did not weigh, which the literature supplies:

1. **A published climatology already exists** (Jia 2022) that implements most of the merged recipe
   (A3/A2 inputs, blue-sky mix, snow flag, SZA cut, multi-year mean, 500 m with mean aggregation).
   Building our own duplicates it with different, unpublished thresholds; adopting it moves the
   method from "ours, grounded step by step" to "theirs, cited, plus two documented
   post-processing steps".
2. **The 0.05-degree route has a strong precedent** (Duveiller 2018; Li 2015) that the designs set
   aside on the coastal-averaging caution; that caution is real but affects only shoreline cells and
   can be bounded with the MOD44W water fraction. The designs' judgement stands, but it is a
   trade-off, not a disqualification.

What remains a judgement whichever option is chosen: the water rule and its reference year; the
minimum coverage for a cell-month to enter the calibration; the treatment of the winter north; and
the snowy-albedo construction for the sensitivity test.

---

## 6. Ranked recommendation and decisions for Chris

**Recommendation (mine, not evidence):**

1. **Adopt Jia et al. 2022 at 500 m** as the primary calibration response (all-conditions and
   snow-free), with MOD44W water screening before the cell mean and plain monthly means of the daily
   climatology. Cite: Jia et al. 2022 (product), Carroll et al. 2017 (mask), Hasler et al. 2024 (land
   definition and snow/snow-free combination), Oleson 2003 / Jia 2022 (area aggregation). Keep the
   legacy raster as the legacy setting for the regression contract.
2. **MCD43C3 v061 Duveiller-style** as the fallback if Chris or Andria require 2000-2009 and v061:
   22 GB, one script, citing Duveiller et al. 2018 and Li et al. 2015, with the coastal caveat stated.
3. **The 500 m rebuild** only if the manuscript's ERA5 total-sky diffuse fraction and v061/2000-2009
   are non-negotiable; then follow the reconciled recipe and label its thresholds as judgements.
4. **CLARA-A3 BAL** and the 0.05-degree Jia set as independent checks of whichever response is used
   and of the legacy raster.

**Decisions for Chris:**

1. Period and collection: accept a 2001-2020 Collection 6 climatology (option 1) or insist on
   2000-2009 v061 (options 2-3)? This also decides whether the methods text changes; Andria must agree.
2. Diffuse fraction: accept the published AOD-based clear-sky weighting (Jia) or require ERA5
   total-sky (rebuild)? Published precedent exists for both; the manuscript currently says ERA5.
3. Water rule at 500 m: all four MOD44W sub-pixels land (designs) and the 2015 mask; and whether any
   cell-level minimum land share applies.
4. Snowy albedo for the sensitivity test: derive from all-conditions, snow-free and a monthly snow
   fraction (Hasler/Jiao route, MOD10C1 or MCD43C3 Percent_Snow), or build it only in the rebuild?
5. Minimum coverage for a cell-month (no published number; recommend reporting counts and letting a
   stated minimum be a config key).
6. Whether to run the 0.05-degree Jia set (5 GB) first as a quick comparison against the legacy raster
   before any 500 m download.

---

## References (all DOIs resolved on Crossref or doi.org, 2026-10-15)

Products and documentation
- Schaaf, C. B. et al. (2002). First operational BRDF, albedo nadir reflectance products from MODIS. RSE 83, 135-148. doi:10.1016/S0034-4257(02)00091-3
- Lucht, W., Schaaf, C. B., Strahler, A. H. (2000). An algorithm for the retrieval of albedo from space using semiempirical BRDF models. IEEE TGRS 38, 977-998. doi:10.1109/36.841980
- Gao, F. et al. (2005). MODIS BRDF and albedo Climate Modeling Grid products and the variability of albedo for major global vegetation types. JGR 110, D01104. doi:10.1029/2004JD005190
- Wang, Z., Schaaf, C. B., Sun, Q., Shuai, Y., Román, M. O. (2018). Capturing rapid land surface dynamics with Collection V006 MODIS BRDF/NBAR/Albedo (MCD43) products. RSE 207, 50-64. doi:10.1016/j.rse.2018.02.001
- Sun, Q., Wang, Z., Li, Z., Erb, A., Schaaf, C. (2017). Evaluation of the global MODIS 30 arc-second spatially and temporally complete snow-free land surface albedo and reflectance anisotropy dataset. IJAEOG 58, 36-49. doi:10.1016/j.jag.2017.01.011
- Román, M. O. et al. (2010). Assessing the coupling between surface albedo derived from MODIS and the fraction of diffuse skylight over spatially-characterized landscapes. RSE 114, 738-760. doi:10.1016/j.rse.2009.11.014
- Stroeve, J. et al. (2005). Accuracy assessment of the MODIS 16-day albedo product for snow. RSE 94, 46-60. doi:10.1016/j.rse.2004.09.001
- Wang, X., Zender, C. S. (2010). MODIS snow albedo bias at high solar zenith angles relative to theory and to in situ observations in Greenland. RSE 114, 563-575. doi:10.1016/j.rse.2009.10.014
- Moody, E. G. et al. (2005). Spatially complete global spectral surface albedos: value-added datasets derived from Terra MODIS land products. IEEE TGRS 43, 144-158. doi:10.1109/TGRS.2004.838359
- Moody, E. G. et al. (2007). Northern Hemisphere five-year average (2000-2004) spectral albedos of surfaces in the presence of snow. RSE 111, 337-345. doi:10.1016/j.rse.2007.03.026
- Moody, E. G., King, M. D., Schaaf, C. B., Platnick, S. (2008). MODIS-derived spatially complete surface albedo products: spatial and temporal pixel distribution and zonal averages. JAMC 47, 2879-2894. doi:10.1175/2008JAMC1795.1
- Jia, A., Wang, D., Liang, S., Peng, J., Yu, Y. (2022). Global daily actual and snow-free blue-sky land surface albedo climatology from 20-year MODIS products. JGR Atmos. 127, e2021JD035987. doi:10.1029/2021JD035987; data doi:10.5281/zenodo.6359686 (500 m) and 10.5281/zenodo.6350821 (CMG)
- Liang, S. et al. (2021). The Global Land Surface Satellite (GLASS) product suite. BAMS 102, E323-E337. doi:10.1175/BAMS-D-18-0341.1
- Qu, Y. et al. (2014). Direct-estimation algorithm for mapping daily land-surface broadband albedo from MODIS data. IEEE TGRS 52, 907-919. doi:10.1109/TGRS.2013.2245670
- Liu, Q. et al. (2013). Preliminary evaluation of the long-term GLASS albedo product. IJDE 6, 69-95. doi:10.1080/17538947.2013.804601
- Liang, S. et al. (2013). A long-term Global LAnd Surface Satellite (GLASS) data-set for environmental studies. IJDE 6, 5-33. doi:10.1080/17538947.2013.805262
- Karlsson, K.-G. et al. (2023). CLARA-A3: the third edition of the AVHRR-based CM SAF climate data record on clouds, radiation and surface albedo covering the period 1979 to 2023. ESSD 15, 4901-4926. doi:10.5194/essd-15-4901-2023; data doi:10.5676/EUM_SAF_CM/CLARA_AVHRR/V003
- Pinty, B. et al. (2005). Coupling diffuse sky radiation and surface albedo. J. Atmos. Sci. 62, 2580-2591. doi:10.1175/jas3479.1
- He, T., Liang, S., Song, D.-X. (2014). Analysis of global land surface albedo climatology and spatial-temporal variation during 1981-2010 from multiple satellite products. JGR Atmos. 119, 10281-10298. doi:10.1002/2014JD021667
- Gao, F. et al. (2014). Multiscale climatological albedo look-up maps derived from MODIS BRDF/albedo products. J. Appl. Remote Sens. 8, 083532. doi:10.1117/1.JRS.8.083532
- Solvik, K. K. et al. (2019). ABoVE: MODIS-derived daily mean blue sky albedo for northern North America, 2000-2017. ORNL DAAC. doi:10.3334/ORNLDAAC/1605
- Potter, S. et al. (2019). Climate change decreases the cooling effect from postfire albedo in boreal North America. GCB. doi:10.1111/gcb.14888
- Zhao, K., Jackson, R. B. (2014). MODIS-derived biophysical parameters for 5-km land cover, North America, 2000-2012. ORNL DAAC. doi:10.3334/ORNLDAAC/1210
- Liu, Y. et al. (2017). Evaluation of the VIIRS BRDF, albedo and NBAR products suite and an assessment of continuity with the long term MODIS record. RSE. doi:10.1016/j.rse.2017.09.020
- Carroll, M. L. et al. (2009). A new global raster water mask at 250 m resolution. IJDE 2, 291-308. doi:10.1080/17538940902951401
- Carroll, M. L. et al. (2017). Development of an operational land water mask for MODIS Collection 6, and influence on downstream data products. IJDE 10, 207-218. doi:10.1080/17538947.2016.1232756
- Hall, D. K. et al. (2002). MODIS snow-cover products. RSE 83, 181-194. doi:10.1016/S0034-4257(02)00095-0
- Friedl, M. A. et al. (2010). MODIS Collection 5 global land cover. RSE 114, 168-182. doi:10.1016/j.rse.2009.08.016
- Hersbach, H. et al. (2020). The ERA5 global reanalysis. QJRMS 146, 1999-2049. doi:10.1002/qj.3803
- Shuai, Y. et al. (2011). An algorithm for the retrieval of 30-m snow-free albedo from Landsat surface reflectance and MODIS BRDF. RSE 115, 2204-2216. doi:10.1016/j.rse.2011.04.019
- Li, Z. et al. (2018). Preliminary assessment of 20-m surface albedo retrievals from Sentinel-2A surface reflectance and MODIS/VIIRS surface anisotropy measures. RSE 217, 352-365. doi:10.1016/j.rse.2018.08.025
- NASA LP DAAC product DOIs: 10.5067/MODIS/MCD43A3.061; 10.5067/MODIS/MCD43A2.061; 10.5067/MODIS/MCD43C3.061; 10.5067/MODIS/MCD43D42.061; 10.5067/MODIS/MCD43GF.061; 10.5067/VIIRS/VNP43C3.002

Studies
- Duveiller, G., Hooker, J., Cescatti, A. (2018). The mark of vegetation change on Earth's surface energy balance. Nat. Commun. 9, 679. doi:10.1038/s41467-017-02810-8
- Duveiller, G., Hooker, J., Cescatti, A. (2018). A dataset mapping the potential biophysical effects of vegetation cover change. Sci. Data 5, 180014. doi:10.1038/sdata.2018.14; data doi:10.6084/m9.figshare.c.3829333
- Lejeune, Q. et al. (2020). Biases in the albedo sensitivity to deforestation in CMIP5 models and their impacts on the associated historical radiative forcing. ESD 11, 1209-1232. doi:10.5194/esd-11-1209-2020
- Bright, R. M. et al. (2015). Radiative forcing bias of simulated surface albedo modifications linked to forest cover changes at northern latitudes. Biogeosciences 12, 2195-2205. doi:10.5194/bg-12-2195-2015
- Bright, R. M. et al. (2017). Local temperature response to land cover and management change driven by non-radiative processes. Nat. Clim. Change 7, 296-302. doi:10.1038/nclimate3250 (methods not read)
- Bright, R. M., O'Halloran, T. L. (2019). Developing a monthly radiative kernel for surface albedo change from satellite climatologies of Earth's shortwave radiation budget: CACK v1.0. GMD 12, 3975-3990. doi:10.5194/gmd-12-3975-2019
- Zhao, K., Jackson, R. B. (2014). Biophysical forcings of land-use changes from potential forestry activities in North America. Ecol. Monogr. 84, 329-353. doi:10.1890/12-1705.1
- Li, Y. et al. (2015). Local cooling and warming effects of forests based on satellite observations. Nat. Commun. 6, 6603. doi:10.1038/ncomms7603
- Ghimire, B. et al. (2014). Global albedo change and radiative cooling from anthropogenic land cover change, 1700 to 2005. GRL 41, 9087-9096. doi:10.1002/2014GL061671
- Jiao, T. et al. (2017). Global climate forcing from albedo change caused by large-scale deforestation and reforestation. Climatic Change 142, 463-476. doi:10.1007/s10584-017-1962-8
- Hasler, N. et al. (2024). Accounting for albedo change to identify climate-positive tree cover restoration. Nat. Commun. 15, 2275. doi:10.1038/s41467-024-46577-1
- Myhre, G. et al. (2005). Radiative forcing due to anthropogenic vegetation change based on MODIS surface albedo data. GRL 32(21), 2005GL024004. doi:10.1029/2005GL024004
- Barnes, C. A., Roy, D. P. (2008). Radiative forcing over the conterminous United States due to contemporary land cover land use albedo change. GRL 35(9), 2008GL033567. doi:10.1029/2008GL033567
- Oleson, K. W. et al. (2003). Assessment of global climate model land surface albedo using MODIS data. GRL 30, 1443. doi:10.1029/2002GL016749
- Alkama, R., Cescatti, A. (2016). Biophysical climate impacts of recent changes in global forest cover. Science 351, 600-604. doi:10.1126/science.aac8083 (not read)
- Shell, K. M., Kiehl, J. T., Shields, C. A. (2008). Using the radiative kernel technique to calculate climate feedbacks in NCAR's CAM. J. Climate 21, 2269-2282. doi:10.1175/2007JCLI2044.1
- Hopcroft, P. O. et al. (2026). Biogeophysical impact of land-use scenarios on Holocene surface temperatures. GRL 53, e2025GL118518. doi:10.1029/2025GL118518
- Dawson, A. et al. (2025). Holocene land cover change in North America. Clim. Past 21, 2031. doi:10.5194/cp-21-2031-2025

Not verifiable at source: Lewis and Barnsley 1994 (conference, no DOI); GlobAlbedo papers (Lewis et
al. 2012; Muller et al. 2013); the AGU 2023 abstract text; any MuSyQ albedo paper; the Globus
location of the Gao 2014 maps.

Downloaded sources are in `fable_sources/` (PDFs, HTML, converted .txt, CMR and Crossref JSON).
