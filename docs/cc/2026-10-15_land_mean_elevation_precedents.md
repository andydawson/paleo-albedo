# Precedents for land-only cell-mean elevation (literature check by Fable, 2026-10-15)

Asked because Chris approved the land-mean elevation only if grounded in published practice. Fable's report, unedited. Outcome: Chris chose ocean excluded, lakes kept (2026-10-15).

---

# Precedents for land-only cell-mean elevation (literature check, 2026-10-15)

Question: is there a published precedent for taking a grid cell's elevation as the
mean over the land part of the cell only (ocean and inland water excluded), from a
finer DEM, for use alongside land-only quantities?

Short answer: two verified precedents exclude the **ocean** part of the cell from the
mean (New et al. 2002; ISLSCP II HYDRO1k). Neither excludes **inland lakes** (both DEMs
carry lake-surface elevations, and nothing in the text removes them). No MODIS-albedo
or vegetation study was found that states a land-only mean elevation. The wording
"the mean elevation of the land pixels ... from GTOPO30" attributed to New et al. 2002
in the question is **not** in that paper: the DEM is GLOBE, and the sentence is worded
differently (quoted below).

Working files (PDFs and text extractions) are in this folder.

## Accepted precedents

### 1. New, Lister, Hulme & Makin (2002), CRU CL 2.0

New M, Lister D, Hulme M, Makin I (2002) A high-resolution data set of surface climate
over global land areas. Climate Research 21: 1-25. doi:10.3354/cr021001
(Crossref resolves: title, journal, vol 21, pp 1-25, 2002.)

Source read: the publisher PDF https://www.int-res.com/articles/cr2002/21/c021p001.pdf
(WebFetch was rate-limited with HTTP 429; a direct curl download succeeded) and the
copy on the CRU site https://crudata.uea.ac.uk/cru/data/hrg/tmc/new_et_al_10minute_climate_CR.pdf.
The two files are byte-identical (md5 ca877fce3931a53bb9355cb586223ecf).

Passage, p. 7 (section 2.3, left column, immediately before Fig. 10):

> "The elevation data used to create grids of the interpolated surfaces derives from
> the GLOBE 1 km elevation database, downloaded from the National Geophysical Data
> Centre (NGDC 2000). The GLOBE elevation data were resampled to produce a 10'
> elevation grid, simply by calculating the average of all GLOBE elevations in each
> 10' grid box. A 10' cell was considered to be ocean only if < 25% of the constituent
> GLOBE elevation cells were land. As a result, the elevation of any 10' cell
> designated as land represents the average elevation of at least 100 (maximum 400)
> GLOBE elevation points."

Supporting sentence (under 30 words, p. 7): "the elevation of any 10' cell designated
as land represents the average elevation of at least 100 (maximum 400) GLOBE elevation
points."

What it supports:
- Cell-mean elevation from a 1 km DEM for a coarser climatology grid: yes, explicitly.
- Mean restricted to the land part of the cell: yes, by necessary implication. A 10'
  box holds 400 GLOBE cells; "at least 100 (maximum 400)" points can only be the number
  of land cells (>= 25% of 400), so ocean cells are not in the average. This is
  consistent with GLOBE itself: ocean cells carry the no-data flag -500, not an
  elevation (GLOBE documentation, Section 11C: "Every tile contains values of -500
  for oceans, with no values between -500 and the minimum value for land noted here",
  https://www.ngdc.noaa.gov/mgg/topo/report/s11/s11C.html; also GLOBE documentation
  manual: cells "masked as 'no data' and have been assigned a value of -500").
- The paper does not use the words "land pixels" and does not use GTOPO30.
- Inland lakes: not excluded. GLOBE has elevation values over lake surfaces, and the
  text gives no lake mask, so the "land" mean includes lakes.
- Context match: the covariate (elevation) is the co-predictor for a land-only
  climatology (global land areas), i.e. covariate support matched to a land-only
  response. Elevation is used as a co-predictor in thin-plate spline interpolation
  (p. 7 and section 3).

### 2. ISLSCP II HYDRO1k Elevation-derived Products (Verdin 2011)

Verdin KL (2011) ISLSCP II HYDRO1k Elevation-derived Products. In: Hall FG, Collatz G,
Meeson B, Los S, Brown de Colstoun E, Landis D (eds) ISLSCP Initiative II Collection.
ORNL DAAC, Oak Ridge, Tennessee. doi:10.3334/ORNLDAAC/1007
(Crossref resolves: "ISLSCP II HYDRO1K ELEVATION-DERIVED PRODUCTS", Verdin, 2011-04-08.)
Collection paper: Hall FG et al. (2006) The ISLSCP Initiative II global data sets.
J Geophys Res 111, doi:10.1029/2006JD007366 (not re-read here; cited in the data doc).

Source read: the data-set documentation PDF
https://data.ornldaac.earthdata.nasa.gov/public/islscp_ii/hydrology_soils/hydro1k_elevation_xdeg/comp/1_hydro1k_elevation_doc.pdf
(saved as islscp_hydro1k.pdf).

Passages:
- Section 6.1: "All of the data provided here in the ISLSCP Initiative II data
  collection are based on the HYDRO1k DEM at 1-km spatial resolution which has been
  aggregated to spatial resolutions of 0.5 and 1.0 degrees." and "the basis of all of
  the HYDRO1k data layers is a hydrologically conditioned DEM. This is, essentially, a
  quality controlled global DEM called GTOPO30".
- Section 8.2 (parameter table): "Elev_mean  Mean elevation value within each cell",
  units metres, source HYDRO1k.
- Section 9.2.1: "The spatial extent of the HYDRO1k data for each continent was
  projected into geographics - defining the extent of the non-ocean pixels within the
  geographic framework" ... "This projected template was used to define the zones for
  preparation of the summary statistics. Summary statistics were calculated using
  ARCVIEW zonal stats functions."
- Section 9.2.3: "the original data had elevation information where the ISLSCP II
  mask showed land (i.e. 50% or greater of the cell is land)".

Supporting sentence (under 30 words, section 9.2.1): "defining the extent of the
non-ocean pixels within the geographic framework" together with "Summary statistics
were calculated using ARCVIEW zonal stats functions."

What it supports:
- Cell-mean (0.5 degree and 1 degree) elevation computed as a zonal mean of 1 km
  GTOPO30-derived (HYDRO1k) pixels: yes, explicitly.
- Mean restricted to the land (non-ocean) part of the cell: yes; the zones are
  defined by the extent of the non-ocean pixels, and HYDRO1k has no values over
  ocean (section 9.2.3 says missing values -9999 were recoded only afterwards).
- Delivered as a land-surface boundary-condition set alongside land-only fields
  (ISLSCP II, "Surface Boundary Conditions and Atmospheric Forcings for
  Land-Atmosphere Studies"), i.e. the same support-matching rationale.
- Inland lakes: not excluded; HYDRO1k carries lake-surface elevations and nothing in
  the document removes them.
- This is a DOI'd data-set document, not a peer-reviewed article.

## Candidates checked that do NOT support it

| Candidate | Source read | Finding |
|---|---|---|
| CRU regional climatologies (Hulme, CRU page) | https://crudata.uea.ac.uk/cru/data/hulme/regional/ | Counter-example. "Working from an original 5' (for Europe 10') elevation dataset, we have produced mean, max. and min. elevations for each 0.5 degree cell" as "the average of 36 values"; "The cell is classified as 'land' if the max. elevation is positive". The 36 values include any ocean sub-cells; no land-only mean. |
| CRU TS 2.1 documentation | https://crudata.uea.ac.uk/cru/data/hrg/timm/grid/CRU_TS_2_1_text.html | Only: "An elevations file for the 0.5 degree grid ... also implicitly describes the land-sea mask, since only land boxes are given elevations." Nothing on how the cell value was formed. |
| Mitchell & Jones (2005), doi:10.1002/joc.1181 | UEA eprint (no full text); indiawaterportal copy hung | Not verified; no open copy obtained. |
| Harris et al. (2014) CRU TS3.10, doi:10.1002/joc.3711 | open copy at eesm.science.energy.gov (joc3711.pdf), section 3 | "The Terrainbase 5 elevation database ... is used to determine land cells: only if all 36 Terrainbase gridcells are marked as 'ocean', will the enclosing CRU TS grid cell be marked as 'ocean'." A land-mask rule, not a land-only mean. |
| Harris et al. (2020) CRU TS4, doi:10.1038/s41597-020-0453-3 | Europe PMC full text PMC7125108 | "Elevation is not specifically included in the interpolation; it is introduced via the climatologies". Nothing on cell elevation. |
| New, Hulme & Jones (1999) J Climate 12:829-856 | AMS site returned 403 | Not verified. |
| Hijmans et al. (2005) WorldClim 1, doi:10.1002/joc.1276 | Wiley full text 403; abstract (ice.ucdavis.edu) and WorldClim 1.4 methods page | Only "The SRTM elevation database (aggregated to 30 arc-seconds, '1 km')". No land/water statement found. |
| Fick & Hijmans (2017) WorldClim 2, doi:10.1002/joc.5086 | Wiley 403; WorldClim 2.1 elevation page | Page says only "derived from the SRTM elevation data". Not verified. |
| CHELSA (Karger et al. 2017, doi:10.1038/sdata.2017.122) | tech-spec PDF URLs returned 404; chelsa-climate.org model page | GMTED2010 used at its native 30 arc-sec grid, no aggregation step; nothing on land-only means. |
| Daymet V4 (Thornton et al. 2021, doi:10.1038/s41597-021-00973-0) | Europe PMC full text PMC8302764 | DEM is SRTM 30 arc-sec "resampled to each 1 km x 1 km Daymet grid" (resampled, not a land-only mean); land/water mask from MOD44W; "Inland water bodies are considered as land in the Daymet domain" - the opposite of excluding lakes. |
| Daymet overview / V4 user guide | daymet.ornl.gov/overview; ORNL DAAC guide | Lists "a digital elevation model ... a land water mask" as inputs; no method. |
| VEMAP Phase 1 user guide, section 7.3.1 | ORNL DAAC Phase_1_User_Guide.pdf | Counter-example: "Aggregated elevation for each 0.5deg. cell was computed as a simple mean of nine 10-minute grid cell modal values. Elevations for inland water bodies are included". |
| ERA5-Land documentation | ECMWF Confluence "ERA5-Land: data documentation" | Geopotential is a time-invariant field interpolated from ERA5; fields "masked for all oceans". No land-only orography averaging stated. |
| CLM5 technical note, section 2.2.3.3 | escomp.github.io CTSM tech note | Elevation and slope from USGS HYDRO1K; percent lake "area-averaged ... using the MODIS land-mask". No land-only mean elevation statement. |
| ECOCLIMAP-II (Faroux et al. 2013, GMD) | PDF fetch returned a logo file | Not verified. |
| GEM-Surf land-surface algorithm document (ECCC, 2012, grey literature) | collaboration.cmc.ec.gc.ca PDF | Orography from SRTM + GTOPO30; MODIS albedo "representative of the land surface component ... do not include the effect of water surfaces". Confirms MODIS albedo is land-only, says nothing about land-only orography. |
| UNH RIMS GtopoElevation readme | wsag.unh.edu text file | "Elevation in meters above sea level for all land grid cells. Data from GTOPO data set, original grid resolution at 30 arc seconds." Unpublished readme, no method; too weak to cite. |
| GMTED2010 (Danielson & Gesch 2011, doi:10.3133/ofr20111073) | already verified by the caller | Cell-mean product; no land-only restriction. |
| MODIS-albedo regression studies | several web searches; MCD43 user guide fetch 404 | No study found that states a land-only mean elevation paired with land-only albedo. |

## Bottom line

- Verified precedent for "cell elevation = mean of the fine-DEM pixels that are land
  (non-ocean)", used as the elevation co-predictor of a land-only gridded product:
  New et al. (2002), p. 7, with GLOBE 1 km; and ISLSCP II HYDRO1k (Verdin 2011) with
  GTOPO30-derived 1 km data at 0.5 and 1 degree.
- No verified precedent for additionally excluding inland lakes from the mean. Both
  precedents implicitly include lake surfaces; Daymet explicitly treats inland water
  as land. If the project excludes lakes with MOD44W, that step has to be justified on
  its own terms (matching the elevation support to the MOD44W-masked albedo support),
  not by citation.
- Two explicit counter-examples (CRU regional 0.5 degree climatologies; VEMAP) average
  over all sub-cells including water.
