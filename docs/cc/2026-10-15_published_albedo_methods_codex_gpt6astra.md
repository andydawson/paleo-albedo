# Published methods for gridded albedo rasters: review by Astra (Codex gpt-6-astra, max effort)

Independent of Fable's review. Unedited. Nothing here is a decision.

---

**Published methods are available, but I found no verified, ready-made product that simultaneously supplies the exact 2000–2009 period, a land-only response for every mixed coastal or lake cell, and the required blue-sky snow treatments.** The literature supports established retrievals and processing steps; it does not prescribe one universal monthly climatology recipe.

Three published products deserve consideration before implementing either rebuild design:

- **Jia et al. (2022):** ready-made actual and snow-free blue-sky climatologies, with a manageable 6 GB download at 0.05°. However, they represent 2001–2020 and use clear-sky atmospheric assumptions. [Paper, §§2.1–2.2](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021JD035987).
- **CLARA-A3:** monthly blue-sky albedo, including separate snow-free and combined snow/non-snow fields, covering the requested decade. Mixed land/water cells remain an obstacle. [Product manual, Table 4-1](https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/saf-cm-fmi-pum-gac-sal-3-1.pdf).
- **Hou et al. (2025), ALLUMs:** monthly 1° land-cover-specific albedo and areas, with published water screening and reconstruction methods. It introduces land-cover-conditioned gap filling into any reconstructed response. [Methods](https://www.nature.com/articles/s41586-025-08987-z).

If the present requirements remain fixed, processing MCD43A3/A2 v061 remains the strongest route. That can be grounded in published practice without inventing an albedo retrieval algorithm. Nevertheless, the water mask, averaging target, QA policy and coverage requirements still need explicit decisions. **Neither existing rebuild design is fully settled by the literature.**

The review below is current to **15 October 2026**. I formed the literature assessment before reading the local designs and reconciliations. I changed no files and opened neither `writing/` nor repository PDFs, Word documents or presentations.

**Verification limit:** identifiers were checked against primary papers or official records. Successful end-to-end DOI retrieval was verified for Jia (2022), Riihelä et al. (2024) and Karlsson et al. (2023). For the other identifiers below, successful resolver retrieval remains **unverified**: requests encountered access or retrieval failures, although the primary records established their identities. The local Crossref checker also failed because its host could not resolve; I cannot report a completed two-checker audit. Unverified product details are identified individually below. No downloaded raster values were inspected.

**The quantities need to be distinguished before selecting a product.** Black-sky albedo describes direct illumination at a specified solar angle; white-sky albedo assumes isotropic diffuse illumination. The conventional blue-sky approximation combines them:

\[
\alpha_{\mathrm{blue}}=(1-f_{\mathrm{diffuse}})\alpha_{\mathrm{black}}
+f_{\mathrm{diffuse}}\alpha_{\mathrm{white}}.
\]

The solar angle, atmospheric assumptions and averaging period matter. These distinctions and the need to match spatial and temporal support are covered by Wang et al. (2019), *Global Surface Albedo Product Validation Best Practices Protocol*, CEOS WGCV LPV, DOI **10.5067/DOC/CEOSWGCV/LPV/ALBEDO.001**, especially §§3 and 5. [Protocol](https://lpvs.gsfc.nasa.gov/PDF/CEOS_ALBEDO_Protocol_20190307_v1.pdf).

For this project, three snow quantities are also different:

| Quantity | Meaning for the calibration |
|---|---|
| Actual or all-conditions albedo | Includes the snow conditions represented in the observations or reconstruction. |
| Observed snow-free albedo | Conditions on observations classified as snow-free; winter values can be absent. |
| Reconstructed snow-free albedo | Estimates a snow-free surface where observations are missing or snow-covered. This adds modelling assumptions. |

Thus, “actual” need not mean all-weather illumination, and “snow-free” need not mean a complete hypothetical winter without snow.

**The main product families provide the following evidence.** Access, licences and volumes are compared separately afterwards.

1. **MCD43A3/A2 and MCD43C3, Collection 6.1**

   MCD43A3 supplies daily nominal 500 m black-sky albedo at local solar noon and white-sky albedo: seven spectral bands plus visible, near-infrared and total shortwave broadband, the latter **0.3–5.0 μm**. Each daily estimate uses a moving 16-day observation window. MCD43A2 supplies detailed inversion quality, snow status and land/water information. These permit quality and snow-state selection, but A3 is not itself a blue-sky or gap-filled snow-free climatology. The team recommends band-specific full-inversion results. [A3 guide, product description and QA tables](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a3-albedo-product/); [A2 specification](https://ladsweb.modaps.eosdis.nasa.gov/filespec/MODIS/61/MCD43A2).

   MCD43C3 supplies the corresponding quantities globally at **0.05°**. Its guide explicitly says that shallow-water inputs along coasts enter the coarse averages. Its QA describes the underlying mixture; percentage snow does not provide separate snowy and snow-free albedos. The team recommends the finer MCD43D or GF products where possible. [C3 guide, product description](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43c3-cmg-albedo-product/).

   The record begins during early 2000, so January–February 2000 cannot supply a complete MODIS contribution. Daily files are overlapping retrievals, not independent daily observations. [MODIS guide, Introduction](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/introduction/).

   Foundation: Schaaf et al. (2002), *First operational BRDF, albedo nadir reflectance products from MODIS*, **Remote Sensing of Environment 83**, 135–148; DOI **10.1016/S0034-4257(02)00091-3**. Product DOIs: **10.5067/MODIS/MCD43A3.061**, **10.5067/MODIS/MCD43A2.061**, **10.5067/MODIS/MCD43C3.061**.

   Validation: the current NASA status is **Stage 4**, superseding older guide text saying Stage 3. NASA warns about solar zenith angles above 70° and rapid surface transitions. Wang et al. (2018), *Capturing rapid land surface dynamics with Collection V006 MODIS BRDF/NBAR/Albedo products*, DOI **10.1016/j.rse.2018.02.001**, reports RMSE below 0.0318 in its validation—not a guarantee for every northern winter cell. [Current validation statement](https://modis-land.gsfc.nasa.gov/ValStatus.php?ProductID=MOD43); [paper abstract](https://modis.gsfc.nasa.gov/sci_team/pubs/abstract.php?id=27680).

2. **MCD43GF: spatially and temporally completed snow-free MODIS**

   GF supplies daily **30 arc-second** black-/white-sky albedo, BRDF parameters and related quantities. It retains high-quality retrievals and fills gaps through temporal phenology fitting, spatial fitting and smoothing; QA distinguishes observed and reconstructed values. It is a snow-free reconstruction, not a paired actual/snowy product and not blue-sky without further atmospheric processing. [GF guide, algorithm and QA descriptions](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43gf-cmg-gap-filled-snow-free-products/).

   A material availability issue: an **August 2026 LP DAAC notice** says v061 is available only for **2013–2021**, with no completion timetable; it recommends v006 for earlier years. Consequently, v061 cannot currently provide this project’s decade. [Official notice](https://forum.earthdata.nasa.gov/viewtopic.php?t=8102). DOIs: **10.5067/MODIS/MCD43GF.061** and **10.5067/MODIS/MCD43GF.006**.

   Sun et al. (2017), *Evaluation of the global MODIS 30 arc-second spatially and temporally complete snow-free land surface albedo and reflectance anisotropy dataset*, **International Journal of Applied Earth Observation and Geoinformation 58**, 36–49; DOI **10.1016/j.jag.2017.01.011**. Its reported red/NIR reconstruction errors should not be presented as total-shortwave validation errors. [Author-institution abstract](https://collaborate.umb.edu/en/publications/evaluation-of-the-global-modis-30-arc-second-spatially-and-tempor/).

3. **Moody et al. 2005/2008, and the related North American work by Fang et al.**

   Moody’s products use older Terra MOD43B3 data to provide spatially completed **snow-free black-/white-sky** albedo at **1 arc-minute**, for 23 sixteen-day periods. The 2000–2004 products include individual years and a five-year aggregate. High-quality inputs are retained; ecosystem-dependent phenological filling supplies missing values, with separate processing QA. Exact water screening was **not verified**. [NASA product description](https://modis-images.gsfc.nasa.gov/ALBEDO/index.html); [FAQ](https://modis-images.gsfc.nasa.gov/ALBEDO/faq.html).

   References:

   - Moody et al. (2005), *Spatially Complete Global Spectral Surface Albedos: Value-Added Datasets Derived From Terra MODIS Land Products*, **IEEE TGRS 43**, 144–158; DOI **10.1109/TGRS.2004.838359**.
   - Moody et al. (2008), *MODIS-Derived Spatially Complete Surface Albedo Products: Spatial and Temporal Pixel Distribution and Zonal Averages*, **JAMC 47**, 2879–2894; DOI **10.1175/2008JAMC1795.1**. §§2–3 describe filling/QA; §5 evaluates interpolation. Its reported 3–8% interpolation errors are not equivalent to independent broadband tower validation. [Paper](https://journals.ametsoc.org/view/journals/apme/47/11/2008jamc1795.1.xml).

   **Fang et al. (2007)** is particularly relevant geographically: *Developing a spatially continuous 1 km surface albedo data set over North America from Terra MODIS products*, **JGR 112**, D20206; DOI **10.1029/2006JD008377**. It processes 2000–2004 MOD43B3, separates snow conditions during temporal/spatial filtering, restricts processing to terrestrial pixels and generates monthly composites. Field comparisons report RMSE 0.066 for snow-free and 0.089 for all pixels. These are older-product results, not expected errors for v061. Current dataset access, licence and size were **not verified**. [Paper, §§2–5 and ¶¶33, 53–54](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2006JD008377).

4. **Jia et al. (2022): the closest ready-made paired blue-sky climatology**

   Jia, Wang, Liang, Peng and Yu (2022), *Global Daily Actual and Snow-Free Blue-Sky Land Surface Albedo Climatology From 20-Year MODIS Products*, **JGR Atmospheres 127**, e2021JD035987; DOI **10.1029/2021JD035987**.

   It uses **MCD43A3 C06, 2001–2020**, A2 snow status and MODIS aerosol information. Blue-sky mixing uses a solar-angle/aerosol lookup table. For each calendar day, it removes the maximum and minimum across years and averages the remainder; snow-free estimates exclude snow observations. Values above 75° solar zenith angle are excluded. Winter snow-free gaps can remain.

   This is a **clear-sky atmospheric treatment**, unlike an ERA5-based all-weather diffuse fraction. Validation against 38 stations gives overall RMSE **0.031**, with poorer snowy-season performance. Exact native water exclusion and a full-inversion-only QA rule were **not verified**. [Paper, §§2.1–2.2, 3.1 and 5](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021JD035987).

   Published resolutions are **500 m, 0.05° and 0.5°**. The coarse dataset is DOI **10.5281/zenodo.6350821**; the referenced native dataset is **10.5281/zenodo.6359686**. The climatology cannot be cropped temporally to recover a 2000–2009 climatology. [Dataset description and files](https://zenodo.org/records/6350821).

5. **GLASS: useful, but the version and albedo quantity must be specified**

   The current catalogue advertises broadband albedo over **1981–2024**, at **4-/8-day** intervals and resolutions from **250 m to 0.05°**, depending on product generation. These are not interchangeable versions. [Official overview, product 2](https://www.glass.hku.hk/overview.html).

   Current algorithm documentation distinguishes black-/white-sky visible, NIR and shortwave products at 500 m/0.05° from a **250 m clear-sky shortwave blue-sky** product. Retrievals are combined and gap-filled using statistical temporal filtering. The family includes explicit water and sea-ice algorithms: “land surface” does not establish water exclusion. A separately catalogued snow-free climatology points back to Jia’s work; the catalogue’s longer date label should not override the paper’s verified 2001–2020 inputs. [Official algorithms, items 2 and 20](https://www.glass.hku.hk/algorithms.html); [official brochure, GLASS Albedo page](https://www.glass.hku.hk/pdf/overview/GLASS%E4%BA%A7%E5%93%81%E5%AE%A3%E4%BC%A0%E5%86%8C-%E8%8B%B1%E6%96%87-%E6%B1%87%E6%80%BB-final.pdf).

   Key references are:

   - Liang et al. (2021), *The Global Land Surface Satellite (GLASS) Product Suite*, **BAMS 102**; DOI **10.1175/BAMS-D-18-0341.1**, “Broadband albedo” subsection. [Paper](https://journals.ametsoc.org/view/journals/bams/102/2/BAMS-D-18-0341.1.xml).
   - Qu et al. (2014), *Direct-Estimation Algorithm for Mapping Daily Land-Surface Broadband Albedo From MODIS Data*, **IEEE TGRS 52**, 907–919; DOI **10.1109/TGRS.2013.2245670**.
   - Liu et al. (2013), *A statistics-based temporal filter algorithm to map spatiotemporally continuous shortwave albedo from MODIS data*, **HESS 17**, 2121–2129; DOI **10.5194/hess-17-2121-2013**, §§2–4. [Paper](https://hess.copernicus.org/articles/17/2121/2013/).

   A verified, matched actual/snow-free pair with documented water screening for 2000–2009 was **not established**.

6. **Copernicus Global Land / Climate Change Service albedo**

   The current CDS catalogue combines successive AVHRR, SPOT-VGT, PROBA-V and Sentinel-3 records. It supplies **black-/white-sky spectral and broadband albedo**, generally every **10 days**, with uncertainty, quality, observation-count and observation-age fields.

   Relevant coverage includes AVHRR at approximately 4 km, **1981–2005**, and SPOT-VGT at approximately 1 km, **1998–2014**. Newer Sentinel-3 products are approximately 300 m from **2018**. The current algorithm uses a 20-day moving window, giving greater weight to recent observations. The family is not a ready-made paired snow-free/actual blue-sky climatology. Exact inland-water screening and gap treatment must be checked for the selected version. [CDS catalogue, “Data description” and “Related variables”](https://cds.climate.copernicus.eu/datasets/satellite-albedo?tab=overview).

   Dataset DOI: **10.24381/cds.ea87ed30**. Algorithm reference: Carrer et al. (2021), *Surface Albedo Retrieval from 40-Years of Earth Observations through EUMETSAT/LSA SAF and EU/C3S Programmes: The Versatile Algorithm of PYALUS*, **Remote Sensing 13**, 372; DOI **10.3390/rs13030372**. [Paper, algorithm and product-specific sections](https://confluence.ecmwf.int/download/attachments/37264204/remotesensing-13-00372-v2.pdf?api=v2&modificationDate=1613584125288&version=1).

   For validation, Sánchez-Zapero et al. (2023) compared C3S, MCD43A3 C6.1 and GLASS V4: best-quality ground-match RMSDs were approximately **0.03**, with regional differences. This does not establish one universal winner. DOI **10.3390/rs15041081**, *Surface ALbedo VALidation (SALVAL) Platform…*, abstract and validation results. [Author-agency record](https://www.ars.usda.gov/research/publications/publication/?seqNo115=401293).

7. **CLARA-A3, CM SAF**

   This AVHRR climate record supplies broadband **black-, white- and blue-sky** albedo at **0.25°**, with **pentad and monthly** products; polar grids are also available. Its climate data record covers **1979–2020**, followed by an interim continuation. It therefore spans the requested decade.

   The important detail is that the files contain both `blue_sky_albedo_all_mean`, combining snow/ice and non-snow observations, and `blue_sky_albedo_mean`, using non-snow/ice observations. Counts and other statistics accompany them. The retrieval uses cloud screening and atmospheric correction; blue-sky processing estimates prevailing direct/diffuse illumination. Albedo is **not normalised to local noon**. The product also contains water and sea-ice estimates. I did **not** verify a separate terrestrial albedo for the land portion of each mixed 0.25° cell. [Product manual, §§3.3–3.6 and Table 4-1, pp. 24–26](https://www.cen.uni-hamburg.de/en/icdc/data/land/docs-land/saf-cm-fmi-pum-gac-sal-3-1.pdf).

   References:

   - Riihelä, Jääskeläinen and Kallio-Myers (2024), *Four decades of global surface albedo estimates in the third edition of the CM SAF … CLARA climate data record*, **ESSD 16**, 1007–1028; DOI **10.5194/essd-16-1007-2024**. §4 validates against representative ground observations, including snow/ice sites; relative biases are generally within 10% at representative sites. [Paper](https://essd.copernicus.org/articles/16/1007/2024/).
   - Karlsson et al. (2023), *CLARA-A3: The third edition of the AVHRR-based CM SAF climate data record on clouds, radiation and surface albedo covering the period 1979 to 2023*, **ESSD 15**, 4901–4926; DOI **10.5194/essd-15-4901-2023**. [Paper](https://essd.copernicus.org/articles/15/4901/2023/).

   Dataset DOI: **10.5676/EUM_SAF_CM/CLARA_AVHRR/V003**.

8. **GlobAlbedo and QA4ECV**

   **GlobAlbedo** combines multiple sensors using BRDF retrieval and prior information. Products cover **1998–2011**, with 1 km and coarser grids, eight-day/monthly outputs, and snow, snow-free and merged processing. They provide black-/white-sky broadband quantities, requiring further work for blue-sky use. The algorithm warns that unobserved snow-free winter estimates can revert to the prior. [CEDA record](https://catalogue.ceda.ac.uk/uuid/6a81b77693c847929e0d31a6ce84a821/); [ATBD v3.1, §8.2, p. 165](https://www.globalbedo.org/docs/previos/GlobAlbedo_Albedo_ATBD_V3.1.pdf).

   Reference: Lewis et al. (2012), *The ESA GlobAlbedo project: Algorithm*, IGARSS, 5745–5748; DOI **10.1109/IGARSS.2012.6352306**. Exact independent validation statistics and the precise inland-water treatment were **not verified**.

   **QA4ECV albedo** extends a multisensor record over **1982–2016**, with daily 0.05° processing and 0.5° daily/monthly products. It uses a MODIS-derived prior, supplies uncertainty and measures of prior influence, and explicitly targets land with snow/sea-ice/water flags. Its coarse aggregation is designed to conserve energy. Broadband ranges differ from MODIS—for example, total shortwave is 0.4–3 μm. It is not a supplied paired blue-sky climatology. [Nightingale et al. (2018), §4.1, pp. 8–9](https://discovery.ucl.ac.uk/10055429/1/remotesensing-10-01254.pdf).

   Reference: Nightingale et al., *Quality Assurance Framework Development Based on Six New ECV Data Products to Enhance User Confidence for Climate Applications*, **Remote Sensing 10**, 1254; DOI **10.3390/rs10081254**. Independent validation metrics for the particular albedo release were **not verified**.

9. **Gao et al. (2014) albedo look-up maps**

   Gao et al., *Multiscale climatological albedo look-up maps derived from moderate resolution imaging spectroradiometer BRDF/albedo products*, **Journal of Applied Remote Sensing 8**, 083532; DOI **10.1117/1.JRS.8.083532**.

   These are monthly **land-cover-class** black-/white-sky visible, NIR and shortwave albedos under snow-covered and snow-free conditions, derived from **2001–2011** MODIS information. They use monthly medians and a hierarchy of spatial neighbourhoods to obtain class estimates where local samples are insufficient. Water is an explicit land-cover class, not evidence that every delivered coarse quantity is a measured land-only cell mean.

   Their reconstruction check reports annual snow-free mean absolute difference around **0.009** against MODIS. This tests reconstruction consistency with the input product; it is not independent ground validation. [Paper, §§2–3 and §4.3/Table 3](https://www.researchgate.net/publication/267810717_Multiscale_climatological_albedo_look-up_maps_derived_from_moderate_resolution_imaging_spectroradiometer_BRDFalbedo_products).

   Dataset DOI **10.15482/USDA.ADC/1523392**. [USDA data record](https://catalog.data.gov/dataset/data-from-multiscale-climatological-albedo-look-up-maps-derived-from-moderate-resolution-i).

10. **Hou et al. (2025), ALLUMs**

    Hou et al., *Radiative forcing reduced by early twenty-first century increase in land albedo*, **Nature 641**, 1162–1171; DOI **10.1038/s41586-025-08987-z**.

    This is a particularly relevant recent precedent. It masks water using native-resolution land-cover maps, separates snowy/snow-free MCD43A3 C6.1 observations with A2, and calculates monthly class means at 1°. Missing class albedos are reconstructed using a four-dimensional inverse-distance method. Blue-sky calculations use monthly NCEP direct/diffuse radiation. Snow is pooled as a separate class rather than retaining a separate snowy albedo for every vegetation class. [Methods, “Surface albedo”, “Area, white- and black-sky albedo…” and “Blue-sky albedo”](https://www.researchgate.net/publication/392163402_Radiative_forcing_reduced_by_early_twenty-first_century_increase_in_land_albedo).

    The dataset supplies **2001–2020 monthly** class-specific BSA/WSA, class areas, land/grid areas and QA. Those fields make a published reconstruction of whole-cell terrestrial albedo a real candidate, although they do not directly supply the project’s independent observed response. [Dataset description](https://zenodo.org/records/13981586).

    Concept DOI **10.5281/zenodo.13981585**; inspected version DOI **10.5281/zenodo.13981586**. That record advertises a newer version, whose files I could not verify. Validation includes reconstruction tests and comparisons with observations; detailed independent validation figures were **not verified**.

11. **MuSyQ and VIIRS**

    | Product | Verified provision and limits |
    |---|---|
    | **MuSyQ** | The version assessed by Wu et al. supplies 1 km BSA/WSA every five days, using a ten-day multisensor BRDF window and observation-count QA. Its Heihe Basin validation gave snow-free RMSE about 0.033, close to MODIS in that particular comparison. Full 2000–2009 coverage, exact water treatment, access, licence and archive size were **not verified**. Wu et al. (2018), *Accuracy Assessment on MODIS (V006), GLASS and MuSyQ Land-Surface Albedo Products…*, **Remote Sensing 10**, 2045; DOI **10.3390/rs10122045**, Table 2 and results. [Author paper](https://www.researchgate.net/publication/329742158_Accuracy_Assessment_on_MODIS_V006_GLASS_and_MuSyQ_Land-Surface_Albedo_Products_A_Case_Study_in_the_Heihe_River_Basin_China). |
    | **NASA VNP43** | VNP43MA3 supplies daily **1 km** spectral and three-band broadband BSA/WSA using a 16-day retrieval window. The 500 m IA3 product has three imaging-band quantities and should not be mistaken for the complete broadband MA3 product. Snow/quality information accompanies the family; blue-sky and snow-free climatologies require processing. The record starts in **2012**, excluding it as the primary response here. [Team guide](https://www.umb.edu/spectralmass/viirs-user-guides-c1-and-c2/vnp43ia3-and-vnp43ma3-albedo-products/). Validation/continuity reference: Román et al. (2024), *Continuity between NASA MODIS Collection 6.1 and VIIRS Collection 2 land products*, **RSE 302**, 113963; DOI **10.1016/j.rse.2023.113963**. |
    | **NOAA gap-filled VIIRS** | A distinct product family supplies daily gap-filled surface albedo, including actual-illumination treatment. It is also post-MODIS-decade. The evaluation paper is *An evaluation of the NOAA global daily gap-filled VIIRS surface albedo* (2023), DOI **10.1016/j.rse.2023.113822**. Detailed release-specific water, licence and size information was **not verified**. [Paper](https://doi.org/10.1016/j.rse.2023.113822). |

12. **Landsat, Sentinel-2 and Hi-GLASS**

    These improve spatial detail, but do not automatically improve suitability for a ten-year continental climatology.

    - **Historical Landsat:** Shuai et al. (2014), *An approach for the long-term 30-m land surface snow-free albedo retrieval from historic Landsat surface reflectance and MODIS-based a priori anisotropy knowledge*, **RSE 152**, 467–479; DOI **10.1016/j.rse.2014.07.009**. The method retrieves 30 m snow-free albedo using Landsat plus MODIS anisotropy information, with ground/product comparisons. A ready, continent-wide paired climatology for this task was **not verified**. [Paper, abstract and methods](https://www.sciencedirect.com/science/article/abs/pii/S0034425714002545).
    - **Sentinel-2:** Lin et al. (2022), *Estimating 10-m land surface albedo from Sentinel-2 satellite observations using a direct estimation approach with Google Earth Engine*, **ISPRS Journal 194**, 1–20; DOI **10.1016/j.isprsjprs.2022.09.016**, reports ground RMSE about 0.030. Its record cannot cover 2000–2009. [Paper](https://www.sciencedirect.com/science/article/pii/S092427162200260X).
    - **Operational Copernicus S2GM HR albedo:** supplies 10 m visible and 20 m NIR/shortwave BSA/WSA using atmospheric correction and VIIRS prior information, with QA/gap flags. The ATBD estimates roughly **450 MB output per 110 × 110 km tile**, excluding inputs. Exact water handling and product licence were **not verified** here. [ATBD v3.1, §§2.1–2.2](https://land.copernicus.eu/en/technical-library/algorithm-theoretical-basis-document-sentinel-2-global-mosaic-hr-albedo/@@download/file).
    - **Hi-GLASS, 2025:** the overview describes 30 m instantaneous products and 16-day gap-filled products including albedo. Published availability was principally US/China **2013–2022**, plus global **2015 and 2020**, despite the ambition of extending to the 1980s. This does not establish a downloadable 2000–2009 record. *An overview of the high-resolution global LAnd surface satellite (Hi-GLASS) products suite*, **Science of Remote Sensing 12**, 100263; DOI **10.1016/j.srs.2025.100263**, abstract/data availability. Albedo-specific snow treatment, licence, volume and validation figures were **not verified**. [Paper](https://doi.org/10.1016%2Fj.srs.2025.100263).

**Access and storage favour several published alternatives.** “Unverified” below means that I did not establish the requested detail; public availability alone does not establish a particular licence.

| Product | Access and licence evidence | Size relevant to this assessment |
|---|---|---|
| MCD43A3/A2 | NASA Earthdata/LAADS/LP DAAC; open NASA data, with attribution expected. | Whole-job volume **unverified**; native tile processing requires substantially more transfer than the final climatology. |
| MCD43C3 | Same NASA access. | An inspected current global daily file was about 190 MB: roughly **0.7 TB per decade by extrapolation**, not a measured historical inventory. [Directory](https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/61/MCD43C3/2026/140/). |
| MCD43GF | NASA Earthdata; v006 needed for the target years. | **Calculated:** two global 16-bit albedo layers plus one byte QA layer at 30″ are about 4.7 GB/day, approximately 17 TB/decade uncompressed. Subsetting/streaming is necessary. |
| Moody | Legacy NASA download/FTP documentation; current transfer route and exact licence **unverified**. | NASA advertises up to **445 MB per map layer**, 40 MB for a 0.5° statistical file; the latter is a different statistical product. [Product page](https://modis-images.gsfc.nasa.gov/ALBEDO/index.html). |
| Fang | Historical University of Maryland distribution; current access/licence **unverified**. | **Unverified.** |
| Jia | Public Zenodo downloads; licence text **not verified** from the retrieved record. | **6.0 GB** for 0.05°, **79.3 MB** for 0.5°, plus 11.2 MB land-cover files. The displayed 1.3 TB is cumulative download traffic, not dataset size. Native 500 m size **unverified**. [Files](https://zenodo.org/records/6350821). |
| GLASS | Public HKU/BNU/UMD distribution; exact selected-release licence **unverified**. | **Unverified**; strongly resolution/version dependent. [Archive](https://glass.hku.hk/archive/Albedo/). |
| Copernicus | CDS account/API; catalogue lists CC-BY and Copernicus Global Land terms, depending on component. | **Unverified**; request a regional/version-specific subset. [Catalogue](https://cds.climate.copernicus.eu/datasets/satellite-albedo?tab=overview). |
| CLARA-A3 | CM SAF ordering interface; CM SAF distribution uses **CC BY 4.0**. [Access](https://wui.cmsaf.eu/safira/action/viewDoiDetails?acronym=CLARA_AVHRR_V003). | Archive size **unverified**. **Calculated:** one global float32 monthly field for ten years is about **0.50 GB**; two blue-sky means about 1 GB, before metadata/compression. |
| GlobAlbedo | CEDA reports restored public access in February 2026. Custom GlobAlbedo licence linked; its full terms **unverified**. | **Unverified.** [Record](https://catalogue.ceda.ac.uk/uuid/6a81b77693c847929e0d31a6ce84a821/). |
| QA4ECV | CEDA distribution identified; current transfer and exact licence **unverified**. | **Unverified.** |
| Gao LUMs | USDA/Globus; record explicitly identifies **public-domain** data. | Four archives identified; total size **unverified**. [Record](https://catalog.data.gov/dataset/data-from-multiscale-climatological-albedo-look-up-maps-derived-from-moderate-resolution-i). |
| Hou ALLUMs | Zenodo; selected-release licence **unverified**. | Inspected v1: **3.4 GB albedo**, 2.9 GB transition areas. Latest-version size **unverified**. [Files](https://zenodo.org/records/13981586). |
| MuSyQ / NOAA VIIRS | Release-specific access/licence **unverified**. | **Unverified.** |
| NASA VNP43 | NASA Earthdata; open NASA data. | **Unverified**; unsuitable period regardless. |
| Landsat/Sentinel/Hi-GLASS derivatives | Access and licence depend on the derived product, not merely the underlying satellite imagery. | Continental archive totals **unverified**. |

The **1.3 TB free space is therefore sufficient for evaluating the small climatology products**. It need not prevent native MODIS processing if inputs are processed and released in batches. Transfer volume and peak disk use should be estimated separately.

**The closest studies use several different, defensible constructions.** None supplies a universal prescription for every choice in paleo-RF.

| Study and supporting location | Albedo, water, snow, QA and temporal treatment | Spatial support and relevance |
|---|---|---|
| **Duveiller, Hooker & Cescatti (2018)**, *The mark of vegetation change on Earth’s surface energy balance*, **Nature Communications 9**, 679; DOI **10.1038/s41467-017-02810-8**. Companion data paper, *A dataset mapping the potential biophysical effects of vegetation cover change*, DOI **10.1038/sdata.2018.14**, “Data preparation—Albedo”. [Data paper](https://www.nature.com/articles/sdata201814). | MCD43C3 **C005**, 0.05°; selects the composite nearest mid-month and takes monthly medians over **2008–2012**. Uses **½(BSA + WSA)**, a fixed diffuse fraction rather than observed atmospheric mixing. Snow is retained. Water, urban, ice and bare surfaces enter as nuisance land-cover fractions. Exact QA-bit selection was **not verified**. | Local regressions in **5 × 5** coarse-pixel windows estimate vegetation contrasts, subsequently aggregated to **1°** using uncertainty information. The released maps describe **changes between cover types**, not absolute calibration albedo. Dataset DOI **10.6084/m9.figshare.c.3829333**. |
| **Zhao & Jackson (2014)**, *Biophysical forcings of land-use changes from potential forestry activities in North America*, **Ecological Monographs 84**, 329–353; DOI **10.1890/12-1705.1**. [Paper, pp. 331, 333–335](https://jacksonlab.stanford.edu/sites/g/files/sbiybj20871/files/media/file/em2014.pdf). | MCD43A1/A2 **C5**, 500 m, **2001–2011**. Climatological means for eight-day periods; best-quality retrievals are used first, with lower-quality retrievals given half weight where needed. WSA is used for class comparisons; direct/diffuse weighting from WRF-CLM is used for forcing. Seasonal snow remains. Vegetation-class sampling excludes water samples, without constructing every cell’s whole-land mean. | Nearby vegetation pairs within **15 km**, constrained by slope/elevation; forcing subsequently aggregated to **1°**. Strong precedent for North American class contrasts and transparent quality–coverage trade-offs. |
| **Bright et al. (2015)**, *Radiative forcing bias of simulated surface albedo modifications linked to forest cover changes at northern latitudes*, **Biogeosciences 12**, 2195–2205; DOI **10.5194/bg-12-2195-2015**. [§2.1](https://bg.copernicus.org/articles/12/2195/2015/bg-12-2195-2015.pdf). | Daily 500 m MCD43A V006 **local-noon BSA**, visible/NIR, for selected winter/spring periods in 2007–2009. Forest and open pixels are related to local meteorological observations. This is not a blue-sky monthly raster recipe. Exact water-mask and QA-bit rules were **not established** in the inspected methods. | **135 forest and 12 open pixels**, three Norwegian regions, near weather stations. Relevant to snow masking and model bias, with limited geographical transferability. |
| **Bright et al. (2017)**, *Local temperature response to land cover and management change driven by non-radiative processes*, **Nature Climate Change 7**, 296–302; DOI **10.1038/nclimate3250**. [Methods, PDF p. 8](https://bpb-us-w2.wpmucdn.com/u.osu.edu/dist/7/24705/files/2024/09/nclimate3250.pdf). | Uses Gao’s monthly **shortwave WSA** for snow-covered/free states, mixed with MODIS snow fractions for **2001–2011**. Source QA/water assumptions are inherited; no separately verified universal water-screening rule. | Common **1°** support with radiation data. Shows that an influential study deliberately used WSA rather than constructing an ERA5 blue-sky response. |
| **Lejeune et al. (2020)**, *Biases in albedo sensitivity to deforestation in CMIP5 models and their impacts on associated historical radiative forcing*, **Earth System Dynamics 11**, 1209–1232; DOI **10.5194/esd-11-1209-2020**. [§§2.1.1–2.1.2](https://esd.copernicus.org/articles/11/1209/2020/esd-11-1209-2020.html). | Uses monthly **GlobAlbedo 1998–2011**, with fixed ½(BSA + WSA), and an alternative Duveiller estimate. Selects cells with **≥95%** specified vegetation using GlobCover; actual snow remains. This purity rule does not prove complete exclusion of water within retained pixels. | Area-weighted aggregation to **2°** for the direct class-comparison method. The alternative includes climate-based extrapolation. Neither is a ready independent observed 1° response. |
| **Hasler et al. (2024)**, *Accounting for albedo change to identify climate-positive tree cover restoration*, **Nature Communications 15**, 2275; DOI **10.1038/s41467-024-46577-1**. [Methods, “Mapping albedo change”, Eq. 1](https://www.nature.com/articles/s41467-024-46577-1). | Gao snow-covered/free BSA/WSA; MODIS snow climatology approximately **2000–2021**; NCEP direct/diffuse radiation climatology **1981–2010**. Domain includes cells with **≥1% non-water area**—this is a domain rule, not proof of land-only measured albedo. QA is inherited from input products. | Transition estimates originate at **0.05°**; resampling to 500 m does not create 500 m albedo information. Six kernels are compared. Released forcing/carbon-equivalent maps, DOI **10.7910/DVN/G17RXL**, are not absolute albedo responses. |
| **Gao et al. (2014)**, cited above, §§2–3. | Class-specific snowy/free BSA/WSA, monthly medians, hierarchical spatial borrowing and land-cover purity selection. No supplied weather-specific blue-sky climatology. | Supports a **class lookup/reconstruction method**. Adopting it would change the proposed regression’s response and possibly its purpose. |
| **Ghimire et al. (2014)**, *Global albedo change and radiative cooling from anthropogenic land cover change, 1700 to 2005 based on MODIS, land use harmonization, radiative kernels, and reanalysis*, **GRL**; DOI **10.1002/2014GL061671**. [§§2.2–2.3, Eqs. 1–2](https://wordpress.clarku.edu/wp-content/uploads/sites/22/2015/01/Ghimire_GRL_2014_GlobalAlbedoChange_RadiativeForcing_LandCoverChange.pdf). | Uses MODIS-derived class lookup albedos, monthly modern snow and NCEP direct/diffuse information. Historical land-cover fractions reconstruct albedo. Water handling is inherited from class products; a further strict mask was **not verified**. Crucially, its forcing equation uses **separate BSA/WSA changes with corresponding kernels**, rather than simply feeding blue-sky albedo to any kernel. | **1°**, historical reconstruction. One of the closest precedents for the land-cover-fractions → albedo → forcing chain. |
| **Boisier, de Noblet-Ducoudré & Ciais (2013)**, *Inferring past land use-induced changes in surface albedo from satellite observations: a useful tool to evaluate model simulations*, **Biogeosciences 10**, 1501–1516; DOI **10.5194/bg-10-1501-2013**. [§2.1, pp. 1502–1504](https://www.researchgate.net/publication/265694801_Inferring_past_land_use-induced_changes_in_surface_albedo_from_satellite_observations_A_useful_tool_to_evaluate_model_simulations). | MCD43C3 **WSA**, **2000–2011**, separate snowy/free monthly climatologies; five cover groups including evergreen/deciduous trees. Selects >95% dominant-class cells; bilinearly interpolates missing class estimates. Exact QA filters and independent strict water screening are **not reported in the inspected procedure**. | Native **0.05° → 0.5° → 2°**. Combines class albedos with historical fractions and snow cover. Especially relevant to the proposed functional groups, but the response is reconstructed. |
| **Otto, Raddatz & Claussen (2011)**, *Strength of forest-albedo feedback in mid-Holocene climate simulations*, **Climate of the Past 7**, 1027–1039; DOI **10.5194/cp-7-1027-2011**. [Appendix A, p. 1036; Appendix B](https://cp.copernicus.org/articles/7/1027/2011/cp-7-1027-2011.pdf). | Uses MODIS **2001–2004 WSA**, visible/NIR, identified in the paper as MOD43C1, with MOD15A2 fAPAR. Snow-covered observations are excluded; albedo and fAPAR are matched to 16-day periods and regressed to derive vegetation/soil parameters. Exact QA and water rules are **not specified there**. | **0.05° inputs remapped to 0.25°**, then parameterisation in approximately 3.75° ECHAM5/JSBACH. A genuine Holocene MODIS-calibration precedent, but not the proposed pollen-fraction blue-sky regression. |
| **Hopcroft et al. (2026)**, *Biogeophysical Impact of Land-Use Scenarios on Holocene Surface Temperatures*, **GRL 53**, e2025GL118518; DOI **10.1029/2025GL118518**. [§2.2](https://research-portal.uu.nl/ws/files/283235263/Geophysical_Research_Letters_-_2026_-_Hopcroft_-_Biogeophysical_Impact_of_Land_Use_Scenarios_on_Holocene_Surface.pdf). | The inspected main paper uses **HadCM3** simulations with alternative Holocene land-use/vegetation histories. It does not document a MODIS calibration-response raster. | Relevant to the paleo question and land-use scenarios. It should not be cited as precedent for a MODIS compositing, QA or water-screening recipe. Supplementary implementation details were not verified. |
| **Hou et al. (2025)**, cited above. | Provides the clearest recent precedent for **native water exclusion, monthly snow separation and 1° class processing**. Its imputation and snow reconstruction are additional scientific components. | Potential adoptable dataset and processing precedent; class conditioning needs consideration before using it as a regression response. |

Bright, Zhao, Jackson and Cherubini’s other **2015** paper, *Quantifying surface albedo and other direct biogeophysical climate forcings of forestry activities*, **Global Change Biology 21**, 3246–3266, DOI **10.1111/gcb.12951**, is a **review**, not a separate operational raster recipe. [Author-hosted paper](https://jacksonlab2.sites.stanford.edu/sites/g/files/sbiybj20871/files/media/file/bright_et_al._2015_gcb.pdf).

The Duveiller dataset also has a **2021 addendum**, DOI **10.1038/s41597-020-00787-6**, adding climate-based spatial extrapolations. Its extended fields should not be mistaken for additional direct observations; original fields remain available. [Addendum, dataset-extension description](https://www.nature.com/articles/s41597-020-00787-6).

**The state of the art is a set of established components, with application-specific choices between them.**

There is broad agreement on using documented retrievals, keeping quality information, distinguishing illumination quantities, accounting for snow and respecting spatial support. There is also published precedent for masking water before coarse aggregation, and for retaining uncertainty or measures of reconstruction influence.

There is **no single agreed monthly reducer or QA policy**:

| Choice | Published alternatives | Consequence for paleo-RF |
|---|---|---|
| Temporal statistic | Means; medians; Jia’s trimmed calendar-day means. | A median is a published choice, but need not preserve the influence of snow occurrence in a monthly mean. |
| QA | Team-recommended full inversions; quality-weighted or broader valid-retrieval inclusion. | Strict QA improves retrieval confidence but can change which seasons and surfaces remain represented. |
| Illumination | WSA alone; fixed BSA/WSA mixtures; aerosol-based mixing; reanalysis-based mixing. | These represent different quantities and assumptions. Calling each “blue-sky” does not make them equivalent. |
| Snow gaps | Leave absent; fill phenological trajectories; borrow class values spatially; reconstruct using snow fractions. | Complete snow-free winter fields are partly modelled wherever snow-free observations are unavailable. |
| Spatial aggregation | Area-weighted means; class-specific aggregation; local regression and uncertainty weighting. | These answer different questions. A transition estimate cannot be substituted for an absolute observed response. |

Oleson et al. (2003), *Assessment of global climate model land surface albedo using MODIS data*, **GRL 30**, 1443, DOI **10.1029/2002GL016749**, is an explicit example of accepting broader QA for coverage and area-weighting to a model grid. Its older water correction and exclusion of mixed land/ocean cells did not eliminate all lake-related bias. It therefore supports transparent trade-offs, not an assertion that coarse water contamination is easily removed. [§§2.1–2.2](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2002gl016749).

Two mathematical implications are important:

- **A land-fraction mask cannot unmix an already mixed albedo.** If a coarse value contains land and water, multiplying it by land fraction does not recover terrestrial albedo. Removing the whole coarse pixel also removes its land contribution.
- **Mean albedo and flux-weighted albedo are different targets.** An arithmetic monthly mean and \(\sum_t F_t\alpha_t/\sum_t F_t\) need not agree. A monthly radiative kernel does not, by itself, prove that the calibration response must use daily incoming-radiation weights.

These are implications of the quantities being averaged, not additional retrieval methods. The kernel’s definition should determine the required consistency between albedo change and forcing calculation.

**Published class products can be adopted, but their role needs care.** Gao, Boisier and ALLUMs deliberately use land cover to reconstruct albedo. If their reconstructed field becomes the response in another land-cover regression, some apparent association may reflect that earlier reconstruction. This is a methodological concern, not proof of circularity: an area-weighted combination of fully observed class means can reproduce an observed land mean. The concern becomes stronger where missing classes are supplied using class-conditioned interpolation.

**The comparison with the two rebuild designs leaves several decisions open.**

The relevant local sources are the [Astra design](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/2026-10-15_albedo_rebuild_design_codex_gpt6astra.md), [Fable design](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/2026-10-15_albedo_rebuild_design_fable.md), and their [Astra reconciliation](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/2026-10-15_albedo_rebuild_reconciliation_codex_gpt6astra.md:55) and [Fable reconciliation](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/2026-10-15_albedo_rebuild_reconciliation_fable.md:72).

| Design judgement | What the literature settles | What remains a project judgement |
|---|---|---|
| **500 m A3/A2 rather than C3 for water control** | C3 explicitly includes coastal shallow-water inputs; Hou demonstrates water exclusion before monthly 1° processing. | Whether to adopt Hou’s land-cover mask, MOD44W, another documented mask, or a specified combination. |
| **Require all four MOD44W subpixels to be land** | No inspected precedent establishes this as the standard rule. | It conservatively rejects mixed pixels, including their real land area. It cannot guarantee an exact whole-land response or perfect agreement with the sensor footprint. |
| **Static 2015 mask, annual intersection/union, water QA** | These alternatives are not settled by the cited albedo papers. | Mask epoch, uncertain-water treatment and acceptable loss of terrestrial area. Their effects should be reported as coverage, not hidden as cleaning. |
| **Full-inversion-only versus all processed retrievals** | Both stricter selection and broader/weighted inclusion have published precedents. QA decoding itself follows the product specification. | Primary policy remains unresolved. The reconciliations move in opposite directions: Astra towards all processed retrievals, Fable towards full inversion. |
| **ERA5 hour selection and interpolation** | BSA/WSA mixing is established. Published examples also use monthly radiation or aerosol lookup tables. | Centred apparent-noon interval versus containing hour, equation-of-time treatment, nearest versus bilinear interpolation, and any low-flux floor. None was established as a universal published prescription. |
| **70° versus 75° solar zenith cutoff** | NASA identifies concerns above 70°; Jia uses 75°. | The acceptable accuracy–coverage trade-off. A claim that the cutoff loses nothing in April–September across this domain is not defensible. |
| **Equal-day/equal-year means, pooled means, medians, radiation weights** | Each has a distinct meaning; means and medians both occur in close studies. | The desired climatological quantity and exact hierarchy. The claim that monthly kernels require the proposed daily radiation weighting is unsupported. |
| **Minimum days, years and retained area** | No inspected source establishes the proposed 10/15/20 days, five/eight years, or 50/90/95% coverage as standards for this task. | These remain thresholds requiring a reason tied to coverage and calibration stability. Overlapping daily MODIS windows do not make each accepted day independent. |
| **Missing snow-free winter values** | Published completed products exist, with documented modelling assumptions. | Leaving gaps versus adopting a published reconstruction. Substituting nearby snow-free months would require its own justification. |
| **Exact overlap versus centre assignment** | Area weighting and aggregation to common support are established. | Numerical implementation and acceptable approximation error. This is not settled merely by choosing a particular cited paper. |
| **Validation against the legacy raster** | Independent observations and products with matched quantities/support are the appropriate evidence. | Agreement with an undocumented, water-contaminated raster cannot be the principal acceptance criterion. The expected sign of water removal is not universal because open water and ice differ. |
| **Published-product alternative** | Jia and CLARA provide actual alternatives; ALLUMs supplies a new relevant precedent. | Acceptance of changed period, illumination, spatial sampling or class-conditioned reconstruction. Jia’s licence should not be treated as verified solely because a local design says CC BY. |

The two reconciliations therefore contain **competing proposed resolutions**, rather than a single scientifically agreed specification. Literature research removes the need to invent the underlying retrieval and several processing components; it does not remove the need to state the remaining choices.

**My ranked recommendation, with the current requirements kept fixed, is:**

| Rank | Option | Fitness and defensibility | Effort, volume and remaining judgements |
|---|---|---|---|
| **1** | **MCD43A3/A2 v061, using a small set of explicitly cited processing steps** | Best control over native water screening, the requested dates and snow-state selection. Strong product validation. Hou supplies a close water/month/1° precedent; other studies supply atmospheric mixing precedents. | Highest processing effort. Stream native tiles; independently inventory transfer volume. Still decide mask, QA, illumination target, averaging and coverage. Explicitly acknowledge incomplete early 2000. |
| **2** | **CLARA-A3 monthly blue-sky fields** | Best verified alternative retaining the requested decade and supplying both snow treatments with independent sensor heritage. | Low-to-moderate effort; selected monthly fields are small. Primary adoption requires resolving mixed land/water support or accepting a restricted terrestrial sample. Otherwise use as a comparison product. |
| **3** | **Jia et al. climatology** | Closest ready-made paired product. **It moves to first place for ease of adoption if Chris accepts its period and illumination definition and the water audit is satisfactory.** | Approximately 6 GB at 0.05°. Very low compositing effort. Cannot recover 2000–2009 from the published climatology; native water/QA and licence remain checks. |
| **4** | **Hou ALLUMs and its published reconstruction** | Strong recent precedent for monthly 1° terrestrial albedo, with explicit water exclusion. More directly relevant than a dataset containing only transition effects. | Albedo archive inspected: 3.4 GB. Requires blue-sky processing, a 2001-onward period, and acceptance of class-conditioned filling and pooled snow treatment. |
| **5** | **A precisely identified GLASS or Copernicus product** | Established products with substantial validation; useful alternatives where native resolution permits water screening. | Moderate effort. Verify exact quantity/version, snow separation and mask before committing. No matched ready pair was established here. |
| **6** | **MCD43GF v006 as the snow-free companion** | Defensible published completion method, especially if complete snow-free fields are essential. | Does not supply the actual/snowy counterpart; introduces cross-product/version consistency issues and potentially large transfers. |
| **7** | **C3, GlobAlbedo or QA4ECV coarse products** | Useful benchmarks or responses for a deliberately restricted land sample. | Easier processing, but cannot automatically recover all terrestrial portions of mixed cells. |
| **8** | **Gao/Duveiller/Hasler derived datasets, or newer high-resolution products** | Valuable benchmarks for class contrasts and forcing. They generally change the response being estimated or miss the target period. | Do not substitute them directly for absolute observed calibration albedo without changing the study design. |

**Decisions for Chris:**

1. **Is 2000–2009 fixed?** In particular, is a nine-year complete MODIS period acceptable, and could Jia’s 2001–2020 climatology answer the scientific question?
2. **Which illumination quantity is required?** Local-noon blue-sky under prevailing weather, clear-sky blue-sky, or a quantity chosen to match the kernels?
3. **What does the snow sensitivity test compare?** Observed snow-free conditions versus actual conditions, or a reconstructed winter without snow?
4. **What terrestrial population must the response represent?** All non-water land, or a documented subset excluding mixed pixels—and how should permanent ice and other surfaces absent from the pollen fractions be treated?
5. **Can the response include land-cover-conditioned reconstruction?** If yes, ALLUMs becomes more attractive; if no, favour observed products and explicit missingness.
6. **Which single QA/coverage policy should be primary?** Resolve this before building either design, with any additional variant justified by its benefit to the paper.