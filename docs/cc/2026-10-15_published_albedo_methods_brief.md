# Published methods for gridded albedo rasters: research brief (2026-10-15)

Asked after Chris said, of the rebuild pilot: "I think more research is needed. There must be published research that generates albedo raster and therefore there must be standard/state of the art ways to do this." Given unchanged to Astra and Fable.

---

# Research brief: published, state-of-the-art methods for gridded albedo rasters (paleo-RF)

## Why this is asked
Chris (the contractor) rejected a pilot of our own rebuild recipe for the calibration albedo
with: "I think more research is needed. There must be published research that generates
albedo rasters and therefore there must be standard/state of the art ways to do this." His
standing rule: every method must be grounded in published practice; we do not invent methods.

## What the albedo is for
paleo-RF regresses monthly broadband blue-sky albedo (2000-2009 climatology) on
pollen-reconstructed land-cover fractions (evergreen trees, summergreen trees, open land) in
1-degree cells across North America (lon -172 to -52, lat 17 to 79), with a spatial smooth and
elevation, then predicts Holocene albedo and converts albedo change to radiative forcing with
radiative kernels. The response should describe the land part of each cell (water excluded),
and we need snow-free and snowy (or all-conditions) albedo for a snow sensitivity test. The
existing raster (0.25 degree, monthly, MCD43A3 v061 + ERA5 diffuse fraction, undocumented,
band names "median") contains lake and sea-ice values.

Two independent designs for our own rebuild exist (read them only after forming your own
view, and only to compare at the end): docs/cc/2026-10-15_albedo_rebuild_design_*.md and the
reconciliations docs/cc/2026-10-15_albedo_rebuild_reconciliation_*.md on this branch.

## Questions
1. **Published gridded albedo products and climatologies** (MODIS-based and others), as of
   October 2026: for each, what it provides (blue/black/white-sky, broadband, snow-free vs
   actual, spatial and temporal resolution, period, land/water handling, gap filling, quality
   handling, licence, access, size), its reference paper(s), and validation. At least: MCD43A3/
   C3 and MCD43GF (gap-filled snow-free), Moody et al. 2005/2008, Jia et al. 2022 (daily actual
   and snow-free blue-sky climatology, Zenodo), GLASS albedo (Liang, Qu, Liu et al.), Copernicus
   Global Land albedo, CLARA-A3 surface albedo (CM SAF), GlobAlbedo / QA4ECV, Gao et al. 2014
   look-up maps, and any newer products (e.g. MuSyQ, VIIRS-based VNP43, Landsat/Sentinel-2
   albedo).
2. **Methods used in studies closest to ours**: studies that relate MODIS albedo to land-cover
   class or fractions to estimate albedo change or forcing from land-cover change. At least:
   Duveiller, Hooker and Cescatti (2018, Nature Communications) and its dataset; Bright et al.
   (e.g. 2015, 2017); Zhao and Jackson (2014); Lejeune et al. (2020); Hasler et al. (2024);
   Gao et al. (2014); Hopcroft et al. (2026) and other Holocene or paleo studies that calibrated
   albedo from MODIS. For each: which albedo product and quantity they used, how they handled
   water, snow, blue-sky mixing, quality and temporal compositing, and the spatial support
   (pixel, window, grid cell).
3. **The state of the art**: is there a standard way to build a monthly land-only blue-sky
   albedo climatology at coarse resolution? Where do published methods agree, and where do they
   make different (each defensible) choices?
4. **Adopt or build?** Could an existing published product, or a published study's dataset,
   serve directly as our calibration response (possibly with documented, published
   post-processing such as water screening), so that our method is "we used product X
   (citation), aggregated as in study Y (citation)"? Rank the options for this project:
   fitness, defensibility to reviewers, effort, data volume (the workstation has 1.3 TB free;
   downloads are fine), and what each would leave as unpublished judgements.
5. Where our two rebuild designs made judgements, say which ones a published method settles
   and which remain judgements.

## Rules
Read-only. Do not open anything under writing/, nor any .docx/.pptx/.pdf in the repository.
You may read public papers and documentation on the web (including PDFs from publishers or
agencies). Verify every claim about a product or paper from its own documentation or text;
give DOIs (checked to resolve) and the supporting passage's location; mark anything you could
not verify. Distinguish evidence from recommendation. Plain words, British spelling. Start with
a summary of at most 300 words and end with a ranked recommendation and a short list of
decisions for Chris.
