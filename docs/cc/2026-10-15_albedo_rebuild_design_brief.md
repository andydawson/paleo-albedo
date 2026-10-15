# Albedo raster rebuild: design brief (2026-10-15)

Written by Claude Code from Chris's decisions of 2026-10-15; given unchanged to Astra and Fable for independent designs.

---

# Design brief: rebuilding the monthly blue-sky albedo raster (paleo-RF)

## Benefit to the paper
The calibration albedo is the response the whole forcing estimate rests on; today it comes
from a raster whose construction is undocumented. A rebuild from public data with a written,
literature-grounded recipe makes the method reportable and reproducible, lets water be removed
at the satellite's native scale (so the land-only calibration is exact rather than an
approximation), and yields snowy and snow-free versions for the snow sensitivity test.

## Confidentiality (hard rule)
Do not open anything under `writing/`, nor any .docx, .pptx or .pdf file (unpublished
manuscript; copyrighted papers). Web searches are about data products and methods only.

## The existing raster (to be kept as the legacy setting)
`data/blue_sky_monthly_2000-2009.tif` (the large data directory is in the main checkout at
/home/expert/Documents/contracting/Andria/paleo-RF/data/; read-only): 12 bands, 0.25 degree,
lon/lat WGS84, extent -175.125 to -50.125 E, 10.125 to 80.125 N (pixel centres on whole
quarter degrees, the same lattice as ERA5's 0.25-degree grid), values 0.004 to 0.83. All
12 band names are `median`; README.md (data table) says "2000-2009 mean". The manuscript's
methods section (not available to you) says, in substance: MODIS MCD43A3 v061 from Terra and
Aqua at daily scale; the fraction of direct and diffuse shortwave from ERA5; monthly blue-sky
albedo per grid cell for each year 2000-2009 as (1 - f_diffuse) * black-sky + f_diffuse *
white-sky; then a summary over the ten years. Nothing else is known: not the reducer over
days or years, the quality and snow filters, gap filling, water handling, the time of day of
the diffuse fraction, or how 500 m was brought to 0.25 degree. Chris believes the person who
built it has moved on and the details may be lost; Andria will be asked for any surviving
code.

How it is used: script 2 (`scripts/2_calibration_lct_bluesky.R`) extracts it for the 1-degree
grid (`data/grid.RDS`, -172 to -52 E, 17 to 79 N; 2,860 land-cover cells); the decision of
2026-10-14 is to calibrate on the mean albedo over the **land** part of each 1-degree cell
(inland water and ocean excluded) and to use land-mean elevation likewise. See also
`R/preparation.R`, `config.R`, `configs/legacy_config.R`, and on this branch
`docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md` (the review that led here)
.

## Inputs already on disk
MOD44W v6.1 land/water mask, 2015 annual layer, native sinusoidal 231.66 m, 7 files in
`data/mod44w/2015/` of the main checkout (water mask 0 land / 1 water, plus its quality
layer), downloaded via AppEEARS for the study area (`data/mod44w/paleo_rf_area.geojson`).

## Chris's decisions and constraints
- Rebuild from public data, grounded in published methodology; every choice cited or
  explicitly labelled a judgement.
- Remove water at the native MODIS scale using MOD44W before any spatial averaging.
- **Download everything and process locally** on the workstation, not in Google Earth Engine.
  Workstation: 48 cores (the project may use at most 24), 125 GB RAM, 1.3 TB free disk on one
  NVMe drive. Chris holds a NASA Earthdata login and will create a Copernicus CDS account;
  credentials live in `~/.netrc` and `~/.cdsapirc`, never in the repository.
- Also produce what the snow sensitivity test needs: snow-free albedo (and the snowy share /
  valid-day counts) alongside the all-conditions albedo, by month.
- The pipeline must stay runnable from R alone on any OS (no bash scaffolding); preparation
  tools live in `tools/` and are run once, writing documented, checksummed outputs and a
  manifest; run options are config keys with the original as the legacy setting (here, the
  existing raster).
- Readability before elegance: simple, explicit code a scientist can read.

## Measured data volumes (NASA CMR granule queries, 2026-10-15)
- MCD43A3 v061 (500 m, daily, HDF4 with all 7 bands + 3 broadbands, BSA/WSA and quality):
  ~56 sinusoidal tiles intersect the domain's bounding box (h02-h17, v00-v07, plus a few
  spurious high-latitude ones), about 50 MB per tile-day: ~2.8 GB per day, ~10 TB for
  2000-2009.
- MCD43A2 v061 (quality, snow flag, land/water): ~15 MB per tile-day, ~0.9 GB per day,
  ~3.3 TB for ten years.
- MCD43C3 v061 (0.05-degree climate-modelling grid, daily, global, includes percent snow and
  percent inputs): ~182 MB per day, ~0.65 TB for ten years (before cropping).
- ERA5 hourly single levels (surface solar radiation downwards, and its direct component) at
  0.25 degree for the domain: small by comparison.
So the 500 m route needs streaming (download, reduce, delete) or a layer subset (e.g.
AppEEARS, OPeNDAP/Harmony, or reading only the needed SDS); keeping raw files is not possible.

## What to produce
1. The recipe, step by step, each step with its published basis (e.g. the MCD43 user guide
   and ATBD, Schaaf et al. 2002, Lucht et al. 2000, Roman et al. 2010 on blue-sky albedo, the
   ERA5 documentation, Moody et al. 2005 for screening water before aggregation, and anything
   more recent), and the alternatives considered. Cover at least: product and layers;
   shortwave broadband choice; quality filter (full vs magnitude inversions, which QA bits);
   snow handling and the snow-free version; the diffuse fraction (which ERA5 variables, which
   hour or daily weighting, solar-noon issues); how to combine daily values into a month and
   the ten years into a climatology (mean or median, minimum valid days, weighting); polar
   night and low sun; water screening rule for 500 m pixels (four 250 m mask pixels each:
   all land? a threshold?) and coastal pixels; reprojection from sinusoidal and aggregation
   to the 1-degree land mean (directly, or via 0.25 degree for comparison with the legacy
   raster); the outputs' format and provenance.
2. The route to get the data locally, comparing at least: full HDF download with streaming
   reduction; layer-subset services; the 0.05-degree CMG product (does it already exclude
   water? is 0.05 degree good enough for land-only?). Give disk, download volume and
   processing load for each; recommend one. Do not estimate durations in hours or days;
   volumes and counts are fine.
3. How it fits the pipeline: tool(s) in `tools/`, config keys (with the existing raster as
   the legacy setting), outputs, manifest, tests, and how the regression contract stays
   intact for the legacy configuration. R-only; name the R packages.
4. Validation: comparison with the existing raster (where should they agree, where not and
   why), with an independent blue-sky climatology (e.g. Jia et al. 2022), spot checks
   (lakes, coasts, snow regions), and what result would make us stop.
5. Risks, open questions for Andria (if any), and a short list of decisions for Chris.

Write in plain words, British spelling. Start with a summary of at most 250 words.
Recommendations must be distinct from evidence; mark anything unverified.
