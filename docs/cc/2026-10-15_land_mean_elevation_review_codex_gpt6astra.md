# Land-mean elevation: review by Astra (Codex gpt-6-astra, max effort), with reconciliation

reject

- **Error — [prepare_land_elevation.R:81](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/prepare_land_elevation.R:81):** QA4 misses nearshore ocean. Supplied data at 122.48°W, 47.80°N (Puget Sound) are water=1, QA1, so sea enters the mean. Validate ocean classification, including QA5, and regenerate.
- **Error — [line 87](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/prepare_land_elevation.R:87):** averaging discards NAs before coverage is measured. An in-memory GDAL example with 25% valid input reports 100% coverage. Average a validity indicator; test partial gaps. Line 94 also silently drops DEM voids; guard against incomplete non-ocean coverage.
- **Judgement — [line 69](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/prepare_land_elevation.R:69):** re-registration shifts elevations approximately 15.5 m northward, not 4 m. Check coastal sensitivity against original georeferencing.

The [C6.1 guide](https://landweb.modaps.eosdis.nasa.gov/data/userguide/MOD44W_User_Guide_C61.pdf) defines QA: 1 confident observations; 2/3 uncertain water/land; 4 buffered ocean; 5 ocean without water observations, including sea ice; 6 burns; 7 impervious; 8 restored inland water; 9 slope-rejected water; 10/253 fill/no-data. [Carroll 2017](https://doi.org/10.1080/17538947.2016.1232756) describes a separate 500 m classification.

The intended mean follows [New](https://crudata.uea.ac.uk/cru/data/hrg/tmc/new_et_al_10minute_climate_CR.pdf) and [HYDRO1k](https://daac.ornl.gov/ISLSCP_II/guides/hydro1k_elevation_xdeg.html). Area weights, units, 120×120 alignment, lake retention and config routing look correct. Six mountain/coast/lake/Arctic checks reproduce CSV values. Both legacy outputs match anchor MD5s exactly; current verification logs show unchanged land-cover columns. R reruns were blocked by temporary-file permissions.

---

## Reconciliation reply

reject

The coverage/NA finding and 15 m correction are resolved; removing MOD44W also removes its coastal-strip and Nettilling errors.

Baffin needs a fix to keep marine heights out of the calibration predictor. `tools/prepare_land_elevation.R:83` counts nonzero marine elevations as land; (−64.5, 67.5) now has “non-ocean share” 0.9985. The stated limitation does not resolve that contamination.

New’s precedent differs: [GLOBE distinguishes ocean (−500) from zero-elevation land](https://www.ngdc.noaa.gov/mgg/topo/report/s4/s4.html). Consequently, the “both shared by the precedents” claim at lines 30–31 is incorrect.

Simplest defensible correction: replace the elevation-zero test with a full-resolution [GSHHG level-1 coastline mask](https://www.soest.hawaii.edu/pwessel/gshhg/) before area-weighted aggregation, retaining lakes. Published basis: Wessel, P. & Smith, W.H.F. (1996), *A global, self-consistent, hierarchical, high-resolution shoreline database*, JGR 101, 8741–8743, [doi:10.1029/96JB00104](https://doi.org/10.1029/96JB00104). Check Baffin, Nettilling and genuine zero-elevation land; mixed coastal DEM pixels remain a limitation.

New issues: `scripts/1_veg_lct_prep.R:78` still records MOD44W provenance. The legacy verification spec’s line-8 correction exists only uncommitted; HEAD still checks nonexistent `...legacy-2-2`, permitting a vacuous pass. Commit the correction and verification reports.