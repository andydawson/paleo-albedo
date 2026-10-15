# Land-mean elevation: review by Fable (with reconciliation)

# Review: land-mean elevation (branch i16c-elevation)

Reviewer: Fable (science-reviewer role), 2026-10-15. Read-only; R was run only to inspect inputs and outputs, as the packet allowed; nothing was written outside this file.

**Verdict.** Approve with changes.

**Summary (under 200 words).** The code does what its header says, and I confirmed it independently: an equal-area recomputation of all 2,870 cells in the MOD44W sinusoidal grid (nearest-sampled at 1.85 km, GMTED projected onto it) agrees with the committed table to within 4.3 m for 99% of cells (max 15 m, steep Mexican cells; shares within 0.012). GMTED registration, dimensions, the 4 m shift, the aggregation alignment, NA handling, area weights and the guards are right. The legacy branch is the original code re-indented, and the cache has no NA, so the new stop cannot fire there. Two scientific deviations from "ocean out, lakes in" come from the MOD44W ocean mask itself, both verified against the C6.1 user guide and the data: (1) the mask by design starts about 1 km offshore, so a coastal strip of sea is kept as "non-ocean" at 0 m, biasing fjord-coast cells low by up to 77 m (116 cells by more than 10 m); (2) Nettilling Lake (freshwater, 28 m) is 97% "ocean mask", so three Baffin cells lose a lake they should keep. Both need a decision by Chris, not a code fix on my own authority. The tool's header cites two data READMEs that do not exist, so the inputs cannot be re-obtained from the repository.

**Benefit.** Each cell's elevation describes the same area as its land-cover fractions, so the calibration's elevation smooth is fitted on matched support and coastal cells no longer get sea-floor values (eight cells rose from -28..-1 m to 38..543 m).

## Hand check

Two GMTED pixels of equal area A in one cell. Pixel 1: all land, 100 m. Pixel 2: half sea, GMTED mean 20 m (GMTED's 30-arc-second mean is over all 900 one-arc-second source cells, and the sea cells are 0, so the land half really averages 40 m). MOD44W ocean share: 0 and 0.5.

Code lines 92-104 (`tools/prepare_land_elevation.R`): weight1 = A, weight2 = A/2; sum_elev_weight = 100A + 20·A/2 = 110A; sum_weight = 1.5A; elev = 73.3 m; non_ocean_share = 1.5A/2A = 0.75. The code's arithmetic agrees with this on paper (I re-derived it from the lines; the full-table recomputation confirms it numerically).

The physically right land mean is (100A + 40·A/2)/1.5A = 80 m: the 6.7 m shortfall is the sea-zeros inside GMTED's mixed coastal pixels, which no MOD44W weighting can remove. This is inherent to any 1 km DEM mean and shared by both precedents (GLOBE, HYDRO1k), so it is a known limitation to state, not a defect of the tool. It has the same sign as finding 1 below.

## Verification of the documentation claims (from the sources)

MOD44W C6.1 user guide (Carroll et al., https://lpdaac.usgs.gov/documents/1915/MOD44W_User_Guide_ATBD_V61.pdf):
- p. 4, "Ocean mask": "To derive the ocean mask for collection 6 we used the MOD44W C5 to define the boundaries of the continents. The coastline was buffered by 4 pixels (~1 km) into the water bodies. This buffered water was intersected with the ocean (all 3 ocean classes merged to one class) from MOD44WA1 to isolate ocean from inland water. This mask was applied to all annual collection 6 water maps to account for ocean water that is routinely obscured by clouds, ice, or other visual impediments."
- p. 5, "Final output maps": "land = 0, water = 1, Fill = 250 (outside of projected area), No Data = 253". The GeoTIFFs declare NoData 253; the data hold only 0, 1 and NA (no 250).
- p. 5-6, "QA description": 1 = high confidence observation ("no distinction is made in the QA between land or water"); 2 = lower confidence water ("Few observations of water but MOD44W collection 5 indicated water"); 3 = lower confidence land; 4 = ocean mask ("Buffered out from coast by ~1 km (i.e. 4 pixels)"); 5 = ocean mask but no water observations recorded ("Probable continuous sea ice, or land expansion through deltas or urban reclamation"); 6 = burned area; 7 = urban or impervious surface; 8 = no water detected but MOD44W shows inland water; 9 = water detected but DEM slope indicated no water; 10 = fill; 253 = no data.
- p. 16, Appendix, MOD44WA1 table 2: the ocean classes are 0 shallow ocean (depth < 160 ft), 6 moderate ocean, 7 deep ocean, from ETOPO1 bathymetry; 3 inland water; 5 deep inland water (> 160 ft, e.g. the Laurentian Great Lakes, p. 3).

So "ocean = water 1 and QA 4" is a defensible reading: QA 4 is exactly the product's own ocean class. But it is the product's ocean class minus a 1 km coastal strip, and it is wherever MOD44WA1's bathymetry-based classes said ocean, which swallows at least one large freshwater lake (findings 1 and 2). QA 5 pixels (ocean mask, nothing observed) carry water = 1 and are not QA 4; in 2015 over the study area they are 70 sampled pixels, negligible.

GMTED2010 report (Danielson and Gesch 2011, OFR 2011-1073, https://pubs.usgs.gov/of/2011/1073/pdf/of2011-1073.pdf):
- p. 8, left column: "SRTM DTED2, the primary input source data for GMTED2010, follows the 'pixel center' coordinate referencing convention in which integer lines of latitude and longitude that define the boundary of a 1 x 1-degree tile are located at the center of a pixel (fig. 8)."
- p. 9, right column: "the full extent of each GMTED2010 grid as defined by the outside edges of the pixels differs from an integer value of latitude or longitude by 0.000138888888 degree (or 1/2 arc-second) (fig. 10)."
- p. 9, table 3: mean, 30 arc-seconds: -180.0001388888 / -90.0001388888 / 179.9998611111 / 83.9998611111; 20,880 rows; 43,200 columns. The tool's header comment and `stopifnot` (lines 62-68) match this exactly; the shipped grid reads as origin -180.000138889, 83.9998604.
- p. 9, left column: "each 30-arc-second (0.008333333333 degree) pixel covers the exact area covered by 900 1-arc-second SRTM pixels (a 30 x 30-pixel matrix)" (the mean is over all 900, hence the sea-zeros in coastal pixels).
- p. 12: "North of 84 degrees the void value is assigned to 'NoData'" (-32768 in GeoTIFF). The report nowhere says the sea is NoData; in the data the sea is 0 (checked at 15 open-sea points and over 1.6 million sampled QA-4 pixels: 100.0% zero south of 60 N, 99.6% north). This is why the MOD44W mask carries the whole burden of excluding the sea, and why finding 1 matters.

## Findings (worst first)

**1. Should-fix (scientific definition; a decision for Chris).** `tools/prepare_land_elevation.R:30-33, 81`. The MOD44W "ocean mask" starts about 1 km offshore by design (user guide p. 4-5, quoted above), so the first ~4 pixels of sea along every coast are water with QA 1 or 2. The tool keeps them as non-ocean, and GMTED gives them 0 m, so they pull coastal cells' means down. Measured on the 1.85 km equal-area sample: 13-15% of all non-QA-4 water in the study area is at 0 m, i.e. sea; 424 cells have more than 1% of their area in such pixels, 60 more than 5%; the implied shortfall (share / non_ocean_share x elev) exceeds 10 m in 116 cells and 25 m in 34, largest 77 m at (-124.5, 50.5) Vancouver Island (999.7 m), 50-60 m along the BC and Alaska Panhandle fjord coast and Labrador. Consequence: a systematic low bias of up to 8% in exactly the coastal cells the change was made for; smaller than the correction itself (median absolute change versus the centre point 36 m, 95th percentile 283 m) but one-signed. The header's step 1 ("Lakes are water in other QA classes and stay in") presents the non-QA-4 water as lakes; a seventh of it is sea. Verification: compute per cell the area share of (water = 1, QA not 4, GMTED = 0) and report it; or test the alternative rules below on the 34 cells and show the difference. Options for Chris: (a) accept and state the limitation in the tool header and README with the numbers; (b) add the sea-level rule "a water pixel at GMTED 0 m is sea" (one extra condition, removes the strip, also drops the few 0 m lagoons and delta lakes whose elevation is 0 anyway); (c) define the sea from the DEM alone (water = 1 and GMTED = 0), which is in fact closer to both precedents (New et al. use GLOBE's own ocean flag; HYDRO1k its own non-ocean extent) and drops the QA layer. Meanwhile the code does (a) without saying so.

**2. Should-fix (scientific definition; a decision for Chris).** Same lines. The MOD44W ocean mask inherits MOD44WA1's bathymetry-based ocean classes, and at least one large freshwater lake is in it: Nettilling Lake, Baffin Island (66.5 N 70.5 W, surface ~30 m, GMTED 28 m) is 97.0% QA 4 over its water pixels (15,588 sampled), against 0.0% for Amadjuak, Great Bear, Great Slave, Winnipeg, Athabasca, Iliamna, Okeechobee, Superior, Great Salt, Reindeer, Mistassini, Dubawnt, Teslin, Yathkyed and Baker (all 89-100% QA 1). Lake Melville (89% QA 4) and Lake Pontchartrain (96%) are brackish arms of the sea and defensible either way. Consequence: cells (-70.5, 66.5), (-71.5, 66.5), (-69.5, 66.5) lose 53%, 22% and 9% of their area as "ocean" though the decision keeps lakes; (-70.5, 66.5) reads 68.0 m with non_ocean_share 0.470 where a lakes-kept mean is about 47 m with share ~1.0. Over the whole table the lake-as-ocean area is 0.04% (QA-4 water above 20 m), so this is local. Verification: the per-cell share of QA-4 water with GMTED > 20 m, as I computed (only the three Baffin cells and (-64.5, 67.5) exceed 5%; the latter are fjord-edge sampling artefacts, not lakes). Options: accept and name the exception in the header; or rule (c) of finding 1, which fixes both.

**3. Should-fix (documentation).** `tools/prepare_land_elevation.R:22` says "see data/gmted2010/README.md and data/mod44w/README.md"; neither file exists in the main checkout (`ls` fails on both). The AppEEARS request (`data/mod44w/2015/paleo-rf-mod44w-2015-request.json`) and the polygon (`data/mod44w/paleo_rf_area.geojson`) are untracked and unmentioned. Consequence: Andria cannot re-obtain the inputs or re-run the tool from the repository. Fix: write the two READMEs (GMTED `mn30_grd.zip` URL and the verified SHA-256 dfd0d6c6486f4da22109be6107c93a70ef9917a5b6f954cd82d87cf8b920149d, which I recomputed and which matches README.md; the AppEEARS product, layers, year and polygon), and commit the request JSON and polygon (small text files).

**4. Nit (records).** `docs/cc/questions_for_andria_scientific.md`, question B9 (the elevation question) is untouched by the branch: it still describes the centre point as the method and asks whether a cell mean was considered, without saying that Chris chose the land mean on 2026-10-14/15 as an interim default and what the code now does. Add that note; the question stays open for Andria.

**5. Nit (verification).** `docs/cc/verification/2026-10-15_i16c-elevation-current.R` only shows the elev column changed. See "What the proposed verification misses".

**6. Nit (code).** `tools/prepare_land_elevation.R:55`: the cells come from `data/elev_interp_cells.RDS`, the legacy cache that README invites the user to delete to force a fresh lookup; the tool then cannot run. Read the cells from the land-cover product (`unique` x, y of `veg_posts_interp_ice.RDS`) or `grid.RDS`. Line 72 asserts only xmin/ymax alignment; adding `ncol(gmted) %% fact == 0, nrow(gmted) %% fact == 0` would state the step-4 reliance the comment on line 59 describes (it holds: 14,160 x 7,440). `non_ocean_share` is written to the table but unused by script 1; fine as a diagnostic, say so in README.

**7. Nit (documentation).** `README.md:128` "script 1 ~4 min (mostly the elevation lookup)" is stale under both settings (the lookup is cached or absent). `tests/test_config.R` has no `rejects` case for an illegal `elevation$method`; one line.

**Checked and found right (not findings).** Legacy path: the `else` branch (`scripts/1_veg_lct_prep.R:79-94`) is the original statements re-indented; the NA stop at line 97 cannot fire on the cache (0 NA in 2,870 rows); byte-identity rests on the legacy spec's anchor run, not yet reported. New path: `read_input(run, path, read.csv)` matches `R/run_manifest.R:94`; all 2,870 land-cover cells are in the table. Config and schema: plain `check_choice`, both configs carry the key. Tool: the extent reset (line 69) is the documented half-arc-second; crop and 120 x 120 aggregation land on the .5 centres; `project(..., 'average')` with NA outside the download gives the coverage guard its meaning, and NA occurs only where both layers are NA; area weights are geodesic (`cellSize`); the one cell with sampled nodata, (-171.5, 63.5), has 0.66% at the polygon's curved western edge in my chord-approximated polygon, not in the tool's pixel-wise test. Plausibility: Colorado (-106.5, 39.5) 3,018 m versus centre 3,492; Mexican highlands (-98.5, 19.5) 2,594 versus 2,704; Death Valley cell (-116.5, 36.5) 896 versus 708; Hudson Bay west coast (-93.5, 58.5) 28 m, share 0.73; Alaska Peninsula (-160.5, 55.5) 194 m, share 0.51; Ellesmere (-78.5, 78.5) 982 m; Great Lakes pixels are QA 1 with GMTED 179 m at Lake Superior (true 183). The Great Lakes and Florida Keys spot cells are not land-cover cells at all. Method versus precedents: non-ocean mean, lakes kept, as in New et al. (2002, p. 7) and Verdin (2011, sec. 9.2.1), with the two deviations above.

## What the proposed verification misses

- All-land invariant (needs GMTED only): for the 2,241 cells with non_ocean_share = 1, elev must equal the area-weighted mean of the cell's 14,400 GMTED pixels (independent of MOD44W and of the reprojection).
- One coastal cell by an independent route: equal-area mean in the sinusoidal grid over non-ocean pixels (as I did), agreeing to a few metres.
- Table integrity: 2,870 rows, unique (x, y) on .5, elev finite, share in (0, 1], and every land-cover cell present.
- The two diagnostic shares per cell from findings 1 and 2 (sea at 0 m outside the mask; mask "ocean" above 20 m), kept as a documented table so the decision is traceable.
- `tests/test_config.R`: an illegal elevation method is rejected.

## Questions for Chris or Andria

1. Ocean definition (finding 1): keep MOD44W QA 4 and document the 1 km strip (bias up to 77 m, 116 cells over 10 m); add "water at GMTED 0 m is sea"; or define the sea from the DEM alone (water and GMTED 0), closest to the precedents. Meanwhile the code keeps QA 4 with the strip counted as land at 0 m.
2. Nettilling Lake (finding 2): accept the exception or fix it with the rule above. Meanwhile three Baffin cells treat the lake as ocean.
3. Should the inherent coastal-pixel dilution of GMTED's 30-arc-second mean (hand check) be stated as a limitation in the paper's methods? Meanwhile it is unstated.

## Not checked

The verification runs themselves (no report yet). The GMTED mixed-pixel dilution magnitude (hand check mechanism only). GDAL's exact contributing-pixel rule for 'average' where the 250 m source is coarser than the 185 m GMTED pixel width at 78 N (the aggregate agreement makes it immaterial at cell scale). The precedents' PDFs (taken from the packet's verified record). Anything under writing/ or any .docx/.pptx (not opened).

Files read: tools/prepare_land_elevation.R, scripts/1_veg_lct_prep.R, config.R, configs/legacy_config.R, R/config_schema.R, R/paths.R, R/run_manifest.R (read_input), tests/test_paths.R, tests/test_config.R, README.md (elevation lines), docs/cc/2026-10-15_land_mean_elevation_precedents.md, docs/cc/questions_for_andria_scientific.md (B9), the two verification specs, data/elev_land_mean_cells.csv, data/elev_interp_cells.RDS, data/veg_posts_interp_ice.RDS (cells), the MOD44W GeoTIFFs, QA lookup, statistics and AppEEARS README, the GMTED grid and its metadata, the MOD44W C6.1 user guide and the GMTED2010 report (PDFs); 27 tool calls (5 Read, 18 Bash of which 5 ran R, 1 ToolSearch, 1 WebSearch, 2 WebFetch).


---

## Reconciliation reply

# Reconciliation: elevation tool revised (sea = GMTED 0 m), Fable, 2026-10-15

Checked read-only: commits d1afc46, 29879f1 (and specs 95fedfa, e15faed), tools/prepare_land_elevation.R, new table versus the mask-based table (8733137), plus a sampled MOD44W cross-check.

AGREE: findings 1 (coastal strip) and 2 (Nettilling, now 46.8 m, share 1.0) are resolved; finding 3 (inputs re-obtainable: URL, size, SHA-256 in the header) resolved; the ncol/nrow assertion and void check are in; 15 m is right (1/7200 degree x 111 km). Code lines 80-91 are the hand check verbatim; verification PASS taken as stated.

Table against the mask version: 637 cells higher (median 4.2 m), 2,198 unchanged, 35 lower. The lower ones are wider than "four cells of eastern Baffin Island": by more than 20 m, (-64.5, 67.5) -179, (-65.5, 67.5) -67, (-89.5, 65.5) -62 (Wager Bay, Nunavut), (-145.5, 60.5) -31 (Prince William Sound, Alaska), and Nettilling -21 (intended); 14 Ellesmere and Arctic cells by 3-13 m. GMTED's CDED and NED-Alaska sources carry land heights over several inlets, not only Baffin fjords. The header (lines 31-33) should say so and list the five.

Exact-0 m land (MOD44W land at GMTED 0): 192 cells above 1% of area, 12 above 5%, none above 20%; the overstatement is at most about 7% (31 m at (-136.5, 68.5), Mackenzie delta). Bounded; shared with GLOBE; fine as stated.

Acceptability: acceptable as a stated limitation (5 cells of 2,870 off by 31-179 m, inside the forcing domain). If Chris wants a fix, the simplest is the union "sea = GMTED 0 m or MOD44W ocean class 4": it fixes the inlets and the strip and costs Nettilling (21 m, one cell) and the MOD44W reprojection again; no published precedent for either combination, so it is his call.

New, nit: line 78 `values(gmted, mat = FALSE)` loads 105 M cells; `global(gmted, 'isNA')` does the same check without the memory.
