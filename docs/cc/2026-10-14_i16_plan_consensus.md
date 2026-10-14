# I16 plan: Fable-Astra reconciliation and consensus, 2026-10-14

**What this is.** Chris would not proceed with I16 until Fable and Astra agreed. Fable
replied point by point to Astra's check (`2026-10-14_i16_plan_check_codex_gpt6astra.md`),
agreeing with all eight points and consolidating the plan; Astra then signed the
consolidated plan as written and, on the one remaining disagreement (the B5 remedy), chose
Fable's "quantify once in the preprocessing tool, switch to an exact per-cell table only if
the difference matters". Astra noted one typo in Fable's reply: cos(71)/cos(70) is 0.952,
not the reverse.

**Who wrote it.** Fable (Claude Code subagent, maximum effort, read-only) and OpenAI Codex
`gpt-6-astra` (reasoning effort `max`), 2026-10-14.

---

## Fable's reply and the consolidated plan


Point by point: 1 B5 NA handling AGREE (corrected form: num = resample(land * ifel(observed, albedo, 0)); den = resample(land * observed); result ifel(den > 0, num / den, NA); land carries NA only outside its tiles and 0 for all-water, so numerator and denominator see the same pixels). 2 Geometry AGREE on facts (cos(70N)/cos(71N) = 0.952, ~4.8%; edge pixels are 44% of a cell's area, first-order at coasts and lakes); the result is a weighted mean, not area-exact; the preprocessing tool also computes the exact piece-weighted (pixel x cell intersection, area-weighted) version from the 250 m mask for every cell and reports the difference distribution; if it matters, switch to a committed per-cell table of exact values. Land raster built on rast(bluesky) as template; target is grid as at 2:60,66. 3 B4 AGREE (reads at 5:56, 5:126, 5:364 move to the new REML prediction-fit file; 5:346-353 stay on the ladder; check_calibration_month invoked with legacy-run inputs and a registry REML callback; ML path checked separately by a January pilot; seven ML plus one REML). 4 Seed 1 AGREE: three separate check classes (frozen-input byte checks; tolerated one-month refit; seed-1 byte checks of 5-6 from identical fits); legacy_config.R is the input, not evidence. 5 C2 tests AGREE: expected _parts built from adjusted fractions; telescoping on a monotone retreat; sign reversal on the endpoint arithmetic moved into a small tested function endpoint_albedo_differences(); test header reworded (I16 fixes the advance; the first-pair readvance error stays, readvance policy is Andria's); the endpoint branch uses is_ice_* from the adjusted history and the per-month glacier albedo, never alb_ice_thresh. 6 AGREE: B5 replaces only the extract at 2:140 (bs_interp_df -> calibration_table, read by 4:23); the coarse product stays a diagnostic; B8 filters lct_interp_modern right after 2:39. 7 B9 AGREE: DEM masked at fine resolution in preprocessing, per-cell land mean, no quarter-degree land fractions. 8 AGREE: configs/clear_sky.R with allow_stale = TRUE (recorded) and a seeded run directory (albedo_differences, coarse_differences, read unconditionally at 9:324).

Consolidated plan:
i16a tail: kernel keys hadgem_variable ("albedo_sw"; legacy "albedo_sw_cs"), cam5_variable ("FSNT"; legacy "FSNSC"), cack_variable ("CACK CM"; legacy "CACK"), cack_year kept for "CACK"; ice$difference_method ("endpoint"; legacy "legacy_components"); one KERNEL_VARIABLES table in R/ for legal lists and script 9 labels; configs/legacy_config.R complete literal list with draws 100/100, seed = 1, run$dir ".rundirs/legacy", allow_stale FALSE, plus a test that it validates; script 8 varname from keys at 8:49, 8:90, one if/else for CACK at 8:99-102 (CACK CM: band = month, no level); script 9 labels from the table, remove the factor-of-two title; C5 a comment, no key; R/differences.R difference_method argument, endpoint branch for *_thresh only. Verify: legacy from frozen inputs byte-identical (ALB_diffs, forcing table, four CSVs); new config runs (36 pairs change); CACK CM vs ncvar_get at two cells and vs the 16-year mean; clear vs all sky within each model via seeded configs/clear_sky.R.
i16b calibration: fit_calibration_model(..., method = "REML"); calibration$comparison_method ("ML"; legacy "REML"), calibration$ice_mask ("centre_in_1ka_polygon"; legacy "none"); script 4 ladder by comparison method, experiments REML, third loop = REML fit of model_id to a new catalogue id, AIC section unchanged, print rank/converged; script 5 reads the new file at 56, 126, 364; B8 in script 2 after line 39 via tested cells_in_polygons(), recorded; B1 check_fractions() after 1:56, no key. Verify: ML smoke test on mod1; January ML ladder pilot; legacy AIC table byte-identical from frozen fits; seed-1 anchor with the new file seeded from frozen mod8; one-month refit via the tool; B8 48 expected, not hard-coded.
i16c land support (when MOD44W is on disk): tools for the land-fraction raster (with the exact piece-weighted comparison) and the masked DEM per-cell table, committed with READMEs; keys calibration$albedo_support ("land_mean"; legacy "centre_pixel") and elevation$method ("land_mean"; legacy "centre_point"); script 2 corrected four lines in the else branch at 2:140; script 1 chooses the elevation cache file. Verify: synthetic 2x2 case incl. c(0.4, NA); coverage recorded; lake, Hudson Bay, coastal cells; legacy branches byte-identical.
Then I13b: full legacy run and final-config run separately; 1,000-draw decision there.
Remaining disagreement: B5 remedy only — Fable keeps the visible resample() form and quantifies once in the tool, switching to an exact per-cell table only if the difference matters; Astra asked for area weights or quantification.

## Astra's sign-off

1. **SATISFIED** — B5 NA handling and shared masks.
2. **SATISFIED** — geometry, alignment and exact-reference comparison. Reply-only typo: **cos(71°)/cos(70°) ≈ 0.952**, not the reverse.
3. **SATISFIED** — all three REML readers, checker handoff and separate ML pilot.
4. **SATISFIED** — frozen-input, tolerated-refit and seed-1 checks separated.
5. **SATISFIED** — adjusted endpoints, appropriately scoped tests and corrected header.
6. **SATISFIED** — actual calibration-table wiring and early B8 filtering.
7. **SATISFIED** — fine-resolution water masking before DEM aggregation.
8. **SATISFIED** — both required upstream products seeded and stale reuse explicitly recorded.

**I sign the consolidated plan as written: yes.**

**B5: I choose Fable’s “quantify once, switch to an exact per-cell table only if it matters.”** My original requirement explicitly allowed quantification. The proposed exact, area-weighted comparison for every cell addresses both latitude weighting and land placement within boundary pixels, providing evidence for the switch decision. This resolves the stated disagreement; I require no further plan changes.