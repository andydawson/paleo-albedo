# I15 readability cleanup: inventory refreshed against the current code

**What this is.** The inventory of issue #18 (I15), with its three comments, checked item by item against branch `i16b-calibration` (5deabac): done, obsolete, partly done or to do, with current line numbers; the to-do items grouped into atomic commits with the cheapest adequate check for each; risks. Prepared 2026-10-15 so the cleanup can start once I16 is merged.

**Who wrote it.** A Claude Code subagent (read-only), 2026-10-15.

---

# I15 (#18) inventory refreshed against `i16b-calibration` (5deabac), 2026-10-15

Line numbers are for `git show i16b-calibration:<file>`. Script names are under `scripts/`.
Status: DONE / TO DO / PARTLY DONE / OBSOLETE; KEEP = a "retained" row that still applies
and needs no change. Row numbers (#) are only for the grouping in part 2.

## 1. Item status

### Issue body, main table

| # | Item | Status | Where now | Note |
|---|---|---|---|---|
| 1 | 1: `lct_interp` holds draws, then means | DONE | - | 156a537 (I04a): `reduce_land_cover()` in R/preparation.R:16; `lct_interp` now only holds the draws (1:36), the summary is `lct_interp_wide` (1:56) |
| 2 | 1: `ele_get_cells` | TO DO | 1:75-78 | only runs when the elevation cache file is absent; I16c will edit this block |
| 3 | 1: `present`, table `status`, `sums`, callback `d` | TO DO | 1:133-134; 1:172-198; 1:226-231; 1:217-220, 234 | keep column `status` (1:178-180, 186, 197) and all printed text |
| 4 | 2: `bs_interp_df`, `bs_interp_df_coarse` | TO DO | 2:67-73, 153-163 | I16c will edit 2:153-160 |
| 5 | 2: overwritten colnames, unused `cols_fill`, `vname` | TO DO | 2:72; 2:112-119; 2:126, 140 | dead code |
| 6 | 3: `cal_long` changes meaning | TO DO | 3:50-54, 156-160, 188 (alias), 190-212 | |
| 7 | 3: unused `color_values_four` | TO DO | 3:84 | |
| 8 | 3: tutorial comments, "month 5" | TO DO | 3:125, 129, 132, 134-135; 3:222, 229 | comments only |
| 9 | 4: unused `cal_data2`, `cal_data3` | TO DO | 4:25-26 | |
| 10 | 4: five `ctrl` assignments | DONE | - | 7dda080 (I04a): one `CALIBRATION_CONTROL`, R/model_registry.R:55; no `ctrl` left in script 4 |
| 11 | 5: `mod8_interp` as selected-model binding | DONE | - | e612b69 (I04a) + ba26bb5 (I16b): selected model is `cal_interp_model`, read from `calibration_prediction_fit` (5:56, 126, 364); `mod8_interp` now means ladder model 8 only (5:353). `selected_fit` name optional |
| 12 | 5: `model_name` + `eval(as.name(...))` | TO DO | 5:251-262 | named list indexed by the existing ids in `models` (5:234) |
| 13 | 5: `cal_interp_sim_gam` = draws, then table | TO DO | 5:72-89; 5:142-155; 5:392-410 | |
| 14 | 5: `sim`, `sim_df`, `sim_melt` | TO DO | 5:264-277 | |
| 15 | 5: `cal_diff` holds stats | TO DO | 5:212-224 | CSV headers set explicitly at 5:222 |
| 16 | 5: unused `cal_sim_SPE` | TO DO | 5:226-232 | |
| 17 | 6: long object family | TO DO | 6:46-80 | I10 was dropped, so this falls to I15 |
| 18 | 6: first column renamed by position | TO DO | 6:37 | `ages` -> `year` by name |
| 19 | 7: `paleo_interp_sim_gam` reads point predictions | TO DO | 7:152 | read but never used; keep the read |
| 20 | 7: `breaks`; `labels` reused | TO DO | 7:27-29, 94-118; 7:438-490 | `labels` = bin ids (7:96) then period text (7:438); `labels_period` (7:475) repeats `period_labels(years)` |
| 21 | 7: `thresh`, `values` | TO DO | 7:492-525 | |
| 22 | 7: overwritten definitions | TO DO | 7:27-29; 7:495; 7:498-503; 7:512-518 | also: the final `sc_colour_diverge` (7:520-525) is never used, nor are `sc_colour`, `sc_fill`, `sc_fill_seq`, `sc_colour_seq` (7:98-116) |
| 23 | 7a: callback `x`; `idx_ice` twice; `ice_status_veg` | TO DO | 7a:54; 7a:68-69 and 157-167; 7a:74-78 | 7a:76 `hist()` takes its title from the expression (risk 4) |
| 24 | 7a: `spdf_2`, `sf_data` | TO DO | 7a:121-124 | |
| 25 | 7a: `cell_id_missing`, `_new` objects | TO DO | 7a:172-193 | better removed as dead code (comment 2, item 4) than renamed |
| 26 | 7a: repeated `rowSums()` pair | DONE | - | 61d59a2 (I11), now in R/differences.R:77-78 |
| 27 | 7a: `foo2` | TO DO | 7a:271-277 | |
| 28 | 8: `bar` | TO DO | 8:164-196 | |
| 29 | 9: `d`, `d7` | TO DO | 9:140-176 (and inside `aggregate_forcing`, 9:167); 9:328-335, 346 | |
| 30 | 9: `KERNELS`, `keep`, `main`, `sens`, `col`, `k`, `v` | TO DO | 9:131-133; 9:154-162; 9:179-186; 9:189-194; uses at 229, 282, 304, 367-368 | |
| 31 | 9: `np`, `np_full` | TO DO | 9:153-158 | `n_pairs` already names the per-period table (9:153), so the proposed column name clashes; rename the table too |
| 32 | 9: `read_erf()` `p`, `x`; `panel_plot()` `dat` | TO DO | 9:197; 9:248-249 | |
| 33 | 9: `hol`, `both`, `hol3`, `rng` | TO DO | 9:229-242; 9:267-276; 9:282-289; 9:355-357 | |
| 34 | Headers: "every remaining line unchanged"; 7a's unfinished sentence | TO DO | 1:2-5; 2:2-3; 4:2-4; 5:2-5; 7:2-3; 7a:1-3; 8:2-3 | 4's "files written are unchanged" is false since I16b; 1's omits `check_fractions()` (I16b); 6:2-4 is accurate enough |
| 35 | R/calendar.R `unknown`, `dup` | TO DO | R/calendar.R:17-21 | |
| 36 | R/config.R syntax-tree `e`, `f`, `what`, `args`, `a` | OBSOLETE | - | f404f58: the restricted parser is gone, config.R is read with plain `source()` |
| 37 | R/config.R `dup`, `k`, `i` | TO DO | R/config.R:69-70; 75-76; 87-92 | keep `config_path(args = ...)` |
| 38 | R/config_schema.R `bad`, `top` | TO DO | R/config_schema.R:51-54; 68-74 | the same `bad` pattern is new in R/preparation.R:40-44 |
| 39 | R/map_helpers.R `g`, `b`, `span` | TO DO | R/map_helpers.R:38-39 | |
| 40 | R/paths.R `d`, `rel` | TO DO | R/paths.R:114; 128, 135-151, 187-192, 198 | |
| 41 | R/paths.R `t`, `out`, `ms`, `ks`, `m`, `k` | TO DO | R/paths.R:207-213; 219-227 | |
| 42 | R/paths.R `done`, callback `f` | TO DO | R/paths.R:240-251 | |
| 43 | R/run_manifest.R `p`, `sz`, `h`, `t0` ... `L` | DONE | - | 6399b6e (I05) rewrote it with role names; only `pkgs` and callback `p` remain (R/run_manifest.R:300-303), small scope |
| 44 | make_grid.R `vars`, `xy`, `ext` | TO DO | scripts/make_grid.R:5-8 | do it with the move into R/preparation.R (comment 2, item 5) |

### "Names deliberately retained" table

| # | Item | Status | Where now | Note |
|---|---|---|---|---|
| 45 | 5: first summary's `month` column holds observed albedo | TO DO (comment only) | 5:81 (built), 5:84, 92-93 | add the explanation; keep the column |
| 46 | 6: `alb_mean`, `iter` on disk | KEEP | 6:50, 64-66, 73 | |
| 47 | 7: loop `year` vs column `year` | KEEP | 7:321-326 | `# CM:` note already at 7:318-320 |
| 48 | 3, 9: `variable`, `value`, `ETS`/`STS`/`OVL`, `striplab` | KEEP | 3:156-212; 9:248-276 | |
| 49 | 7a, 8: saved coordinate/area/ice/difference/forcing columns | KEEP | 7a:221-237; 8:68-70, 128-162 | |
| 50 | model ids, path oddities, comparison labels | KEEP | R/model_registry.R:15-51; R/paths.R:154-166; 5:234, 251-258 | |
| 51 | `test = "Chisq"` | KEEP | 4:70; 5:360 | |
| 52 | Scripts outside the pipeline: remove unless Andria keeps them | TO DO (waiting for Andria) | scripts/archive/ (14 files), GCM_snow_prob.R, thornthwaite.R, beta_veg_lct_modern.R, 6_prediction_model_spatial_eval.R | general questions sections 6 and 7 are still open on the branch |

### Comment 1 (Astra's readability review)

| # | Item | Status | Where now | Note |
|---|---|---|---|---|
| 53 | Contracts: sign, units, zero vs NA, kernel conventions | PARTLY DONE | 2:159-160; 8:57-58, 64-66, 101-114, 128-162 | R/differences.R:7-37 now states sign (younger minus older), NA and zero rules for 7a. Still missing: units, and the kernel conventions in 8 (lon 360/lat 180, `t(flip())`, x100, minus sign on CACK), and why 2 sets zero albedo to 1e-4 |
| 54 | Indent guarded blocks with a purpose comment | TO DO | 5:343-426; 7:211-236, 265-291, 294-315; 9:346-363 | whitespace only; 5:340-342 says "not re-indented" |
| 55 | Named list instead of `eval(as.name())`; registry formulas by `reformulate()` | PARTLY DONE | 5:251-262 | the list part = row 12. The `reformulate()` part is OBSOLETE: since I04a the registry stores formulas, not text (R/model_registry.R:15-61), and changing how they are built could change the fitted objects |
| 56 | Select/rename by name; explicit `by =`; "one row per ..." comments | TO DO | 6:37; 7a:126; 7a:255 (and 7:420); 2:62; comments at the writes 1:97, 101; 2:165-166; 5:224; 6:52, 68, 80, 105-106; 7:75, 436, 485-486; 7a:269; 8:198 | |
| 57 | Script 9: pass table, Earth area, limits as arguments | TO DO | 9:166-176; 9:248-264 | globals `d`, `EARTH_AREA`, `rng` |
| 58 | Comments describing an earlier state | TO DO | 1:29, 45, 207-208; R/model_registry.R:16; headers (row 34); also 2:87 ("line 123"), 9:60-65 (script 8's `labels_period` no longer exists), 9:329 ("27-74 N" is config now) | comments only |
| 59 | 5: save the selected model once per month; label sections by product | TO DO | 5:59, 129, 367 | three writes of the same file |
| 60 | Calculation and writes first, optional plots after (7, 8, 1) | PARTLY DONE | 8:164-198; 7:395-486 | 1 already has its diagnostics after both writes (1:103-253). 8 writes after its `bar` checks. 7 builds its difference table and ice outlines between the plots |
| 61 | Split seed creation from recording | TO DO | 5:37-38; 6:27-28 | |
| 62 | Check tools say what they did | TO DO | tools/check_anchors.R:16-37; tools/compare_tables.R:88; tools/check_calibration_month.R:20-22, 39, 55 | no totals; "within tolerance" printed with no tolerance file; model 8 row hard-coded, not printed; stale `get(month) ~` comment |

### Comment 2 (from the dropped extraction issues)

| # | Item | Status | Where now | Note |
|---|---|---|---|---|
| 63 | 6: rename by name, seed split, "one row per cell, slice and draw" | TO DO | 6:37; 6:27-28; 6:68 | same as rows 18, 61, 56 |
| 64 | 9: `aggregate_forcing()` / `panel_plot()` arguments | TO DO | as row 57 | same as row 57 |
| 65 | 5: named list; dead `cal_sim_SPE`, `cal_data2/3`; save once | TO DO | as rows 12, 16, 9, 59 | same items |
| 66 | 7a: dead coverage check and binning | TO DO | 7a:172-193; 7a:195-210; also 7a:215-216 (`facets`) and 7a:92-93 (only feed `facets`) | keep 7a:217: it makes the saved `month` column a factor |
| 67 | Move `make_grid()` into R/preparation.R | TO DO | scripts/make_grid.R; sourced at 3:62 | |
| 68 | 8: join-uniqueness `stopifnot()` | TO DO (unclear) | - | script 8 has no join on this branch; kernels are attached by spatial extraction (8:80, 95, 119). The joins are at 9:157, 162, 332 and 7a:128. Ask Chris which was meant |
| 69 | Comments at sign, NA, kernel sites | PARTLY DONE | as row 53 | same item |

### Comment 3 (Fable's review)

| # | Item | Status | Where now | Note |
|---|---|---|---|---|
| 70 | Remove "(issue I0x)" tags and "removed here" comments | TO DO | about 96 tags in scripts 1-9 (1:10, 2:7, 3:4, 4:10, 5:9, 6:6, 7:15, 7a:15, 8:12, 9:8), about 23 in R/. Removal notes at 7:31-32; 7a:29-30, 103-104, 212-213; 8:32-34, 36-38. History notes at 4:88-92 and 5:245-250 | keep the `# CM:` notes on open questions (2:173-175; 7:175-181, 318-320, 372-373, 454-456) and 3:217-220 (explains a dropped block of Andria's code); keep the question references |
| 71 | Dead `alb_prod = paths$tag` | TO DO | 4:14; 5:15; 6:14; 7a:14; 8:14; 9:129 (+ comment 9:126) | not script 7, which uses it 17 times |
| 72 | False headers in 7, 7a, 8 | TO DO | as row 34 | part of row 34 |
| 73 | `source('R/model_registry.R')` in 4 and 5; drop extra `source('R/calendar.R')` | TO DO | add near 4:20-21 and 5:25; calendar at 2:24, 4:30, 5:43, 6:33, 7:37, 7a:34, 8:26, 9:108 | calendar already comes through run_manifest.R -> paths.R:29 and config_schema.R:16; 9 sources it before run_manifest |
| 74 | Comment-only or dead-code commits, each checked by parsing or a quick run | TO DO (method) | - | followed in part 2 |

### Found during this check, not in the inventory (need Chris's yes before adding)

- 7: unused `ll_proj` (7:143), `lims` (7:345). `pbs` is read but never used (7:25, 92), and `pbs_ll` is read twice (7:23, 91).
- 7a: unused `N_times` (7a:28), `labels` (7a:90), `labels_period_full` (7a:95-99), `ll_proj` (7a:101). The first `breaks_alb` and `alb_bin` on the predictions are never used (7a:37, 43). `pbs_ll`/`pbs` are read but not used (7a:87-88).
- 8: unused ice colours (8:40-44) and `pbs_ll`/`pbs` reads (8:46-47). The ggplot inside the loop is never printed (8:83-89), so `rk_hadgem_df` and the unused `rk_cam5_df` (8:98-99) and `rk_cack_df` (8:116-117) are dead. 8:173 is leftover arithmetic.
- 6:64 has a trailing commented-out vector.
- questions_for_andria_general.md section 7 ("model 8 fitted three times, lines 98, 154, 234") is out of date: since I16b the third fit is a separate REML fit used for prediction.

## 2. Commits for the TO DO items, in order, with the cheapest adequate check

Before starting: branch from the trunk only after `i16b-calibration` is merged. Decide whether I16c goes first: it edits 2:153-160 (row 4) and 1's elevation block (row 2). Every check run uses `--config configs/legacy_config.R` (seed 1, REML ladder, no ice mask, original kernels and `legacy_components`). It writes to its own run directory, with the frozen inputs seeded. The default config.R no longer reproduces the anchors.

Anchors that cover each script, from tests/anchors/live_paths.tsv and MD5SUMS.txt:

| Script | Anchor files | Not anchored (compare with a run of the old code) | Measured run time |
|---|---|---|---|
| 1 | interp-allmonths: lct_modern_reveals_interp.RDS, lct_paleo_reveals_interp.RDS | diagnostic CSV, 28 diagnostic PNGs | 1 min |
| 2 | interp-allmonths: calibration_modern_lct_interp_bluesky.RDS | coarse table (compare with frozen data/ copy), figures | 4 min |
| 3 | none | all figures + Rplots.pdf | 1 min |
| 4 | interp-allmonths: AIC_table.csv (REML, legacy) | the fits (use tools/check_calibration_month.R) | 27 h in full |
| 5 | interp-seed1: calibration_model_stats.csv | selected-model RDS (compare with the frozen fits), figures, Rplots.pdf | 10-12 min |
| 6 | interp-seed1: 12 samps + 13 summary files; interp-allmonths: paleo_interp_predict_gam_bluesky.RDS | - | 17-20 min |
| 7 | interp-tail: ice_fort.RDS, ice_fort_diff_young.RDS, ice_fort_diff_old.RDS, alb_interp_preds_diffs_bluesky.RDS | figures | about 110 min, 23 GB |
| 7a | interp-tail: ALB_diffs_bluesky.RDS | Rplots.pdf (hist pages, the two `foo2` plots) | 70-100 min |
| 8 | interp-tail: RF_holocene_all_cases.RDS | Rplots.pdf | under 1 min |
| 9 | interp-forcing: the four CSVs | seven figures | under 1 min |

Figures are compared byte for byte with a run of the old code, with only the existing PDF creation-date treatment.

### Phase A: comments and whitespace only

Check (a) for every changed file: `identical(lapply(parse(old, keep.source = FALSE), deparse), lapply(parse(new, keep.source = FALSE), deparse))`. No run needed.

1. Remove the issue tags and "removed here" notes, one commit for scripts 1-9 and one for R/ (row 70).
2. Correct the headers and other comments that describe an earlier state (rows 34, 58, 72).
3. Script 3: describe the actual colours and plots instead of the tutorial and "month 5" text (row 8).
4. Contract comments: units and kernel conventions in 8, the zero-albedo choice in 2, "one row per ..." at the saved tables, 5's `month` column (rows 45, 53, 56 comment part, 69).
5. Indent the guarded blocks in 5, 7 and 9, one commit per script (row 54). Do this after reconciliation with Andria's edits, or review it with `git diff -w`.

### Phase B: R/ helpers and tools

Check (c): run the listed tests/test_*.R files.

6. R/calendar.R names (row 35): test_calendar.R, test_config.R, test_paths.R.
7. R/config.R names (row 37): test_config.R, test_tools.R, test_run_manifest.R.
8. R/config_schema.R names, plus `bad` in R/preparation.R (row 38): test_config.R, test_preparation.R.
9. R/paths.R, three commits: `d`/`rel`, the template functions, `seed_run_dir` (rows 40-42). Tests: test_paths.R, test_tools.R, test_run_manifest.R.
10. R/map_helpers.R names (row 39). No test file covers it. Cheapest check: `identical()` of `prepare_boundaries(pbs_ll, w)` from the old and new file, for the calibration window. Otherwise run script 3 (1 min) and compare its figures.
11. Move `make_grid()` into R/preparation.R with the new local names, and point 3:62 at the new file (rows 44, 67). Check `identical()` of old and new `make_grid(cal_data, ~ x + y, ...)`, then run script 3 and compare its figures (no anchor). Update README/CONTRIBUTING if they list scripts/make_grid.R.
12. Tools report what they did (row 62): test_tools.R, plus one `check_anchors.R` call on an existing anchor. This changes printed text only.

### Phase C: code in the scripts

Check (b). Commit each logical change separately, then run each script once per batch.

- **Script 8** (under 1 min, from the frozen ALB_diffs): commit 13 renames `bar` (row 28); commit 14 removes `alb_prod` (row 71); commit 15 drops the calendar source (row 73); commit 16 moves the write ahead of the `bar` checks (row 60). Check RF_holocene_all_cases.RDS and Rplots.pdf. Remove the extra dead code only if Chris agrees.
- **Script 9** (under 1 min): commit 17 renames `d`/`d7` and passes the table, Earth area and limits as arguments (rows 29, 57); commit 18 renames `KERNELS` and the others (row 30); commit 19 renames the pair counts and their table (row 31); commit 20 renames the `read_erf`/`panel_plot` locals (row 32); commit 21 renames the plot data (row 33); commit 22 removes `alb_prod` and the calendar source. Check the four CSVs and seven figures.
- **Script 1** (1 min, with `run$diagnostic = TRUE` in a scratch copy of the legacy config): commit 23 renames the diagnostic names (row 3); commit 24 renames `ele_get_cells` (row 2). No run reaches row 2 while the cache file exists, so parse it and review it. Check both tables against the anchor, and the CSV and figures against the old run.
- **Script 3** (1 min): commit 25 removes `color_values_four` (row 7); commit 26 splits `cal_long` (row 6). No anchor covers script 3: check the figures and Rplots.pdf against the old run.
- **Script 2** (4 min, after I16c): commit 27 removes the dead lines (row 5); commit 28 renames the albedo tables (row 4); commit 29 builds `atts` by name (row 56); commit 30 drops the calendar source. Check the anchor, the coarse table and the figures.
- **Script 5** (10-12 min, seed 1, from frozen fits and prediction-fit files copied from frozen mod8, as in I16b): commit 31 is the named list of comparison fits (rows 12, 14); commit 32 splits draws from table (row 13); commit 33 renames to `calibration_stats` (row 15); commit 34 removes `cal_sim_SPE` and `alb_prod` (row 16); commit 35 splits the seed (row 61); commit 36 adds the registry source and drops the calendar source (row 73). Commit 37, saving once per month (row 59), changes the manifest, so keep it separate and get Chris's yes. Check the stats CSV against seed1, the 12 selected models against the frozen fits, and the figures and Rplots.pdf.
- **Script 6** (17-20 min, seed 1, from frozen selected fits): commit 38 renames the object family (row 17); commit 39 renames the column by name (row 18); commit 40 splits the seed (row 61); commit 41 removes `alb_prod` and the calendar source. Check all 25 seed1 files of script 6 and the point predictions.
- **Script 4**: commit 42 removes `cal_data2/3` and `alb_prod` (row 9); commit 43 adds the registry source and drops the calendar source. No cheap full run exists (27 h). Cheapest: a one-month legacy run, comparing that month's AIC with the anchor column, plus `tools/check_calibration_month.R`. Or accept parse plus a grep that the names are never read; that is Chris's call.
- **Scripts 7 and 7a**: build all commits first, then run once, 7 and 7a in parallel, then rerun 8 and 9 on their outputs.
  - Script 7: commit 44 removes overwritten and unused definitions (row 22); commit 45 handles `breaks`/`labels`/period labels (row 20); commit 46 renames `thresh`/`values` (row 21); commit 47 renames point predictions (row 19); commit 48 adds an explicit `by =` at 7:420. Commit 49, the optional reorder (row 60), comes after the renames.
  - Script 7a: commit 50 removes the dead coverage check, bins and `facets` (rows 25, 66); commit 51 renames `x`/`idx_ice`/`ice_status_veg` (row 23); commit 52 renames the grid polygons (row 24); commit 53 renames `foo2` (row 27); commit 54 renames columns by name and adds `by =` (row 56); commit 55 removes `alb_prod` and the calendar source.
  - Check the four interp-tail files of script 7 and ALB_diffs, the figures and Rplots.pdf, then RF_holocene_all_cases.RDS and the four forcing CSVs.

## 3. Risks now

1. **The anchors are the original analysis.** Checks must use configs/legacy_config.R; a run under config.R (ML, ice mask, all-sky kernels, `endpoint`) will differ by design.
2. **Base and line numbers.** i16b is not merged into feature/single-config. It changed scripts 1, 2, 4, 5 and R/preparation.R, model_registry.R, paths.R and config_schema.R, so branch after the merge. I16c (pending) edits 2:153-160 and 1's elevation block, which conflicts with rows 2 and 4.
3. **Saved values that come from names.** `models` (5:234) feeds the seed labels (`experiment_<model>`) and the `model` facet. `KERNELS` names become the `kernel` column of the saved CSV and the legend keys. `labels` feeds the saved `facets` of ice_fort_diff_*.RDS and alb_interp_preds_diffs. 7a:217 sets the class of the saved `month` column, so it must survive the dead-bin removal.
4. **Plot titles taken from expressions.** 7a:76 `hist(ice_status_veg[,2])` puts "Histogram of ice_status_veg[, 2]" into 7a's Rplots.pdf, so renaming that object changes the figure. Keep the line, or pass the old title and label explicitly. ggplot labels in 5, 7a and 8 come from `aes()`, so renaming the data objects there is safe.
5. **Argument names that look like variables.** `labels = labels` and `values = values` (7), `limits`, and `factor(labels = ...)`: a mechanical search-and-replace would break them.
6. **Lookups at call time.** `panel_plot()` reads `rng`, which is reassigned at 9:355 before the EGU plot, so pass the right value at each call. `aggregate_forcing()` reads `d` after the filter at 9:162. `facet_grid(striplab ~ .)` finds the data column, not the argument.
7. **Script 7 ordering.** `labels` (7:96 vs 7:438) and `ages_sub` (7:33 vs 7:442) change meaning mid-script. Reordering before renaming would change the legend labels.
8. **Unused reads.** Removing `read_input()` calls (7:25, 91-92, 152; 7a:87-88; 8:46-47) changes the manifest's inputs and stale-input checks. 7:152 is explicitly kept. Same for "save once" in script 5 (row 59).
9. **Re-indenting Andria's code** (5's November block, 7, 9) produces large whitespace diffs that collide with her edits at reconciliation.
10. **Removing tags.** Keep question references and the `# CM:` notes on pending figure slips; strip only issue numbers and the "removed here" notes.
11. **`reformulate()` in the registry** could change the formula environment and representation in every fitted object; recommend dropping that half of row 55.
12. **Unverified paths.** Row 2 runs only without the elevation cache (network). Script 4 has no cheap full run. Both rest on parse and review unless Chris accepts the one-month run.
13. **Running checks.** Do not edit the checkout while a check run reads it (AGENTS.md); use a worktree.
