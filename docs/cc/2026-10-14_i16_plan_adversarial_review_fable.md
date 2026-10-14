# Adversarial review of the I16 plan (method options), 2026-10-14

**What this is.** Before implementing I16 (#19), Chris asked for an adversarial review of
the implementation plan under his priorities (readability for Andria, plain R, settings in
`config.R`, nothing verified loosely, no overstated decisions). Verdict: the decisions and
the rule "original lines inline, new method as a tested function, one if/else per key" are
right; cut the sky/level kernel keys (use the variable names), the switches for B1 and C5
and exact pixel-area geometry; give B4's REML prediction fit its own file; pilot A3;
pre-process MOD44W once into a small committed raster; three branches (tail, calibration,
land support). Chris's decisions on it are in the change log; he asked Astra to check it.

**Who wrote it.** A Claude Code subagent (Fable), maximum effort, read-only, on branch
i11-pair-differences, reading #19 with comments, #12, #16, #17, #18, the 2026-10-14 reviews,
the change log, the regression contract, the questions file, `config.R`, `R/`, scripts 1-9
and the tests; the kernel files and the albedo GeoTIFF were inspected without running R.

---

## 1. Verdict

**Right.** The landing sites in #19 are right. C2 belongs inside `pair_albedo_differences()`.
`legacy_config.R` as a complete literal `list()` loaded by `load_config()` is right.

**Over-engineered (cut):**
- Four kernel keys plus combination logic (`cam5_level`, `cam5_sky`, `hadgem_sky`, `cack_mode`).
  Three keys naming the variable read (`hadgem_variable`, `cam5_variable`, `cack_variable`) do
  the same with no branch for HadGEM3 and CAM5.
- B1 as a config switch: a check that passes changes no byte. One call to a tested
  `check_fractions()` after script 1's reduction, no key.
- C5 as a config switch: the headline plot already plots `global_equiv`; a comment in
  `config.R` records the decision.
- Physical intersection areas for B5/B9: within a 1-degree cell cos(lat) varies under 1%;
  land-fraction weights through the same `resample()` suffice.
- A closure check on the 200 input draws: beyond the decision (reduced table only).

**Under-specified:**
- B4's hand-over: if the REML fit of `model_id` were saved over the ladder file (as the third
  fit is today), the ladder would mix eleven ML fits and one REML fit. It needs its own
  catalogue entry.
- A3's storage: at 1,000 draws the samples file is about 70 M rows per month (about 5 GB in
  memory before `melt()`'s copies), about 20 GB on disk for twelve months, read by nothing.
- B5 with the actual raster offset: the albedo GeoTIFF's pixel centres sit on the quarter
  degrees (origin -175.125, 80.125), so a 1-degree cell is not 4 x 4 pixels; numerator and
  denominator through the same `resample()` cancel the edge treatment exactly. MOD44W at 250 m
  over the continent is gigapixels: a one-off preprocessing to a small committed raster on the
  albedo grid, not a pipeline input.

**Risks to the anchors:** the legacy branch of C2 must keep `rowSums(..., na.rm = TRUE)` and
the `rbind` loop exactly; B4 moves the file script 5 reads, so frozen-input verifications must
seed the new file name with the frozen ladder mod8; B5/B8/B9 change the calibration table and
can only be verified as new-method runs.

## 2. Key by key

**C3a/C4 (script 8):**
```r
kernels = list(hadgem_variable = "albedo_sw",   # C4; legacy "albedo_sw_cs" (clear-sky). Both TOA.
               cam5_variable   = "FSNT",        # C3a, C4; legacy "FSNSC" (clear-sky, surface).
               cack_variable   = "CACK CM",     # C3b; legacy "CACK" (one year, cack_year)
               cack_year       = 2003)          # used only with cack_variable = "CACK"
```
A small table in `R/` (kernel, variable, sky, level, label) serves the schema's legal lists and
script 9's labels; script 9's "factor of two" title must go (false under all-sky).
Clear-sky as a sensitivity is a second config file and a rerun of 8 and 9.

**C3b:** one `if/else` in script 8: legacy `varname = 'CACK', level = month, band = year - 2000`;
new `varname = 'CACK CM', band = month` (3-D: Month is the third dimension; no `level`). Keep
the existing flip/transpose and coordinates; verify with a direct `ncvar_get` lookup at two
cells.

**C5:** no key; a comment in `config.R`.

**C2 (`R/differences.R`):** one argument `difference_method` (`"legacy_components"` |
`"endpoint"`), covering only the `*_thresh` family. The `*_part(s)` family needs no fix: the
sum of its parts is already the difference of the mixed endpoint albedos. "Still ice gives 0"
falls out of the endpoint arithmetic because the glacier albedo is per month:
```r
alb_young = ifelse(is_ice_young, alb_cell_filled$ice_albedo_fixed[-n], alb_cell_filled$alb_veg[-n])
alb_old   = ifelse(is_ice_old,   alb_cell_filled$ice_albedo_fixed[-1], alb_cell_filled$alb_veg[-1])
alb_diff_veg_ice_thresh = alb_young - alb_old
alb_diff_veg_thresh = ifelse(!is_ice_young & !is_ice_old, alb_diff_veg_ice_thresh, NA)
alb_diff_ice_thresh = ifelse( is_ice_young |  is_ice_old, alb_diff_veg_ice_thresh, NA)
```
plus the scaled twin. The readvance adjustment stays above the branch, unchanged in both; the
first-pair index-0 error stays in both. Add sign-reversal and telescoping tests.

**A3:** value only; pilot one month first (time, peak memory, file size); decide whether to keep
writing the samples files at 1,000.

**B4:** `fit_calibration_model(..., method = "REML")`; script 4's ladder fitted with the
comparison method; the AIC section unchanged; the spatial experiments stay REML; the third loop
becomes the REML fit of the configured `model_id` saved under its own catalogue entry, which
script 5 reads. Under legacy this is today's behaviour. Pilot `bam(method = "ML")` with `betar`
on mod1 first. `tools/check_calibration_month.R` checks the legacy config only.

**B8 (script 2):** read the 1 ka polygons, filter the modern table by a tested
`cells_in_polygons()`, record the excluded cells with `run_record()`; paleo rows untouched.
Expect 48, do not hard-code it.

**B1:** `check_fractions()` in `R/preparation.R`, called once after script 1's reduction;
tolerance a function default, not a key.

**B5:** the land-weighted mean with the same shape as the existing coarse mean:
`resample(albedo * land, grid, "average") / resample(land * observed, grid, "average")`, with
coverage recorded as a diagnostic; a cell-month with no observed land is missing (no
threshold, as decided open). The mask comes from a one-off `tools/` script that aggregates
MOD44W to a land-fraction raster on the albedo grid (committed, with a provenance README).

**B9:** a committed per-cell table of land-mean elevation made once by a `tools/` script from
the DEM and the mask; script 1 chooses which cache file to read.

**`configs/legacy_config.R`:** a complete literal list with every key at its legacy value;
`run$dir` its own directory; the seed (Chris: `seed = 1`, one file). A test that it loads.

## 3. Split and order

One issue, three branches merged and verified in turn: (1) tail: kernels, C2, A3, C5 comment,
`legacy_config.R`; (2) calibration: B4, B8, B1; (3) land support: B5, B9 and the preprocessing,
when MOD44W is on disk. What waits for Andria (coverage threshold, readvance policy, land-area
basis, B2a) blocks none of them.

## 4. Overstated or silently closed

- `draws$evaluation` "remains 100": not decided (Chris later decided 100 for both for now).
- A fixed seed for the final run: a recommendation, not a decision.
- A closure check on the input draws: beyond the decision.
- C2's attribution of transitions to the ice component follows the existing convention; it is
  the implementation, not a new choice.
- C3(d) kernel alignment stays open.
- B8's "48 ice-cap cells" are 48 cell centres inside 1 ka polygons.

## 5. Decisions for Chris

Kernel keys as variable names; no keys for B1 and C5; the legacy seed; `draws$evaluation`;
whether to keep writing 1,000-draw samples files; the prediction-fit file name; the MOD44W/DEM
specification if Andria does not answer; the final run's seed.
