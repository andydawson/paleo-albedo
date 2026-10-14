# I16a review: the endpoint method also removes a double count (Astra)

**What this is.** Verifying I16a (2026-10-14) showed the new "endpoint" ice method changed
9 pairs of slices, not only the 3 ice advances it was meant to fix. Chris asked for Fable
and Astra to review before merging. This file holds Astra's review and, below it, the brief
both reviewers received. Fable's review is in
`2026-10-14_i16a_double_count_review_fable.md`. Outcome: both agree the diagnosis is right,
the double count should go, and no separate switch is needed; comments and tests changed
accordingly (commit "I16a: describe the double count; guard the method name; tests for both").

**Who wrote it.** OpenAI Codex CLI, model `gpt-6-astra`, reasoning effort `max`, static
review (no shell), 2026-10-14.

---

**The diagnosis is right. Recommend (c): keep the two methods, but add regression tests, audit classification disagreements, and correct the documentation before merging.**

1. **The six extra changes follow directly from the code.** For adjusted vegetation–vegetation pairs, legacy retains `V_y − V_o`. If the *raw* older fraction exceeded the threshold, it also retains `V_y − G_o`. Thus legacy reports:

   `total = (V_y − V_o) + (V_y − G_o)`

   Endpoint correctly reports only `V_y − V_o`. The precomputed raw mask causes the inconsistency.

2. **Removing that double count is correct under the retained readvance policy.** Both endpoint classifications should use the adjusted fractions. Using raw fractions consistently would also define coherent endpoints, but would change whether the readvance adjustment affects threshold forcing. That is a separate scientific choice for C2; the code cannot settle it.

   Script 7a’s earlier mask is not a reason to override the adjusted classification.

3. **The existing method switch is sufficient.** Define `"endpoint"` as “subtract complete slice albedos using adjusted ice states,” and `"legacy_components"` as “reproduce the original arithmetic, including its inconsistencies.”

   A separate double-count switch would introduce another hybrid method that is harder to explain. If Andria later chooses whether to apply readvance smoothing, expose that policy separately, retaining the existing adjustment as its legacy setting.

4. **Yes, output agreement can hide classification disagreements.** A useful regression fixture is:

   ```text
   raw fractions:       0.10, 0.80, 0.20
   adjusted fractions:  0.10, 0.15, 0.20
   vegetation albedos:   0.20, 0.25, 0.30
   glacier albedo:       0.68
   legacy totals:      −0.53, −0.05
   endpoint totals:    −0.05, −0.05
   ```

   Both pairs touch the reclassified middle slice; only the first pair’s total changes. Legacy never consults the raw **younger** threshold mask directly.

   Other hidden cases include adjusted ice–ice pairs forced to zero by legacy, coincidentally zero extra components, and pairs where both methods return NA. Smoothing can also promote a raw vegetation slice to ice or make a finite fraction NA through a missing neighbour.

   Audit raw versus adjusted classifications at **both ends**, independently of output differences. Compare all five threshold columns, including NA-to-value transitions; a numerical `!=` comparison alone misses those.

   Update the function header, config comment, script 7a mask comment, and test introduction to describe adjusted classification, raw legacy masks, and both corrected faults. Add the fixture above, a promotion case, and a telescoping test containing a readvance.

5. **Other findings:**

   - **Missing selected albedo is another behavior change.** With known vegetation states but a missing vegetation albedo, legacy can turn two NA components into zero; endpoint returns NA. That is sensible, but needs documentation and tests. Also test that missing *unused* albedo does not contaminate a valid endpoint.
   - **“Still ice means zero” is conditional.** Endpoint returns `G_y − G_o`, possibly nonzero or NA. Script 7a’s constant glacier albedo for each month normally makes it zero. Correct the blanket docstring and “forcing 0” log. The claim that area mixtures equal mixed endpoint differences likewise assumes glacier albedo is constant across ages.
   - **Unknown method strings silently select legacy.** Reject them here unless upstream validation already guarantees the allowed values.
   - **The first-pair fault is described too broadly.** With fractions `c(.9, .8, .1)`, vector recycling produces adjusted fractions `c(.85, .5, .1)` instead of stopping. Cases 8/14 cover only an isolated first-pair readvance. Record this pre-existing C2 defect separately; the missing boundary neighbour is the **younger** neighbour.

The endpoint helper itself implements the stated subtraction correctly.

*Static review only: sandbox startup failed, so I could not run R or independently verify the reported dataset counts.*
---

# The brief

# Review request: an unexpected effect of the I16a "endpoint" ice-advance fix

Chris McWilliams (contractor; priority: readability and understandability for the PI, Andria
Dawson, over elegance; method changes are config switches with the original kept as a
"legacy" setting) asks for an adversarial review before merging. Be concrete and terse.

## Background (agreed earlier, by consensus of the two reviewers and Chris)
Question C2: the original arithmetic reported an ice advance across the 50% threshold
(younger slice ice, older slice vegetation) as zero albedo change. The agreed fix is a config
switch, `analysis$ice$difference_method`: "legacy_components" (original) or "endpoint" (new
default): take each slice's complete albedo (glacier albedo if the slice counts as ice,
otherwise vegetation albedo) and subtract, younger minus older. The plan expected the fix to
change the 3 advance pairs (36 cell-months).

## What verification found (full run of script 7a, 2026-10-14)
The endpoint method changed 9 pairs of slices (108 cell-months), July values:

    cell  year   ice young/old (adjusted) | legacy veg / ice / total      | endpoint total
    3077  8000   0.220 0.350 | -0.0237 -0.5825 -0.6062 | -0.0237
    3395  1500   0.543 0.410 |  NA      NA      0.0000 |  0.4205   (advance)
    3396  1000   0.524 0.483 |  NA      NA      0.0000 |  0.4123   (advance)
    3693 10000   0.459 0.419 | -0.0084 -0.5307 -0.5390 | -0.0084
    3695  5000   0.588 0.494 |  NA      NA      0.0000 |  0.4999   (advance)
    3966  7000   0.228 0.369 |  0.0051 -0.5945 -0.5894 |  0.0051
    7551 10500   0.110 0.330 | -0.0006 -0.5998 -0.6004 | -0.0006
    7554 10000   0.337 0.419 | -0.0107 -0.6067 -0.6174 | -0.0107
    9086 10500   0.247 0.458 | -0.0190 -0.5801 -0.5990 | -0.0190

Cause (my diagnosis; check it): `pair_albedo_differences()` first applies the readvance
adjustment (an ice fraction that rises from older to younger is replaced, once, by the mean
of its neighbours) and classifies ice/vegetation from the ADJUSTED fractions. But script 7a
builds `alb_ice_thresh` (glacier albedo where the slice is ice, else NA) BEFORE, from the
UNADJUSTED fraction (lines quoted below). In the 6 non-advance pairs the adjusted older-slice
fraction is below 0.5 while the unadjusted one was above, so the legacy arithmetic kept the
vegetation difference (both adjusted ends vegetation) AND added an ice difference
(veg_young - glacier_old, since alb_ice_thresh_old was not NA), then summed them with
rowSums(na.rm=TRUE): a double count of about -0.6 in July albedo. The endpoint method uses
the adjusted fractions throughout and keeps only the vegetation change.

Forcing effect (global-equivalent, mW/m2, HadGEM3 clear-sky kernel, which isolates the
method): legacy vs endpoint by period: 0.05-0.5 ka -44.08/-44.08; 0.5-2 -47.36/-47.82;
2-4 62.62/62.62; 4-6 -65.54/-65.89; 6-8 108.98/108.59; 8-10 302.80/302.45; 10-12 270.17/265.51.
The legacy config reproduces the original outputs byte for byte. The extra effect is now
recorded in question C2 for Andria.

## Questions for you
1. Is the diagnosis right? Check against the code below.
2. Is removing the double count correct, or should the endpoint method instead classify each
   slice from the unadjusted fraction (as script 7a's alb_ice_thresh does), or something else?
   Note the readvance adjustment itself is Andria's open question C2.
3. Chris's options: (a) merge as is; (b) make the double-count removal a separate config
   setting from the advance fix; (c) other. Recommend one, bearing in mind readability for the
   PI and that every method change is a config switch with the original as the legacy value.
4. Are there other cell-pairs where the two classifications disagree that the comparison
   would not reveal (e.g. legacy and endpoint agree by accident, or NA patterns)? Which
   docstrings, comments or tests should change to state this behaviour?
5. Anything else wrong in the endpoint implementation or its tests.

## Code
### R/differences.R
```r
# Albedo change between consecutive time slices, for one cell and month (script 7a).
#
# Why a function: this is the arithmetic behind every forcing variant, and where the ice
# rules act, so it is tested on small hand-made cases in tests/test_differences.R. The
# lines are script 7a's, moved unchanged (issue I11, 2026-10-14).
#
# Input: one cell and month, one row per configured slice in ascending age (years BP), as
# script 7a builds it: ice_frac (Dalton ice fraction), alb_veg (vegetation albedo),
# ice_albedo_fixed and ice_albedo_sc (glacier albedo, fixed and scaled), alb_ice_thresh and
# alb_icesc_thresh (glacier albedo where the slice counts as ice, otherwise NA).
#
# Output: one row per consecutive pair of slices, younger slice first: the ice fractions at
# both ends and the ten albedo differences, each younger minus older. Two families:
#   *_thresh  a slice is all ice or all vegetation, by ice_threshold;
#   *_part(s) ice and vegetation albedo mixed by area fraction.
#
# Rules carried over from the original, which questions C2 and C0 put to Andria:
#   - an ice fraction that increases from an older to a younger slice (a readvance) is
#     replaced by the mean of its neighbouring slices, once;
#   - where a pair is ice at both ends, the ice differences are 0 ("still ice");
#   - where an ice fraction is missing, the ice and combined differences are NA.
#
# difference_method (config.R analysis$ice$difference_method, issue I16) chooses how the
# threshold variants are computed. "legacy_components" is the original arithmetic, in which
# an ice advance across the threshold (ice in the younger slice, vegetation in the older)
# comes out as zero change. "endpoint" takes each slice's albedo, ice or vegetation, and
# subtracts (endpoint_albedo_differences() below), so advances count. The area-mixed
# *_part(s) variants already equal the difference of the mixed endpoint albedos and are the
# same in both.
pair_albedo_differences = function(alb_cell_filled, ice_threshold, difference_method = 'legacy_components') {

  idx_ice_increase = which(diff(alb_cell_filled$ice_frac)<0)
  if(length(idx_ice_increase)>1){
    print('Ice increase over multiple time periods')
  }

  alb_cell_filled$ice_frac_adj = alb_cell_filled$ice_frac
  alb_cell_filled$ice_frac_adj[idx_ice_increase] = (alb_cell_filled$ice_frac_adj[idx_ice_increase + 1] +  alb_cell_filled$ice_frac_adj[idx_ice_increase - 1])/2

  ice_frac_young = alb_cell_filled$ice_frac_adj[1:(nrow(alb_cell_filled)-1)]
  ice_frac_old = alb_cell_filled$ice_frac_adj[2:nrow(alb_cell_filled)]

  is_ice_young =  ice_frac_young > ice_threshold
  is_ice_old =  ice_frac_old > ice_threshold
  is_ice_both = is_ice_young & is_ice_old

  is_ice_increase = ice_frac_old < ice_frac_young

  is_ice_missing = is.na(ice_frac_young) | is.na(ice_frac_old)

  if (sum(is_ice_both[!is_ice_missing], na.rm=TRUE)>0){
    print(paste0('Still ice, forcing 0: ', sum(is_ice_both[!is_ice_missing], na.rm=TRUE)))
  }

  if (any(is_ice_increase[!is_ice_missing])) {
    print(paste0('Increasing ice fraction: ', sum(is_ice_increase[!is_ice_missing], na.rm=TRUE)))
  }

  alb_diff_veg_part = -diff(alb_cell_filled$alb_veg) * (1-ice_frac_old)

  alb_diff_ice_part = (alb_cell_filled$alb_veg[1:(nrow(alb_cell_filled)-1)] - alb_cell_filled$ice_albedo_fixed[2:nrow(alb_cell_filled)]) * (ice_frac_old-ice_frac_young)
  alb_diff_icesc_part = (alb_cell_filled$alb_veg[1:(nrow(alb_cell_filled)-1)] - alb_cell_filled$ice_albedo_sc[2:nrow(alb_cell_filled)]) * (ice_frac_old-ice_frac_young)

  alb_diff_veg_ice_parts = rowSums(cbind(alb_diff_veg_part, alb_diff_ice_part))
  alb_diff_veg_icesc_parts =  rowSums(cbind(alb_diff_veg_part, alb_diff_icesc_part))

  if (difference_method == 'endpoint') {
    n = nrow(alb_cell_filled)
    fixed  = endpoint_albedo_differences(is_ice_young, is_ice_old, alb_cell_filled$alb_veg[-n], alb_cell_filled$alb_veg[-1],
                                         alb_cell_filled$ice_albedo_fixed[-n], alb_cell_filled$ice_albedo_fixed[-1])
    scaled = endpoint_albedo_differences(is_ice_young, is_ice_old, alb_cell_filled$alb_veg[-n], alb_cell_filled$alb_veg[-1],
                                         alb_cell_filled$ice_albedo_sc[-n], alb_cell_filled$ice_albedo_sc[-1])
    alb_diff_veg_thresh       = fixed$vegetation
    alb_diff_ice_thresh       = fixed$ice
    alb_diff_veg_ice_thresh   = fixed$total
    alb_diff_icesc_thresh     = scaled$ice
    alb_diff_veg_icesc_thresh = scaled$total
  } else {
    alb_diff_veg_thresh = -diff(alb_cell_filled$alb_veg)
    alb_diff_veg_thresh[is_ice_old | is_ice_young] = NA

    alb_diff_ice_thresh = alb_cell_filled$alb_veg[1:(nrow(alb_cell_filled)-1)] -
      alb_cell_filled$alb_ice_thresh[2:nrow(alb_cell_filled)]

    alb_diff_icesc_thresh =  alb_cell_filled$alb_veg[1:(nrow(alb_cell_filled)-1)] -
      alb_cell_filled$alb_icesc_thresh[2:nrow(alb_cell_filled)]

    alb_diff_ice_thresh[is_ice_both] = 0
    alb_diff_icesc_thresh[is_ice_both] = 0
    alb_diff_ice_thresh[is_ice_missing] = NA
    alb_diff_icesc_thresh[is_ice_missing] = NA

    alb_diff_veg_ice_thresh = rowSums(cbind(alb_diff_veg_thresh, alb_diff_ice_thresh), na.rm=TRUE)
    alb_diff_veg_icesc_thresh =  rowSums(cbind(alb_diff_veg_thresh, alb_diff_icesc_thresh), na.rm=TRUE)

    alb_diff_veg_ice_thresh[is_ice_missing] = NA
    alb_diff_veg_icesc_thresh[is_ice_missing] = NA
  }

  data.frame(ice_frac_young = ice_frac_young,
             ice_frac_old = ice_frac_old,
             alb_diff_veg_thresh = alb_diff_veg_thresh,
             alb_diff_ice_thresh = alb_diff_ice_thresh,
             alb_diff_veg_ice_thresh = alb_diff_veg_ice_thresh,
             alb_diff_icesc_thresh = alb_diff_icesc_thresh,
             alb_diff_veg_icesc_thresh = alb_diff_veg_icesc_thresh,
             alb_diff_veg_part = alb_diff_veg_part,
             alb_diff_ice_part = alb_diff_ice_part,
             alb_diff_veg_ice_parts = alb_diff_veg_ice_parts,
             alb_diff_icesc_part = alb_diff_icesc_part,
             alb_diff_veg_icesc_parts = alb_diff_veg_icesc_parts)
}

# Threshold albedo change for consecutive pairs from each slice's complete albedo state: a
# slice that counts as ice has the glacier albedo, otherwise its vegetation albedo; the
# change is younger minus older. Returns the total, and the same value split by kind: a
# pair with vegetation at both ends is a vegetation change, a pair with ice at either end an
# ice change (the other entry NA). An unknown ice state (missing fraction) gives NA.
endpoint_albedo_differences = function(is_ice_young, is_ice_old, alb_veg_young, alb_veg_old, ice_albedo_young, ice_albedo_old) {
  albedo_young = ifelse(is_ice_young, ice_albedo_young, alb_veg_young)
  albedo_old   = ifelse(is_ice_old,   ice_albedo_old,   alb_veg_old)
  total = albedo_young - albedo_old
  list(total      = total,
       vegetation = ifelse(!is_ice_young & !is_ice_old, total, NA),
       ice        = ifelse( is_ice_young |  is_ice_old, total, NA))
}
```
### scripts/7a_alb_diff_full.R lines 120-170 and 215-266
```r

spdf_2 <- as(grid_NA,'SpatialPolygonsDataFrame')

sf_data = st_as_sf(spdf_2)
grid_df$area <- st_area(sf_data) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')

alb_grid = merge(alb_grid, grid_df, by = c('cell_id', 'lat', 'long'))

alb_grid_full = alb_grid

alb_glacier = read_input(run, path_file(paths, 'glacier_albedo'), read.csv, header=TRUE)

# The glacier-albedo columns for the fixed and scaled ice variants: config.R
# analysis$ice$albedo_fixed and albedo_scaled (question C0a; issue I04b).
alb_grid_full$ice_albedo_fixed = alb_glacier[match(alb_grid_full$month, alb_glacier$month), cfg$analysis$ice$albedo_fixed]
alb_grid_full$ice_albedo_sc = alb_glacier[match(alb_grid_full$month, alb_glacier$month), cfg$analysis$ice$albedo_scaled]

alb_grid_full$alb_veg = alb_grid_full$alb_mean

alb_grid_full$alb_ice_part = alb_grid_full$ice_albedo_fixed * alb_grid_full$ice_frac
alb_grid_full$alb_icesc_part = alb_grid_full$ice_albedo_sc * alb_grid_full$ice_frac

alb_grid_full$alb_veg_part = alb_grid_full$alb_veg * (1 - alb_grid_full$ice_frac)

alb_grid_full$alb_veg_ice_parts = alb_grid_full$alb_ice_part + alb_grid_full$alb_veg_part
alb_grid_full$alb_veg_icesc_parts = alb_grid_full$alb_icesc_part + alb_grid_full$alb_veg_part

# A cell is ice when its ice fraction exceeds config.R analysis$ice$threshold (questions
# C0, C2; issue I04b).
ice_threshold = cfg$analysis$ice$threshold
source('R/differences.R')   # pair_albedo_differences(), the per-pair arithmetic (issue I11)
idx_ice = which(alb_grid_full$ice_frac > ice_threshold)
idx_veg = which(alb_grid_full$ice_frac <= ice_threshold)

alb_grid_full$alb_veg_thresh = alb_grid_full$alb_veg
alb_grid_full$alb_veg_thresh[idx_ice] = NA

alb_grid_full$alb_ice_thresh = alb_grid_full$ice_albedo_fixed
alb_grid_full$alb_ice_thresh[idx_veg] = NA

alb_grid_full$alb_icesc_thresh = alb_grid_full$ice_albedo_sc
alb_grid_full$alb_icesc_thresh[idx_veg] = NA

alb_grid_full$alb_veg_ice_thresh = rowSums(cbind(alb_grid_full$alb_veg_thresh, alb_grid_full$alb_ice_thresh), na.rm=TRUE)
alb_grid_full$alb_veg_icesc_thresh = rowSums(cbind(alb_grid_full$alb_veg_thresh, alb_grid_full$alb_icesc_thresh), na.rm=TRUE)

alb_grid_full_cells = alb_grid_full %>%
  group_by(year) %>%
  dplyr::summarize(N_all = n_distinct(cell_id))
# ...
years_df = data.frame(year = ages)

alb_diff_colnames = c('cell_id', 'lat', 'long', 'x', 'y', 'year', 'month', 'area', 'area_km2')

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=length(alb_diff_colnames)+12))

colnames(alb_diff_df) = c(alb_diff_colnames,
                          'ice_frac_young',
                          'ice_frac_old',
                          'alb_diff_veg_thresh',
                          'alb_diff_ice_thresh',
                          'alb_diff_veg_ice_thresh',
                          'alb_diff_icesc_thresh',
                          'alb_diff_veg_icesc_thresh',
                          'alb_diff_veg_part',
                          'alb_diff_ice_part',
                          'alb_diff_veg_ice_parts',
                          'alb_diff_icesc_part',
                          'alb_diff_veg_icesc_parts')

cell_ids = unique(alb_grid_full$cell_id)
N_cells  = length(cell_ids)

for (i in 1:N_cells){

  print(paste0('Cell ', i, ' of ', N_cells))

  for (month in months){

    alb_cell = alb_grid_full[which((alb_grid_full$cell_id == cell_ids[i])&(alb_grid_full$month == month)),]
    alb_cell = alb_cell[order(alb_cell$year),]

    if (nrow(alb_cell) == 1){
      next
    }

    alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)

    # The albedo differences of each consecutive pair of slices (R/differences.R, issue I11).
    alb_diff_df = rbind(alb_diff_df,
                        data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ],
                                          select=alb_diff_colnames),
                                   pair_albedo_differences(alb_cell_filled, ice_threshold,
                                                           cfg$analysis$ice$difference_method)))

  }
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]

write_output(run, alb_diff_df, path_file(paths, 'albedo_differences'))

```
### tests/test_differences.R
```r
# Tests for R/differences.R (issue I11). Run from the repository root: Rscript tests/test_differences.R
#
# Small hand-made cells, one month each, slices in ascending age (row 1 is the youngest).
# Cases 1-8 document the original behaviour ("legacy_components"), inherited from script
# 7a, including two known faults (question C2): an ice advance across the threshold comes
# out as zero change, and a readvance in the first pair stops with an error. Cases 9 on
# test the "endpoint" method of issue I16, which counts advances; the first-pair error
# stays in both, because the readvance rule itself is Andria's call (C2).
source("R/differences.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12))

# A cell as script 7a builds it: glacier albedo 0.68 (fixed) / 0.73 (scaled); the *_thresh
# glacier albedo is NA where the slice counts as vegetation (ice fraction <= threshold).
cell <- function(ice_frac, alb_veg, threshold = 0.5) {
  is_veg <- !is.na(ice_frac) & ice_frac <= threshold
  data.frame(ice_frac = ice_frac, alb_veg = alb_veg, ice_albedo_fixed = 0.68, ice_albedo_sc = 0.73,
             alb_ice_thresh = ifelse(is_veg, NA, 0.68), alb_icesc_thresh = ifelse(is_veg, NA, 0.73))
}
diffs <- function(..., method = "legacy_components") { invisible(capture.output(r <- pair_albedo_differences(cell(...), 0.5, method))); r }

# 1. Vegetation only: albedo 0.20 now, 0.25 before; younger minus older = -0.05.
r <- diffs(ice_frac = c(0, 0), alb_veg = c(0.20, 0.25))
stopifnot(nrow(r) == 1,
          same(r$alb_diff_veg_thresh, -0.05), is.na(r$alb_diff_ice_thresh), same(r$alb_diff_veg_ice_thresh, -0.05),
          same(r$alb_diff_veg_part, -0.05), same(r$alb_diff_ice_part, 0), same(r$alb_diff_veg_ice_parts, -0.05))

# 2. Ice retreat: the older slice is 90% ice, the younger 10%.
r <- diffs(ice_frac = c(0.1, 0.9), alb_veg = c(0.20, 0.30))
stopifnot(is.na(r$alb_diff_veg_thresh),                          # an ice end: no vegetation difference
          same(r$alb_diff_ice_thresh, 0.20 - 0.68),               # vegetation now minus ice before
          same(r$alb_diff_veg_ice_thresh, 0.20 - 0.68),
          same(r$alb_diff_icesc_thresh, 0.20 - 0.73),
          same(r$alb_diff_veg_part, (0.20 - 0.30) * (1 - 0.9)),
          same(r$alb_diff_ice_part, (0.20 - 0.68) * (0.9 - 0.1)),
          same(r$alb_diff_veg_ice_parts, (0.20 - 0.30) * 0.1 + (0.20 - 0.68) * 0.8))

# 3. Ice at both ends ("still ice"): no change attributed to ice.
r <- diffs(ice_frac = c(0.9, 0.95), alb_veg = c(0.20, 0.30))
stopifnot(same(r$alb_diff_ice_thresh, 0), is.na(r$alb_diff_veg_thresh), same(r$alb_diff_veg_ice_thresh, 0))

# 4. Exactly at the threshold counts as vegetation (ice means > threshold).
r <- diffs(ice_frac = c(0.5, 0.5), alb_veg = c(0.20, 0.25))
stopifnot(same(r$alb_diff_veg_thresh, -0.05), same(r$alb_diff_veg_ice_thresh, -0.05))

# 5. A missing ice fraction: the ice and combined differences are unknown.
r <- diffs(ice_frac = c(0.2, NA), alb_veg = c(0.20, 0.25))
stopifnot(is.na(r$alb_diff_ice_thresh), is.na(r$alb_diff_veg_ice_thresh), is.na(r$alb_diff_veg_ice_parts))

# 6. The readvance adjustment: ice that increases towards the present is replaced, once, by
#    the mean of its neighbouring slices. Here slice 3 (0.9) follows 0.1 in slice 4 and is
#    set to (0.1 + 0.95) / 2 = 0.525, still above the threshold.
r <- diffs(ice_frac = c(0.95, 0.95, 0.9, 0.1), alb_veg = c(0.20, 0.25, 0.30, 0.35))
stopifnot(same(r$ice_frac_young, c(0.95, 0.925, 0.525)), same(r$ice_frac_old, c(0.925, 0.525, 0.1)))

# 7. KNOWN FAULT (C2): the last pair of case 6 is an ice advance across the threshold (young
#    0.525 ice, old 0.1 vegetation). Both components are NA and their na.rm sum reports a
#    confident zero change. I16 fixes this behind ice$difference_method.
stopifnot(is.na(r$alb_diff_veg_thresh[3]), is.na(r$alb_diff_ice_thresh[3]), same(r$alb_diff_veg_ice_thresh[3], 0))

# 8. KNOWN FAULT (C2): a readvance in the first pair has no older neighbour on one side and
#    stops with an error (not met in the data so far).
stopifnot(fails(diffs(ice_frac = c(0.9, 0.1), alb_veg = c(0.20, 0.25))))

# 9. "endpoint": the advance of case 7 now counts: ice (0.68) in the younger slice minus
#    vegetation (0.35) in the older one.
e <- diffs(ice_frac = c(0.95, 0.95, 0.9, 0.1), alb_veg = c(0.20, 0.25, 0.30, 0.35), method = "endpoint")
stopifnot(same(e$alb_diff_veg_ice_thresh[3], 0.68 - 0.35), same(e$alb_diff_ice_thresh[3], 0.68 - 0.35),
          is.na(e$alb_diff_veg_thresh[3]), same(e$alb_diff_veg_icesc_thresh[3], 0.73 - 0.35),
          same(e$alb_diff_veg_ice_thresh[1:2], c(0, 0)))                     # still ice: 0, as before
# ... the area-mixed variants are the same under both methods
stopifnot(identical(e[, c("alb_diff_veg_part", "alb_diff_ice_part", "alb_diff_veg_ice_parts", "alb_diff_icesc_part", "alb_diff_veg_icesc_parts")],
                    r[, c("alb_diff_veg_part", "alb_diff_ice_part", "alb_diff_veg_ice_parts", "alb_diff_icesc_part", "alb_diff_veg_icesc_parts")]))

# 10. "endpoint" agrees with the original where the original was right: retreat, still ice,
#     vegetation only.
for (case in list(list(c(0.1, 0.9), c(0.20, 0.30)), list(c(0.9, 0.95), c(0.20, 0.30)), list(c(0, 0), c(0.20, 0.25)))) {
  a <- diffs(ice_frac = case[[1]], alb_veg = case[[2]]); b <- diffs(ice_frac = case[[1]], alb_veg = case[[2]], method = "endpoint")
  stopifnot(identical(a$alb_diff_veg_ice_thresh, b$alb_diff_veg_ice_thresh), identical(a$alb_diff_veg_icesc_thresh, b$alb_diff_veg_icesc_thresh))
}

# 11. "endpoint": a missing ice fraction makes every threshold difference unknown.
e <- diffs(ice_frac = c(0.2, NA), alb_veg = c(0.20, 0.25), method = "endpoint")
stopifnot(is.na(e$alb_diff_veg_ice_thresh), is.na(e$alb_diff_veg_thresh), is.na(e$alb_diff_ice_thresh))

# 12. "endpoint": consecutive changes add up to the change between the first and last slice
#     (a monotone retreat: vegetation 0.10 now, ice before).
e <- diffs(ice_frac = c(0, 0.3, 0.6, 0.9), alb_veg = c(0.10, 0.15, 0.20, 0.25), method = "endpoint")
stopifnot(same(sum(e$alb_diff_veg_ice_thresh), 0.10 - 0.68))

# 13. Swapping the two slices reverses the sign of every change.
f <- endpoint_albedo_differences(c(TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE), c(0.2, 0.3, 0.4), c(0.25, 0.35, 0.45), 0.68, 0.68)
g <- endpoint_albedo_differences(c(FALSE, TRUE, FALSE), c(TRUE, FALSE, FALSE), c(0.25, 0.35, 0.45), c(0.2, 0.3, 0.4), 0.68, 0.68)
stopifnot(same(f$total, -g$total))

# 14. The first-pair readvance still stops, under either method (readvance policy: C2).
stopifnot(fails(diffs(ice_frac = c(0.9, 0.1), alb_veg = c(0.20, 0.25), method = "endpoint")))

cat("test_differences: all tests passed\n")
```
### config.R ice section
```r
    # cells strictly between lat_min and lat_max before computing forcing; script 7 drops
    # cells east of lon_max_coarse before its coarse differences (which script 9 uses for
    # the EGU 2024 recipe). Inherited, not chosen (questions D3, C5).
    domain = list(lon_min = -170, lat_min = 27, lat_max = 74, lon_max_coarse = -60),

    # Ice. A cell counts as ice when its ice fraction exceeds `threshold` (script 7a, the
    # *_thresh forcing variants; questions C0, C2). The ice albedo per month comes from
    # data/albedo_glacier_monthly.csv; each role names the column it uses, one of
    # "ice_albedo", "ice_albedo_fixed" or "ice_albedo_sc". Script 7 uses `ice_albedo` and
    # script 7a the other two; the file's source and the meaning of the columns are
    # question C0a.
    ice = list(
      threshold = 0.5,
      # How script 7a turns each pair of slices into an albedo change, for the threshold
      # variants: "endpoint" takes each slice's albedo (ice or vegetation) and subtracts,
      # so an ice advance is counted (decided by Chris on 2026-10-14, question C2); the
      # original "legacy_components" reported an advance across the threshold as zero.
      # The readvance smoothing is the same in both (Andria's call, C2).
      difference_method = "endpoint",
      albedo_coarse = "ice_albedo",          # script 7: the coarse ice-albedo maps
      albedo_fixed  = "ice_albedo_fixed",    # script 7a: the *_ice_* forcing variants
      albedo_scaled = "ice_albedo_sc"),      # script 7a: the *_icesc_* forcing variants

    # Radiative kernels (script 8): the variable read from each kernel file, which decides
    # clear-sky or all-sky and top of atmosphere (TOA) or surface. The legal names and what
    # each is are in R/kernels.R. Decided by Chris on 2026-10-14, to be reviewed by Andria:
    # all-sky kernels at the TOA for the headline (C3a, C4) and CACK's 2001-2016
    # climatological mean (C3b). The original analysis read HadGEM3 "albedo_sw_cs"
    # (clear, TOA), CAM5 "FSNSC" (clear, surface) and CACK "CACK" for one year
    # (configs/legacy_config.R). cack_year is used only with cack_variable = "CACK"; the
    # file's Year dimension is "2001 = 1".
```
