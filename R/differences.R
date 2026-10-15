# Albedo change between consecutive time slices, for one cell and month (script 7a).
#
# Why a function: this is the arithmetic behind every forcing variant, and where the ice
# rules act, so it is tested on small hand-made cases in tests/test_differences.R. The
# lines are script 7a's, moved unchanged.
#
# Input: one cell and month, one row per configured slice in ascending age (years BP), as
# script 7a builds it: ice_frac (Dalton ice fraction), alb_veg (vegetation albedo),
# ice_albedo_fixed and ice_albedo_sc (glacier albedo, fixed and scaled), alb_ice_thresh and
# alb_icesc_thresh (glacier albedo where the slice counts as ice by its unadjusted fraction,
# otherwise NA; read by "legacy_components" only).
#
# Output: one row per consecutive pair of slices, younger slice first: the ice fractions at
# both ends and the ten albedo differences, each younger minus older. Two families:
#   *_thresh  a slice is all ice or all vegetation, by ice_threshold;
#   *_part(s) ice and vegetation albedo mixed by area fraction.
#
# Rules carried over from the original, which questions C2 and C0 put to Andria:
#   - an ice fraction that increases from an older to a younger slice (a readvance) is
#     replaced by the mean of its neighbouring slices, once. A readvance in the youngest
#     pair has no younger neighbour: with two slices this stops with an error; with more it
#     silently averages the wrong values (see tests/test_differences.R);
#   - where a pair is ice at both ends, the ice differences are 0 ("still ice"; in
#     "endpoint" this follows because the glacier albedo depends on the month only);
#   - where an ice fraction is missing, the ice and combined differences are NA.
#
# difference_method (config.R analysis$ice$difference_method) chooses how the
# threshold variants are computed. "legacy_components" is the original arithmetic: a
# vegetation term, NA where either end is ice by the ADJUSTED fraction, plus an ice term
# that subtracts alb_ice_thresh, chosen by the UNADJUSTED fraction, summed with na.rm. Where
# the two classifications agree it is right; where the readvance adjustment moves the older
# slice across the threshold it double-counts (both terms numeric, 6 cell-pairs in the data)
# or reports zero or the wrong sign; and an ice advance across the threshold (ice in the
# younger slice, vegetation in the older) comes out as zero (3 cell-pairs). "endpoint"
# classifies both ends from the adjusted fraction, takes each slice's albedo, ice or
# vegetation, and subtracts (endpoint_albedo_differences() below); it never reads
# alb_ice_thresh. The area-mixed *_part(s) variants are the same in both.
pair_albedo_differences = function(alb_cell_filled, ice_threshold, difference_method = 'legacy_components') {

  # Guard here too, so a misspelt method can never fall through to the original arithmetic.
  if (!difference_method %in% c('legacy_components', 'endpoint')) {
    stop('difference_method must be "legacy_components" or "endpoint", not "', difference_method, '"')
  }

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
