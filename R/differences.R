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
pair_albedo_differences = function(alb_cell_filled, ice_threshold) {

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

  alb_diff_veg_ice_parts = rowSums(cbind(alb_diff_veg_part, alb_diff_ice_part))
  alb_diff_veg_icesc_parts =  rowSums(cbind(alb_diff_veg_part, alb_diff_icesc_part))

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
