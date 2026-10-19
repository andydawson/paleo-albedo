# forcing_components.R: the threshold split of the forcing into a vegetation part and an
# ice part, made to add up over a period, for script 9's sensitivity table (its note A2).
#
# R/differences.R gives each slice-pair of the threshold method EITHER a vegetation
# difference (alb_diff_veg_thresh: the cell is vegetation at both ends) OR an ice difference
# (alb_diff_ice_thresh: the cell is ice at either end), and NA for the other; script 8
# multiplies each by the kernel. Script 9 sums the pairs of a period with na.rm = FALSE
# (its note A5), so a cell with both kinds of step in one period got NA in both parts and
# dropped out of both bars: at 8-10 ka, 343 cells carrying 99% of the total (found
# 2026-10-18, docs/cc/2026-10-18_forcing_code_check_codex_gpt6astra.md). Setting the part
# that does not apply to 0 keeps those cells in, and the two parts then add up to the
# total, pair by pair and so over any period: the split "by kind of step" that Chris chose
# on 2026-10-19. A pair whose total is NA (a missing ice fraction) is genuinely unknown and
# stays NA in both parts.
#
# This holds only for difference_method = "endpoint", where every known pair is exactly one
# kind of step. The original arithmetic ("legacy_components") has pairs with both parts
# (the readvance double count) and pairs with neither but a total of 0 (ice advances across
# the threshold), question C2. Chris chose (2026-10-19) to leave the legacy configuration's
# outputs exactly as they were, so under it the table is returned unchanged.
#
# Only the fixed-glacier-albedo split is completed (veg_thresh + ice_thresh =
# veg_ice_thresh). The month-by-month glacier albedo's ice part (icesc_thresh) is not
# reported, and completing it too would rewrite the shared vegetation column a second time.
complete_threshold_components = function(forcing_by_cell, kernel_id, difference_method) {
  if (difference_method != 'endpoint') return(forcing_by_cell)
  column = function(part) paste0('rf_', kernel_id, '_', part)
  veg   = forcing_by_cell[[column('veg_thresh')]]
  ice   = forcing_by_cell[[column('ice_thresh')]]
  total = forcing_by_cell[[column('veg_ice_thresh')]]
  known = !is.na(total)

  # Stop rather than guess if the components no longer mean what this assumes.
  not_one_kind = known & (is.na(veg) == is.na(ice))
  if (any(not_one_kind))
    stop(sum(not_one_kind), ' pairs with a known total have both or neither of the vegetation ',
         'and ice parts: the split by kind of step does not apply')
  veg = ifelse(known, ifelse(is.na(veg), 0, veg), NA)
  ice = ifelse(known, ifelse(is.na(ice), 0, ice), NA)
  if (any(abs(veg + ice - total) > 1e-9, na.rm = TRUE))
    stop('the vegetation and ice parts do not add up to the total')

  forcing_by_cell[[column('veg_thresh')]] = veg
  forcing_by_cell[[column('ice_thresh')]] = ice
  forcing_by_cell
}
