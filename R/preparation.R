# Preparation of the land-cover tables (script 1).
#
# Why: script 1 reduced the 200 posterior draws of land cover to their mean, with Andria's
# note "not sure if should use median" (question A1). The summary is now a setting
# (config.R, analysis$land_cover$reduction), and the reduction a function with a stated
# contract, so the median can be run without editing the script (issue I04a, 2026-10-14).
# Issue I08 adds the other preparation steps of scripts 1 to 3 here.

# One value per cell, slice and land-cover class from the posterior draws, as one row per
# cell and slice with a column per class (cell_id, x, y, ages, ET, OL, ST).
#   "mean"   the mean of the draws: the original reduction, kept exactly as it was. Each
#            draw's fractions sum to 1, so their means do too; they are not rescaled,
#            which would change the last bit of sums such as 1.0000000000000002.
#   "median" the median of the draws, then rescaled so ET + OL + ST = 1 in every cell
#            and slice (the medians of the three classes need not sum to 1).
reduce_land_cover = function(posts, method = c('mean', 'median')) {
  method = match.arg(method)
  per_class = posts %>%
    dplyr::group_by(cell_id, x, y, ages, LCT)
  per_class = if (method == 'mean') {
    dplyr::summarize(per_class, value = mean(value), .groups = 'drop_last')
  } else {
    dplyr::summarize(per_class, value = stats::median(value), .groups = 'drop_last')
  }
  wide = tidyr::pivot_wider(per_class,
                            id_cols = c('cell_id', 'x', 'y', 'ages'),
                            names_from = c('LCT'),
                            values_from = c('value'))
  wide = data.frame(wide)
  if (method == 'median') wide = close_fractions(wide)
  wide
}

# Rescale the class fractions of each row to sum to 1. Stops, naming the cells and slices,
# if any fraction is missing or negative or a row's total is not positive, rather than
# making up a land cover.
close_fractions = function(wide, classes = c('ET', 'OL', 'ST')) {
  fractions = as.matrix(wide[, classes])
  total = rowSums(fractions)
  bad = !is.finite(total) | total <= 0 | apply(!is.finite(fractions) | fractions < 0, 1, any)
  if (any(bad)) {
    where = paste0('cell ', wide$cell_id[bad], ' at ', wide$ages[bad], ' BP')
    stop(sprintf('close_fractions: cannot rescale the land cover of %d cell-slice(s) to sum to 1 (a fraction missing or negative, or all zero): %s%s',
                 sum(bad), paste(head(where, 10), collapse = '; '), if (sum(bad) > 10) '; ...' else ''),
         call. = FALSE)
  }
  wide[, classes] = fractions / total
  wide
}
