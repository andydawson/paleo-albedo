# The slice-pairs of script 8's forcing table grouped into the coarse periods (scripts 9 and
# 10).
#
# Why a function: script 9 sums forcing over each period, and script 10 breaks the same sums
# down by month and latitude; both must use the same pairs and the same cells, or the
# breakdown would not add up to script 9's totals. The lines are script 9's (assumptions A4,
# A5 and A10 in its header), moved unchanged.
#
# Input: script 8's table (one row per cell, month and consecutive pair of slices; `year` is
# the younger slice of the pair) and the coarse ages that bound the periods (config.R,
# analysis$coarse$ages).
# Output: the same table without the pairs outside every period and without the cell-months
# that miss a pair within their period, with two columns added: `age_old` (the older slice
# of the pair) and `period` (a factor labelled by period_labels() of R/calendar.R). Prints
# the number of pairs per period and of cell-month-periods dropped.
# Needs dplyr.
assign_forcing_periods <- function(forcing_by_cell, ages_sub) {
  labels_period = period_labels(ages_sub)

  # --- A4/A5: assign each slice-pair to a period -------------------------------
  # A row's `year` is the YOUNGER end of the pair; the pair spans year -> next age.
  ages      = sort(unique(c(forcing_by_cell$year, 11500)))
  forcing_by_cell$age_old = ages[match(forcing_by_cell$year, ages) + 1]
  forcing_by_cell$period  = cut(forcing_by_cell$year, breaks = ages_sub, right = FALSE, labels = labels_period)
  forcing_by_cell         = forcing_by_cell[!is.na(forcing_by_cell$period), ]

  # How many pairs fall in each period (2, 3, 4, 4, 4, 4, 3).
  cat('slice-pairs per period:\n'); print(table(unique(forcing_by_cell[, c('year','period')])$period))

  # --- A10: keep only cells complete within a period ---------------------------
  pairs_per_period = forcing_by_cell %>% group_by(period) %>% summarise(n_pairs = n_distinct(year), .groups='drop')
  complete_cell_month_periods = forcing_by_cell %>%
    group_by(period, cell_id, month) %>%
    summarise(n_pairs = n_distinct(year), .groups = 'drop') %>%
    left_join(pairs_per_period, by = 'period', suffix = c('', '_full')) %>%
    filter(n_pairs == n_pairs_full) %>%
    select(period, cell_id, month)
  cat('cell-month-period combinations dropped as incomplete:',
      nrow(distinct(forcing_by_cell[, c('period','cell_id','month')])) - nrow(complete_cell_month_periods), '\n')
  inner_join(forcing_by_cell, complete_cell_month_periods, by = c('period','cell_id','month'))
}
