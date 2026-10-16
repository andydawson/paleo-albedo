# Script 10's breakdown of the forcing by month and latitude band.
#
# Why functions: the band edges and the four columns are what a reader of script 10's table
# relies on, so they are tested on a small hand-worked case in tests/test_forcing_breakdown.R.

# Latitude band edges: the domain's limits and every multiple of `width` strictly between
# them, e.g. 27, 30, 40, 50, 60, 70, 74 for 27-74 N and 10 degrees. A width with no multiple
# inside the domain gives one band, the whole domain.
latitude_band_edges <- function(lat_min, lat_max, width) {
  multiples = width * seq_len(floor(lat_max / width))
  c(lat_min, multiples[multiples > lat_min & multiples < lat_max], lat_max)
}

# Labels for the bands between consecutive edges: "27-30 N", "30-40 N", ...
latitude_band_labels <- function(edges) paste0(head(edges, -1), '-', edges[-1], ' N')

# One forcing column broken down by period, month and latitude band. Input: script 8's
# table after assign_forcing_periods() (R/forcing_periods.R), with a lat_band column added;
# the forcing column's name; Earth's area. Script 9's arithmetic (its header, A5 to A8),
# with the months and bands kept apart. Output, one row per period, month and band:
#   global_equiv_month     sum over the band's cells of rf x area / Earth's area: that
#                          month's forcing as a global equivalent, W m-2. Summed over months
#                          it is 12 times script 9's annual figure only where every cell has
#                          all twelve months; use contribution_to_annual to add up.
#   contribution_to_annual the month's part of script 9's annual figure, W m-2: script 9
#                          averages each cell over its months with a forcing value (A6), so
#                          each cell-month counts rf x area / Earth's area / that number of
#                          months. Summed over months and bands it gives script 9's
#                          global_equiv exactly.
#   band_mean              area-weighted mean forcing over the band's cells WITH a value
#                          that month, W m-2 (NA if none has one): how strong it is there.
#   area_m2                area of all the band's cells, m2.
#   valid_area_m2          area of the band's cells with a forcing value that month, m2. Where
#                          it is below area_m2, cells had no value (for example CACK in polar
#                          night, or a variant's structural gaps) and add nothing: a zero in
#                          the first two columns with valid_area_m2 = 0 is missing, not zero.
# A cell-month has no value when its kernel has none or any slice-pair's forcing is missing.
# Needs dplyr.
breakdown_forcing <- function(forcing_by_cell, forcing_column, earth_area) {
  forcing_by_cell %>%
    group_by(period, cell_id, month, area, lat_band) %>%
    summarise(rf = sum(.data[[forcing_column]], na.rm = FALSE), .groups = 'drop') %>%   # A5
    group_by(period, cell_id) %>%
    mutate(months_with_value = sum(!is.na(rf))) %>%                                     # A6's denominator
    group_by(period, month, lat_band) %>%
    summarise(global_equiv_month     = sum(rf * area, na.rm = TRUE) / earth_area,
              contribution_to_annual = sum(rf * area / months_with_value, na.rm = TRUE) / earth_area,
              band_mean              = weighted.mean(rf, area, na.rm = TRUE),
              area_m2                = sum(area),
              valid_area_m2          = sum(area[!is.na(rf)]), .groups = 'drop')
}
