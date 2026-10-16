# 10_forcing_breakdown.R: where in the year and where in latitude the Holocene forcing of
# script 9 comes from.
#
# WHY (Chris, 2026-10-15; question B2d, what the paper claims about past snow). The
# calibration has no snow term, so past snow seasons are in effect today's. Before any snow
# sensitivity test, this shows how much of each period's forcing comes from the snowy months
# and the northern latitudes, where that assumption matters most. It is a breakdown of
# script 9's numbers, not a new estimate: the parts add up to script 9's totals, and the
# script stops if they do not.
#
# WHERE THIS SITS IN THE PIPELINE
#   Step 10, after script 9, whose period totals it reads to check against. Same settings:
#   the forcing variant and kernels of config.R (analysis$forcing), the periods of
#   analysis$coarse$ages.
#
# WHAT COMES IN
#   output/forcing/RF_holocene_all_cases.RDS   script 8: forcing per cell, month, slice-pair
#   output/forcing/forcing_by_period.csv       script 9: the period totals, for the check
#
# WHAT GOES OUT
#   output/forcing/forcing_by_month_latitude.csv   one row per kernel, period, month and
#                                                  latitude band
#   figures/forcing_by_month_latitude.pdf          HadGEM3, the parts as a month x band grid
#
# THE ARITHMETIC (script 9's, kept apart by month and band; its header, A4 to A8)
#   1. Slice-pairs grouped into periods, incomplete cell-months dropped: R/forcing_periods.R,
#      the function script 9 uses.
#   2. Forcing summed over the slice-pairs of a period, per cell and month (A5).
#   3. Cells grouped into latitude bands by their centre: edges at multiples of
#      analysis$forcing$latitude_band_width degrees, the outer bands cut at the domain's
#      limits (analysis$domain), e.g. 27-30, 30-40, ..., 70-74 N for 10 degrees.
#   4. For each period, month and band:
#        global_equiv_month      sum(rf x area) / Earth's area: that month's forcing as a
#                                global equivalent (A8), W m-2;
#        contribution_to_annual  the same month's share of script 9's annual figure, W m-2:
#                                script 9 averages each cell over its months with a kernel
#                                value (A6), so each cell-month counts rf x area / Earth's
#                                area / the cell's number of such months (12, or fewer where
#                                CACK has no value in polar night). Summed over months and
#                                bands it gives script 9's global_equiv exactly;
#        band_mean               area-weighted mean over the band's cells (A7), W m-2: how
#                                strong the forcing is there, not how much it adds;
#        area_m2                 the band's area.
#   Positive = warming (albedo fell), as in scripts 8 and 9.
#
# No new method: the same sums as script 9, kept apart instead of added together. It changes
# no number, it only shows where the existing ones come from.

source('R/calendar.R')
source('R/forcing_periods.R')
source('R/run_manifest.R')
run   = run_begin('10_forcing_breakdown')
cfg   = run$cfg
paths = run$paths

library(ggplot2)
library(dplyr)

EARTH_AREA      = cfg$analysis$forcing$earth_area_m2
FORCING_COLUMNS = setNames(paste0('rf_', cfg$analysis$forcing$report_kernels, '_', cfg$analysis$forcing$variant),
                           cfg$analysis$forcing$report_kernels)
months          = cfg$analysis$months

# Latitude band edges: multiples of the band width inside the domain, plus its limits.
band_width = cfg$analysis$forcing$latitude_band_width
lat_min    = cfg$analysis$domain$lat_min
lat_max    = cfg$analysis$domain$lat_max
band_edges = sort(unique(c(lat_min, seq(ceiling(lat_min / band_width) * band_width, lat_max, by = band_width), lat_max)))
band_labels = paste0(head(band_edges, -1), '-', band_edges[-1], ' N')

# Step 1.
forcing_by_cell = read_input(run, path_file(paths, 'forcing'))
forcing_by_cell = assign_forcing_periods(forcing_by_cell, cfg$analysis$coarse$ages)

# Step 3: the band of each cell, by its centre.
forcing_by_cell$lat_band = cut(forcing_by_cell$lat, breaks = band_edges, labels = band_labels, include.lowest = TRUE)
if (anyNA(forcing_by_cell$lat_band)) stop('cells outside the latitude bands: ', paste(unique(forcing_by_cell$lat[is.na(forcing_by_cell$lat_band)]), collapse = ' '))

# Steps 2 and 4 for one forcing column.
breakdown_forcing <- function(forcing_by_cell, forcing_column, earth_area) {
  forcing_by_cell %>%
    group_by(period, cell_id, month, area, lat_band) %>%
    summarise(rf = sum(.data[[forcing_column]], na.rm = FALSE), .groups = 'drop') %>%   # step 2 (A5)
    group_by(period, cell_id) %>%
    mutate(months_with_value = sum(!is.na(rf))) %>%                                     # A6's denominator
    group_by(period, month, lat_band) %>%
    summarise(global_equiv_month     = sum(rf * area, na.rm = TRUE) / earth_area,
              contribution_to_annual = sum(rf * area / months_with_value, na.rm = TRUE) / earth_area,
              band_mean              = weighted.mean(rf, area, na.rm = TRUE),
              area_m2                = sum(area), .groups = 'drop')
}

breakdown = bind_rows(lapply(names(FORCING_COLUMNS), function(kernel_id)
  breakdown_forcing(forcing_by_cell, FORCING_COLUMNS[[kernel_id]], EARTH_AREA) %>% mutate(kernel = kernel_id)))
breakdown$month = factor(breakdown$month, levels = months)
breakdown = breakdown %>% arrange(kernel, period, month, lat_band) %>%
  select(kernel, period, month, lat_band, global_equiv_month, contribution_to_annual, band_mean, area_m2)

# The check: the parts add up to script 9's totals, for every kernel and period.
forcing_by_period = read_input(run, path_file(paths, 'forcing_by_period'), read.csv)
totals = breakdown %>% group_by(kernel, period) %>% summarise(sum_of_parts = sum(contribution_to_annual), .groups = 'drop') %>%
  mutate(period = as.character(period)) %>%
  inner_join(forcing_by_period[, c('kernel', 'period', 'global_equiv')], by = c('kernel', 'period'))
if (nrow(totals) != length(FORCING_COLUMNS) * nlevels(breakdown$period))
  stop('script 9 totals missing for some kernel or period: run script 9 with the same config first')
worst = max(abs(totals$sum_of_parts - totals$global_equiv))
cat('largest gap between the sum of the parts and script 9:', signif(worst, 3), 'W/m2\n')
if (worst > 1e-9) stop('the breakdown does not add up to script 9 (largest gap ', signif(worst, 3), ' W/m2)')

# Two summaries printed for the record: each month's and each band's share of the annual
# forcing, HadGEM3, per period (shares can exceed 100% or be negative where parts cancel).
hadgem = breakdown %>% filter(kernel == 'hadgem')
cat('\nHadGEM3, contribution to the annual global-equivalent forcing by month (W/m2):\n')
print(as.data.frame(hadgem %>% group_by(period, month) %>% summarise(w = sum(contribution_to_annual), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = month, values_from = w)), digits = 2)
cat('\nHadGEM3, contribution by latitude band (W/m2):\n')
print(as.data.frame(hadgem %>% group_by(period, lat_band) %>% summarise(w = sum(contribution_to_annual), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = lat_band, values_from = w)), digits = 2)

# The figure: HadGEM3 contribution to the annual figure, month x band, one panel per period.
p = ggplot(hadgem, aes(x = month, y = lat_band, fill = contribution_to_annual)) +
  geom_tile() +
  scale_fill_gradient2(low = '#00BFC4', mid = 'white', high = '#F8766D', midpoint = 0,
                       name = expression('W/m'^2)) +
  facet_wrap(~ period) +
  labs(x = NULL, y = NULL,
       title = 'Where the Holocene forcing comes from (HadGEM3, global equivalent)',
       subtitle = 'Each panel sums to that period\'s bar in script 9; positive = warming') +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(path_figure(paths, 'forcing_by_month_latitude.pdf'), p, width = 11, height = 8)

write_output(run, breakdown, path_file(paths, 'forcing_breakdown'), write.csv, row.names = FALSE)

run_finish(run)
