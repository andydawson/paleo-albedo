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
#   3. Cells grouped into latitude bands by their centre: edges at the whole-degree
#      multiples of analysis$forcing$latitude_band_width, the outer bands cut at the domain's
#      limits (analysis$domain), e.g. 27-30, 30-40, ..., 70-74 N for 10 degrees.
#   4. For each period, month and band, five columns, defined exactly in R/forcing_breakdown.R:
#      global_equiv_month (that month's global-equivalent forcing), contribution_to_annual
#      (its part of script 9's annual figure; these add up to script 9), band_mean (how
#      strong the forcing is there), area_m2 and valid_area_m2 (the band's area, and the part
#      of it with a forcing value that month: a missing month is not a zero).
#   Positive = warming (albedo fell), as in scripts 8 and 9.
#
# WHAT IT SHOWS AND WHAT IT DOES NOT. The forcing variant of config.R (analysis$forcing$
# variant), which by default combines vegetation change and ice-sheet retreat; the tables and
# the figure name the variant and kernels. It locates where that combined forcing comes from;
# it does not measure what snow does to it (it reads no snow data).
#
# No new method: the same sums as script 9, kept apart instead of added together. It changes
# no number, it only shows where the existing ones come from.

source('R/calendar.R')
source('R/forcing_periods.R')
source('R/forcing_breakdown.R')
source('R/kernels.R')
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

# What the numbers are, for the tables and the figure: the variant and each kernel's
# variable, sky and level (R/kernels.R).
VARIANT       = cfg$analysis$forcing$variant
KERNEL_LABELS = kernels_read(cfg)$label
names(KERNEL_LABELS) = rownames(kernels_read(cfg))
cat('forcing variant', VARIANT, '; kernels:', paste(KERNEL_LABELS[names(FORCING_COLUMNS)], collapse = ', '), '\n')

# Latitude band edges (R/forcing_breakdown.R).
band_edges  = latitude_band_edges(cfg$analysis$domain$lat_min, cfg$analysis$domain$lat_max,
                                  cfg$analysis$forcing$latitude_band_width)
band_labels = latitude_band_labels(band_edges)

# Step 1.
forcing_by_cell = read_input(run, path_file(paths, 'forcing'))
forcing_by_cell = assign_forcing_periods(forcing_by_cell, cfg$analysis$coarse$ages)

# Step 3: the band of each cell, by its centre.
forcing_by_cell$lat_band = cut(forcing_by_cell$lat, breaks = band_edges, labels = band_labels, include.lowest = TRUE)
if (anyNA(forcing_by_cell$lat_band)) stop('cells outside the latitude bands: ', paste(unique(forcing_by_cell$lat[is.na(forcing_by_cell$lat_band)]), collapse = ' '))

# Steps 2 and 4, per kernel (R/forcing_breakdown.R).
breakdown = bind_rows(lapply(names(FORCING_COLUMNS), function(kernel_id)
  breakdown_forcing(forcing_by_cell, FORCING_COLUMNS[[kernel_id]], EARTH_AREA) %>%
    mutate(kernel = kernel_id, kernel_label = KERNEL_LABELS[[kernel_id]], variant = VARIANT)))
breakdown$month = factor(breakdown$month, levels = months)
breakdown = breakdown %>% arrange(kernel, period, month, lat_band) %>%
  select(kernel, kernel_label, variant, period, month, lat_band, global_equiv_month,
         contribution_to_annual, band_mean, area_m2, valid_area_m2)

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
cat('\n', KERNEL_LABELS[['hadgem']], ', variant ', VARIANT, ': contribution to the annual global-equivalent forcing by month (W/m2):\n', sep = '')
print(as.data.frame(hadgem %>% group_by(period, month) %>% summarise(w = sum(contribution_to_annual), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = month, values_from = w)), digits = 2)
cat('\n', KERNEL_LABELS[['hadgem']], ', variant ', VARIANT, ': contribution by latitude band (W/m2):\n', sep = '')
print(as.data.frame(hadgem %>% group_by(period, lat_band) %>% summarise(w = sum(contribution_to_annual), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = lat_band, values_from = w)), digits = 2)

# The figure: HadGEM3 contribution to the annual figure, month x band, one panel per period.
p = ggplot(hadgem, aes(x = month, y = lat_band, fill = contribution_to_annual)) +
  geom_tile() +
  scale_fill_gradient2(low = '#00BFC4', mid = 'white', high = '#F8766D', midpoint = 0,
                       name = expression('W/m'^2)) +
  facet_wrap(~ period) +
  labs(x = NULL, y = NULL,
       title = paste0('Where the Holocene forcing comes from: ', KERNEL_LABELS[['hadgem']],
                      ', variant ', VARIANT, ', global equivalent'),
       subtitle = 'Each panel sums to that period\'s bar in script 9; positive = warming') +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(path_figure(paths, 'forcing_by_month_latitude.pdf'), p, width = 11, height = 8)

write_output(run, breakdown, path_file(paths, 'forcing_breakdown'), write.csv, row.names = FALSE)

run_finish(run)
