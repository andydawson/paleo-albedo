# 1_veg_lct_prep.R: turn the land-cover posterior into two plain tables
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.
# The draws are reduced by reduce_land_cover() and the fractions checked by
# check_fractions() (R/preparation.R); the elevation is read from a prepared table; a
# diagnostic switch (config.R run$diagnostic) adds checks and plots at the end.

# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('1_veg_lct_prep')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(dplyr)
library(tidyr)

# Diagnostic mode. Off by default. Switch on with `diagnostic = TRUE` in the run section of
# config.R (or of the file given with --config).
# In diagnostic mode the script additionally (1) reports on the ice flag it discards,
# (2) characterises the cell-slices absent from the input, (3) draws the ice extent at
# every time slice into figures/diagnostics/, and (4) compares the mean and the median of
# the 200 posterior draws as summaries of land cover: how far apart they are, where the
# difference concentrates, and whether the medians still sum to one across the three
# classes (the pipeline uses config.R's analysis$land_cover$reduction, the mean by
# default). Nothing in diagnostic mode changes the outputs.
diagnostic = cfg$run$diagnostic


lct_interp = read_input(run, path_file(paths, 'veg_posts'))

if (diagnostic) {
  # Keep what the summarize step below discards, for the diagnostics at the end:
  # the ice flag per cell-slice, and three flagged rows as an example.
  ice_flags = unique(lct_interp[lct_interp$iter == 1 & lct_interp$LCT == 'ET', c('cell_id', 'ages', 'ice')])
  ice_example = head(lct_interp[!is.na(lct_interp$ice) & lct_interp$iter == 1 & lct_interp$ages == 10000,
                                c('cell_id', 'x', 'y', 'ages', 'LCT', 'value', 'ice')], 3)
  # Mean, median and spread of the 200 draws per cell-slice-class, to compare the two
  # summaries (the pipeline uses config.R's choice, the mean by default; see diagnostic 4).
  draw_stats = lct_interp %>%
    group_by(cell_id, ages, LCT) %>%
    dplyr::summarize(mean = mean(value), median = median(value), sd = sd(value), .groups = 'drop')
}

#  not sure if should use median
# CM: the draws are summarised by reduce_land_cover() (R/preparation.R): the mean, as here
# before, or the median, chosen in config.R (analysis$land_cover$reduction,
# question A1).
source('R/preparation.R')
lct_interp_wide = reduce_land_cover(lct_interp, cfg$analysis$land_cover$reduction)

# The fractions must sum to one in every cell and slice: the calibration smooth of the
# three is fitted on that sheet only (question B1). Stops otherwise; the result
# is recorded in the run manifest.
run_record(run, 'land_cover_fractions', check_fractions(lct_interp_wide))

locations_interp = lct_interp_wide[,c('x', 'y')]

# Elevation of each cell (config.R analysis$elevation$method, question B9).
# "land_mean": the GMTED2010 mean over the cell's non-ocean area, a table prepared once by
# tools/prepare_land_elevation.R (committed).
# "centre_point" (the original): the elevation at each cell centre from the AWS terrain
# tiles, data/elev_interp_cells.RDS (committed; one row per cell: x, y, elev). If it is
# missing, tools/prepare_centre_elevation.R remakes it with the original query.
if (cfg$analysis$elevation$method == 'land_mean') {
  elev_lookup = read_input(run, path_file(paths, 'elevation_land_mean'), read.csv)
  run_record(run, 'elevation', list(method = 'land_mean',
                                    source = 'GMTED2010 30-arc-second mean over non-sea area (sea = GMTED 0 m)'))
} else {
  elev_file = path_file(paths, 'elevation_cache')
  if (!file.exists(elev_file)) stop(elev_file, ' is missing: run tools/prepare_centre_elevation.R once')
  elev_lookup = read_input(run, elev_file)
}
elev_interp = elev_lookup$elev[match(paste(locations_interp$x, locations_interp$y),
                                     paste(elev_lookup$x, elev_lookup$y))]
if (anyNA(elev_interp)) stop('cells without an elevation in the elevation table: ', sum(is.na(elev_interp)))

lct_interp_all = data.frame(lct_interp_wide[,c('ages', 'x', 'y')],
                     elev = elev_interp,
                     lct_interp_wide[,c('ET', 'OL', 'ST')])



lct_interp_modern = lct_interp_all[which(lct_interp_all$ages == 50), ]

lct_interp_modern = lct_interp_modern[, which(!(colnames(lct_interp_modern) %in% c('ages')))]

# One row per modern cell (x, y): elevation and the three land-cover fractions.
write_output(run, lct_interp_modern, path_file(paths, 'lct_modern'))

lct_interp_paleo = lct_interp_all

# One row per cell and slice (x, y, ages).
write_output(run, lct_interp_paleo, path_file(paths, 'lct_paleo'))

############################################################################################
# Diagnostics (config.R run$diagnostic = TRUE only). Added 2026-09-25. Nothing below changes any output.
############################################################################################
if (diagnostic) {
  library(ggplot2)

  # ---- 1. The ice flag this script discards ----------------------------------------------
  # Shows that no cell is flagged at 50, 200 or 500 BP although 48 are at 1,000 BP: the flag
  # is applied only from 1,000 BP back, so present-day ice caps reach the calibration table
  # as vegetated cells (question B8 in docs/cc/questions_for_andria_scientific.md).
  cat('\n==== Ice flag in veg_posts_interp_ice.RDS (dropped at the summarize step) ====\n')
  cat('Values taken by the flag:', paste(unique(ice_flags$ice), collapse = ' / '), '\n')
  cat('Cell-slices flagged ICE:', sum(!is.na(ice_flags$ice)), 'of', nrow(ice_flags),
      sprintf('(%.1f%%)\n', 100 * mean(!is.na(ice_flags$ice))))
  cat('\nFlagged cells per slice (years BP):\n')
  print(table(factor(ice_flags$ages[!is.na(ice_flags$ice)], levels = sort(unique(ice_flags$ages)))))
  cat('\nNote the zeros at 50, 200 and 500 BP against', sum(!is.na(ice_flags$ice) & ice_flags$ages == 1000),
      'at 1,000 BP: the flag is applied only from 1,000 BP back, so present-day\n',
      'ice caps are unflagged and enter the modern calibration table as vegetated cells (question B8).\n')
  cat('\nFlagged cell-slices still carry land-cover values (first three at 10,000 BP):\n')
  print(ice_example, row.names = FALSE)

  # ---- 2. Cell-slices absent from the input ----------------------------------------------
  # The product should hold every cell at every slice. Build that full cell x slice grid,
  # mark which combinations the input lacks, and report them by cell and by zone. The zones
  # are the ones the pipeline cares about: script 8 keeps only the latitudes of config.R
  # analysis$domain (27-74 N by default), so anything absent outside that band never
  # reaches the forcing. Question D3 has the interpretation.
  cells = unique(lct_interp_all[, c('x', 'y')])
  slices = sort(unique(lct_interp_all$ages))
  full_grid = merge(cells, data.frame(ages = slices))
  present_cell_slice_keys = paste(lct_interp_all$x, lct_interp_all$y, lct_interp_all$ages)
  full_grid$absent = !(paste(full_grid$x, full_grid$y, full_grid$ages) %in% present_cell_slice_keys)
  # Zones from config.R analysis$domain; at the defaults the labels read
  # 'south of 27N', 'north of 74N' and 'west of 170W'.
  dom = cfg$analysis$domain
  full_grid$zone = ifelse(full_grid$y < dom$lat_min, paste0('south of ', dom$lat_min, 'N'),
                   ifelse(full_grid$y > dom$lat_max, paste0('north of ', dom$lat_max, 'N'),
                   ifelse(full_grid$x < dom$lon_min, paste0('west of ', -dom$lon_min, 'W'), 'interior (used by the pipeline)')))
  absent = full_grid[full_grid$absent, ]
  cat('\n==== Cell-slices absent from the input ====\n')
  cat('Cells:', nrow(cells), '| slices:', length(slices), '| possible cell-slices:', nrow(full_grid),
      '| absent:', nrow(absent), sprintf('(%.1f%%)\n', 100 * nrow(absent) / nrow(full_grid)))
  # One row per incomplete cell: how many slices it lacks and which.
  per_cell = absent %>%
    group_by(x, y, zone) %>%
    dplyr::summarize(n_absent = n(), absent_slices = paste(sort(ages), collapse = ' '), .groups = 'drop') %>%
    arrange(zone, y, x)
  cat('\nIncomplete cells by zone (a cell is incomplete if it lacks any slice):\n')
  print(table(per_cell$zone))
  cat('\nAbsent cell-slices by zone:\n')
  print(tapply(per_cell$n_absent, per_cell$zone, sum))
  cat('\nAbsent cell-slices by slice and zone:\n')
  print(table(absent$ages, absent$zone))
  if (any(per_cell$zone == 'interior (used by the pipeline)')) {
    cat('\nWARNING: interior cells are missing slices; script 7a will drop them and the\n',
        'forcing will lose coverage. Cells:\n')
    print(as.data.frame(per_cell[per_cell$zone == 'interior (used by the pipeline)', ]), row.names = FALSE)
  } else {
    cat(sprintf('\nNo interior cell is missing any slice: every incomplete cell is outside %s-%s N and\n', dom$lat_min, dom$lat_max),
        'is removed by the trim in script 8 before any forcing is computed.\n')
  }
  write.csv(as.data.frame(per_cell), path_figure(paths, 'diagnostics', '1_absent_cell_slices.csv'), row.names = FALSE)
  cat('\nPer-cell listing written to figures/diagnostics/1_absent_cell_slices.csv\n')

  # ---- 3. Ice extent at every time slice ---------------------------------------------------
  # One map per slice and one overview, from the input's own ice flag: each 1-degree cell is
  # drawn as a tile, coloured ice (flagged), land (present, not flagged) or absent (no row in
  # the input at that slice). Using the flag rather than the polygons keeps this a picture of
  # what the land-cover product itself says; the 1 ka polygons would add 48 cells at 50 BP.
  ice_status_table = full_grid
  key_ice = paste(ice_flags$cell_id, ice_flags$ages)[!is.na(ice_flags$ice)]
  cell_key = lct_interp_all[!duplicated(paste(lct_interp_all$x, lct_interp_all$y)), c('x', 'y')]
  cell_key$cell_id = lct_interp_wide$cell_id[match(paste(cell_key$x, cell_key$y),
                                                    paste(lct_interp_wide$x, lct_interp_wide$y))]
  ice_status_table$cell_id = cell_key$cell_id[match(paste(ice_status_table$x, ice_status_table$y),
                                                    paste(cell_key$x, cell_key$y))]
  ice_status_table$status = ifelse(ice_status_table$absent, 'absent',
                  ifelse(paste(ice_status_table$cell_id, ice_status_table$ages) %in% key_ice, 'ice', 'land'))
  ice_status_table$status = factor(ice_status_table$status, levels = c('land', 'ice', 'absent'))
  status_cols = c(land = 'grey80', ice = 'steelblue', absent = 'firebrick')
  # The map window is the diagnostics window in config.R.
  diag_window = cfg$presentation$map_windows$diagnostics
  for (a in slices) {
    p = ggplot(ice_status_table[ice_status_table$ages == a, ]) +
      geom_tile(aes(x = x, y = y, fill = status)) +
      scale_fill_manual(values = status_cols, drop = FALSE, name = NULL) +
      coord_fixed(xlim = diag_window$x, ylim = diag_window$y) +
      labs(title = paste0('Ice flag in the land-cover product, ', a, ' BP'),
           subtitle = paste0(sum(ice_status_table$ages == a & ice_status_table$status == 'ice'), ' cells flagged ice; ',
                             sum(ice_status_table$ages == a & ice_status_table$status == 'absent'), ' cells absent'),
           x = NULL, y = NULL) +
      theme_bw(11)
    ggsave(path_figure(paths, 'diagnostics', sprintf('1_ice_flag_%05dBP.png', a)), p, width = 9, height = 5, dpi = 120)
  }
  p_all = ggplot(ice_status_table) +
    geom_tile(aes(x = x, y = y, fill = status)) +
    scale_fill_manual(values = status_cols, drop = FALSE, name = NULL) +
    coord_fixed(xlim = diag_window$x, ylim = diag_window$y) +
    facet_wrap(~ages, ncol = 5) +
    labs(title = 'Ice flag in the land-cover product, all 25 slices (years BP)', x = NULL, y = NULL) +
    theme_bw(9) + theme(axis.text = element_blank(), axis.ticks = element_blank())
  ggsave(path_figure(paths, 'diagnostics', '1_ice_flag_all_slices.png'), p_all, width = 14, height = 11, dpi = 120)
  cat('\nIce-extent maps written to figures/diagnostics/ (one per slice plus 1_ice_flag_all_slices.png)\n')

  # ---- 4. Mean or median of the 200 draws? ------------------------------------------------
  # The pipeline summarises each cell-slice-class by the MEAN of its 200 posterior draws
  # (Andria's note above the summarize step asks whether the median would be better). This
  # compares the two. Two things to look at: how large the difference is and where it sits
  # (it grows with the posterior spread and with skew near 0 or 1), and whether the median
  # keeps the three classes summing to one. The mean does so by construction; medians of
  # three separate fractions need not.
  draw_stats$diff = draw_stats$median - draw_stats$mean
  cat('\n==== Mean versus median of the 200 draws, per cell-slice-class ====\n')
  cat('Cell-slice-class combinations:', nrow(draw_stats), '\n')
  cat('\nmedian minus mean, by class (fractions, 0..1):\n')
  print(do.call(rbind, lapply(split(draw_stats, draw_stats$LCT), function(class_draw_stats) data.frame(
    LCT = class_draw_stats$LCT[1], mean_abs_diff = round(mean(abs(class_draw_stats$diff)), 4), p95_abs_diff = round(quantile(abs(class_draw_stats$diff), 0.95), 4),
    max_abs_diff = round(max(abs(class_draw_stats$diff)), 4), share_over_0.01 = round(mean(abs(class_draw_stats$diff) > 0.01), 3),
    share_over_0.05 = round(mean(abs(class_draw_stats$diff) > 0.05), 4)))), row.names = FALSE)
  cat('\nFor scale: the smallest slice-to-slice change in vegetation albedo that script 7a treats as\n',
      'real is of order 0.001-0.01, and land-cover fractions enter the albedo model through a smooth\n',
      'whose gradient is of order 0.1-0.3 albedo per unit fraction, so a 0.01 shift in a fraction is\n',
      'roughly 0.001-0.003 in albedo. Compare the columns above with that.\n')
  # Sum-to-one: the mean fractions sum to 1 exactly; the medians do not.
  cover_fraction_sums = draw_stats %>% group_by(cell_id, ages) %>%
    dplyr::summarize(sum_mean = sum(mean), sum_median = sum(median), .groups = 'drop')
  cat('\nSum of the three class fractions per cell-slice:\n')
  cat('  using the mean:   range', paste(round(range(cover_fraction_sums$sum_mean), 4), collapse = ' to '), '\n')
  cat('  using the median: range', paste(round(range(cover_fraction_sums$sum_median), 4), collapse = ' to '),
      '| share of cell-slices off 1 by more than 0.01:', round(mean(abs(cover_fraction_sums$sum_median - 1) > 0.01), 3), '\n')
  # Where the difference is largest: it should track the posterior spread.
  cat('\nCorrelation of |median - mean| with the posterior sd, by class:\n')
  print(round(sapply(split(draw_stats, draw_stats$LCT), function(class_draw_stats) cor(abs(class_draw_stats$diff), class_draw_stats$sd)), 3))
  # Plot: median against mean per class, and the difference against the mean.
  p_mm = ggplot(draw_stats, aes(x = mean, y = median)) +
    geom_point(alpha = 0.05, size = 0.3) +
    geom_abline(slope = 1, intercept = 0, colour = 'red') +
    facet_wrap(~LCT) + coord_fixed() +
    labs(title = 'Median versus mean of the 200 posterior draws, per cell-slice-class',
         subtitle = 'points on the red line: no difference', x = 'mean of draws', y = 'median of draws') +
    theme_bw(11)
  ggsave(path_figure(paths, 'diagnostics', '1_mean_vs_median.png'), p_mm, width = 12, height = 4.5, dpi = 120)
  p_md = ggplot(draw_stats, aes(x = mean, y = diff)) +
    geom_point(alpha = 0.05, size = 0.3) +
    geom_hline(yintercept = 0, colour = 'red') +
    facet_wrap(~LCT) +
    labs(title = 'Median minus mean, against the mean, per cell-slice-class',
         subtitle = 'skew shows as a systematic offset; it concentrates near 0 and 1', x = 'mean of draws', y = 'median - mean') +
    theme_bw(11)
  ggsave(path_figure(paths, 'diagnostics', '1_mean_vs_median_difference.png'), p_md, width = 12, height = 4.5, dpi = 120)
  cat('\nPlots written: figures/diagnostics/1_mean_vs_median.png and 1_mean_vs_median_difference.png\n')
}

run_finish(run)
