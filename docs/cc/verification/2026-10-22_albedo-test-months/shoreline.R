# Read-only: cell albedo for the four test months under the two land rules (land only; land or
# shoreline), using the build script's own cell_table; nothing is written but this comparison.
suppressMessages(source(file.path(Sys.getenv('WT'), 'tools/prepare_bluesky_albedo.R')))
cells = readRDS(CELLS_FILE)[, c('x', 'y')]
tiles = tiles_for_cells(cells)
months = as.Date(c('2000-03-01', '2005-03-01', '2005-07-01', '2005-12-01'))
run = function(rule) { LAND_RULE <<- rule; cell_table(tiles, cells, file.path(WORK_DIR, 'monthly'), months) }
a = run('land'); b = run('land_or_shore')
m = merge(a[c('x', 'y', 'month', 'land_pixels', 'full_75', 'full_75_coverage')],
          b[c('x', 'y', 'month', 'land_pixels', 'full_75', 'full_75_coverage')],
          by = c('x', 'y', 'month'), all = TRUE, suffixes = c('_land', '_shore'))
write.csv(m, file.path(Sys.getenv('OUT'), 'shoreline_cells.csv'), row.names = FALSE)
cat('cell-months: land rule', nrow(a), '; land-or-shoreline rule', nrow(b), '\n')
cat('cell-months with an albedo: land', sum(!is.na(m$full_75_land)), '; land or shoreline', sum(!is.na(m$full_75_shore)),
    '; only with shoreline', sum(is.na(m$full_75_land) & !is.na(m$full_75_shore)), '\n')
both = m[!is.na(m$full_75_land) & !is.na(m$full_75_shore), ]
both$shore_share = 1 - both$land_pixels_land / both$land_pixels_shore
both$d = both$full_75_shore - both$full_75_land
both$band = cut(both$shore_share, c(-Inf, 0.05, 0.2, 0.5, Inf), labels = c('under 5%', '5-20%', '20-50%', 'over 50%'))
summ = function(g) data.frame(cell_months = nrow(g), mean_diff = round(mean(g$d), 4), median_diff = round(median(g$d), 4),
                               p05 = round(quantile(g$d, 0.05), 4), p95 = round(quantile(g$d, 0.95), 4),
                               mean_abs = round(mean(abs(g$d)), 4), row.names = NULL)
cat('\nfull_75 albedo, land-or-shoreline minus land only, by shoreline share of the cell\'s pixels:\n')
print(do.call(rbind, lapply(split(both, both$band), function(g) cbind(band = g$band[1], summ(g)))), row.names = FALSE)
cat('\nby month:\n')
print(do.call(rbind, lapply(split(both, both$month), function(g) cbind(month = g$month[1], summ(g),
      median_shore_share = round(median(g$shore_share), 3)))), row.names = FALSE)
cat('\nshoreline share of the cell (all months): median', round(median(both$shore_share), 3), '; over 20%:', round(mean(both$shore_share > 0.2), 3), '\n')
