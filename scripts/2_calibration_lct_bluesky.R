# 2_calibration_lct_bluesky.R: attach modern satellite albedo to the modern land cover
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('2_calibration_lct_bluesky')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(terra)
library(ggplot2)
library(reshape2)
library(tidyterra)

# All twelve months from the shared calendar (R/calendar.R). Deliberately not
# config.R's month selection: the calibration table keeps every month, so scripts 4 onward
# can run on any selection without rebuilding it.
source('R/calendar.R')
months = MONTHS

pbs_ll = read_input(run, path_file(paths, 'boundaries_ll'))

# Boundaries for the maps, prepared by the shared helper (see R/map_helpers.R for why the
# raw file cannot be drawn as-is: the "weirdness" noted on 2026-09-24). Fix added 2026-09-25.
# The map window is the calibration window in config.R.
map_window = cfg$presentation$map_windows$calibration
source('R/map_helpers.R')
pbs_land = prepare_boundaries(pbs_ll, map_window)

grid <- rast(read_input(run, path_file(paths, 'grid')))


lct_interp_modern = read_input(run, path_file(paths, 'lct_modern'))

# Present-day ice caps out of the calibration (config.R analysis$calibration$ice_mask,
# question B8): cells whose centre lies under the 1 ka ice polygons (the first of
# the glacier shapes) carry interpolated vegetation fractions but the satellite's ice albedo.
# They stay in the prediction domain; only the calibration table loses them. The cells left
# out are recorded in the run manifest.
if (cfg$analysis$calibration$ice_mask == 'centre_in_1ka_polygon') {
  source('R/preparation.R')
  ice_1ka   = read_input(run, path_file(paths, 'glacier_shapes'))[[1]]
  under_ice = cells_in_polygons(lct_interp_modern$x, lct_interp_modern$y, ice_1ka)
  run_record(run, 'calibration_ice_mask', list(method = 'centre_in_1ka_polygon',
                                               cells_left_out = sum(under_ice),
                                               cells = lct_interp_modern[under_ice, c('x', 'y')]))
  lct_interp_modern = lct_interp_modern[!under_ice, ]
}

longitude_interp = lct_interp_modern[,c('x')]
latitude_interp = lct_interp_modern[,c('y')]
lonlat_interp = cbind(longitude_interp, latitude_interp)

lct_interp_spat = vect(lonlat_interp,
                crs  = crs("+init=epsg:4326"),
                atts = lct_interp_modern[,3:ncol(lct_interp_modern)])

# construct objects to hold albedo data

# albedo native resolution
bs_interp_df = data.frame(matrix(NA, nrow=nrow(lct_interp_spat), ncol=12))
colnames(bs_interp_df) = MONTHS

# albedo coarser resolution (same as grid?)
bs_interp_df_coarse = data.frame(matrix(NA, nrow=nrow(lct_interp_spat), ncol=12))
colnames(bs_interp_df_coarse) = paste0('bs', c(paste0('0', seq(1, 9)), seq(10,12)))
colnames(bs_interp_df_coarse) = MONTHS

grid = rast(grid)

blue_all_months = read_input(run, path_file(paths, 'bluesky_raster'), rast)

# coarsen albedo by averaging albedo cells within a grid cell
# CM: this cell-averaged albedo is saved but not currently used downstream
blue_all_months_coarse = resample(blue_all_months, grid, method="average")

names(blue_all_months) = MONTHS
names(blue_all_months_coarse) = MONTHS

# crop albedo (I think this removes the ocean grid cells? not sure)
# CM: this crop is not needed (and not doing anything - cropping the raster to a box that is bigger than itself) - line 123 samples the raster at the 2870 land cover cells.
blue_all_months_coarse = crop(blue_all_months_coarse, ext(pbs_ll))

# some weirdness going on with these figures

ggplot() +
  boundary_layers(pbs_land, map_window) +
  geom_spatraster(data = blue_all_months, alpha=0.8) +
  scale_fill_gradientn(colours = terrain.colors(10), na.value='transparent', name = "Albedo") +
  theme_light() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave(path_figure(paths, 'albedo_maps_monthly_bluesky_native.pdf'))
ggsave(path_figure(paths, 'albedo_maps_monthly_bluesky_native.png'))

ggplot() +
  boundary_layers(pbs_land, map_window) +
  geom_spatraster(data = blue_all_months_coarse, alpha=0.8) +
  scale_fill_gradientn(colours = terrain.colors(10), na.value='transparent', name = "Albedo") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave(path_figure(paths, 'albedo_maps_monthly_bluesky_coarse.pdf'))
ggsave(path_figure(paths, 'albedo_maps_monthly_bluesky_coarse.png'))

cols_fill = scale_fill_gradientn(colours=terrain.colors(10),
                                 na.value='transparent',
                                 name = "Albedo")

cols_fill = scale_fill_brewer(palette="YlOrBr",
                              direction = -1,
                                 na.value='transparent',
                                 name = "Albedo")

for(i in 1:length(months)) {

  month = months[i]

  blue_month = blue_all_months[[month]]
  vname = sprintf('bs%02d', i)

  p <- ggplot() +
    boundary_layers(pbs_land, map_window) +
    geom_spatraster(data=blue_month, alpha=0.8) +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    theme_bw() +
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+

  print(p)
  ggsave(path_figure(paths, paste0('albedo_maps_', month, '_bluesky.pdf')))
  ggsave(path_figure(paths, paste0('albedo_maps_', month, '_bluesky.png')))

  blue_month_coarse = blue_all_months_coarse[[month]]
  vname = sprintf('bs%02d', i)

  p_coarse <- ggplot() +
    boundary_layers(pbs_land, map_window) +
    geom_spatraster(data=blue_month_coarse, alpha=0.8) +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    theme_bw() +
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+

  print(p_coarse)
  ggsave(path_figure(paths, paste0('albedo_maps_', month, '_bluesky_coarse.pdf')))
  ggsave(path_figure(paths, paste0('albedo_maps_', month, '_bluesky_coarse.png')))

  bs_interp_df_coarse[,month] <- terra::extract(blue_month_coarse, lct_interp_spat)[,month]

  bs_interp_df[,month] <- terra::extract(blue_month, lct_interp_spat)[,month]

}

bs_interp_df[bs_interp_df==0] = 1e-4
bs_interp_df_coarse[bs_interp_df_coarse==0] = 1e-4

lct_interp_bs = data.frame(lct_interp_modern, bs_interp_df)
lct_interp_bs_coarse = data.frame(lct_interp_modern, bs_interp_df_coarse)

write_output(run, lct_interp_bs, path_file(paths, 'calibration_table'))
write_output(run, lct_interp_bs_coarse, path_file(paths, 'calibration_table_coarse'))

lct_interp_bs_melt = melt(lct_interp_bs, id.vars = c('x', 'y', 'elev', 'ET', 'OL', 'ST'))
lct_interp_bs_coarse_melt = melt(lct_interp_bs_coarse, id.vars = c('x', 'y', 'elev', 'ET', 'OL', 'ST'))

lct_interp_bs_merged = merge(lct_interp_bs_melt, lct_interp_bs_coarse_melt, by = c('x', 'y', 'elev', 'ET', 'OL', 'ST', 'variable'))

# CM: axis labels look swapped: after the merge value.x is the native albedo and value.y
# the coarse one, but x is labelled coarse and y native. Left as is pending Andria
# (questions_for_andria_general.md, section 8).
ggplot(data = lct_interp_bs_merged) +
  geom_point(aes(x=value.x, y=value.y), alpha=0.6) +
  geom_abline(intercept=0, slope=1, colour='red', lwd=1, alpha=0.5) +
  xlab('albedo resampled coarse') +
  ylab('albedo native') +
  theme_bw(18) +
  coord_fixed() +
  xlim(c(0,0.8)) +
  ylim(c(0,0.8))
ggsave(path_figure(paths, 'albedo_native_vs_coarse_scatter_interp.pdf'))
ggsave(path_figure(paths, 'albedo_native_vs_coarse_scatter_interp.png'))

cor(lct_interp_bs_merged$value.x, lct_interp_bs_merged$value.y, use = 'complete.obs')

run_finish(run)
