# 2_calibration_lct_bluesky.R: attach modern satellite albedo to the modern land cover
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(terra)
library(ggplot2)
library(reshape2)
library(tidyterra)

# Month list from the shared calendar (R/calendar.R) rather than typed out in each script
# (issue I02, 2026-10-12).
source('R/calendar.R')
months = MONTHS

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

# Boundaries for the maps, prepared by the shared helper (see R/map_helpers.R for why the
# raw file cannot be drawn as-is: the "weirdness" noted on 2026-09-24). Fix added 2026-09-25.
source('R/map_helpers.R')
pbs_land = prepare_boundaries(pbs_ll)

grid <- rast(readRDS("data/grid.RDS"))

dir.create('figures', showWarnings = FALSE)

lct_interp_modern = readRDS('data/lct_modern_reveals_interp.RDS')

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

blue_all_months = rast('data/blue_sky_monthly_2000-2009.tif')

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
  boundary_layers(pbs_land) +
  geom_spatraster(data = blue_all_months, alpha=0.8) +
  scale_fill_gradientn(colours = terrain.colors(10), na.value='transparent', name = "Albedo") +
  theme_light() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_native.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_native.png')

ggplot() +
  boundary_layers(pbs_land) +
  geom_spatraster(data = blue_all_months_coarse, alpha=0.8) +
  scale_fill_gradientn(colours = terrain.colors(10), na.value='transparent', name = "Albedo") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_coarse.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_coarse.png')

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
    boundary_layers(pbs_land) +
    geom_spatraster(data=blue_month, alpha=0.8) +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    theme_bw() +
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+

  print(p)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.png'))

  blue_month_coarse = blue_all_months_coarse[[month]]
  vname = sprintf('bs%02d', i)

  p_coarse <- ggplot() +
    boundary_layers(pbs_land) +
    geom_spatraster(data=blue_month_coarse, alpha=0.8) +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    theme_bw() +
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+

  print(p_coarse)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.png'))

  bs_interp_df_coarse[,month] <- terra::extract(blue_month_coarse, lct_interp_spat)[,month]

  bs_interp_df[,month] <- terra::extract(blue_month, lct_interp_spat)[,month]

}

bs_interp_df[bs_interp_df==0] = 1e-4
bs_interp_df_coarse[bs_interp_df_coarse==0] = 1e-4

lct_interp_bs = data.frame(lct_interp_modern, bs_interp_df)
lct_interp_bs_coarse = data.frame(lct_interp_modern, bs_interp_df_coarse)

saveRDS(lct_interp_bs, 'data/calibration_modern_lct_interp_bluesky.RDS')
saveRDS(lct_interp_bs_coarse, 'data/calibration_modern_lct_interp_bluesky_coarse.RDS')

lct_interp_bs_melt = melt(lct_interp_bs, id.vars = c('x', 'y', 'elev', 'ET', 'OL', 'ST'))
lct_interp_bs_coarse_melt = melt(lct_interp_bs_coarse, id.vars = c('x', 'y', 'elev', 'ET', 'OL', 'ST'))

lct_interp_bs_merged = merge(lct_interp_bs_melt, lct_interp_bs_coarse_melt, by = c('x', 'y', 'elev', 'ET', 'OL', 'ST', 'variable'))

ggplot(data = lct_interp_bs_merged) +
  geom_point(aes(x=value.x, y=value.y), alpha=0.6) +
  geom_abline(intercept=0, slope=1, colour='red', lwd=1, alpha=0.5) +
  xlab('albedo resampled coarse') +
  ylab('albedo native') +
  theme_bw(18) +
  coord_fixed() +
  xlim(c(0,0.8)) +
  ylim(c(0,0.8))
ggsave('figures/albedo_native_vs_coarse_scatter_interp.pdf')
ggsave('figures/albedo_native_vs_coarse_scatter_interp.png')

cor(lct_interp_bs_merged$value.x, lct_interp_bs_merged$value.y, use = 'complete.obs')
