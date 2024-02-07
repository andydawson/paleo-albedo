############################################################################################
# 
############################################################################################

# Warning message:
#   In .varName(nc, varname, warn = warn) : varname used is: 
# DHR_VIS
# If that is not correct, you can set it to one of: 
# DHR_VIS, DHR_NIR, DHR_SW, BHR_VIS, BHR_NIR, BHR_SW, DHR_sigmaVIS, DHR_sigmaNIR, 
# DHR_sigmaSW, BHR_sigmaVIS, BHR_sigmaNIR, BHR_sigmaSW, DHR_alpha_VIS_NIR, DHR_alpha_VIS_SW, 
# DHR_alpha_NIR_SW, BHR_alpha_VIS_NIR, BHR_alpha_VIS_SW, BHR_alpha_NIR_SW, Weighted_Number_of_Samples, 
# Goodness_of_Fit, Time_to_the_Closest_Sample, Data_Mask, Snow_Fraction, Solar_Zenith_Angle, Relative_Entropy

library(terra)
library(rasterVis)
library(ggplot2)
library(reshape2)
library(tidyterra)

# library(sp)
# library(raster)
# library(ncdf4)
# library(rgdal)
# library(rhdf5)

############################################################################################
# 
############################################################################################
alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

grid <- rast(readRDS("data/grid.RDS"))

############################################################################################
# lct REVEALS
############################################################################################

# lct_modern = readRDS('data/lct_modern.RDS')
lct_modern = readRDS('data/lct_modern_reveals.RDS')

longitude = lct_modern[,c('long')]
latitude = lct_modern[,c('lat')]
lonlat = cbind(longitude, latitude)

lct_spat = vect(lonlat, 
                crs  ="+init=epsg:4326", 
                atts = lct_modern[,5:ncol(lct_modern)])

# do we need to do this?
# lct_modern = readRDS('data/lct_modern.RDS')
lct_modern_point = readRDS('data/lct_modern_reveals_point.RDS')

longitude_point = lct_modern_point[,c('long')]
latitude_point = lct_modern_point[,c('lat')]
lonlat_point = cbind(longitude_point, latitude_point)

lct_spat_point = vect(lonlat_point,
                crs  ="+init=epsg:4326",
                atts = lct_modern_point[,5:ncol(lct_modern_point)])

bs_df = data.frame(matrix(NA, nrow=nrow(lct_spat), ncol=12))
colnames(bs_df) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

bs_df_coarse = data.frame(matrix(NA, nrow=nrow(lct_spat), ncol=12))
colnames(bs_df_coarse) = paste0('bs', c(paste0('0', seq(1, 9)), seq(10,12)))
colnames(bs_df_coarse) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')


bs_df_point = data.frame(matrix(NA, nrow=nrow(lct_spat_point), ncol=12))
colnames(bs_df_point) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

blue_all_months = rast('data/blue_sky_monthly_2000-2009.tif')
# blue_coarse_interp = resample(blue_all_months, grid)
blue_all_months_coarse = resample(blue_all_months, grid, method="average")

# 
names(blue_all_months) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
names(blue_all_months_coarse) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

blue_all_months_coarse = crop(blue_all_months_coarse, ext(pbs_ll))

ggplot() +
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
  geom_spatraster(data=blue_all_months, alpha=0.8) +
  scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") + 
  theme_bw() + 
  theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_native.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_native.png')

ggplot() +
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
  geom_spatraster(data=blue_all_months_coarse, alpha=0.8) +
  scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") + 
  theme_bw() + 
  theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_coarse.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_coarse.png')

cols_fill = scale_fill_brewer(palette="YlOrBr", 
                              direction = -1)#,
                              # na.value='transparent', 
                              # name = "Albedo") 

cols_fill = scale_fill_distiller(palette="YlOrBr", 
                                direction = -1,
                                na.value=NA,#'transparent', 
                                name = "Albedo",
                                limits=c(0,1)) 

# lct_spat = cbind(lct_spat, bs_df)
# pdf('figures/monthly_BLUESKY_maps.pdf')
for(i in 1:length(months)) {

  month = months[i]
  # month = i
  
  blue_month = blue_all_months[[month]]
  vname = sprintf('bs%02d', i)
  
  # plot(blue_month)
  
  p <- ggplot() + 
    # facet_wrap(~variable) +
    # scale_fill_continuous(na.value='white') +
    geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
    geom_spatraster(data=blue_month, alpha=0.8) +
    # geom_tile(aes(fill = value)) +
    # geom_raster(aes(fill = value)) +
    #geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    # scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    cols_fill +
    # scale_fill_brewer(palette='Blues')+#, na.value='transparent', name = "Albedo") +
    theme_bw(14) + 
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+
    # xlab('longitude') +
    # ylab('latitude') #+
    # ggtitle(months[month]) #+
    # theme(plot.title=element_text(hjust=0.8, vjust=-0.2))#margin=margin(l=50,b=-30)))
    # geom_text(x=-165, y=80, label=months[i], size=6)
  
  print(p)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.png'))
  
  
  blue_month_coarse = blue_all_months_coarse[[month]]
  vname = sprintf('bs%02d', i)

  # plot(blue_month)

  p_coarse <- ggplot() + 
    # facet_wrap(~variable) +
    # scale_fill_continuous(na.value='white') +
    geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
    geom_spatraster(data=blue_month_coarse, alpha=0.8) +
    # geom_tile(aes(fill = value)) +
    # geom_raster(aes(fill = value)) +
    #geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    # scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    cols_fill + 
    # scale_fill_brewer(palette='Blues')+#, na.value='transparent', name = "Albedo") +
    theme_bw(14) + 
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+
  # xlab('longitude') +
  # ylab('latitude') #+
  # ggtitle(months[month]) #+
  # theme(plot.title=element_text(hjust=0.8, vjust=-0.2))#margin=margin(l=50,b=-30)))
  # geom_text(x=-165, y=80, label=months[i], size=6)
  

  print(p_coarse)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.png'))
  
  # 
  # # foo = terra::extract(blue_month, lct_spat)
  bs_df_coarse[,month] <- terra::extract(blue_month_coarse, lct_spat)[,month]
  
  bs_df_point[,month] <- terra::extract(blue_month, lct_spat_point)[,month]
  
  # foo = terra::extract(blue_month, lct_spat)
  bs_df[,month] <- terra::extract(blue_month, lct_spat)[,month]
  
}
# dev.off()

bs_df[bs_df==0] = 1e-4
bs_df_coarse[bs_df_coarse==0] = 1e-4
bs_df_point[bs_df_point==0] = 1e-4

lct_bs = data.frame(lct_modern, bs_df)
lct_bs_coarse = data.frame(lct_modern, bs_df_coarse)
lct_bs_point = data.frame(lct_modern_point, bs_df_point)


saveRDS(lct_bs, 'data/calibration_modern_lct_bluesky.RDS')
saveRDS(lct_bs_coarse, 'data/calibration_modern_lct_bluesky_coarse.RDS')
saveRDS(lct_bs_point, 'data/calibration_modern_lct_bluesky_point.RDS')

lct_bs_melt = melt(lct_bs, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
lct_bs_coarse_melt = melt(lct_bs_coarse, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))

lct_bs_merged = merge(lct_bs_melt, lct_bs_coarse_melt, by = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'variable'))

ggplot(data = lct_bs_merged) +
  geom_point(aes(x=value.x, y=value.y), alpha=0.6) +
  geom_abline(intercept=0, slope=1, colour='red', lwd=1, alpha=0.5) +
  xlab('albedo resampled coarse') +
  ylab('albedo native') +
  theme_bw(18) +
  coord_fixed() +
  xlim(c(0,0.8)) +
  ylim(c(0,0.8))
ggsave('figures/albedo_native_vs_coarse_scatter.pdf')
ggsave('figures/albedo_native_vs_coarse_scatter.png')

cor(lct_bs_merged$value.x, lct_bs_merged$value.y, use = 'complete.obs')


############################################################################################
#  interp
############################################################################################

# lct_modern = readRDS('data/lct_modern.RDS')
lct_interp_modern = readRDS('data/lct_modern_reveals_interp.RDS')

longitude_interp = lct_interp_modern[,c('x')]
latitude_interp = lct_interp_modern[,c('y')]
lonlat_interp = cbind(longitude_interp, latitude_interp)

lct_interp_spat = vect(lonlat_interp, 
                crs  = crs("+init=epsg:4326"), 
                atts = lct_interp_modern[,3:ncol(lct_interp_modern)])

# # do we need to do this?
# # lct_modern = readRDS('data/lct_modern.RDS')
# lct_modern_point = readRDS('data/lct_modern_reveals_point.RDS')
# 
# longitude_point = lct_modern_point[,c('long')]
# latitude_point = lct_modern_point[,c('lat')]
# lonlat_point = cbind(longitude_point, latitude_point)
# 
# lct_spat_point = vect(lonlat_point,
#                       crs  ="+init=epsg:4326",
#                       atts = lct_modern_point[,5:ncol(lct_modern_point)])

bs_interp_df = data.frame(matrix(NA, nrow=nrow(lct_interp_spat), ncol=12))
colnames(bs_interp_df) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

bs_interp_df_coarse = data.frame(matrix(NA, nrow=nrow(lct_interp_spat), ncol=12))
colnames(bs_interp_df_coarse) = paste0('bs', c(paste0('0', seq(1, 9)), seq(10,12)))
colnames(bs_interp_df_coarse) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')


# bs_df_point = data.frame(matrix(NA, nrow=nrow(lct_spat_point), ncol=12))
# colnames(bs_df_point) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

grid = rast(grid)

blue_all_months = rast('data/blue_sky_monthly_2000-2009.tif')
# blue_coarse_interp = resample(blue_all_months, grid)
blue_all_months_coarse = resample(blue_all_months, grid, method="average")

# 
names(blue_all_months) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
names(blue_all_months_coarse) = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

blue_all_months_coarse = crop(blue_all_months_coarse, ext(pbs_ll))

ggplot() +
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
  geom_spatraster(data=blue_all_months, alpha=0.8) +
  scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") + 
  theme_bw() + 
  theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_native.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_native.png')

ggplot() +
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
  geom_spatraster(data=blue_all_months_coarse, alpha=0.8) +
  scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") + 
  theme_bw() + 
  theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
  facet_wrap(~lyr)
ggsave('figures/albedo_maps_monthly_bluesky_coarse.pdf')
ggsave('figures/albedo_maps_monthly_bluesky_coarse.png')

# lct_spat = cbind(lct_spat, bs_df)
# pdf('figures/monthly_BLUESKY_maps.pdf')

cols_fill = scale_fill_gradientn(colours=terrain.colors(10), 
                                 na.value='transparent', 
                                 name = "Albedo") 

cols_fill = scale_fill_brewer(palette="YlOrBr", 
                              direction = -1,
                                 na.value='transparent', 
                                 name = "Albedo") 

for(i in 1:length(months)) {
  
  month = months[i]
  # month = i
  
  blue_month = blue_all_months[[month]]
  vname = sprintf('bs%02d', i)
  
  # plot(blue_month)
  
  p <- ggplot() + 
    # facet_wrap(~variable) +
    # scale_fill_continuous(na.value='white') +
    geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
    geom_spatraster(data=blue_month, alpha=0.8) +
    # geom_tile(aes(fill = value)) +
    # geom_raster(aes(fill = value)) +
    #geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    # scale_fill_brewer(palette='Blues')+#, na.value='transparent', name = "Albedo") +
    theme_bw() + 
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+
  # xlab('longitude') +
  # ylab('latitude') #+
  # ggtitle(months[month]) #+
  # theme(plot.title=element_text(hjust=0.8, vjust=-0.2))#margin=margin(l=50,b=-30)))
  # geom_text(x=-165, y=80, label=months[i], size=6)
  
  print(p)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky.png'))
  
  
  blue_month_coarse = blue_all_months_coarse[[month]]
  vname = sprintf('bs%02d', i)
  
  # plot(blue_month)
  
  p_coarse <- ggplot() + 
    # facet_wrap(~variable) +
    # scale_fill_continuous(na.value='white') +
    geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey50") +
    geom_spatraster(data=blue_month_coarse, alpha=0.8) +
    # geom_tile(aes(fill = value)) +
    # geom_raster(aes(fill = value)) +
    #geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    # scale_fill_brewer(palette='Blues')+#, na.value='transparent', name = "Albedo") +
    theme_bw() + 
    theme(axis.text.x= element_blank(), axis.ticks = element_blank(), axis.title = element_blank())#+
  # xlab('longitude') +
  # ylab('latitude') #+
  # ggtitle(months[month]) #+
  # theme(plot.title=element_text(hjust=0.8, vjust=-0.2))#margin=margin(l=50,b=-30)))
  # geom_text(x=-165, y=80, label=months[i], size=6)
  
  
  print(p_coarse)
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.pdf'))
  ggsave(paste0('figures/albedo_maps_', month, '_bluesky_coarse.png'))
  
  # 
  # # foo = terra::extract(blue_month, lct_spat)
  bs_interp_df_coarse[,month] <- terra::extract(blue_month_coarse, lct_interp_spat)[,month]
  
  # bs_interp_df_point[,month] <- terra::extract(blue_month, lct_interp_spat_point)[,month]
  
  # foo = terra::extract(blue_month, lct_spat)
  bs_interp_df[,month] <- terra::extract(blue_month, lct_interp_spat)[,month]
  
}
# dev.off()

bs_interp_df[bs_interp_df==0] = 1e-4
bs_interp_df_coarse[bs_interp_df_coarse==0] = 1e-4
# bs_df_point[bs_df_point==0] = 1e-4

lct_interp_bs = data.frame(lct_interp_modern, bs_interp_df)
lct_interp_bs_coarse = data.frame(lct_interp_modern, bs_interp_df_coarse)
# lct_bs_point = data.frame(lct_modern_point, bs_interp_df_point)


saveRDS(lct_interp_bs, 'data/calibration_modern_lct_interp_bluesky.RDS')
saveRDS(lct_interp_bs_coarse, 'data/calibration_modern_lct_interp_bluesky_coarse.RDS')
# saveRDS(lct_bs_point, 'data/calibration_modern_lct_bluesky_point.RDS')

# lct_interp_bs_melt = melt(lct_interp_bs, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
# lct_interp_bs_coarse_melt = melt(lct_interp_bs_coarse, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
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
