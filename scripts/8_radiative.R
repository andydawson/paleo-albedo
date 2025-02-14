library(sp)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggbreak)

alb_prod = "bluesky"

# months = c('feb', 'may', 'aug', 'nov')
# months_number = c(2, 5, 8, 10)

months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
months_number = seq(1, 12)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ylim = c(12, 82) 
xlim = c(-166, -50) 

ice_fort = readRDS('data/ice_fort.RDS')
ice_fort_diff_young = readRDS('data/ice_fort_diff_young.RDS')
ice_fort_diff_old = readRDS('data/ice_fort_diff_old.RDS')

labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

ages = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)

# ice_fort_diff_old$facets = factor(ice_fort_diff_old$facets,
#                                   levels = labels_period,
#                                   labels = labels_period)
# 
# ice_fort_diff_young$facets = factor(ice_fort_diff_young$facets,
#                                   levels = labels_period,
#                                   labels = labels_period)

ice_fill = 'gainsboro'
ice_colour = 'gray60'

ice_fill_dark = 'ivory3'
ice_colour_dark = 'gray40'

###############################################################################################################
## read data
###############################################################################################################

# map data
pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

# radiative kernel
rk_hadgem = brick('data/radiative-kernels/HadGEM3-GA7.1_TOA_kernel_L19.nc', level=1, varname='albedo_sw_cs')
# 
# rk_cam5 = raster('data/radiative-kernels/CAM5/alb.kernel.nc', varname='FSNSC', band=month)
# 
# rk_cack = raster('data/radiative-kernels/CACKv1.0/CACKv1.0.nc', varname='CACK', level=month, band=3)
# rk_cack = t(rk_cack)


# albedo prediction differences
# alb_diff = readRDS(paste0('data/alb_preds_diffs_', alb_prod, '.RDS'))

# alb_interp_diff = readRDS(paste0('data/alb_interp_preds_diffs_', alb_prod, '.RDS'))
# alb_interp_diff[which(alb_interp_diff$year == 11500)] = 12000
# 
# alb_diff = alb_interp_diff

alb_interp_diff_full = readRDS(paste0('data/alb_interp_preds_full_diffs_', alb_prod, '.RDS'))
# alb_interp_diff_full[which(alb_interp_diff_full$year == 11500)] = 12000

alb_diff = alb_interp_diff_full

# alb_interp_diff_ice_res = readRDS(paste0('data/alb_interp_preds_ice_res_diffs_', alb_prod, '.RDS'))
# alb_interp_diff_ice_res[which(alb_interp_diff_ice_res$year == 11500)] = 12000
# 
# alb_diff = alb_interp_diff_ice_res

###############################################################################################################
## determine radiative forcing
###############################################################################################################

# # unique coordinates across all time periods
# alb_diff_unique = alb_diff[!duplicated(alb_diff[,c('lat', 'long')]),]
# head(alb_diff_unique)

# radiative kernel is WGS84 
# longitude scale: 0 - 360
# alb_diff$long360 = abs(alb_diff$long - 180)
# alb_diff$lat180 = abs(alb_diff$lat + 90)

alb_diff$long360 = 180 + 180 - abs(alb_diff$long)
alb_diff$lat180 = alb_diff$lat + 90

# create spatial object of unique coordingates
alb_diff_spatial = SpatialPointsDataFrame(coords = alb_diff[,c('long360', 'lat')], 
                                          data = alb_diff, 
                                          proj4string = CRS(ll_proj))

# create spatial object of unique coordingates
alb_diff_spatial_cack = SpatialPointsDataFrame(coords = alb_diff[,c('long360', 'lat180')], 
                                               data = alb_diff, 
                                               proj4string = CRS(ll_proj))

# # unique coordinates across all time periods
# alb_diff_unique = alb_diff[!duplicated(alb_diff[,c('lat', 'long')]),]
# head(alb_diff_unique)
# 
# # radiative kernel is WGS84 
# # longitude scale: 0 - 360
# alb_diff_unique$long360 = abs(alb_diff_unique$long - 180)
# alb_diff_unique$lat180 = abs(alb_diff_unique$lat + 90)
# 
# # create spatial object of unique coordingates
# alb_diff_spatial = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat')], 
#                                           data = alb_diff_unique, 
#                                           proj4string = CRS(ll_proj))
# 
# # create spatial object of unique coordingates
# alb_diff_spatial_cack = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat180')], 
#                                                data = alb_diff_unique, 
#                                                proj4string = CRS(ll_proj))

alb_diff$rk_hadgem = NA
alb_diff$rk_cam5 = NA
alb_diff$rk_cack = NA

for (month in months){
  
  print(month)
  month_number = months_number[which(months == month)]
  print(month_number)
  ##
  ## HADGEM
  ##
  
  idx_month = which(alb_diff$month == month)
  
  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_hadgem_month = raster::extract(rk_hadgem[[month_number]], alb_diff_spatial[idx_month,])
  alb_diff$rk_hadgem[idx_month] = rk_hadgem_month 
  
  rk_hadgem_df = as.data.frame(rk_hadgem[[month_number]], xy=TRUE)
  colnames(rk_hadgem_df) = c('x', 'y', 'kernel')
  
  # ggplot() +
  #   geom_raster(data=rk_hadgem_df, aes(x=x, y=y, fill=kernel)) +
  #   geom_point(data=data.frame(alb_diff_spatial), aes(x=long360, y=lat), shape=1, alpha=0.1) +
  #   scale_fill_gradientn(colours = terrain.colors(10))
  
  ##
  ## CAM5
  ##
  
  rk_cam5 = raster('data/radiative-kernels/CAM5/alb.kernel.nc', 
                   varname='FSNSC', 
                   band=month_number)
  
  
  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_cam5_month = raster::extract(rk_cam5, alb_diff_spatial[idx_month,])
  alb_diff$rk_cam5[idx_month] = rk_cam5_month 
  
  rk_cam5_df = as.data.frame(rk_cam5, xy=TRUE)
  colnames(rk_cam5_df) = c('x', 'y', 'kernel')
  
  # ggplot() +
  #   geom_raster(data=rk_cam5_df, aes(x=x, y=y, fill=kernel)) +
  #   geom_point(data=data.frame(alb_diff_spatial), aes(x=long360, y=lat), shape=1, alpha=0.1) +
  #   scale_fill_gradientn(colours = terrain.colors(10))
  # ggsave('figures/check_projection_cam5.pdf')

  ##
  ## CACKv1.0
  ##
  
  # rk_cack = rast('data/radiative-kernels/CACKv1.0/CACKv1.0.nc', 
  #                  varname='CACK', 
  #                  level=month_number, 
  #                  band=3)
  # 
  # 
  # rk_cack = nc_open('data/radiative-kernels/CACKv1.0/CACKv1.0.nc', 
  #                  varname='CACK', 
  #                  level=month_number, 
  #                  band=3)
  
  rk_cack = raster('data/radiative-kernels/CACKv1.0/CACKv1.0.nc', 
                   varname='CACK', 
                   level=month_number, 
                   band=3)
  # rk_cack = t(rk_cack)
  rk_cack =  t(flip((rk_cack)))
  
  # rk_cack_test =  t(flip((rk_cack)))
  # plot(rk_cack_test)
  
  rk_cack_df = as.data.frame(rk_cack, xy=TRUE)
  colnames(rk_cack_df) = c('x', 'y', 'kernel')
  
  # ggplot() +
  #   geom_raster(data=rk_cack_df, aes(x=x, y=y, fill=kernel/100)) +
  #   geom_point(data=data.frame(alb_diff_spatial), aes(x=long360, y=lat180), shape=1, alpha=0.1) +
  #   scale_fill_gradientn(colours = terrain.colors(10))
  # ggsave('figures/check_projection_cack.pdf')
  # 
  # 
  # ggplot() +
  #    geom_raster(data=rk_cack_df, aes(x=x, y=y, fill=layer))
  # 
  # ggplot() +
  #   # geom_raster(data=rk_cack_df, aes(x=x, y=y, fill=layer)) +
  #   geom_point(data=data.frame(alb_diff_spatial_cack), aes(x=long360, y=lat180))

  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_cack_month = raster::extract(rk_cack, alb_diff_spatial_cack[idx_month,])
  alb_diff$rk_cack[idx_month] = rk_cack_month 
  
}

# # merge radiative kernel values with paleo albedo differences data frame
# # note for a given set of coordinates rk fixed through time
# alb_diff = merge(alb_diff, 
#                  alb_diff_unique[,c('lat', 'long', 'month', 'rk_hadgem', 'rk_cam5', 'rk_cack')], 
#                  by=c('lat', 'long', 'month'))

# # calculate radiative forcing
# alb_diff$rf_hadgem = alb_diff$alb_diff*100 * alb_diff$rk_hadgem
# alb_diff$rf_hadgem_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_hadgem
# 
# # calculate radiative forcing
# alb_diff$rf_cam5 = alb_diff$alb_diff*100 * alb_diff$rk_cam5
# alb_diff$rf_cam5_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_cam5
# 
# # calculate radiative forcing
# alb_diff$rf_cack = alb_diff$alb_diff * (-alb_diff$rk_cack)
# alb_diff$rf_cack_ice = alb_diff$alb_diff_ice * (-alb_diff$rk_cack)
# 
# saveRDS(alb_diff, paste0('output/forcing/RF_holocene_full.RDS'))
# # saveRDS(alb_diff, paste0('output/forcing/RF_holocene.RDS'))


alb_diff = alb_diff[which(alb_diff$lat>27),]
alb_diff = alb_diff[which(alb_diff$lat<74),]

alb_diff$rf_hadgem_veg = alb_diff$alb_diff_veg*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_ice = alb_diff$alb_diff_veg_ice*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts*100 * alb_diff$rk_hadgem

# calculate radiative forcing
alb_diff$rf_cam5_veg = alb_diff$alb_diff_veg*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_ice = alb_diff$alb_diff_veg_ice*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts*100 * alb_diff$rk_cam5

# calculate radiative forcing
alb_diff$rf_cack_veg = alb_diff$alb_diff_veg * (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_ice = alb_diff$alb_diff_veg_ice * (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts * (-alb_diff$rk_cack)


saveRDS(alb_diff, paste0('output/forcing/RF_holocene_full.RDS'))

