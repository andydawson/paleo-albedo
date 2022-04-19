library(raster)
library(ncdf4)
library(sp)


# length(seq(22000, 0, by=-10))

# In both paleoclimatic simulations, the timescale is expressed as time before present, where ‘present’
# follows radiocarbon dating conventions and is deﬁned as 0 ka BP (1950). Decade 0 is deﬁned as from 
# 1st January 1951 to 31st December 1960. CCSM3 data extends from decade −2200 to +3, which means it
# ends on December 31st, 1990,

ccsm3_bins = c(-3, -2, -1, seq(0, 2000, by=1))

paleo_bins = seq(100, 2000, by=200)

# ccsm3_bins[which(ccsm3_bins %in% paleo_bins)]

idx_keep = which(ccsm3_bins %in% paleo_bins)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


lct_paleo = readRDS('data/lct_paleo.RDS')

sites = sort(unique(lct_paleo$site))
N_sites = length(sites)

lct_paleo = lct_paleo[!duplicated(lct_paleo$site),]

lct_paleo_spatial = SpatialPointsDataFrame(coords=lct_paleo[,c('x', 'y')], 
                                           data=lct_paleo, 
                                           proj4string = crs(alb_proj))

# # get temps
# tmax_pm = data.frame(matrix(NA, nrow(lct_paleo_spatial), 12))
# tmin_pm = data.frame(matrix(NA,  nrow(lct_paleo_spatial), 12))
# ppt_pm = data.frame(matrix(NA,  nrow(lct_paleo_spatial), 12))
# 
# for (i in 1:12){
#   
#   # tmax_month = raster('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
#   tmax_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
#   tmax_month_alb = projectRaster(tmax_month$X3, crs = crs(alb_proj))
#   tmax_month_pm = raster::extract(tmax_month_alb, lct_paleo_spatial)
#   tmax_pm[,i] = tmax_month_pm
#   
#   tmin_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmin')
#   tmin_month_alb = projectRaster(tmax_month$X3, crs = crs(alb_proj))
#   tmin_month_pm = raster::extract(tmin_month_alb, lct_paleo_spatial)
#   tmin_pm[,i]   = tmin_month_pm
#   
#   ppt_month = brick('data/GCM/ccsm3_22-0k_prcp.nc', level=i, varname='prcp')
#   ppt_month_alb = projectRaster(ppt_month$X3, crs = crs(alb_proj))
#   ppt_month_pm = raster::extract(ppt_month_alb, lct_paleo_spatial)
#   ppt_pm[,i]   = ppt_month_pm
#   
# }
# 
# tmax_pm = data.frame(df_pm[,c('x', 'y')], tmax_pm)
# tmin_pm = data.frame(df_pm[,c('x', 'y')], tmin_pm)
# ppt_pm  = data.frame(df_pm[,c('x', 'y')], ppt_pm)
# 
# saveRDS(tmax_pm, 'data/tmax_pm.RDS')
# saveRDS(tmin_pm, 'data/tmin_pm.RDS')
# saveRDS(ppt_pm, 'data/ppt_pm.RDS')

library(reshape2)

# get temps
# tmax_pm = data.frame(matrix(NA, nrow(df_pm), 12))
# tmin_pm = data.frame(matrix(NA,  nrow(df_pm), 12))
# ppt_pm = data.frame(matrix(NA,  nrow(df_pm), 12))

tmax_pm = data.frame(month=numeric(0),
                     lat=numeric(0),
                     long=numeric(0),
                     year=character(0),
                     tmax=numeric(0))

tmin_pm = data.frame(month=numeric(0),
                     lat=numeric(0),
                     long=numeric(0),
                     year=character(0),
                     tmax=numeric(0))

ppt_pm = data.frame(month=numeric(0),
                     lat=numeric(0),
                     long=numeric(0),
                     year=character(0),
                     tmax=numeric(0))

for (i in 1:12){
  
  for (k in 1:length(idx_keep)){
    
    # lct_paleo_sub = lct_paleo[which(lct_paleo$cut == k),]
    # 
    # lct_paleo_spatial_sub = SpatialPointsDataFrame(coords=lct_paleo_sub[,c('x', 'y')], 
    #                                            data=lct_paleo_sub, 
    #                                            proj4string = crs(alb_proj))
    
    # tmax_month = raster('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
    tmax_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmax')
    tmax_month_alb = projectRaster(tmax_month[[idx_keep[k]]], crs = crs(alb_proj))
    # tmax_month_alb = projectRaster(tmax_month, crs = crs(alb_proj))
    tmax_month_pm = raster::extract(tmax_month_alb, lct_paleo_spatial)
    
    if (any(is.na(tmax_month_pm))){
      
      tmax_month_pm[which(is.na(tmax_month_pm))] = raster::extract(tmax_month_alb, 
                                                                   lct_paleo_spatial[which(is.na(tmax_month_pm)),], 
                                                                   buffer=350000, 
                                                                   fun=mean)
      
    }
    
    
    tmax_month_pm = melt(tmax_month_pm)
    tmax_month_pm = data.frame(lat=lct_paleo_spatial$lat, 
                               long=lct_paleo_spatial$long, 
                               year=k, 
                               tmax=tmax_month_pm$value)
    tmax_pm = rbind(tmax_pm, 
                    data.frame(month = i, tmax_month_pm))
    
    tmin_month = brick('data/GCM/ccsm3_22-0k_temp.nc', level=i, varname='tmin')
    tmin_month_alb = projectRaster(tmin_month[[idx_keep[k]]], crs = crs(alb_proj))
    tmin_month_pm = raster::extract(tmin_month_alb, lct_paleo_spatial)
    
    if (any(is.na(tmin_month_pm))){
      tmin_month_pm[which(is.na(tmin_month_pm))] = raster::extract(tmin_month_alb, 
                                                                   lct_paleo_spatial[which(is.na(tmin_month_pm)),], 
                                                                   buffer=350000, 
                                                                   fun=mean)
      
    }
    
    tmin_month_pm = melt(tmin_month_pm)
    tmin_month_pm = data.frame(lat=lct_paleo_spatial$lat, 
                               long=lct_paleo_spatial$long, 
                               year=k, 
                               tmin=tmin_month_pm$value)
    tmin_pm = rbind(tmin_pm,
                    data.frame(month = i, tmin_month_pm))

    ppt_month = brick('data/GCM/ccsm3_22-0k_prcp.nc', level=i, varname='prcp')
    ppt_month_alb = projectRaster(ppt_month[[idx_keep[k]]], crs = crs(alb_proj))
    ppt_month_pm = raster::extract(ppt_month_alb, lct_paleo_spatial)
    
    if (any(is.na(ppt_month_pm))){
      ppt_month_pm[which(is.na(ppt_month_pm))] = raster::extract(ppt_month_alb, 
                                                                 lct_paleo_spatial[which(is.na(ppt_month_pm)),], 
                                                                   buffer=350000, 
                                                                   fun=mean)
      
    }
    
    ppt_month_pm = melt(ppt_month_pm)
    ppt_month_pm = data.frame(lat=lct_paleo_spatial$lat, 
                               long=lct_paleo_spatial$long, 
                               year=k, 
                               ppt=ppt_month_pm$value)
    ppt_pm = rbind(ppt_pm,
                   data.frame(month = i, ppt_month_pm))

  }
}

# tmax_pm = data.frame(df_pm[,c('x', 'y')], tmax_pm)
# tmin_pm = data.frame(df_pm[,c('x', 'y')], tmin_pm)
# ppt_pm  = data.frame(df_pm[,c('x', 'y')], ppt_pm)

write.csv(tmax_pm, 'data/tmax_GCM.csv', row.names = FALSE)
write.csv(tmin_pm, 'data/tmin_GCM.csv', row.names = FALSE)
write.csv(ppt_pm, 'data/ppt_GCM.csv', row.names = FALSE)
