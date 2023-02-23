library(sp)
library(raster)

lct_modern = readRDS('data/lct_modern.RDS')

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_spatial = SpatialPointsDataFrame(coords=lct_modern[,c('x', 'y')], data=lct_modern, proj4string = CRS(alb_proj))

for(m in 1:12) {
  palb = raster(sprintf('data/ABOVE/albedo/new-month-%02d.tif', m))
  lct_spatial[[sprintf('fine-alb%02d', m)]] <- raster::extract(palb, lct_spatial)/1000
  
  idx_missing = which(is.na(lct_spatial[[sprintf('fine-alb%02d', m)]]))
  
  if (!is.null(idx_missing)){
    lct_missing = SpatialPointsDataFrame(coords=lct_modern[idx_missing,c('x', 'y')], 
                                         data=lct_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    lct_spatial[[sprintf('fine-alb%02d', m)]][idx_missing] <- raster::extract(palb, lct_missing, buffer=25000, fun=mean)/1000
  }
  
  
  idx_missing = which(is.na(lct_spatial[[sprintf('fine-alb%02d', m)]]))
  
  if (!is.null(idx_missing)){
    lct_missing = SpatialPointsDataFrame(coords=lct_modern[idx_missing,c('x', 'y')], 
                                         data=lct_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    lct_spatial[[sprintf('fine-alb%02d', m)]][idx_missing] <- raster::extract(palb, lct_missing, buffer=50000, fun=mean)/1000
  }
  
  idx_missing = which(is.na(lct_spatial[[sprintf('fine-alb%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_modern[idx_missing,c('x', 'y')], 
                                         data=lct_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    lct_spatial[[sprintf('fine-alb%02d', m)]][idx_missing] <- raster::extract(palb, lct_missing, buffer=100000, fun=mean)/1000
  }
  
  idx_missing = which(is.na(lct_spatial[[sprintf('fine-alb%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_modern[idx_missing,c('x', 'y')], 
                                         data=lct_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    lct_spatial[[sprintf('fine-alb%02d', m)]][idx_missing] <- raster::extract(palb, lct_missing, buffer=250000, fun=mean)/1000
  }
  
  
  idx_missing = which(is.na(lct_spatial[[sprintf('fine-alb%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_modern[idx_missing,c('x', 'y')], 
                                         data=lct_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    lct_spatial[[sprintf('fine-alb%02d', m)]][idx_missing] <- raster::extract(palb, lct_missing, buffer=375000, fun=mean)/1000
  }
  
}

lct_albedo = data.frame(lct_spatial)
# lct_albedo$missing = 0
# lct_albedo$missing[1] = 1
# lct_albedo$missing[160] = 1
# # lct_albedo$missing[idx_missing] = 1

#removing extra coordinates not necessary 
lct_albedo = lct_albedo[,!(colnames(lct_albedo) %in% c('x.1', 'y.1', 'optional'))]

saveRDS(lct_albedo, 'data/lct_albedo_modern.RDS')

# 
# ggplot(data=lct_albedo) +
#   geom_point(aes(x=x, y=y, colour=missing))


library(sp)
library(raster)


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_albedo_modern = readRDS('data/lct_albedo_modern.RDS')

snow_spatial = SpatialPointsDataFrame(coords=lct_albedo_modern[,c('x', 'y')], 
                                      data=lct_albedo_modern, 
                                      proj4string = crs(alb_proj))

for(m in 1:12) {
  psnow = raster(sprintf('data/ABOVE/snow/snow-month-%02d.tif', m))
  snow_spatial[[sprintf('psnow%02d', m)]] <- raster::extract(psnow, snow_spatial)
  
  idx_missing = which(is.na(snow_spatial[[sprintf('psnow%02d', m)]]))
  
  if (!is.null(idx_missing)){
    lct_missing = SpatialPointsDataFrame(coords=lct_albedo_modern[idx_missing,c('x', 'y')], 
                                         data=lct_albedo_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    snow_spatial[[sprintf('psnow%02d', m)]][idx_missing] <- raster::extract(psnow, lct_missing, buffer=25000, fun=mean, na.rm=TRUE)/1000
  }
  
  
  idx_missing = which(is.na(snow_spatial[[sprintf('psnow%02d', m)]]))
  
  if (!is.null(idx_missing)){
    lct_missing = SpatialPointsDataFrame(coords=lct_albedo_modern[idx_missing,c('x', 'y')], 
                                         data=lct_albedo_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    snow_spatial[[sprintf('psnow%02d', m)]][idx_missing] <- raster::extract(psnow, lct_missing, buffer=50000, fun=mean, na.rm=TRUE)/1000
  }
  
  idx_missing = which(is.na(snow_spatial[[sprintf('psnow%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_albedo_modern[idx_missing,c('x', 'y')], 
                                         data=lct_albedo_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    snow_spatial[[sprintf('psnow%02d', m)]][idx_missing] <- raster::extract(psnow, lct_missing, buffer=100000, fun=mean, na.rm=TRUE)/1000
  }
  
  idx_missing = which(is.na(snow_spatial[[sprintf('psnow%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_albedo_modern[idx_missing,c('x', 'y')], 
                                         data=lct_albedo_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    snow_spatial[[sprintf('psnow%02d', m)]][idx_missing] <- raster::extract(psnow, 
                                                                            lct_missing, 
                                                                            buffer=250000, 
                                                                            fun=mean,
                                                                            na.rm = TRUE)/1000
  }
  
  idx_missing = which(is.na(snow_spatial[[sprintf('psnow%02d', m)]]))
  
  if (!is.null(idx_missing)){
    
    lct_missing = SpatialPointsDataFrame(coords=lct_albedo_modern[idx_missing,c('x', 'y')], 
                                         data=lct_albedo_modern[idx_missing,], 
                                         proj4string = CRS(alb_proj))
    
    snow_spatial[[sprintf('psnow%02d', m)]][idx_missing] <- raster::extract(psnow, 
                                                                            lct_missing, 
                                                                            buffer=375000, 
                                                                            fun=mean,
                                                                            na.rm = TRUE)/1000
  }
  
}

#After this, the df_pm dataframe will have new columns named psnow01, psnow02, ..., psnow12 for each month.

lct_albedo_snow_modern = data.frame(snow_spatial)

lct_albedo_snow_modern = lct_albedo_snow_modern[,!(colnames(lct_albedo_snow_modern) %in% c('x.1', 'y.1', 'optional'))]

saveRDS(lct_albedo_snow_modern, 'data/lct_albedo_snow_modern.RDS')
