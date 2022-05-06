

# Warning message:
#   In .varName(nc, varname, warn = warn) : varname used is: 
# DHR_VIS
# If that is not correct, you can set it to one of: 
# DHR_VIS, DHR_NIR, DHR_SW, BHR_VIS, BHR_NIR, BHR_SW, DHR_sigmaVIS, DHR_sigmaNIR, 
# DHR_sigmaSW, BHR_sigmaVIS, BHR_sigmaNIR, BHR_sigmaSW, DHR_alpha_VIS_NIR, DHR_alpha_VIS_SW, 
# DHR_alpha_NIR_SW, BHR_alpha_VIS_NIR, BHR_alpha_VIS_SW, BHR_alpha_NIR_SW, Weighted_Number_of_Samples, 
# Goodness_of_Fit, Time_to_the_Closest_Sample, Data_Mask, Snow_Fraction, Solar_Zenith_Angle, Relative_Entropy


library(sp)
library(raster)
library(ncdf4)
library(rgdal)
# library(rhdf5)

# lct_modern = readRDS('data/lct_modern.RDS')
lct_modern = readRDS('data/lct_modern_reveals.RDS')

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_spatial = SpatialPointsDataFrame(coords=lct_modern[,c('long', 'lat')], 
                                     data=lct_modern, 
                                     proj4string = CRS("+init=epsg:4326"))

snow_spatial = SpatialPointsDataFrame(coords=lct_modern[,c('long', 'lat')], 
                                     data=lct_modern, 
                                     proj4string = CRS("+init=epsg:4326"))

for(m in 1:12) {

  bhr_sw  = raster(sprintf('data/GLOBALBEDO/albedo/GlobAlbedo.merge.albedo.005.2004%02d.nc', m), varname='BHR_SW')

  lct_spatial[[sprintf('alb%02d', m)]] <- raster::extract(bhr_sw, lct_spatial)
  
  snow_frac  = raster(sprintf('data/GLOBALBEDO/albedo/GlobAlbedo.merge.albedo.005.2004%02d.nc', m), varname='Snow_Fraction')
  
  snow_spatial[[sprintf('snow%02d', m)]] <- raster::extract(snow_frac, snow_spatial)
  
}


lct_albedo = data.frame(lct_spatial)
lct_albedo = lct_albedo[,!(colnames(lct_albedo) %in% c('long.1', 'lat.1', 'optional'))]


lct_snow = data.frame(snow_spatial)
lct_snow = lct_snow[,!(colnames(lct_snow) %in% c('long.1', 'lat.1', 'optional'))]

lct_both = merge(lct_albedo, lct_snow)

saveRDS(lct_both, 'data/lct_albedo_snow_modern_glob.RDS')

# saveRDS(lct_both, 'data/lct_reveals_albedo_snow_modern_glob.RDS')

