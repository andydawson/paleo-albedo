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

# lct_modern = readRDS('data/lct_modern.RDS')
lct_modern = readRDS('data/lct_modern_reveals.RDS')

longitude = lct_modern[,c('long')]
latitude = lct_modern[,c('lat')]
lonlat = cbind(longitude, latitude)

lct_spat = vect(lonlat, 
                crs  ="+init=epsg:4326", 
                atts = lct_modern[,5:ncol(lct_modern)])

bs_df = data.frame(matrix(NA, nrow=nrow(lct_spat), ncol=12))
colnames(bs_df) = paste0('bs', c(paste0('0', seq(1, 9)), seq(10,12)))


blue_all_months = rast('/scratch/blue-sky/blue_sky_monthly_2000-2009.tif')


# lct_spat = cbind(lct_spat, bs_df)
pdf('figures/monthly_BLUESKY_maps.pdf')
for(month in 1:12) {

  blue_month = blue_all_months[[month]]
  vname = sprintf('bs%02d', month)
  
  # plot(blue_month)
  
  p <- gplot(blue_month) + 
    # facet_wrap(~variable) +
    # scale_fill_continuous(na.value='white') +
    geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    geom_tile(aes(fill = value)) +
    # geom_raster(aes(fill = value)) +
    #geom_path(data=pbs_ll, aes(long,lat, group = group), color="lightgrey") +
    scale_fill_gradientn(colours=terrain.colors(10), na.value='transparent', name = "Albedo") +
    theme_bw(16) +
    coord_equal() +
    xlab('longitude') +
    ylab('latitude') +
    # ggtitle(months[month]) #+
    # theme(plot.title=element_text(hjust=0.8, vjust=-0.2))#margin=margin(l=50,b=-30)))
    geom_text(x=-165, y=80, label=months[month], size=6)
  
  print(p)
  
  # foo = terra::extract(blue_month, lct_spat)
  bs_df[,month] <- terra::extract(blue_month, lct_spat)$median
  
}
dev.off()

bs_df[bs_df==0] = 1e-4

lct_both = data.frame(lct_modern, bs_df)

saveRDS(lct_both, 'data/calibration_modern_lct_bluesky.RDS')
