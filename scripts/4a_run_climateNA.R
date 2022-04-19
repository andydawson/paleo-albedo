# library(raster)
# library(ncdf4)
library(sp)
library(elevatr)
library(rgdal)
library(reshape2)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


pm = readRDS('data/lct_modern.RDS')

# #this changes back to regular lat long
# #assigning a crs to the pollen coordinates
# spdf <- SpatialPointsDataFrame(coords = pm[,c('x','y')], 
#                                data = pm,
#                                proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
# 
# pm_t = spTransform(spdf, CRS("+init=epsg:4326"))
# 
# ll = data.frame(coordinates(pm_t))
# 
# # +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# proj = '+init=epsg:4326 +proj=longlat +ellps=WGS84
# +datum=WGS84 +no_defs +towgs84=0,0,0'
# elev = get_elev_point(ll, 
#                       prj=proj, 
#                       src="aws")

site_meta = data.frame(ID1=paste0('site', seq(1,nrow(pm))),
                       ID2=rep('region1',nrow(pm)), 
                       lat = pm[,'lat'],
                       long = pm[,'long'],
                       el = pm[,5])
colnames(site_meta) = c('ID1', 'ID2', 'lat', 'long', 'el')

site_meta$long = -site_meta$long

write.csv(site_meta, 'data/climateNA_input.csv', row.names = FALSE, quote=FALSE, na='.', eol = "\r\n")


# tmin_month_pm = melt(tmin_month_pm)
# colnames(tmin_month_pm) = c('site', 'year', 'tmin')
# 
# tmin_pm = rbind(tmin_pm, 
#                 data.frame(month = i, tmin_month_pm))
# tmax_pm = data.frame(month=numeric(0),
#                      site=numeric(0),
#                      year=character(0),
#                      tmax=numeric(0))
# 
# tmin_pm = data.frame(month=numeric(0),
#                      site=numeric(0),
#                      year=character(0),
#                      tmin=numeric(0))
# 
# ppt_pm = data.frame(month=numeric(0),
#                     site=numeric(0),
#                     year=character(0),
#                     ppt=numeric(0))

# clim = read.csv('data/climateNA_1901-2019.csv', stringsAsFactors = FALSE)
clim = read.csv('data/climateNA_1901-2019MP.csv', stringsAsFactors = FALSE)

ppt = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='PPT')])
colnames(ppt) = c('site', 'year', seq(1, 12))
ppt_m = melt(ppt, id.vars=c('site', 'year'))
colnames(ppt_m) = c('site', 'year', 'month', 'ppt')
ppt_m = ppt_m[, c('month', 'site', 'year', 'ppt')]
write.csv(ppt_m, 'data/ppt_CRU.csv', row.names=FALSE)

tmean = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='Tav')])
colnames(tmean) = c('site', 'year', seq(1, 12))
tmean_m = melt(tmean, id.vars=c('site', 'year'))
colnames(tmean_m) = c('site', 'year', 'month', 'tmean')
tmean_m = tmean_m[, c('month', 'site', 'year', 'tmean')]
write.csv(tmean_m, 'data/tmean_CRU.csv', row.names=FALSE)

tmin = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tmi')])
colnames(tmin) = c('site', 'year', seq(1, 12))
tmin_m = melt(tmin, id.vars=c('site', 'year'))
colnames(tmin_m) = c('site', 'year', 'month', 'tmin')
tmin_m = tmin_m[, c('month', 'site', 'year', 'tmin')]
write.csv(tmin_m, 'data/tmin_CRU.csv', row.names=FALSE)

tmax = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tma')])
colnames(tmax) = c('site', 'year', seq(1, 12))
tmax_m = melt(tmax, id.vars=c('site', 'year'))
colnames(tmax_m) = c('site', 'year', 'month', 'tmax')
tmax_m = tmax_m[, c('month', 'site', 'year', 'tmax')]
write.csv(tmax_m, 'data/tmax_CRU.csv', row.names=FALSE)

