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
# clim = read.csv('data/climateNA_1901-2019MP.csv', stringsAsFactors = FALSE)
clim = read.csv('data/climateNA_output_2000-2019MSY.csv', stringsAsFactors = FALSE)

ppt = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='PPT')][,1:12])
colnames(ppt) = c('site', 'year', seq(1, 12))
ppt_m = melt(ppt, id.vars=c('site', 'year'))
colnames(ppt_m) = c('site', 'year', 'month', 'ppt')
ppt_m = ppt_m[, c('month', 'site', 'year', 'ppt')]
write.csv(ppt_m, 'data/ppt_CRU.csv', row.names=FALSE)

tmean = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='Tav')][,1:12])
colnames(tmean) = c('site', 'year', seq(1, 12))
tmean_m = melt(tmean, id.vars=c('site', 'year'))
colnames(tmean_m) = c('site', 'year', 'month', 'tmean')
tmean_m = tmean_m[, c('month', 'site', 'year', 'tmean')]
write.csv(tmean_m, 'data/tmean_CRU.csv', row.names=FALSE)

tmin = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tmi')][,1:12])
colnames(tmin) = c('site', 'year', seq(1, 12))
tmin_m = melt(tmin, id.vars=c('site', 'year'))
colnames(tmin_m) = c('site', 'year', 'month', 'tmin')
tmin_m = tmin_m[, c('month', 'site', 'year', 'tmin')]
write.csv(tmin_m, 'data/tmin_CRU.csv', row.names=FALSE)

tmax = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                   year = clim[,'Year'], 
                   clim[,which(substr(colnames(clim), 1, 3)=='Tma')][,1:12])
colnames(tmax) = c('site', 'year', seq(1, 12))
tmax_m = melt(tmax, id.vars=c('site', 'year'))
colnames(tmax_m) = c('site', 'year', 'month', 'tmax')
tmax_m = tmax_m[, c('month', 'site', 'year', 'tmax')]
write.csv(tmax_m, 'data/tmax_CRU.csv', row.names=FALSE)

pas = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                  year = clim[,'Year'], 
                  clim[,which(substr(colnames(clim), 1, 3)=='PAS')][,1:12])
colnames(pas) = c('site', 'year', seq(1, 12))
pas_m = melt(pas, id.vars=c('site', 'year'))
colnames(pas_m) = c('site', 'year', 'month', 'pas')
pas_m = pas_m[, c('month', 'site', 'year', 'pas')]
write.csv(pas_m, 'data/pas_CRU.csv', row.names=FALSE)

ffd = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 4)=='NFFD')][,1:12])
colnames(ffd) = c('site', 'year', seq(1, 12))
ffd_m = melt(ffd, id.vars=c('site', 'year'))
colnames(ffd_m) = c('site', 'year', 'season', 'ffd')
ffd_m = ffd_m[, c('season', 'site', 'year', 'ffd')]
write.csv(ffd_m, 'data/ffd_CRU.csv', row.names=FALSE)

pas = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='PAS')][,13:17])
colnames(pas) = c('site', 'year', 'wt', 'sp', 'sm', 'at', 'annual')
pas_m = melt(pas, id.vars=c('site', 'year'))
colnames(pas_m) = c('site', 'year', 'season', 'pas')
pas_m = pas_m[, c('season', 'site', 'year', 'pas')]
write.csv(pas_m, 'data/pas_season_CRU.csv', row.names=FALSE)

dd0 = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 4)=='DD_0')][,1:12])
colnames(dd0) = c('site', 'year', seq(1, 12))
dd0_m = melt(dd0, id.vars=c('site', 'year'))
colnames(dd0_m) = c('site', 'year', 'month', 'dd0')
dd0_m = dd0_m[, c('month', 'site', 'year', 'dd0')]
write.csv(dd0_m, 'data/dd0_CRU.csv', row.names=FALSE)

dd5 = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='DD5')][,1:12])
colnames(dd5) = c('site', 'year', seq(1, 12))
dd5_m = melt(dd5, id.vars=c('site', 'year'))
colnames(dd5_m) = c('site', 'year', 'month', 'dd5')
dd5_m = dd5_m[, c('month', 'site', 'year', 'dd5')]
write.csv(dd5_m, 'data/dd5_CRU.csv', row.names=FALSE)


dd0 = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 4)=='DD_0')][,13:17])
colnames(dd0) = c('site', 'year', 'wt', 'sp', 'sm', 'at', 'annual')
dd0_m = melt(dd0, id.vars=c('site', 'year'))
colnames(dd0_m) = c('site', 'year', 'season', 'dd0')
dd0_m = dd0_m[, c('season', 'site', 'year', 'dd0')]
write.csv(dd0_m, 'data/dd0_season_CRU.csv', row.names=FALSE)

dd5 = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 3)=='DD5')][,13:17])
colnames(dd5) = c('site', 'year', 'wt', 'sp', 'sm', 'at', 'annual')
dd5_m = melt(dd5, id.vars=c('site', 'year'))
colnames(dd5_m) = c('site', 'year', 'season', 'dd5')
dd5_m = dd5_m[, c('season', 'site', 'year', 'dd5')]
write.csv(dd5_m, 'data/dd5_season_CRU.csv', row.names=FALSE)

ffd = data.frame(site = substr(clim[,c('ID1')], 5, 8), 
                 year = clim[,'Year'], 
                 clim[,which(substr(colnames(clim), 1, 4)=='NFFD')][,13:17])
colnames(ffd) = c('site', 'year', 'wt', 'sp', 'sm', 'at', 'annual')
ffd_m = melt(ffd, id.vars=c('site', 'year'))
colnames(ffd_m) = c('site', 'year', 'season', 'ffd')
ffd_m = ffd_m[, c('season', 'site', 'year', 'ffd')]
write.csv(ffd_m, 'data/ffd_season_CRU.csv', row.names=FALSE)


