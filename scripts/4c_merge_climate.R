library(ClimClass)
library(sp)
library(rgdal)
library(dplyr)
library(geosphere)
library(reshape2)
library(tidyr)
library(ggplot2)


source('scripts/thornthwaite.R')

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +datum=NAD83'

# CRS("+init=EPSG:4326")
proj = '+init=epsg:4326 +proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0'

# lct_albedo_snow_modern = readRDS('data/lct_albedo_snow_modern.RDS')
lct_albedo_snow_modern = readRDS('data/lct_albedo_snow_modern_glob.RDS')

tmax = read.csv('data/tmax_CRU.csv', stringsAsFactors = FALSE)
tmax = tmax[which((tmax$year>=2000)&(tmax$year<=2017)),]
tmin = read.csv('data/tmin_CRU.csv', stringsAsFactors = FALSE)
tmin = tmin[which((tmin$year>=2000)&(tmin$year<=2017)),]
ppt  = read.csv('data/ppt_CRU.csv', stringsAsFactors = FALSE)
ppt = ppt[which((ppt$year>=2000)&(ppt$year<=2017)),]
pas  = read.csv('data/pas_CRU.csv', stringsAsFactors = FALSE)
pas = pas[which((pas$year>=2000)&(pas$year<=2017)),]

pas_season  = read.csv('data/pas_season_CRU.csv', stringsAsFactors = FALSE)
pas_season = pas_season[which((pas_season$year>=2000)&(pas_season$year<=2017)),]

dd0  = read.csv('data/dd0_CRU.csv', stringsAsFactors = FALSE)
dd0  = dd0[which((dd0$year>=2000)&(dd0$year<=2017)),]
dd0_season  = read.csv('data/dd0_season_CRU.csv', stringsAsFactors = FALSE)
dd0_season  = dd0_season[which((dd0_season$year>=2000)&(dd0_season$year<=2017)),]

dd5  = read.csv('data/dd5_CRU.csv', stringsAsFactors = FALSE)
dd5  = dd5[which((dd5$year>=2000)&(dd5$year<=2017)),]
dd5_season  = read.csv('data/dd5_season_CRU.csv', stringsAsFactors = FALSE)
dd5_season  = dd5_season[which((dd5_season$year>=2000)&(dd5_season$year<=2017)),]

ffd  = read.csv('data/ffd_CRU.csv', stringsAsFactors = FALSE)
ffd  = ffd[which((ffd$year>=2000)&(ffd$year<=2017)),]
ffd_season  = read.csv('data/ffd_season_CRU.csv', stringsAsFactors = FALSE)
ffd_season  = ffd_season[which((ffd_season$year>=2000)&(ffd_season$year<=2017)),]


# why won't this join work?!?!
clim = left_join(ppt, tmin)
clim = left_join(clim, tmax)
clim = left_join(clim, pas)
clim = left_join(clim, dd0)
clim = left_join(clim, dd5)
clim = left_join(clim, ffd)


clim = clim[order(clim$site, clim$year, clim$month),]
clim = clim[,c('site', 'year', 'month', 'ppt', 'tmin', 'tmax', 'pas', 'dd0', 'dd5', 'ffd')]
colnames(clim) = c('site', 'year', 'month', 'P', 'Tn', 'Tx', 'S', 'DD0', 'DD5', 'FFD')

write.csv(clim, 'data/climate_CRU.csv', row.names=FALSE)

# why won't this join work?!?!
clim_season = left_join(pas_season, dd0_season)
clim_season = left_join(clim_season, dd5_season)
clim_season = left_join(clim_season, ffd_season)
# clim = left_join(clim, tmax)
# clim = left_join(clim, pas)
# clim = left_join(clim, dd0)

clim_season = clim_season[order(clim_season$site, clim_season$year, clim_season$season),]
clim_season = clim_season[,c('site', 'year', 'season', 'pas', 'dd0', 'dd5', 'ffd')]
# colnames(clim) = c('site', 'year', 'month', 'P', 'Tn', 'Tx', 'S', 'DD0')

write.csv(clim_season, 'data/climate_season_CRU.csv', row.names=FALSE)

