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

# why won't this join work?!?!
clim = left_join(ppt, tmin)
clim = left_join(clim, tmax)

clim = clim[order(clim$site, clim$year, clim$month),]
clim = clim[,c('site', 'year', 'month', 'ppt', 'tmin', 'tmax')]
colnames(clim) = c('site', 'year', 'month', 'P', 'Tn', 'Tx')

write.csv(clim, 'data/climate_CRU.csv', row.names=FALSE)

first_year = min(clim$year)
last_year  = max(clim$year)

sites = unique(clim$site)
N_sites = length(sites)

# try varying snow_melt_coeff
# default value is 1

# sm1: c(0.75, 0.25) 
# sm2: c(0.5, 0.25, 0.25)

# sm1: c(0.75, 0.25) 
# sm2: c(0.5, 0.25, 0.25)
snow_melt_coeff = c(0.3, 0.4, 0.3)
# snow_melt_coeff =  c(0.5, 0.25, 0.25)

Tsnow = 1

thorn = list()
snow = data.frame(month = numeric(0),
                   site = numeric(0), 
                   year = numeric(0), 
                   snow = numeric(0),
                   et = numeric(0))

for (i in 1:N_sites){
  print(i)
  
  site = sites[i]
  dat_sub = clim[which(clim$site == site),2:6]
  if (all(is.na(dat_sub$P))){
    print(i)
    next
  }
  
  normals = climate(dat_sub, 
                    first.yr = first_year, 
                    last.yr  = last_year, 
                    max.perc.missing = 15)
  
  thorn[[i]] <-thornthwaite(series    = dat_sub, 
                            clim_norm = normals,
                            latitude  = lct_albedo_snow_modern[i,'lat'], 
                            first.yr  = first_year, 
                            last.yr   = last_year, 
                            snow_melt_coeff = snow_melt_coeff,
                            Tsnow = Tsnow,
                            TAW = 150)
  
  snow_site = data.frame(site=i, month=as.numeric(seq(1,12)), thorn[[i]]$W_balance$Snowpack)
  snow_site_melt = melt(snow_site, id.vars=c('month', 'site'))
  colnames(snow_site_melt) = c('month', 'site', 'year', 'snow')
  
  et_site = data.frame(site=i, month=as.numeric(seq(1,12)), thorn[[i]]$W_balance$Et0)
  et_site_melt = melt(et_site, id.vars=c('month', 'site'))
  colnames(et_site_melt) = c('month', 'site', 'year', 'et')
  
  
  
  snow_site_melt$year = as.numeric(substr(snow_site_melt$year, 2, 5))
  
  # print(nrow(snow_site_melt))
  
  snow = rbind(snow, 
               data.frame(snow_site_melt, et=et_site_melt$et))
}

# 
# 
# 
# 
# # snow1 = snow
# # snow2 = snow
# snow3 = snow

ggplot()+
  geom_histogram(data = snow, aes(x = snow))

summary(snow$snow)

thresh = 75

snow$snowcover = NA
snow[which(snow$snow >= thresh), 'snowcover'] = 1
snow[which(snow$snow < thresh), 'snowcover'] = 0

snow_site = snow[which(snow$site==1),]
ggplot(data=snow_site) +
  geom_boxplot(aes(x=factor(month), y=snow))
ggplot(data=snow_site) +
  geom_point(aes(x=year, y=snow)) +
  facet_wrap(~month)


snow = merge(snow, clim)

# saveRDS(snow, 'data/thorn_output.RDS')

saveRDS(snow, 'data/thorn_output_glob_sm1.RDS')

# snow2 =snow %>% 
#   group_by('lat', 'long', 'month') %>% 
#   summarize(snow = mean(snowcover))

# snowNy = snow[-c(3,4)]
# pivot_mod = snowNy %>%
#   pivot_wider(names_from = month, values_from = snowcover)

# snow2 = snowNy %>% 
#   group_by(site, month) %>% 
#   summarise(meansnow = mean(snowcover))


# dat_mod = readRDS('data/pollen_modern_pivot.RDS')
# dat_mod = readRDS('data/dat_all_monthly.RDS')
# dat_mod = readRDS('data/snow_pm.RDS')







# 
# # snow3$month = paste0('snowpack', snow3$month)
# snowpack_wide = snow_wide[c(1,2,3)] %>%
#   pivot_wider(names_from = month, values_from = snowpack)



