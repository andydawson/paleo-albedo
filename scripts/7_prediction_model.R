library(raster)
library(tidyr)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(mgcv)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# lct_paleo = readRDS('data/lct_paleo.RDS')
lct_paleo = readRDS('data/lct_paleo_reveals.RDS')
colnames(lct_paleo)[1] = 'year'

# lct_paleo = lct_paleo[which(lct_paleo$month == 4),]

gdd = read.csv('data/gdd_GCM.csv', stringsAsFactors = FALSE)

gdd_season = gdd %>%
  group_by(lat, long, year) %>%
  summarize(dd5_s = sum(gdd), .groups="keep")


tmin = read.csv('data/tmin_GCM.csv', stringsAsFactors = FALSE)

tmin = tmin[which(tmin$month==4),]

lct_paleo = merge(lct_paleo, gdd_season, by=c('lat', 'long', 'year'))
lct_paleo = merge(lct_paleo, tmin, by=c('lat', 'long', 'year'))



# thorn_paleo = readRDS('data/thorn_output_paleo.RDS')
# 
# thorn_paleo$snowpack = thorn_paleo$snow
# 
# # snow_mean_sm2 = thorn_out_sm2 %>%
# #   group_by(site, month) %>%
# #   summarise(snowpack = median(snow), tsnow=mean(snowcover), et=mean(et), ppt=median(P), tmin=median(Tn), .groups='keep')
# # colnames(snow_mean_sm2) = c('site', 'month', 'snowpack_sm2', 'tsnow_sm2', 'et', 'ppt', 'tmin')
# 
# 
# 
# lct_paleo = merge(lct_paleo, thorn_paleo)
# lct_paleo = lct_paleo[which(lct_paleo$mont == 4),]


cal_model = readRDS('data/calibration_model.RDS')


predict_paleo = predict.gam(cal_model, 
                           newdata = lct_paleo , 
                           type    = 'response')
lct_preds = data.frame(lct_paleo, alb_pred = predict_paleo)  

saveRDS(lct_preds,'data/lct_paleo_preds.RDS')


















