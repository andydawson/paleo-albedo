library(raster)
library(tidyr)
library(dplyr)
library(rgdal)
library(sp)
library(ggplot2)
library(mgcv)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

lct_paleo = readRDS('data/lct_paleo_reveals.RDS')
colnames(lct_paleo)[1] = 'year'

# lct_paleo = lct_paleo[which(lct_paleo$month == 4),]

cal_model = readRDS('data/calibration_model5.RDS')


predict_paleo = predict.gam(cal_model, 
                           newdata = lct_paleo , 
                           type    = 'response')
lct_preds = data.frame(lct_paleo, alb_pred = predict_paleo)  

saveRDS(lct_preds,'data/lct_paleo_preds.RDS')


