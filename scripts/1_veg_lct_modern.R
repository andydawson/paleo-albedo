# library(raster)
library(maps)
# library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)

veg_pred = readRDS('data/veg_pred_LC6k.RDS')


breaks = c(-74, 0.1, 0.35, 0.7, seq(1.2, 11.7, by=0.5))
slice_bins = seq(1, 25)
slice_labels = c(50, 200, seq(500, 11500, by=500))


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

taxon2pft = read.csv('data/taxon2LCT_translation_v2.csv')
veg_pred$LCT = taxon2pft$LCT[match(veg_pred$taxon, tolower(taxon2pft$taxon))]

veg_lct = aggregate(meansim ~ dataset_id + long + lat + ages + LCT, veg_pred, sum, na.rm=TRUE)

k = veg_lct

#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = k[,c('long', 'lat')], data = k,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#transforming to the albedo crs
veg_transform = spTransform(spdf, alb_proj)
coords = coordinates(veg_transform)
colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'

veg_lct = data.frame(coords, veg_lct)

pivot_mod = veg_lct %>%
  pivot_wider(names_from = LCT, values_from = meansim)

pivot_mod = data.frame(pivot_mod)

library(elevatr)

ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

latLong = data.frame(pivot_mod[,c('long', 'lat')])

ele_get = get_elev_point(latLong, ele_proj, src = "aws")

lct_all = data.frame(pivot_mod[,c('ages', 'long', 'lat', 'x', 'y')], elev = ele_get$elevation, pivot_mod[,c('ET', 'OL', 'ST')])
lct_all = lct_all[which(lct_all$long<(-40)),]
lct_all = lct_all[which(lct_all$long>(-160)),]
lct_all= lct_all[which(lct_all$lat<(73)),]

lct_all = lct_all[which(!((lct_all$lat<25)&(lct_all$long<(-150)))),]

lct_modern = lct_all[which(lct_all$ages == 50), ]

lct_modern = lct_modern[, which(!(colnames(lct_modern) %in% c('ages')))]

saveRDS(lct_modern, 'data/lct_modern_reveals.RDS')

ggplot() + 
  geom_point(data=lct_modern, aes(x=long, y=lat), colour="blue")


lct_paleo = lct_all
saveRDS(lct_paleo, 'data/lct_paleo_reveals.RDS')

ggplot() + 
  geom_point(data=lct_paleo, aes(x=long, y=lat), colour="blue") +
  facet_wrap(~ages)
