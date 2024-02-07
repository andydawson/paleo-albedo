############################################################################################
# 
############################################################################################

library(sp)
library(dplyr)
library(tidyr)
library(elevatr)
library(ggplot2)
library(raster)
library(terra)
library(reshape2)

############################################################################################
# read in REVEALS reconstructions
# define meta variables
############################################################################################

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

latlimits  <- c(10, 80) 
longlimits <- c(-165, -50) 
bounding_box = c(longlimits[1], latlimits[1], longlimits[2], latlimits[2])

#veg_pred = readRDS('data/veg_pred_LC6k.RDS')
veg_pred = readRDS('data/veg_pred_LGM_8.0.RDS')

taxon2pft = read.csv('data/taxon2LCT_translation_v2.csv')

grid <- readRDS("data/grid.RDS")

foo = rast(grid)

cell_id <- terra::extract(grid, veg_pred[,c('long', 'lat')])

coords   = xyFromCell(grid, veg_pred$cell_id)
veg_pred = cbind(coords, veg_pred)

# breaks = c(-74, 0.1, 0.35, 0.7, seq(1.2, 11.7, by=0.5))
# slice_bins = seq(1, 25)
# slice_labels = c(50, 200, seq(500, 11500, by=500))

############################################################################################
# 
############################################################################################

veg_grid = aggregate(mediansim ~ taxon + ages + cell_id + x+ y, veg_pred, sum)

veg_cast = dcast(veg_grid, cell_id + x + y + ages ~ taxon, value.var='mediansim')
veg_cast[,5:ncol(veg_cast)] = t(apply(veg_cast[,5:ncol(veg_cast)], 1, function(x) x/sum(x)))
veg_grid = melt(veg_cast, id.vars=c('cell_id', 'x', 'y', 'ages'))


veg_grid$LCT = taxon2pft$LCT[match(veg_grid$variable, tolower(taxon2pft$taxon))]
veg_lct = aggregate(value ~ cell_id + x + y + ages + LCT, veg_grid, sum, na.rm=TRUE)

colnames(veg_lct) = c('cell_id', 'long', 'lat', 'ages', 'LCT', 'value')

# veg_lct = aggregate(meansim ~ dataset_id + long + lat + ages + LCT, veg_pred, sum, na.rm=TRUE)

# k = veg_lct

#assigning a crs to the pollen coordinates
veg_lct_spatial = SpatialPointsDataFrame(coords = veg_lct[,c('long', 'lat')], 
                                         data = veg_lct,
                                         proj4string = CRS(ll_proj))

#transforming to the albedo crs
veg_transform = spTransform(veg_lct_spatial, alb_proj)
coords = coordinates(veg_transform)
colnames(coords)[1] <- 'x'
colnames(coords)[2] <- 'y'

veg_lct = data.frame(coords, veg_lct)

pivot_mod = veg_lct %>%
  pivot_wider(names_from = LCT, values_from = value)

pivot_mod = data.frame(pivot_mod)

############################################################################################
# 
############################################################################################

# ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# 
# latLong = data.frame(pivot_mod[,c('long', 'lat')])

locations = pivot_mod[,c('long', 'lat')]
colnames(locations) = c('x', 'y')

ele_get = get_elev_point(locations, prj=ll_proj, src = "aws")

# ele_get = get_elev_point(latLong, ele_proj, src = "aws")

lct_all = data.frame(pivot_mod[,c('ages', 'long', 'lat', 'x', 'y')], 
                     elev = ele_get$elevation, 
                     pivot_mod[,c('ET', 'OL', 'ST')])

ggplot() + 
  geom_point(data=lct_all, aes(x=long, y=lat), colour="blue") +
  facet_wrap(~ages)

lct_all = lct_all[which((lct_all$lat > latlimits[1])&
                          (lct_all$lat < latlimits[2])&
                          (lct_all$long > longlimits[1])&
                          (lct_all$long < longlimits[2])), ]

# lct_all = lct_all[which(lct_all$long<(-40)),]
# lct_all = lct_all[which(lct_all$long>(-160)),]
# lct_all= lct_all[which(lct_all$lat<(73)),]

lct_all = lct_all[which(!((lct_all$lat<25)&(lct_all$long<(-150)))),]

############################################################################################
# 
############################################################################################

lct_modern = lct_all[which(lct_all$ages == 50), ]

lct_modern = lct_modern[, which(!(colnames(lct_modern) %in% c('ages')))]

saveRDS(lct_modern, 'data/lct_modern_reveals.RDS')

ggplot() + 
  geom_point(data=lct_modern, aes(x=long, y=lat), colour="blue")

############################################################################################
# for paleo use 2000 year time bins
############################################################################################
# 
# # veg_pred = readRDS('data/veg_pred_LC6k.RDS')
# veg_pred = readRDS('data/veg_pred_LGM_8.0.RDS')
# 
# taxon2pft = read.csv('data/taxon2LCT_translation_v2.csv')
# 
# # breaks = c(-74, 0.1, 0.35, 0.7, seq(1.2, 11.7, by=0.5))
# # slice_bins = seq(1, 25)
# # slice_labels = c(50, 200, seq(500, 11500, by=500))
# 
# ############################################################################################
# # 
# ############################################################################################
# 
# veg_pred$LCT = taxon2pft$LCT[match(veg_pred$taxon, tolower(taxon2pft$taxon))]
# 
# veg_lct = aggregate(meansim ~ dataset_id + long + lat + ages + LCT, veg_pred, sum, na.rm=TRUE)
# 
# # k = veg_lct
# 
# #assigning a crs to the pollen coordinates
# veg_lct_spatial = SpatialPointsDataFrame(coords = veg_lct[,c('long', 'lat')], 
#                                          data = veg_lct,
#                                          proj4string = CRS(ll_proj))
# 
# #transforming to the albedo crs
# veg_transform = spTransform(veg_lct_spatial, alb_proj)
# coords = coordinates(veg_transform)
# colnames(coords)[1] <- 'x'
# colnames(coords)[2] <- 'y'
# 
# veg_lct = data.frame(coords, veg_lct)
# 
# pivot_mod = veg_lct %>%
#   pivot_wider(names_from = LCT, values_from = meansim)
# 
# pivot_mod = data.frame(pivot_mod)
# 
# ############################################################################################
# # 
# ############################################################################################
# 
# # ele_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# # 
# # latLong = data.frame(pivot_mod[,c('long', 'lat')])
# 
# ele_get = get_elev_point(pivot_mod[,c('long', 'lat')], ll_proj, src = "aws")
# 
# # ele_get = get_elev_point(latLong, ele_proj, src = "aws")
# 
# lct_all = data.frame(pivot_mod[,c('ages', 'long', 'lat', 'x', 'y')], 
#                      elev = ele_get$elevation, 
#                      pivot_mod[,c('ET', 'OL', 'ST')])
# 
# ggplot() + 
#   geom_point(data=lct_all, aes(x=long, y=lat), colour="blue") +
#   facet_wrap(~ages)
# 
# lct_all = lct_all[which((lct_all$lat > latlimits[1])&
#                           (lct_all$lat < latlimits[2])&
#                           (lct_all$long > longlimits[1])&
#                           (lct_all$long < longlimits[2])), ]
# 
# # lct_all = lct_all[which(lct_all$long<(-40)),]
# # lct_all = lct_all[which(lct_all$long>(-160)),]
# # lct_all= lct_all[which(lct_all$lat<(73)),]
# 
# lct_all = lct_all[which(!((lct_all$lat<25)&(lct_all$long<(-150)))),]


lct_paleo = lct_all

# lct_paleo = rbind(data.frame(ages=50, lct_modern), lct_paleo)
saveRDS(lct_paleo, 'data/lct_paleo_reveals.RDS')

ggplot() + 
  geom_point(data=lct_paleo, aes(x=long, y=lat), colour="blue") +
  facet_wrap(~ages)

############################################################################################
# interpolated veg
############################################################################################

lct_interp = readRDS('data/veg_posts_interp_ice.RDS')

lct_interp = lct_interp %>% 
  group_by(cell_id, x, y, ages, LCT) %>%
  summarize(value = mean(value))

lct_interp_wide = pivot_wider(lct_interp, id_cols = c('cell_id', 'x', 'y', 'ages'), names_from = c('LCT'), values_from = c('value'))
lct_interp_wide = data.frame(lct_interp_wide)

locations_interp = lct_interp_wide[,c('x', 'y')]
# colnames(locations_interp) = c('x', 'y')

ele_get_interp = get_elev_point(locations_interp, prj=ll_proj, src = "aws")

lct_interp_all = data.frame(lct_interp_wide[,c('ages', 'x', 'y')], 
                     elev = ele_get_interp$elevation, 
                     lct_interp_wide[,c('ET', 'OL', 'ST')])

lct_interp_modern = lct_interp_all[which(lct_interp_all$ages == 50), ]

lct_interp_modern = lct_interp_modern[, which(!(colnames(lct_interp_modern) %in% c('ages')))]

saveRDS(lct_interp_modern, 'data/lct_modern_reveals_interp.RDS')


lct_interp_paleo = lct_interp_all

# lct_paleo = rbind(data.frame(ages=50, lct_modern), lct_paleo)
saveRDS(lct_interp_paleo, 'data/lct_paleo_reveals_interp.RDS')
