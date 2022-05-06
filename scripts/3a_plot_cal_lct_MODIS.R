library(ggplot2)
library(scales)
library(rgeos)


cal_data = readRDS('data/lct_albedo_snow_modern_glob.RDS')

foo = melt(cal_data[,1:20], id.vars=c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(foo) = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
foo$month = as.numeric(substr(foo$month, 4, 5))


bar = melt(cal_data[,c(1:8,21:ncol(cal_data))], id.vars=c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(bar) = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'snow_prob')
bar$month = as.numeric(substr(bar$month, 5, 6))


cal_long = merge(foo, bar)

###############################################################################################################
## maps data
###############################################################################################################

# 
# #this code provides a map of canada 
# world <- map_data('world')
# world = world[which((world$region %in% c('Canada')) | (world$subregion %in% 'Alaska')),]
# 
# world_sp = SpatialPointsDataFrame(coords=world[,1:2], data=world, proj4string=CRS("+init=epsg:4326"))
# world_proj = spTransform(world_sp, alb_proj)
# coords_world = coordinates(world_proj)
# colnames(coords_world) = c('x', 'y')
# world_proj = data.frame(coords_world, world)
# 
# world = world[world$long<0,]


pbs <- readOGR("data/map-data/geographic/PoliticalBoundaries/boundary_p_v2.shp",
               'boundary_p_v2', encoding='UTF-8')
pbs_ll = spTransform(pbs, CRS("+init=epsg:4326"))
b = bbox(pbs_ll)
b[1,2] = 0

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}
pbs_ll <- gClip(pbs_ll, b)
pbs_ll = subset(pbs_ll, COUNTRY %in% c('CAN', 'USA', 'MEX'))
pbs_ll = subset(pbs_ll, !(STATEABB %in% NA))
pbs_ll = subset(pbs_ll, !(STATEABB %in% c('US-HI')))
pbs_ll = subset(pbs_ll, !(NAME %in% 'United States Virgin Islands'))

# pbs = spTransform(pbs, CRS("+init=epsg:4326"))


saveRDS(pbs_ll, 'data/map-data/geographic/pbs_ll.RDS')


pbs = spTransform(pbs_ll, CRS(alb_proj))

saveRDS(pbs, 'data/map-data/geographic/pbs.RDS')


###############################################################################################################
## LCT maps
###############################################################################################################

library(scatterpie)

# ggplot() +
#   geom_polygon(data=world_proj, aes(x=x, y=y, group=group), color='black', fill=NA) +
#   geom_scatterpie(data=cal_data, aes(x=x, y=y), cols=c('ET', 'OL', 'ST'), pie_scale=0.6, alpha=0.7) +
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank())


ggplot() +
  # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_scatterpie(data=cal_data, aes(x=x, y=y), cols=c('ET', 'OL', 'ST'), pie_scale=0.6, alpha=0.7) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))
ggsave('figures/LCT_cal_pie_map.png')


ggplot() +
  # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
  geom_scatterpie(data=cal_data, aes(x=long, y=lat), cols=c('ET', 'OL', 'ST'), pie_scale=0.6, alpha=0.7) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))
ggsave('figures/LCT_cal_pie_map_ll.png')

###############################################################################################################
## albedo versus other vars
###############################################################################################################

# albedo versus geographic

# strong relationship in month 4 and 5, weaker in 3
ggplot(data=cal_long, aes(x=lat, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

# weak month 4,5, & 11 relationship
ggplot(data=cal_long, aes(x=long, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

# weak month 11 relationship
ggplot(data=cal_long, aes(x=elev, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)


# albedo versus land cover & snow

# strong relationship month5; weak relationship month 3, 4, 10, 11
ggplot(data=cal_long, aes(x=snow_prob, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  # geom_smooth(formula= y~s(x, k=4), method="gam", se=TRUE, size=1) +
  #geom_smooth(method = 'lm', formula = y ~ poly(x, 3)) +
  facet_wrap(~month)


cal_lct_melt = melt(cal_long, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo', 'snow_prob'))

ggplot() +
  geom_point(data=cal_long, aes(x=OL, y=albedo)) +
  facet_wrap(~month)

ggplot() +
  geom_point(data=cal_long, aes(x=ST, y=albedo)) +
  facet_wrap(~month)

ggplot() +
  geom_point(data=cal_long, aes(x=ET, y=albedo)) +
  facet_wrap(~month)

# relationship between land cover type and albedo in month 5; weak but there
ggplot(data=cal_lct_melt, aes(x=value, y=albedo, colour=variable)) +
  geom_point(alpha=0.6) +
  geom_smooth(se = TRUE, method = lm, fullrange=TRUE)+
  facet_wrap(~month)

corr_lct = cal_lct_melt %>% 
  filter((!(is.na(albedo)))&(!(is.na(value)))) %>%
  group_by(variable, month) %>% 
  summarize(cor = cor(albedo, value))


# ggplot(data=corr_lct) + 
#   geom_tile(aes(x=variable, y=factor(month), fill=cor)) +
#   scale_fill_gradient2(low = muted("red"),
#                        mid = "white",
#                        high = muted("blue"),
#                        midpoint = 0,
#                        limits = c(-0.6, 0.6), 
#                        space = "Lab",
#                        na.value = "grey50")

ggplot(data=corr_lct) + 
  geom_point(aes(y=variable, x=factor(month), size=cor, colour=cor)) +
  scale_colour_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0,
                       limits = c(-0.6, 0.6), 
                       space = "Lab",
                       na.value = "grey50") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.ticks = element_line(size=1),
        axis.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))

