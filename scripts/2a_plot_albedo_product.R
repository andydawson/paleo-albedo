library(sp)
library(raster)
library(ggplot2)

alb_above = readRDS('data/lct_albedo_modern.RDS')

alb_glob = readRDS('data/lct_albedo_modern_glob.RDS')

alb = merge(alb_above, alb_glob)

ggplot() +
  geom_point(data=alb, aes(x=fine.alb05, y=alb05))


###############################################################################################################
## maps data
###############################################################################################################

pbs <- readOGR("data/map-data/geographic/PoliticalBoundaries/boundary_p_v2.shp",
               'boundary_p_v2', encoding='UTF-8')
# pbs = spTransform(pbs, CRS("+init=epsg:4326"))
pbs = spTransform(pbs, CRS(alb_proj))
pbs = subset(pbs, COUNTRY %in% c('CAN', 'USA'))
pbs = subset(pbs, !(STATEABB %in% NA))
pbs = subset(pbs, (substr(STATEABB, 1, 2) %in% 'CA')|(STATEABB %in% c('US-AK')))

###############################################################################################################
## LCT maps
###############################################################################################################


ggplot() +
  # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_point(data=alb, aes(x=x, y=y, colour=bhr_sw)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))
# ggsave('figures/LCT_cal_pie_map.png')