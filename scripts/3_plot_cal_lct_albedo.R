library(ggplot2)
library(ggtern)
library(scales)
library(rgeos)
library(rgdal)
library(raster)
library(reshape2)
library(tricolore)

alb_prod = 'bluesky'

# cal_data = readRDS('data/lct_albedo_snow_modern_glob.RDS')
# cal_data = readRDS('data/lct_albedo_snow_modern_albclim.RDS')

cal_data = readRDS(paste0('data/calibration_modern_lct_', alb_prod, '.RDS'))
cal_long = melt(cal_data[,1:20], id.vars=c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(cal_long) = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
cal_long$month = as.numeric(substr(cal_long$month, 4, 5))

cal_data_coarse = readRDS(paste0('data/calibration_modern_lct_', alb_prod, '_coarse.RDS'))
cal_long_coarse = melt(cal_data_coarse[,1:20], id.vars=c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(cal_long_coarse) = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
cal_long_coarse$month = as.numeric(substr(cal_long_coarse$month, 4, 5))

cal_data_point = readRDS(paste0('data/calibration_modern_lct_', alb_prod, '_point.RDS'))
cal_long_point = melt(cal_data_point[,1:20], id.vars=c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(cal_long_point) = c('long', 'lat', 'x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
cal_long_point$month = as.numeric(substr(cal_long_point$month, 4, 5))



###############################################################################################################
## maps data
###############################################################################################################
pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

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

# make grid for NA (or ENA)
source('scripts/make_grid.R')
grid <- make_grid(cal_data, coord_fun = ~ long + lat, projection = '+init=epsg:4326', resolution = 2)

cell_id <- raster::extract(grid, cal_data[,c('long', 'lat')])

cal_data_pie <- data.frame(cell_id, cal_data)

cal_data_pie_agg = cal_data_pie %>% 
  group_by(cell_id) %>%
  summarize(ET = mean(ET), OL = mean(OL), ST = mean(ST), .groups='keep')

coords = xyFromCell(grid, cal_data_pie_agg$cell_id)
colnames(coords) = c('long', 'lat')


cal_data_pie_agg = cbind(coords, cal_data_pie_agg)

# cal_data_pie = aggregate(meansim ~ dataset_id + long + lat + ages + LCT, veg_pred, sum, na.rm=TRUE)

sc_colour_qual <- scale_colour_brewer(type = "qual",
                                            palette = "Dark2",#"BrBG",
                                            # labels = labels,
                                            na.value="transparent",#grey", 
                                            name="Albedo change",
                                            labels = c("Open Land", "Summergreen", "Evergreen"))

sc_fill_qual <- scale_fill_brewer(type = "qual",
                                      palette = "Dark2",#"BrBG",
                                      # labels = labels,
                                      na.value="transparent",#grey", 
                                      name="Albedo change",
                                      labels = c("Open Land", "Summergreen", "Evergreen"))

# ggplot() +
#   # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
#   geom_path(data=pbs, aes(long,lat, group = group), color="black") +
#   geom_scatterpie(data=cal_data_pie_agg, aes(x=x, y=y), cols=c('ET', 'OL', 'ST'), pie_scale=0.6, alpha=0.7) +
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=16))
# ggsave('figures/LCT_cal_pie_map.png')


ggplot() +
  # geom_polygon(data=pbs_ll, aes(x=x, y=y, group=group), color='black', fill="lightgrey") +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_scatterpie(data=cal_data_pie_agg, 
                  aes(x=long, y=lat), 
                  cols=c('OL', 'ST', 'ET'), 
                  pie_scale=0.42, 
                  alpha=0.7) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(fill='Cover') +
  scale_fill_discrete(labels = c("Open Land", "Summergreen", "Evergreen"), guide = guide_legend(reverse = TRUE)) +
  # scale_fill_manual(labels = c("Open Land", "Summergreen", "Evergreen"), values = c("#b15928", "#a6cee3","#33a02c")) +
  # sc_fill_qual +
  coord_fixed()
ggsave('figures/LCT_cal_pie_map_ll.png')
ggsave('figures/LCT_cal_pie_map_ll.pdf')


tric_lct <- Tricolore(cal_data_pie_agg,
                       p1 = 'ET', p2 = 'OL', p3 = 'ST', show_data = FALSE)

tric_lct$key + theme_bw(18) + geom_point(data=cal_data_pie_agg, aes(x=ET, y=OL, z=ST), size=1)
ggsave('figures/LCT_tricolore_key.png')
ggsave('figures/LCT_tricolore_key.pdf')

# add the vector of colors to the `euro_example` data
cal_data_pie_agg$lct_rgb <- tric_lct$rgb

plot_lct <-
  # using data sf data `euro_example`...
  ggplot() +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  # ...draw a choropleth map
  geom_tile(data=cal_data_pie_agg, aes(long, lat, fill = lct_rgb)) +
  # ...and color each region according to the color-code
  # in the variable `educ_rgb`
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  coord_fixed() +
  scale_fill_identity()

plot_lct
ggsave('figures/LCT_tricolore_map.png')
ggsave('figures/LCT_tricolore_map.pdf')

# plot_lct +
#   annotation_custom(
#     ggplotGrob(tric_lct$key), xmin = -40, xmax = -55, ymin = -50, ymax = 80
#   )

cal_lct_melt = melt(cal_long, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))
cal_lct_coarse_melt = melt(cal_long_coarse, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))
cal_lct_point_melt = melt(cal_long_point, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))


ggplot() +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=cal_lct_melt, 
                  aes(x=long, y=lat, fill=value),
                  alpha=0.7) +
  # scale_fill_distiller(type='seq', palette='YlGn') +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), limits=c(0,1)) + 
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(fill='Cover') +
  coord_fixed() +
  facet_grid(variable~.)
ggsave('figures/LCT_gridded_maps_calibration.pdf')
ggsave('figures/LCT_gridded_maps_calibration.png')

###############################################################################################################
## albedo versus other vars
###############################################################################################################

# albedo versus geographic

ggplot(data=cal_long, aes(x=lat, y=albedo)) +
  geom_point() +
  # geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long, aes(x=long, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long, aes(x=elev, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

# coarse
ggplot(data=cal_long_coarse, aes(x=lat, y=albedo)) +
  geom_point() +
  # geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long_coarse, aes(x=long, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long_coarse, aes(x=elev, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)


ggplot(data=cal_long_point, aes(x=lat, y=albedo)) +
  geom_point() +
  # geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long_point, aes(x=long, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)

ggplot(data=cal_long_point, aes(x=elev, y=albedo)) +
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  facet_wrap(~month)


# albedo versus land cover & snow



cal_lct_melt = melt(cal_long, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))

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


# relationship between land cover type and albedo in month 5; weak but there
ggplot(data=cal_lct_melt, aes(x=lat, y=value, colour=variable)) +
  geom_point(alpha=0.6) +
  geom_smooth(se = TRUE, method = lm, fullrange=TRUE)


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
  geom_point(aes(y=variable, x=factor(month), size=abs(cor), colour=cor)) +
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



###############################################################################################################
## interp
###############################################################################################################


cal_data = readRDS('data/calibration_modern_lct_interp_bluesky.RDS')
cal_long = melt(cal_data, id.vars=c('x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(cal_long) = c('x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
# cal_long$month = as.numeric(substr(cal_long$month, 4, 5))

cal_long = data.frame(long = cal_long$x, lat = cal_long$y, cal_long)

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

# make grid for NA (or ENA)
source('scripts/make_grid.R')
grid <- make_grid(cal_data, coord_fun = ~ x + y, projection = '+init=epsg:4326', resolution = 2)

cell_id <- raster::extract(grid, cal_data[,c('x', 'y')])

cal_data_pie <- data.frame(cell_id, cal_data)

cal_data_pie_agg = cal_data_pie %>% 
  group_by(cell_id) %>%
  summarize(ET = mean(ET, na.rm = TRUE), 
            OL = mean(OL, na.rm = TRUE), 
            ST = mean(ST, na.rm = TRUE), 
            .groups='keep')

coords = xyFromCell(grid, cal_data_pie_agg$cell_id)
colnames(coords) = c('x', 'y')


cal_data_pie_agg = cbind(coords, cal_data_pie_agg)

cal_data_pie_agg = cal_data_pie_agg[which(!is.na(cal_data_pie_agg$x)),]

# cal_data_pie = aggregate(meansim ~ dataset_id + long + lat + ages + LCT, veg_pred, sum, na.rm=TRUE)

sc_colour_qual <- scale_colour_brewer(type = "qual",
                                      palette = "Dark2",#"BrBG",
                                      # labels = labels,
                                      na.value="transparent",#grey", 
                                      name="Albedo change",
                                      labels = c("Open Land", "Summergreen", "Evergreen"))

sc_fill_qual <- scale_fill_brewer(type = "qual",
                                  palette = "Dark2",#"BrBG",
                                  # labels = labels,
                                  na.value="transparent",#grey", 
                                  name="Albedo change",
                                  labels = c("Open Land", "Summergreen", "Evergreen"))

# ggplot() +
#   # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
#   geom_path(data=pbs, aes(long,lat, group = group), color="black") +
#   geom_scatterpie(data=cal_data_pie_agg, aes(x=x, y=y), cols=c('ET', 'OL', 'ST'), pie_scale=0.6, alpha=0.7) +
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=16))
# ggsave('figures/LCT_cal_pie_map.png')

color_values_four = c("#CC79A7", "#009E73", "#0072B2", "#D55E00")
color_values_three = c("#009E73","#0072B2", "#D55E00")

ggplot() +
  # geom_polygon(data=pbs_ll, aes(x=x, y=y, group=group), color='black', fill="lightgrey") +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_scatterpie(data=cal_data_pie_agg,
                  aes(x=x, y=y),
                  cols=c('OL', 'ST', 'ET'),
                  pie_scale=0.4,
                  alpha=0.7) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(fill='Land cover \ntype') +
  scale_fill_discrete(labels = c("Open land", "Summergreen", "Evergreen"), 
                      guide = guide_legend(reverse = TRUE)) +
  # scale_fill_manual(values = c("#E69F00", "#CC79A7", "#009E73")) +
  scale_fill_manual(values = color_values_three) + #c("#fde725", "#440154", "#21918c")) +
  # sc_fill_qual +
  coord_fixed()
ggsave('figures/LCT_cal_pie_map_ll_interp.png')
ggsave('figures/LCT_cal_pie_map_ll_interp.pdf')

tric_lct <- Tricolore(cal_data_pie,
                      p1 = 'ET', p2 = 'OL', p3 = 'ST', show_data = TRUE)

tric_lct$key + 
  theme_bw(18) + 
  geom_point(data=cal_data_pie, aes(x=ET, y=OL, z=ST), size=1, alpha=0.2)
# tric_lct$key + theme_bw(18) + geom_point(data=cal_data_pie_agg, size=1)

ggsave('figures/LCT_tricolore_key_interp.png')
ggsave('figures/LCT_tricolore_key_interp.pdf')

# add the vector of colors to the `euro_example` data
cal_data_pie$lct_rgb <- tric_lct$rgb

plot_lct <-
  # using data sf data `euro_example`...
  ggplot() +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  # ...draw a choropleth map
  geom_tile(data=cal_data_pie, aes(x, y, fill = lct_rgb)) +
  # ...and color each region according to the color-code
  # in the variable `educ_rgb`
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  coord_fixed() +
  scale_fill_identity()

plot_lct
ggsave('figures/LCT_tricolore_map_interp.png')
ggsave('figures/LCT_tricolore_map_interp.pdf')

###############################################################################################################
## interp
###############################################################################################################

cal_lct_melt = melt(cal_long, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))
# cal_lct_coarse_melt = melt(cal_long_coarse, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))
# cal_lct_point_melt = melt(cal_long_point, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))

cal_lct_melt$variable = factor(cal_lct_melt$variable, 
                               levels = c('ET', 'ST', 'OL'),
                               labels = c('ETS', 'STS', 'OVL'))

ggplot() +
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=cal_lct_melt, 
            aes(x=long, y=lat, fill=value),
            alpha=0.7) +
  # scale_fill_distiller(type='seq', palette='YlGn') +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), limits=c(0,1)) + 
  theme_bw(12) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(fill='Fractional \nland cover') +
  coord_fixed() +
  facet_grid(variable~.)
ggsave('figures/LCT_gridded_maps_calibration_interp.pdf')
ggsave('figures/LCT_gridded_maps_calibration_interp.png')
