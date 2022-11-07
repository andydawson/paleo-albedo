library(ggplot2)
library(fields)
# library(dplyr)


###############################################################################################################
## read in prediction and map data
###############################################################################################################

alb_preds = readRDS('data/lct_paleo_preds.RDS')

ggplot() + 
  geom_point(data=alb_preds, aes(x=long, y=lat), colour="blue") +
  facet_wrap(~year)


# paleo_bins <- seq(0, 20000, by = 2000)
# paleo_labels = seq(1000, 19000, by = 2000)
# 
# breaks = c(-74, 0.1, 0.35, 0.7, seq(1.2, 11.7, by=0.5))
# slice_bins = seq(1, 25)
# paleo_labels = c(50, 200, seq(500, 11500, by=500))

# alb_preds$year = paleo_labels[alb_preds$year]


pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')


breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 90, 100)/100

# breaks = c(0, 4, 8, 12, 16, 20, 30, 40, 60, 80, 100)/100
# breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# cover_melt2$value_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
# labels = c("0-10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80",  "80 - 90", "90 - 100")
labels = c("0-0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8",  "0.8 - 0.9", "0.9 - 1")

# labels = seq(1, length(breaks)-1)


# plot_cuts = c(0, 1e-5, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
# n_plot_cuts = length(plot_cuts) - 1


# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_colour <- scale_colour_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                                 na.value="white", name="Percent", drop=FALSE)


# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_fill <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                             na.value="white", name="Percent", drop=FALSE)


###############################################################################################################
## summary plots of values
###############################################################################################################

alb_preds$value_bin = cut(alb_preds$alb_pred, breaks, labels=FALSE)


# ggplot(data=alb_preds) + 
#   geom_histogram(aes(x=alb_pred, y=..density..)) +
#   facet_wrap(~year)
# 
# # map predictions albers
# ggplot()+
#   geom_path(data=pbs, aes(long,lat, group = group), color="black") +
#   geom_point(data=alb_preds, aes(x=x,y=y, colour=alb_pred)) +
#   facet_wrap(~year)
# 
# # map predictions lat long
# ggplot()+
#   geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
#   geom_point(data=alb_preds, aes(x=long,y=lat, colour=alb_pred)) +
#   facet_wrap(~year)
# 
# 
# # map predictions albers
# ggplot()+
#   geom_path(data=pbs, aes(long,lat, group = group), color="black") +
#   geom_point(data=alb_preds, aes(x=x,y=y, colour=factor(value_bin))) +
#   sc_colour + 
#   facet_wrap(~year)
# 
# # map predictions albers
# ggplot()+
#   geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
#   geom_point(data=alb_preds, aes(x=long,y=lat, colour=factor(value_bin))) +
#   sc_colour + 
#   facet_wrap(~year)


###############################################################################################################
## aggregate to 1 degree by 1 degree tiles
###############################################################################################################

latlimits  <- c(10, 80)
longlimits <- c(-165, -50)
bounding_box = c(-165, 10, -50, 80)

grid <- readRDS("data/grid.RDS")

cell_id <- raster::extract(grid, alb_preds[,c('long', 'lat')])

alb_grid <- data.frame(cell_id, alb_preds)
coords   = xyFromCell(grid, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords, alb_grid[,c('x', 'y', 'cell_id', 'year', 'alb_pred')])

# take sd as well, mean of other vars too 
alb_grid = aggregate(alb_pred ~ cell_id + long + lat + x + y + year, alb_grid, median)

alb_grid$value_bin = cut(alb_grid$alb_pred, breaks, labels=FALSE)
# alb_grid[which(alb_grid$alb_pred==0),'value_bin'] = 1
# alb_grid[which(alb_grid$alb_pred>0.99999999999999),'value_bin'] = length(breaks)-1
# veg_lct$value_bin[which(is.na(veg_lct$value_bin))] = 1

# map predictions lat long tiled
ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
  # geom_point(data=alb_preds, aes(x=long,y=lat, colour=alb_pred)) +
  geom_tile(data=alb_grid, aes(x=long,y=lat, fill=factor(value_bin))) +
  sc_fill + 
  facet_wrap(~year)

# map predictions lat long points
ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
  geom_point(data=alb_grid, aes(x=long,y=lat, colour=factor(value_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(value_bin))) +
  sc_colour + 
  facet_wrap(~year)


sc_fill_seq <- scale_fill_brewer(type = "seq",
                                 palette = "YlGnBu",#"BrBG",
                                 labels = labels,
                                 na.value="grey", 
                                 direction=-1,
                                 name="Albedo")

sc_colour_seq <- scale_colour_brewer(type = "seq",
                                     palette = "YlGnBu",#"BrBG",
                                     labels = labels,
                                     direction=-1,
                                     na.value="grey", 
                                     name="Albedo")


years = c(50, 2000, 4000, 6000, 8000, 10000)
# years = c(50, 6000, 11000)

alb_grid_sub = subset(alb_grid, year %in% years) 

labels_age = c('0.05', '2', '4', '6', '8', '10')

alb_grid_sub$facets = labels_age[match(alb_grid_sub$year, years)]
alb_grid_sub$facets = factor(alb_grid_sub$facets, levels = labels_age)

# map predictions lat long points
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(value_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(value_bin))) +
  sc_fill_seq + 
  facet_grid(facets~.)+
  # facet_wrap(year~.)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()

# map predictions lat long points
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_point(data=alb_grid_sub, aes(x=long,y=lat, colour=factor(value_bin)), size=0.9) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(value_bin))) +
  sc_colour_seq + 
  facet_grid(facets~.)+
  # facet_wrap(year~.)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()


###############################################################################################################
## diffs accross larger time periods
###############################################################################################################

diff = alb_grid %>%
  group_by(cell_id, long, lat, x, y) %>%
  mutate(diff = alb_pred - lag(alb_pred, order_by =year))

saveRDS(diff, 'data/preds_alb_diffs_all.RDS')

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(diff$diff), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        na.value="grey", 
                                        name="Percent",
                                        limits = c(-thresh,thresh))

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "RdYlBu",#"BrBG",
                                            # labels = labels,
                                            na.value="grey", 
                                            name="Percent",
                                            limits = c(-thresh,thresh))

# breaks_diff = c(-0.1, 4, 8, 12, 16, 20, 30, 40, 60, 80, 100)/100
# # breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# # cover_melt2$value_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
# labels = c("0-10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80",  "80 - 90", "90 - 100")

# ggplot()+
#   geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey") +
#   geom_tile(data=diff, aes(x=long, y=lat, fill = diff))+
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_wrap(~year)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) 
# # scale_fill_brewer(type = "div", palette = 'RdBu')+

ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey") +
  geom_point(data=diff, aes(x=long, y=lat, colour = diff))+
  sc_colour_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_wrap(~year)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
# scale_fill_brewer(type = "div", palette = 'RdBu')+

# ggplot()+
#   geom_point(data=diff, aes(x=x, y=y, colour = diff))+
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.2,0.2))+
#   scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_wrap(~year)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) 
# scale_fill_brewer(type = "div", palette = 'RdBu')+

###############################################################################################################
## diffs accross larger time periods
###############################################################################################################
years = c(50, 2000, 4000, 6000, 8000, 10000)
# years = c(50, 6000, 11000)

diff = subset(alb_grid, year %in% years) %>%
  group_by(cell_id, long, lat, x, y) %>%
  mutate(diff = alb_pred - lag(alb_pred, order_by =year))

diff = diff[which(!(diff$year %in% years[1])),]
diff$diff = -diff$diff

saveRDS(diff, 'data/preds_alb_diffs_sub.RDS')

labels = c('0.05 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10')
diff_years = years[-1]
diff$facets = labels[match(diff$year, diff_years)]
diff$facets = factor(diff$facets, levels =  labels)

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(diff$diff), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

values = c(0,0.4, 0.5, 0.6,1)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        na.value="grey", 
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)

# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "RdYlBu",#"BrBG",
#                                         # labels = labels,
#                                         na.value="grey", 
#                                         name="Percent")##,
#                                         # limits = c(-thresh,thresh),
#                                         # values = values)

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "RdYlBu",#"BrBG",
                                            # labels = labels,
                                            na.value="transparent",#grey", 
                                            name="Albedo change",
                                            limits = c(-thresh,thresh), 
                                            values = values)

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_point(data=diff, aes(x=long, y=lat, colour = diff), size=1, alpha=0.8)+
  sc_colour_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~.)+
  # facet_wrap(year~.)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave('figures/alb_preds_diff_subset_point.png')
ggsave('figures/alb_preds_diff_subset_point.pdf')

ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="grey") +
  geom_tile(data=diff, aes(x=long, y=lat, fill = diff)) +
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(year~.)+
  # facet_wrap(year~.)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_fixed()


###############################################################################################################
## old code
###############################################################################################################


#this changes back to regular lat long
#assigning a crs to the pollen coordinates
spdf <- SpatialPointsDataFrame(coords = predict_bind[,c('x','y')], data = predict_bind,
                               proj4string = CRS('+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

#transforming to the albedo crs: epsg 102001
pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

jjj=data.frame(coordinates(pol_transform), predict_bind)

#dividing into chunks
#paleo <- tree.cores[tree.cores$age >= 150, ]
lat_bins <- seq(40, 80, by = 10)
#using the age column to break up the data by the paleo_bins vector
lat_cut <- cut(jjj$y, include.lowest = TRUE, breaks = lat_bins)
#adds the column cut, 1 cut represents data for 2000 years
jjj$lat_cut <- as.integer(lat_cut)


foo = jjj%>%
  group_by(cut,lat_cut) %>%
  summarise(average = mean(predict_time, na.rm=TRUE), .groups = 'keep')

#million line graph
ggplot()+
  geom_line(data=jjj, aes(x=cut, y=predict_time, group=interaction(x,y)))+
  facet_wrap(~lat_cut)


#average of prediction albedo over time for different lat cuts 
ggplot()+
  geom_line(data=foo, aes(x=cut, y=average, color=factor(lat_cut), group=lat_cut))
ggsave('figures/latcut_average.png')


#albedo prediction for each cut through time 
ggplot()+
  geom_point(data=jjj, aes(x=x.1, y=y.1, color=predict_time ))+
  scale_color_distiller(type = 'seq', palette = "YlOrBr")+
  facet_wrap(~cut)
ggsave('figures/albaverage_cut.png')

