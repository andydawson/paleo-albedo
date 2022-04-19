library(ggplot2)
library(ggplot2)
library(fields)


###############################################################################################################
## read in prediction and map data
###############################################################################################################

alb_preds = readRDS('data/lct_paleo_preds.RDS')

paleo_bins <- seq(0, 20000, by = 2000)
paleo_labels = seq(1000, 19000, by = 2000)

alb_preds$year = paleo_labels[alb_preds$year]

pbs <- readOGR("data/map-data/geographic/PoliticalBoundaries/boundary_p_v2.shp",
               'boundary_p_v2', encoding='UTF-8')
# pbs = spTransform(pbs, CRS("+init=epsg:4326"))
pbs = spTransform(pbs, CRS(alb_proj))
pbs = subset(pbs, COUNTRY %in% c('CAN', 'USA'))
pbs = subset(pbs, !(STATEABB %in% NA))
pbs = subset(pbs, (substr(STATEABB, 1, 2) %in% 'CA')|(STATEABB %in% c('US-AK')))

pbs_ll = spTransform(pbs, CRS("+init=epsg:4326"))
pbs_ll = subset(pbs_ll, coordinates(pbs_ll)[,1]<0)



breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 90, 100)/100
# breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# cover_melt2$value_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
labels = c("0-10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80",  "80 - 90", "90 - 100")



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

sc <- scale_colour_manual(values = tim.colors(length(breaks)), labels = labels,
                        na.value="white", name="Percent", drop=FALSE)

alb_preds$value_bin = cut(alb_preds$alb_pred, breaks, labels=FALSE)


ggplot(data=alb_preds) + 
  geom_histogram(aes(x=alb_pred, y=..density..)) +
  facet_wrap(~year)
  
# map predictions albers
ggplot()+
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_point(data=alb_preds, aes(x=x,y=y, colour=alb_pred)) +
  facet_wrap(~year)


# map predictions albers
ggplot()+
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_point(data=alb_preds, aes(x=x,y=y, colour=alb_pred)) +
  facet_wrap(~year)

# map predictions albers
ggplot()+
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_point(data=alb_preds, aes(x=x,y=y, colour=factor(value_bin))) +
  sc + 
  facet_wrap(~year)


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

alb_grid = cbind(coords, alb_grid[,c('cell_id', 'year', 'alb_pred')])

# take sd as well, mean of other vars too 
alb_grid = aggregate(alb_pred ~ cell_id + x + y + year, alb_grid, median)

alb_grid$value_bin = cut(alb_grid$alb_pred, breaks, labels=FALSE)
# alb_grid[which(alb_grid$alb_pred==0),'value_bin'] = 1
# alb_grid[which(alb_grid$alb_pred>0.99999999999999),'value_bin'] = length(breaks)-1
# veg_lct$value_bin[which(is.na(veg_lct$value_bin))] = 1

# map predictions lat long tiled
ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
  # geom_point(data=alb_preds, aes(x=long,y=lat, colour=alb_pred)) +
  geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(value_bin))) +
  sc_fill + 
  facet_wrap(~year)

# map predictions lat long points
ggplot()+
  geom_path(data=pbs_ll, aes(long,lat, group = group), color="black") +
  geom_point(data=alb_grid, aes(x=x,y=y, colour=factor(value_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(value_bin))) +
  sc_colour + 
  facet_wrap(~year)



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

