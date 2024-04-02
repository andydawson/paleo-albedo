library(sp)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)

alb_prod = "bluesky"

months = c('feb', 'may', 'aug', 'nov')
months_number = c(2, 5, 8, 10)

alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ylim = c(12, 82) 
xlim = c(-166, -50) 

ice_fort = readRDS('data/ice_fort.RDS')
ice_fort_diff_young = readRDS('data/ice_fort_diff_young.RDS')
ice_fort_diff_old = readRDS('data/ice_fort_diff_old.RDS')

labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

# ice_fort_diff_old$facets = factor(ice_fort_diff_old$facets,
#                                   levels = labels_period,
#                                   labels = labels_period)
# 
# ice_fort_diff_young$facets = factor(ice_fort_diff_young$facets,
#                                   levels = labels_period,
#                                   labels = labels_period)

###############################################################################################################
## read data
###############################################################################################################

# map data
pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

# # radiative kernel
# rk_hadgem = brick('data/radiative-kernels/HadGEM3-GA7.1_TOA_kernel_L19.nc', level=1, varname='albedo_sw_cs')
# 
# rk_cam5 = raster('data/radiative-kernels/CAM5/alb.kernel.nc', varname='FSNSC', band=month)
# 
# rk_cack = raster('data/radiative-kernels/CACKv1.0/CACKv1.0.nc', varname='CACK', level=month, band=3)
# rk_cack = t(rk_cack)


# albedo prediction differences
# alb_diff = readRDS(paste0('data/alb_preds_diffs_', alb_prod, '.RDS'))
alb_interp_diff = readRDS(paste0('data/alb_interp_preds_diffs_', alb_prod, '.RDS'))
alb_interp_diff[which(alb_interp_diff$year == 11500)] = 12000

alb_diff = alb_interp_diff

###############################################################################################################
## determine radiative forcing
###############################################################################################################

# unique coordinates across all time periods
alb_diff_unique = alb_diff[!duplicated(alb_diff[,c('lat', 'long')]),]
head(alb_diff_unique)

# radiative kernel is WGS84 
# longitude scale: 0 - 360
alb_diff_unique$long360 = abs(alb_diff_unique$long - 180)
alb_diff_unique$lat180 = abs(alb_diff_unique$lat + 90)

# create spatial object of unique coordingates
alb_diff_spatial = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat')], 
                                          data = alb_diff_unique, 
                                          proj4string = CRS(ll_proj))

# create spatial object of unique coordingates
alb_diff_spatial_cack = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat180')], 
                                               data = alb_diff_unique, 
                                               proj4string = CRS(ll_proj))

for (month in months){
  
  print(month)
  month_number = months_number[which(months == month)]
  print(month_number)
  ##
  ## HADGEM
  ##
  
  idx_month = which(alb_diff_unique$month == month)
  
  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_hadgem_month = raster::extract(rk_hadgem[[month_number]], alb_diff_spatial[idx_month,])
  alb_diff_unique$rk_hadgem[idx_month] = rk_hadgem_month 
  
  ##
  ## CAM5
  ##
  
  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_cam5_month = raster::extract(rk_cam5, alb_diff_spatial[idx_month,])
  alb_diff_unique$rk_cam5[idx_month] = rk_cam5_month 
  
  ##
  ## CACKv1.0
  ##
  
  # extract radiative kernel values at unique coordinate locations
  # for specified month
  rk_cack_month = raster::extract(rk_cack, alb_diff_spatial_cack[idx_month,])
  alb_diff_unique$rk_cack[idx_month] = rk_cack_month 
  
}

# merge radiative kernel values with paleo albedo differences data frame
# note for a given set of coordinates rk fixed through time
alb_diff = merge(alb_diff, 
                 alb_diff_unique[,c('lat', 'long', 'month', 'rk_hadgem', 'rk_cam5', 'rk_cack')], 
                 by=c('lat', 'long', 'month'))

# calculate radiative forcing
alb_diff$rf_hadgem = alb_diff$alb_diff*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_hadgem

# calculate radiative forcing
alb_diff$rf_cam5 = alb_diff$alb_diff*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_cam5

# calculate radiative forcing
alb_diff$rf_cack = alb_diff$alb_diff * (-alb_diff$rk_cack)
alb_diff$rf_cack_ice = alb_diff$alb_diff_ice * (-alb_diff$rk_cack)

saveRDS(alb_diff, paste0('output/forcing/RF_holocene.RDS'))

# ###############################################################################################################
# ## determine radiative forcing
# ###############################################################################################################
# 
# # unique coordinates across all time periods
# alb_diff_unique = alb_diff[!duplicated(alb_diff[,c('lat', 'long')]),]
# head(alb_diff_unique)
# 
# # radiative kernel is WGS84 
# # longitude scale: 0 - 360
# alb_diff_unique$long360 = abs(alb_diff_unique$long - 180)
# alb_diff_unique$lat180 = abs(alb_diff_unique$lat + 90)
# 
# # create spatial object of unique coordingates
# alb_diff_spatial = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat')], 
#                                           data = alb_diff_unique, 
#                                           proj4string = CRS(ll_proj))
# 
# # create spatial object of unique coordingates
# alb_diff_spatial_cack = SpatialPointsDataFrame(coords = alb_diff_unique[,c('long360', 'lat180')], 
#                                                data = alb_diff_unique, 
#                                                proj4string = CRS(ll_proj))
# ##
# ## HADGEM
# ##
# 
# # extract radiative kernel values at unique coordinate locations
# # for specified month
# rk_hadgem_month = raster::extract(rk_hadgem[[month]], alb_diff_spatial)
# alb_diff_unique$rk_hadgem = rk_hadgem_month 
# 
# ##
# ## CAM5
# ##
# 
# # extract radiative kernel values at unique coordinate locations
# # for specified month
# rk_cam5_month = raster::extract(rk_cam5, alb_diff_spatial)
# alb_diff_unique$rk_cam5 = rk_cam5_month 
# 
# ##
# ## CACKv1.0
# ##
# 
# # extract radiative kernel values at unique coordinate locations
# # for specified month
# rk_cack_month = raster::extract(rk_cack, alb_diff_spatial_cack)
# alb_diff_unique$rk_cack = rk_cack_month 
# 
# # merge radiative kernel values with paleo albedo differences data frame
# # note for a given set of coordinates rk fixed through time
# alb_diff = merge(alb_diff, 
#                  alb_diff_unique[,c('lat', 'long', 'rk_hadgem', 'rk_cam5', 'rk_cack')], 
#                  by=c('lat', 'long'))
# 
# # calculate radiative forcing
# alb_diff$rf_hadgem = alb_diff$alb_diff*100 * alb_diff$rk_hadgem
# alb_diff$rf_hadgem_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_hadgem
# 
# # calculate radiative forcing
# alb_diff$rf_cam5 = alb_diff$alb_diff*100 * alb_diff$rk_cam5
# alb_diff$rf_cam5_ice = alb_diff$alb_diff_ice*100 * alb_diff$rk_cam5
# 
# # calculate radiative forcing
# alb_diff$rf_cack = alb_diff$alb_diff * (-alb_diff$rk_cack)
# alb_diff$rf_cack_ice = alb_diff$alb_diff_ice * (-alb_diff$rk_cack)
# 
# saveRDS(alb_diff, paste0('output/forcing/RF_holocene.RDS'))

###############################################################################################################
## plot maps of radiative forcing
###############################################################################################################

# labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# labels_period = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# labels_period = c('0.5 - 0.05', '2 - 0.5', '4 - 2', '6 - 4', '8 - 6', '10 - 8', '12 - 10')
labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

# labels_period = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', '6 - 4 ka', '8 - 6 ka', 
# '10 - 8 ka', '12 - 10 ka')

diff_years = unique(alb_diff$year)
alb_diff$facets = labels_period[match(alb_diff$year, diff_years)]
alb_diff$facets = factor(alb_diff$facets, 
                         levels = labels_period, 
                         labels =  labels_period)

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(alb_diff$rf_hadgem), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

# values = c(0,0.4, 0.45, 0.5, 0.55, 0.6,1)
values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        na.value="grey", 
                                        name="RF (W/m^2)",
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
                                            na.value="transparent", 
                                            name=expression(Forcing (W/m^2)),
                                            limits = c(-thresh,thresh), 
                                            values = values)

# for (year in diff_years){
#   
#   alb_diff_sub = alb_diff[which(alb_diff$year == year),]
#   ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     sc_colour_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~.)+
#     # facet_wrap(year~.)+
#     theme_bw()+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=14),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=14),
#           legend.title = element_text(size=14)) +
#     coord_fixed()
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   ggsave(paste0('figures/alb_preds_radiative_point_single_', year, 'k.png'))
#   ggsave(paste0('figures/alb_preds_radiative_point_single_', year, 'k.pdf'))
# }
# 
# 
# for (year in diff_years){
#   
#   alb_diff_sub = alb_diff[which(alb_diff$year == year),]
#   ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=alb_diff_sub, aes(x=long, y=lat, fill = forcing)) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~.)+
#     # facet_wrap(year~.)+
#     theme_bw()+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=14),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=14),
#           legend.title = element_text(size=14)) +
#     coord_fixed()
#   
#   ggsave(paste0('figures/alb_preds_radiative_tile_single_', year, 'k.png'))
#   ggsave(paste0('figures/alb_preds_radiative_tile_single_', year, 'k.pdf'))
# }

pdf(paste0('figures/alb_preds_radiative_tile_pages_', alb_prod, '.pdf'))
for (year in diff_years){
  
  alb_diff_sub = alb_diff[which(alb_diff$year == year),]
  ice_old = ice_fort_diff_old[which(ice_fort_diff_old$ages == year ),]
  ice_young = ice_fort_diff_young[which(ice_fort_diff_young$ages == year ),]
  
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=alb_diff_sub, aes(x=long, y=lat, fill = forcing)) +
    geom_polygon(data=ice_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    geom_polygon(data=ice_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
    #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
    # sc_colour_diverge + 
    sc_fill_diverge + 
    # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
    # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
    #facet_grid(facets~.)+
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
  # coord_fixed(xlim = xlim, ylim = ylim)
  print(p)
  # scale_fill_brewer(type = "div", palette = 'Rd
  # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.png'))
  # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.pdf'))
}
dev.off()

# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=alb_diff, aes(x=long, y=lat, colour = forcing), size=1)+
#   sc_colour_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~.)+
#   # facet_wrap(year~.)+
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=14),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# # scale_fill_brewer(type = "div", palette = 'Rd
# pdf(paste0('figures/alb_preds_radiative_point_grid_', alb_prod, '.pdf'))


# b = bbox(pbs_ll)
# bbox(pbs_ll)[1] = -170
# bbox(pbs_ll)[2] = -45

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing)) +
  geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  sc_fill_diverge +
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # facet_grid(facets~.)+
  facet_wrap(~facets)+
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
  coord_fixed(xlim = xlim, ylim = ylim)
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/alb_preds_radiative_tile_wrap_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_preds_radiative_tile_wrap_', alb_prod, '.png'))


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing)) +
  geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~.)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.pdf'))


# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing_ice)) +
#   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~.)+
#   #facet_wrap(~facets)+
#   theme_bw(12)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=8),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=12)) +
#   coord_fixed(xlim = xlim, ylim = ylim)
# # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# # scale_fill_brewer(type = "div", palette = 'Rd
# ggsave(paste0('figures/alb_preds_radiative_ice_tile_grid_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_radiative_ice_tile_grid_', alb_prod, '.pdf'))


sc_fill_seq <- scale_fill_distiller(type = "seq",
                                    palette = "YlOrBr",#"YlGnBu",#
                                    # labels = labels,
                                    na.value="grey", 
                                    direction=-1,
                                    name="Radiative \nkernel")

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=subset(alb_diff, year == 50), aes(x=long, y=lat, fill = rk)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  
  #              colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  
  #              colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_seq +
  # scale_fill_gradientn(colours = terrain.colors(7)) +
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white')+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # facet_grid(facets~.)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/radiative_kernel_tile_', alb_prod, '.png'))
ggsave(paste0('figures/radiative_kernel_tile_', alb_prod, '.pdf'))

###############################################################################################################
## summarize rf at continental scale
###############################################################################################################

# read in grid
grid_NA <- readRDS("data/grid.RDS")
grid_df = as.data.frame(grid_NA, xy=TRUE)

spdf_2 <- as(grid_NA,'SpatialPolygonsDataFrame')

sf_data = st_as_sf(spdf_2)
grid_df$area <- st_area(sf_data) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')

alb_diff_merge = merge(alb_diff, grid_df, by = c('cell_id', 'lat', 'long'))


# drop cells that are not included for all time periods
cell_id_drop = as.numeric(names(which(table(alb_diff_merge$cell_id)<7)))
alb_diff_merge = alb_diff_merge[which(!(alb_diff_merge$cell_id %in% cell_id_drop)),]


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_merge, aes(x=long, y=lat, fill = rf_hadgem)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~.)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '.pdf'))


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_merge, aes(x=long, y=lat, fill = rf_hadgem_ice)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~.)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
# ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.pdf'))

foo = subset(alb_diff_merge, year==8000)
foo[which((foo$ice_old == 'ICE') & (foo$forcing_ice < 0)),]

bar = foo[which((foo$ice_old == 'ICE') & (foo$forcing_ice < 0)),'cell_id']

# # incorrect now  
# rf_holocene = alb_diff_merge %>% 
#   group_by(facets) %>%
#   dplyr::summarize(total_forcing = sum(forcing), 
#                    mean_forcing = mean(forcing), 
#                    mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))
# rf_holocene = rf_holocene %>%
#   mutate(pos = mean_forcing > 0)
# rf_holocene$type = rep("Holocene")
# 
# ggplot() +
#   geom_col(data=rf_holocene, aes(x = mean_forcing, y = facets, fill = pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)')
# 
# # data frame of IPCC forcings
# rf_ipcc = data.frame(facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
#                      mean_forcing = c(2.16, 0.54, 0.05, -0.20, -1.06),
#                      pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
#                      type = rep("Modern", 5))
# ggplot() +
#   geom_col(data=rf_ipcc, aes(x = mean_forcing, y = facets, fill = pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)')
# 
# 
# rf_all = rbind(rf_holocene[,c('facets', 'mean_forcing', 'pos', 'type')], rf_ipcc)
# 
# # why do we need this line?!
# rf_all$pos = !rf_all$pos
# 
# ggplot() +
#   geom_col(data=rf_all, aes(x = mean_forcing, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(16) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.pdf'))

###############################################################################################################
## summarize rf at continental scale by land class
###############################################################################################################

labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', 
                  '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
labels_period_flip = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', 
                       '6 - 4 ka', '8 - 6 ka', '10 - 8 ka', '12 - 10 ka')

rf_long = alb_diff_merge %>%
  pivot_longer(cols = c('rf_cack', 'rf_hadgem', 'rf_cam5'),
               names_to = 'rf_kernel',
               values_to = 'rf_value')
rf_long = data.frame(rf_long)

rf_long_ice_filled = alb_diff_merge %>%
  pivot_longer(cols = c('rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice'),
               names_to = 'rf_kernel',
               values_to = 'rf_value_ice')
rf_long_ice_filled = data.frame(rf_long_ice_filled)
rf_long_ice_filled$rf_kernel = substr(rf_long_ice_filled$rf_kernel, 1, nchar(rf_long_ice_filled$rf_kernel)-4)

rf_long = merge(rf_long, 
            rf_long_ice_filled[,c('cell_id', 'lat', 'long', 'year', 'month', 'rf_kernel', 'rf_value_ice')], 
            by = c('cell_id', 'lat', 'long', 'year', 'month', 'rf_kernel'))

rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
               'alb_diff'] = NA
rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
               'rf_value'] = NA

length(unique(rf_long$cell_id))

# alb_diff$facets_flip = alb_diff$facets
# alb_diff$facets_flip = factor(alb_diff$facets_flip, 
#                               levels = labels_period, labels = labels_period_flip)

cell_id_ice = unique(rf_long[which((!is.na(rf_long$ice_young))|
                                            (!is.na(rf_long$ice_old))), 'cell_id'])

length(unique(cell_id_ice))

cell_id_ice_free = unique(rf_long$cell_id[!(rf_long$cell_id %in% cell_id_ice)])
length(cell_id_ice_free)

rf_long_ice = subset(rf_long, cell_id %in% cell_id_ice)
rf_long_ice = subset(rf_long_ice, is.na(ice_young) & is.na(ice_old))
length(unique(rf_long_ice$cell_id))

ggplot(data=rf_long_ice) +
  geom_histogram(aes(x=alb_diff_ice)) +
  facet_grid(facets~month)

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=rf_long_ice, aes(x=long, y=lat, fill = rf_value)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~rf_kernel)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice.pdf'))

rf_holocene_ice = rf_long_ice %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(total_forcing = sum(rf_value), 
                   mean_forcing = mean(rf_value), 
                   mean_weighted = as.numeric(sum(rf_value * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2), 
                   .groups = 'keep')
rf_holocene_ice = rf_holocene_ice %>%
  mutate(pos = mean_forcing > 0)
rf_holocene_ice = rf_holocene_ice %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_holocene_ice$type = rep("once ice")

rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'mean_forcing'] = NA
rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'mean_weighted'] = NA
rf_holocene_ice

ggplot() +
  geom_col(data=rf_holocene_ice, aes(x = mean_forcing, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

ggplot() +
  geom_col(data=rf_holocene_ice, aes(x = mean_weighted, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)



rf_long_ice_filled = subset(rf_long, cell_id %in% cell_id_ice)
# alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
length(unique(rf_long_ice_filled$cell_id))

ggplot(data=rf_long_ice_filled) +
  geom_histogram(aes(x=alb_diff_ice)) +
  facet_grid(facets~rf_kernel)

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=rf_long_ice_filled, aes(x=long, y=lat, fill = rf_value_ice)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge +
  sc_fill_diverge +
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~rf_kernel)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80))
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice_filled.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice_filled.pdf'))


rf_holocene_ice_filled = rf_long_ice_filled %>%
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(total_forcing = sum(rf_value_ice),
                   mean_forcing = mean(rf_value_ice),
                   mean_weighted = as.numeric(sum(rf_value_ice * area) / sum(area)),
                   cells = n_distinct(cell_id),
                   total_area = sum(area_km2),
                   .groups = 'keep')
rf_holocene_ice_filled = rf_holocene_ice_filled %>%
  mutate(pos = mean_forcing > 0)
rf_holocene_ice_filled = rf_holocene_ice_filled %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_holocene_ice_filled$type = rep("once ice filled")

ggplot() +
  geom_col(data=rf_holocene_ice_filled, aes(x = mean_forcing, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

ggplot() +
  geom_col(data=rf_holocene_ice_filled, aes(x = mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)


rf_long_ice_free = subset(rf_long, cell_id %in% cell_id_ice_free)
# alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))

rf_holocene_ice_free = rf_long_ice_free %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(total_forcing = sum(rf_value), 
                   mean_forcing = mean(rf_value), 
                   mean_weighted = as.numeric(sum(rf_value * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2), 
                   .groups = 'keep')
rf_holocene_ice_free = rf_holocene_ice_free %>%
  mutate(pos = mean_forcing > 0)
rf_holocene_ice_free = rf_holocene_ice_free %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_holocene_ice_free$type = rep("never ice")


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=rf_long_ice_free, aes(x=long, y=lat, fill = rf_value)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~rf_kernel)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_never_ice.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_never_ice.pdf'))

ggplot() +
  geom_col(data=rf_holocene_ice_free, aes(x = mean_forcing, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

ggplot() +
  geom_col(data=rf_holocene_ice_free, aes(x = mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

rf_long_land = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))

rf_holocene = rf_long_land %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(total_forcing = sum(rf_value), 
                   mean_forcing = mean(rf_value), 
                   mean_weighted = as.numeric(sum(rf_value * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2),
                   .groups = 'keep')

rf_holocene = rf_holocene %>%
  mutate(pos = mean_forcing > 0)
rf_holocene = rf_holocene %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_holocene$type = rep("land")

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=rf_long_land, aes(x=long, y=lat, fill = rf_value)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~rf_kernel)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_land.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_land.pdf'))


ggplot() +
  geom_col(data=rf_holocene, aes(x = mean_forcing, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

ggplot() +
  geom_col(data=rf_holocene, aes(x = mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

rf_holocene_continent = rf_long %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(total_forcing = sum(rf_value_ice), 
                   mean_forcing = mean(rf_value_ice), 
                   mean_weighted = as.numeric(sum(rf_value_ice * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2),
                   .groups = 'keep')
rf_holocene_continent = rf_holocene_continent %>%
  mutate(pos = mean_forcing > 0)
rf_holocene_continent = rf_holocene_continent %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_holocene_continent$type = rep("land + ice")

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=rf_long, aes(x=long, y=lat, fill = rf_value_ice)) +
  # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  facet_grid(facets~rf_kernel)+
  #facet_wrap(~facets)+
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=8),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_land_ice_filled.png'))
ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_land_ice_filled.pdf'))


ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = mean_forcing, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)


rf_NA = rbind(rf_holocene, rf_holocene_ice)
rf_NA = rbind(rf_NA, rf_holocene_ice_free)
rf_NA = rbind(rf_NA, rf_holocene_continent)
rf_NA$pos = factor(rf_NA$pos, levels = c(TRUE, FALSE))
rf_NA$pos_weighted = factor(rf_NA$pos_weighted, levels = c(TRUE, FALSE))

rf_NA$type = factor(rf_NA$type, levels = c('land + ice', 'land', 'never ice', 'once ice'))

ggplot() +
  geom_col(data=rf_NA, aes(x = mean_forcing, y = facets, fill = pos)) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time period (k years)') +
  facet_grid(type~rf_kernel)
ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.png'), width=6, height=6)
ggsave(paste0('figures/RF_holocene_radiative_bar_landclass_facet_', alb_prod, '.pdf'), width=6, height=6)

ggplot() +
  geom_col(data=rf_NA, aes(x = mean_weighted, y = facets, fill = pos_weighted)) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(type~rf_kernel)
ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.png'), width=6, height=6)
ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.pdf'), width=6, height=6)

ggplot() +
  geom_col(data=rf_NA, aes(x = mean_forcing, y = facets, fill = type), position='dodge') +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  theme_bw(14) +
  theme(panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82'),
        panel.grid.minor.x = element_line(linewidth = 0.3, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)
ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.png'), width=8, height=6)
ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.pdf'), width=8, height=6)

ggplot() +
  geom_point(data=rf_NA, aes(x = mean_forcing, y = facets, colour = type)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  # theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~.)

# data frame of IPCC forcings
rf_ipcc = data.frame(rf_kernel = NA,
                     facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
                     mean_forcing = c(2.16, 0.54, 0.05, -0.20, -1.06),
                     pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                     type = rep("modern", 5))
ggplot() +
  geom_col(data=rf_ipcc, aes(x = mean_forcing, y = facets, fill = pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)')


rf_all = rbind(rf_holocene[,c('rf_kernel', 'facets', 'mean_forcing', 'pos', 'type')], 
               rf_ipcc)

# why do we need this line?!
rf_all$pos = !rf_all$pos

ggplot() +
  geom_col(data=rf_all, aes(x = mean_forcing, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  theme_bw(16) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.pdf'))


rf_all_class = rbind(rf_NA[,c('rf_kernel', 'facets', 'mean_forcing', 'pos', 'type')], rf_ipcc)

# why do we need this line?!
# rf_all_class$pos = !rf_all$pos

ggplot() +
  geom_col(data=rf_all_class, aes(x = mean_forcing, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)


# ###############################################################################################################
# ## summarize rf at continental scale by land class
# ###############################################################################################################
# 
# labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', 
#                   '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
# labels_period_flip = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', 
#                        '6 - 4 ka', '8 - 6 ka', '10 - 8 ka', '12 - 10 ka')
# 
# # alb_diff_melt = alb_diff_merge %>%
# #   pivot_longer(cols = c('rf_cack', 'rf_hadgem', 'rf_cam5'),
# #                names_to = 'rf_kernel',
# #                values_to = 'rf_value')
# # 
# # 
# # foo = alb_diff_merge %>%
# #   pivot_longer(cols = c('rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice'),
# #                names_to = 'rf_kernel_ice',
# #                values_to = 'rf_value_ice')
# 
# alb_diff_merge[which((!is.na(alb_diff_merge$ice_young))|(!is.na(alb_diff_merge$ice_old))), 
#                'alb_diff'] = NA
# alb_diff_merge[which((!is.na(alb_diff_merge$ice_young))|(!is.na(alb_diff_merge$ice_old))), 
#                'rf_hadgem'] = NA
# 
# length(unique(alb_diff_merge$cell_id))
# 
# alb_diff$facets_flip = alb_diff$facets
# alb_diff$facets_flip = factor(alb_diff$facets_flip, 
#                               levels = labels_period, labels = labels_period_flip)
# 
# cell_id_ice = unique(alb_diff_merge[which((!is.na(alb_diff_merge$ice_young))|
#                                             (!is.na(alb_diff_merge$ice_old))), 'cell_id'])
# length(cell_id_ice)
# 
# cell_id_ice_free = unique(alb_diff_merge$cell_id[!(alb_diff_merge$cell_id %in% cell_id_ice)])
# length(cell_id_ice_free)
# 
# alb_diff_ice = subset(alb_diff_merge, cell_id %in% cell_id_ice)
# alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(alb_diff_ice$cell_id))
# 
# ggplot(data=alb_diff_ice) +
#   geom_histogram(aes(x=alb_diff_ice)) +
#   facet_grid(facets~.)
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_ice, aes(x=long, y=lat, fill = rf_hadgem)) +
#   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~.)+
#   #facet_wrap(~facets)+
#   theme_bw(12)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=8),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=12)) +
#   coord_fixed(xlim = xlim, ylim = ylim)
# # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# # scale_fill_brewer(type = "div", palette = 'Rd
# ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice.png'))
# ggsave(paste0('figures/RF_holocene_tile_grid_', alb_prod, '_once_ice.pdf'))
# 
# rf_holocene_ice = alb_diff_ice %>% 
#   group_by(facets) %>%
#   dplyr::summarize(total_forcing = sum(forcing), 
#                    mean_forcing = mean(forcing), 
#                    mean_weighted = as.numeric(sum(forcing * area) / sum(area)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos = mean_forcing > 0)
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos_weighted = mean_weighted > 0)
# rf_holocene_ice$type = rep("once ice")
# 
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'mean_forcing'] = NA
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'mean_weighted'] = NA
# rf_holocene_ice
# 
# ggplot() +
#   geom_col(data=rf_all_class, aes(x = mean_weighted, y = facets, fill = pos_weighted), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)

###############################################################################################################
## summarize rf by region
###############################################################################################################

alb_diff_merge_sub = subset(alb_diff_merge, )

## CASE: ENA
xlo = -100
xhi = -52
ylo = 48
yhi = 67
xlim = c(xlo, xhi)
ylim = c(ylo, yhi)

boxes_ENA = data.frame(yhi = 67, ylo = 48, xlo = -100, xhi = -52, id="ECAN")
boxes_ENA = transform(boxes_ENA, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)

alb_diff_merge_ENA = subset(alb_diff_merge, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_ENA = alb_diff_merge_ENA %>% 
#   group_by(facets) %>%
#   dplyr::summarize(total_forcing = sum(forcing), 
#                    mean_forcing = mean(forcing), 
#                    mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_summary_ENA = alb_diff_merge_ENA %>% 
  group_by(facets) %>%
  dplyr::summarize(total_forcing = sum(forcing_ice), 
                   mean_forcing = mean(forcing_ice), 
                   mean_weighted = as.numeric(sum(forcing_ice * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2))

rf_summary_ENA = rf_summary_ENA %>%
  mutate(pos = mean_forcing > 0)
rf_summary_ENA = rf_summary_ENA %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_summary_ENA$type = rep("ENA")


# CASE: HEMLOCK
xlo = -90
xhi = -60 
ylo = 37
yhi = 48
xlim = c(xlo, xhi)
ylim = c(ylo, yhi)

boxes_HEM = data.frame(yhi = 48, ylo = 37, xlo = -90, xhi = -60, id="NEUS/SEC")
boxes_HEM = transform(boxes_HEM, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)

alb_diff_merge_HEM = subset(alb_diff_merge, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_HEM = alb_diff_merge_HEM %>% 
#   group_by(facets) %>%
#   dplyr::summarize(total_forcing = sum(forcing), 
#                    mean_forcing = mean(forcing), 
#                    mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_summary_HEM = alb_diff_merge_HEM %>% 
  group_by(facets) %>%
  dplyr::summarize(total_forcing = sum(forcing_ice), 
                   mean_forcing = mean(forcing_ice), 
                   mean_weighted = as.numeric(sum(forcing_ice * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2))


rf_summary_HEM = rf_summary_HEM %>%
  mutate(pos = mean_forcing > 0)
rf_summary_HEM = rf_summary_HEM %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_summary_HEM$type = rep("HEM")


# CASE: WCAN
xlo = -165
xhi = -105
ylo = 45
yhi = 75
xlim = c(xlo, xhi)
ylim = c(ylo, yhi)

xmid = -141

boxes_WCAN = data.frame(yhi = 75, ylo = 45, xlo = -165, xhi = -105, id="WCAN/AK")
boxes_WCAN = transform(boxes_WCAN, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)

alb_diff_merge_WCAN = subset(alb_diff_merge, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_WCAN = alb_diff_merge_WCAN %>% 
#   group_by(facets) %>%
#   dplyr::summarize(total_forcing = sum(forcing), 
#                    mean_forcing = mean(forcing), 
#                    mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_summary_WCAN = alb_diff_merge_WCAN %>% 
  group_by(facets) %>%
  dplyr::summarize(total_forcing = sum(forcing_ice), 
                   mean_forcing = mean(forcing_ice), 
                   mean_weighted = as.numeric(sum(forcing_ice * area) / sum(area)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2))

rf_summary_WCAN = rf_summary_WCAN %>%
  mutate(pos = mean_forcing > 0)
rf_summary_WCAN = rf_summary_WCAN %>%
  mutate(pos_weighted = mean_weighted > 0)
rf_summary_WCAN$type = rep("WCAN")

###############################################################################################################
## show regions on a map
###############################################################################################################



ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="darkgrey", fill="grey", alpha=0.4) +
  geom_rect(data=boxes_ENA, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
            color="dodgerblue", fill="transparent", lwd=1) + 
  geom_text(data=boxes_ENA, aes(x=labx, y=laby, label=id), color="dodgerblue", size=8) + 
  geom_rect(data=boxes_HEM, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
            color="darkorange", fill="transparent", lwd=1) + 
  geom_text(data=boxes_HEM, aes(x=labx, y=laby, label=id), color="darkorange", size=8) + 
  geom_rect(data=boxes_WCAN, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
            color="deeppink", fill="transparent", lwd=1) + 
  geom_text(data=boxes_WCAN, aes(x=labx, y=laby, label=id), color="deeppink", size=8) + 
  # geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing)) +
  #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # facet_grid(facets~.)+
  #facet_wrap(~facets)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()
# coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/map_case_study_regions.png'))
ggsave(paste0('figures/map_case_study_regions.pdf'))


###############################################################################################################
## merge and plot region rf data frames
###############################################################################################################

# rf_region = rbind(rf_holocene,
#                   rf_summary_ENA,
#                   rf_summary_HEM,
#                   rf_summary_WCAN)

rf_region = rbind(rf_holocene_continent,
                  rf_summary_ENA,
                  rf_summary_HEM,
                  rf_summary_WCAN)

rf_region$type = factor(rf_region$type, 
                        levels = c('land + ice', 'ENA', 'HEM', 'WCAN'),
                        labels = c('Continental', 'ECAN', 'NEUS/SEC', 'WCAN/AK'))

# rf_region$facets
# rf_region$pos


# ggplot() +
#   geom_col(data=bar, aes(x = mean_forcing, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")

ggplot() +
  geom_col(data=rf_region, aes(x = mean_forcing, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  # theme(legend.position = "none",
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank(),
  #       # panel.background = element_blank(),
  #       strip.text = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       axis.text = element_text(size=12)) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.png'), height=6, width=6, dpi=300)
ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.pdf'), height=6, width=6, dpi=300)

ggplot() +
  geom_col(data=rf_region, aes(x = mean_weighted, y = facets, fill = !pos_weighted), width=0.9) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  # theme(legend.position = "none",
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank(),
  #       # panel.background = element_blank(),
  #       strip.text = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       axis.text = element_text(size=12)) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_weighted_bar_regions_facet_', alb_prod, '.png'), height=6, width=6, dpi=300)
ggsave(paste0('figures/alb_preds_radiative_weighted_bar_regions_facet_', alb_prod, '.pdf'), height=6, width=6, dpi=300)


ggplot() +
  geom_bar(data=rf_region, aes(x = facets, y = mean_forcing, fill = type), stat='identity', position='dodge') +
  # scale_y_discrete(limits=rev) +
  theme_bw(14) +
  # theme(legend.position = "none") +
  xlab('time period (k years)') +
  ylab(bquote(radiative~forcing~(W/m^2))) #+
# facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_bar_regions_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_radiative_bar_regions_', alb_prod, '.pdf'))
