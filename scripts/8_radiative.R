alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

# 
# diffs_paleo = readRDS('data/lct_paleo.RDS')
# 
# sites = sort(unique(lct_paleo$site))
# N_sites = length(sites)
# 
# lct_paleo = lct_paleo[!duplicated(lct_paleo[,c('lat', 'long')]),]
# 
# head(lct_paleo)
# lct_paleo$long360 = abs(lct_paleo$long - 180)
# 
# lct_paleo_spatial = SpatialPointsDataFrame(coords=lct_paleo[,c('long360', 'lat')], 
#                                            data=lct_paleo, 
#                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# diff_paleo = readRDS('data/preds_alb_diffs_all.RDS')
diff_paleo = readRDS('data/preds_alb_diffs_sub.RDS')

diff_paleo_unique = diff_paleo[!duplicated(diff_paleo[,c('lat', 'long')]),]

head(diff_paleo_unique)
diff_paleo_unique$long360 = abs(diff_paleo_unique$long - 180)

diff_paleo_spatial = SpatialPointsDataFrame(coords=diff_paleo_unique[,c('long360', 'lat')], 
                                            data=diff_paleo_unique, 
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


rk = brick('data/radiative-kernels/HadGEM3-GA7.1_TOA_kernel_L19.nc', level=1, varname='albedo_sw_cs')
# foo = as.data.frame(rk, xy=TRUE, na.rm=TRUE)
# ggplot() +
#   geom_raster(data=foo, aes(x=x, y=y, fill=X4))

# rk = brick('data/radiative-kernels/HadGEM3-GA7.1_srf_kernel_L19.nc', level=1)
# rk_proj = projectRaster(rk[[4]], crs =CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
rk_month = raster::extract(rk[[4]], diff_paleo_spatial)

diff_paleo_unique$rk = rk_month 

diff_paleo = merge(diff_paleo, diff_paleo_unique[,c('lat', 'long', 'rk')], by=c('lat', 'long'))

diff_paleo$forcing = diff_paleo$diff*100 * diff_paleo$rk

# labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')

diff_years = unique(diff_paleo$year)
diff_paleo$facets = labels[match(diff_paleo$year, diff_years)]
diff_paleo$facets = factor(diff_paleo$facets, levels =  labels)

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(diff_paleo$forcing), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

# values = c(0,0.4, 0.45, 0.5, 0.55, 0.6,1)
values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        na.value="grey", 
                                        name="Radiative Forcing (W/m^2)",
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

for (year in diff_years){
  
  diff_paleo_sub = diff_paleo[which(diff_paleo$year == year),]
  ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_point(data=diff_paleo_sub, aes(x=long, y=lat, colour = forcing), size=1)+
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
  ggsave(paste0('figures/alb_preds_radiative_subset_point_', year, 'k.png'))
  ggsave(paste0('figures/alb_preds_radiative_subset_point_', year, 'k.pdf'))
}


for (year in diff_years){
  
  diff_paleo_sub = diff_paleo[which(diff_paleo$year == year),]
  ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=diff_paleo_sub, aes(x=long, y=lat, fill = forcing)) +
    #geom_point(data=diff_paleo_sub, aes(x=long, y=lat, colour = forcing), size=1)+
    # sc_colour_diverge + 
    sc_fill_diverge + 
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
  ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.png'))
  ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.pdf'))
}

pdf('figures/alb_preds_radiative_tile_pages.pdf')
for (year in diff_years){
  
  diff_paleo_sub = diff_paleo[which(diff_paleo$year == year),]
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=diff_paleo_sub, aes(x=long, y=lat, fill = forcing)) +
    #geom_point(data=diff_paleo_sub, aes(x=long, y=lat, colour = forcing), size=1)+
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
  print(p)
  # scale_fill_brewer(type = "div", palette = 'Rd
  # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.png'))
  # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.pdf'))
}
dev.off()

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_point(data=diff_paleo, aes(x=long, y=lat, colour = forcing), size=1)+
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
ggsave(paste0('figures/alb_preds_radiative_subset_point.png'))
ggsave(paste0('figures/alb_preds_radiative_subset_point.pdf'))



ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=diff_paleo, aes(x=long, y=lat, fill = forcing)) +
  #geom_point(data=diff_paleo_sub, aes(x=long, y=lat, colour = forcing), size=1)+
  # sc_colour_diverge + 
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
  coord_fixed()
# scale_fill_brewer(type = "div", palette = 'Rd
ggsave(paste0('figures/alb_preds_radiative_subset_tile.png'))
ggsave(paste0('figures/alb_preds_radiative_subset_tile.pdf'))


###############################################################################################################
## summarize
###############################################################################################################


library(sf)
library(raster) 
library(dplyr)

# #Create grid
# grid_ares <- data.frame(lat = ylim, lon = xlim) %>% 
#   st_as_sf(coords=c('lon','lat'), crs=4326) %>% 
#   st_make_grid(cellsize = c(2,1)) %>%
#   st_cast("MULTIPOLYGON") %>%
#   st_sf() %>%
#   mutate(cellid = row_number()) 

# grid$area <- st_area(grid) #area of each "square"
# grid$area_km2 <- units::set_units(grid$area, "km^2") # in km2



grid <- readRDS("data/grid.RDS")
grid_df = as.data.frame(grid, xy=TRUE)

spdf_2 <- as(grid,'SpatialPolygonsDataFrame')

sf_data = st_as_sf(spdf_2)
grid_df$area <- st_area(sf_data) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2

colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')

diff_paleo_merge = merge(diff_paleo, grid_df, by = c('cell_id'))

# diff_paleo$area = grid_df[]


rf_summary = diff_paleo_merge %>% 
  group_by(facets) %>%
  dplyr::summarize(total_forcing = sum(forcing), 
                   mean_forcing = mean(forcing), 
                   mean_weighted = sum(forcing * area) / sum(area),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2))

detach(package:plyr)
foo = diff_paleo_merge %>% 
  group_by(year) %>%
  dplyr::summarize(total = mean(forcing*area)/sum(area), .groups = "keep")
foo
dim(foo)


foo = diff_paleo_merge %>% 
  group_by(year) %>%
  dplyr::summarize(total = sum(forcing), .groups = "keep")
foo
dim(foo)

####
rf_summary = rf_summary %>%
  mutate(pos = mean_forcing > 0)
rf_summary$type = rep("Holocene")

ggplot() +
  geom_col(data=rf_summary, aes(x = mean_forcing, y = facets, fill = pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)')
  

foo = data.frame(facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
           mean_forcing = c(2.16, 0.54, 0.05, -0.20, -1.06),
           pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
           type = rep("IPCC", 5))
ggplot() +
  geom_col(data=foo, aes(x = mean_forcing, y = facets, fill = pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)')


bar = rbind(rf_summary[,c('facets', 'mean_forcing', 'pos', 'type')], foo)
bar$pos = !bar$pos

ggplot() +
  geom_col(data=bar, aes(x = mean_forcing, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave('figures/rf_bar_both.pdf')
ggsave('figures/rf_bar_both.png')

