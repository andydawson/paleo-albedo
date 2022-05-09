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

# rk = brick('data/radiative-kernels/HadGEM3-GA7.1_srf_kernel_L19.nc', level=1)
# rk_proj = projectRaster(rk[[4]], crs =CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
rk_month = raster::extract(rk[[4]], diff_paleo_spatial)

diff_paleo_unique$rk = rk_month 

diff_paleo = merge(diff_paleo, diff_paleo_unique[,c('lat', 'long', 'rk')], by=c('lat', 'long'))

diff_paleo$forcing = diff_paleo$diff * diff_paleo$rk

labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
diff_years = years[-1]
diff_paleo$facets = labels[match(diff_paleo$year, diff_years)]
diff_paleo$facets = factor(diff_paleo$facets, levels =  c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8'))

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(diff_paleo$forcing), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

values = c(0,0.4, 0.45, 0.5, 0.55, 0.6,1)

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
ggsave('figures/alb_preds_radiative_subset_point.png')
ggsave('figures/alb_preds_radiative_subset_point.pdf')
