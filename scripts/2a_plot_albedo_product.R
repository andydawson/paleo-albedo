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

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

borders <- sp::bbox(pbs_ll)

out_rast <- raster::raster(xmn = floor(borders[1,1]),
                           xmx = ceiling(borders[1,2]),
                           ymn = floor(borders[2,1]),
                           ymx = ceiling(borders[2,2]),
                           resolution = 0.5,
                           crs = CRS('+init=epsg:4326'))

grid <- raster::setValues(out_rast, 1:ncell(out_rast))

###############################################################################################################
## LCT maps
###############################################################################################################

bhr_sw  = raster(sprintf('data/GLOBALBEDO/albedo/GlobAlbedo.merge.albedo.005.2004%02d.nc', 3), varname='BHR_SW')

# grid = readRDS("data/grid.RDS")

na_cellid = extract(grid, pbs_ll)
na_coords   = xyFromCell(grid, unlist(na_cellid))

alb_na = extract(bhr_sw, na_coords)#, buffer=0.1, fun=mean)

# alb_na = extract(bhr_sw, na_coords, buffer=0.1, fun=mean)

idx_missing = which(is.na(alb_na))
alb_na[idx_missing] = extract(bhr_sw, na_coords[idx_missing,], buffer=180000, fun=function(x) mean(x[which(x>0)]))



alb_na = data.frame(na_coords, alb=alb_na)
colnames(alb_na) = c('long', 'lat', 'alb')

# 
# 
# cell_id <- raster::extract(grid, alb_preds[,c('long', 'lat')])
# 
# alb_grid <- data.frame(cell_id, alb_preds)
# coords   = xyFromCell(grid, alb_grid$cell_id)
# colnames(coords) = c('long', 'lat')
# 
# foo = rasterToPoints(grid)
# bar = raster::extract(bhr_sw, foo[,c('x', 'y')])
# alb_na = data.frame(foo, alb = bar)
# colnames(alb_na) = c('long', 'lat', 'layer', 'alb')



breaks = c(0, 10, 20, 30, 40, 60, 80, 100)/100
# breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# cover_melt2$value_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
labels = c("   0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.6", "0.6 - 0.8",  "0.8 -  1")

alb_na$value_bin = cut(alb_na$alb, breaks, labels=FALSE)


sc_fill_seq <- scale_fill_brewer(type = "seq",
                                 palette = "YlOrBr",#"BrBG",
                                 labels = labels,
                                 na.value="grey",
                                 direction=1,
                                 name="Albedo")

sc_colour_seq <- scale_colour_brewer(type = "seq",
                                 palette = "YlOrBr",#"BrBG",
                                 labels = labels,
                                 na.value="grey",
                                 direction=1,
                                 name="Albedo")

# sc_fill_seq <- scale_fill_distiller(type = "seq",
#                                  palette = "Oranges",#"BrBG",
#                                  # labels = labels,
#                                  na.value="grey",
#                                  direction=-1,
#                                  name="Albedo")

ggplot() +
  # geom_polygon(data=pbs, aes(x=x, y=y, group=group), color='black', fill=NA) +
  geom_polygon(data=pbs_ll, aes(long, lat, group = group), color="grey", fill="grey") +
  # geom_point(data=alb_na, aes(x=long, y=lat, colour=alb)) +
  geom_tile(data=alb_na, aes(x=long, y=lat, fill=factor(value_bin), colour=factor(value_bin)), na.rm=TRUE) +
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
  sc_fill_seq +
  sc_colour_seq
ggsave('figures/GLOBALBEDO_march.png')
ggsave('figures/GLOBALBEDO_march.pdf')
