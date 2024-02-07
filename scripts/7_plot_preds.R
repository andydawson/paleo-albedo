library(ggplot2)
library(fields)
# library(dplyr)
library(reshape2)
library(raster)


alb_prod = "bluesky"

###############################################################################################################
## read in prediction and map data
###############################################################################################################

alb_preds = readRDS(paste0('data/paleo_predict_gam_summary_', alb_prod, '.RDS'))

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')


breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100
# breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 90, 100)/100
# breaks = c(0, 4, 8, 12, 16, 20, 30, 40, 60, 100)/100
# breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# cover_melt2$alb_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
# labels = c("0 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80",  "80 - 90", "90 - 100")
# labels = c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8",  "0.8 - 0.9", "0.9 - 1")

labels = seq(1, length(breaks)-1)


ages = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)


# plot_cuts = c(0, 1e-5, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
# n_plot_cuts = length(plot_cuts) - 1


ylim = c(12, 82) 
xlim = c(-166, -50) 




###############################################################################################################
## read in prediction and map data
###############################################################################################################

proj_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'


ice = readRDS('data/map-data/ice/glacier_shapefiles_21-1k.RDS')
ice_years = seq(1, 21)*1000

# grid_ice = data.frame(matrix(NA, nrow=0, ncol=5))#land_grid_NA
# land_grid_NA_ice_ages$ice = NA
ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))
alb_preds$ice = NA

# ages_cat_12 = c(ages, 12000)
# ages[which(ages == 11500)] = 12000

for (i in 1:length(ages_sub)){
  
  print(i)
  idx_age = which(alb_preds$year == ages_sub[i])
  
  # print(length(idx_age))
  
  # coords_grid  = SpatialPoints(land_grid_NA[,c('x', 'y')], proj4string=CRS(proj_WGS84))
  coords_veg  = SpatialPoints(alb_preds[idx_age,c('long', 'lat')], proj4string=CRS(proj_WGS84))

  # if (i %in% c(1, 2, 3)){
  #   veg_interp[idx_age, 'ice'] = NA
  # } else {
  idx_ice_match = which.min(abs(ages_sub[i] - ice_years)) 
  
  proj4string(ice[[idx_ice_match]]) = proj_WGS84
  ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))
  
  # ice_status_grid = over(coords_grid, ice[[idx_ice_match]])
  ice_status_veg = over(coords_veg, ice[[idx_ice_match]])
  
  ice_fort_age = fortify(ice[[idx_ice_match]])
  
  ice_fort = rbind(ice_fort, 
                   data.frame(ice_fort_age, 
                              ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)), 
                              ages = rep(ages_sub[i], nrow(ice_fort_age))))
  
  # grid_ice = rbind(grid_ice,
  #                  data.frame(land_grid_NA,
  #                             ice = ice_status_grid[,which(substr(colnames(ice_status_grid), 1,4)=='SYMB')],
  #                             age = rep(ages[i], nrow(land_grid_NA))))
  # 
  alb_preds[idx_age, 'ice'] = ice_status_veg[,which(substr(colnames(ice_status_veg), 1,4)=='SYMB')]
  # # }
  
}

ice_sub = ice_fort[which(ice_fort$ages %in% ages_sub),]

ice_sub$facets = as.character(ice_sub$ages/1000)
ice_sub$facets = factor(ice_sub$facets, 
                        levels = c('0.05', '0.5', '2', '4', '6', '8', '10', '12'),
                        labels = c('0.05 ka', '0.5 ka', '2 ka', '4 ka', '6 ka', '8 ka', '10 ka', '12 ka'))

saveRDS(ice_fort, 'data/ice_fort.RDS')

ice_fill = 'gainsboro'
ice_colour = 'gray60'

ice_fill_dark = 'ivory3'
ice_colour_dark = 'gray40'

###############################################################################################################
## COLOUR PALETTES
###############################################################################################################

# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_colour <- scale_colour_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                                 na.value="white", name="Percent", drop=FALSE)


# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_fill <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                             na.value="white", name="Percent", drop=FALSE)


sc_fill_seq <- scale_fill_brewer(type = "seq",
                                 palette = "Greens",#"BrBG",#"YlGnBu",#
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
sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        direction=1,
                                        na.value="grey", 
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)


sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
                                        # labels = labels,
                                        na.value="grey",
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)



###############################################################################################################
## summary plots of values
###############################################################################################################

alb_preds$alb_bin = cut(alb_preds$alb_mean, breaks, labels=FALSE)

###############################################################################################################
## aggregate to 1 degree by 1 degree tiles
###############################################################################################################

# breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100
# labels = c("0 - 4", "4 - 8", "8 - 12", "12 - 16", "16 - 20", "20 - 40", "40 - 60",  "60 - 80", "80 - 100")

breaks_alb = c(0, 5, 10, 20, 30, 40, 60, 80, 100)/100
labels_alb = c("0 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 60", "60 - 80", "80 - 100")

grid <- readRDS("data/grid.RDS")
paleo_sim_gam = readRDS('data/paleo_predict_gam_bluesky.RDS')



cell_id <- raster::extract(grid, alb_preds[,c('long', 'lat')])

alb_grid <- data.frame(cell_id, alb_preds)
coords   = xyFromCell(grid, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords, alb_grid[,c('x', 'y', 'cell_id', 'year', 'alb_mean', 'alb_sd')])

# IF USE LCT AT POINT SCALE
# take sd as well, mean of other vars too 
# alb_grid = aggregate(alb_pred ~ cell_id + long + lat + x + y + year, alb_grid, median)
# alb_grid = aggregate(alb_mean ~ cell_id + long + lat + year, alb_grid, median)

alb_grid$alb_bin = cut(alb_grid$alb_mean, breaks, labels=FALSE)

breaks_sd = c(0, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09)
labels_sd = c("0 - 0.04", "0.04 - 0.05", "0.05 - 0.06", "0.06 - 0.07", "0.07 - 0.08", "0.08 - 0.09")
alb_grid$alb_sd_bin = cut(alb_grid$alb_sd, breaks_sd, labels=FALSE, include.lowest=TRUE)

alb_grid$alb_cv = alb_grid$alb_sd / alb_grid$alb_mean
breaks_cv = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 10)
labels_cv = c("0 - 0.05", "0.05 - 0.1", "0.1 - 0.15", "0.15 - 0.2", "0.25 - 0.3", "0.3 - 10")
alb_grid$alb_cv_bin = cut(alb_grid$alb_cv, breaks_cv, labels=FALSE, include.lowest=TRUE)

years = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
#years = c(50, 1000, 3000, 5000, 7000, 9000, 11000)
# years = c(50, 6000, 11000)

alb_grid_sub = subset(alb_grid, year %in% years) 

labels_year = c('0.05', '0.5', '2', '4', '6', '8', '10', '12')
#labels = c('0.05', '1', '3', '5', '7', '9', '11')


#renaming labels to create facets column in alb_grid_sub 
alb_grid_sub$facets = labels_year[match(alb_grid_sub$year, years)]
alb_grid_sub$facets = factor(alb_grid_sub$facets, levels = labels_year)


###############################################################################################################
## Plot albedo predictions and uncertainty
###############################################################################################################

# albedo predictions binned 
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_bin))) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_manual(values=terrain.colors(10), na.value='transparent', name = "Albedo", labels=breaks[-1]) + 
  # sc_fill_seq + 
  scale_fill_brewer(type='seq', 
                    palette='YlOrBr', 
                    na.value='transparent', 
                    name = "Albedo", 
                    direction=1, 
                    labels = labels_alb) +
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
ggsave(paste0('figures/alb_preds_binned_tile_grid_', alb_prod, '.pdf'))

# albedo predictions binned 
# wrap: by year 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_manual(values=terrain.colors(10), na.value='transparent', name = "Albedo", labels=breaks[-1]) + 
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
  # sc_fill_seq + 
  # facet_grid(facets~variable)+
  facet_wrap(facets~.)+
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
ggsave(paste0('figures/alb_preds_binned_tile_wrap_', alb_prod, '.pdf'))

# albedo predictions binned 
# pages: single year per page 
pdf(paste0('figures/alb_preds_binned_tile_pages_', alb_prod, '.pdf'))
for (year in years){
  
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, year==year), aes(x=long,y=lat, fill=factor(alb_bin))) +
    # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
    # sc_fill_seq + 
    scale_fill_brewer(type='seq', palette='YlOrBr', 
                      na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
    # facet_grid(facets~.)+
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
  print(p)
}
dev.off()


# albedo prediction standard deviation 
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_sd)) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_distiller(type='seq', palette='YlOrBr', 
                       na.value='transparent', name = "SD", direction=1) +
  # sc_fill_seq +
  # facet_grid(facets~variable)+
  facet_wrap(facets~.)+
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
ggsave(paste0('figures/alb_preds_sd_tile_wrap_', alb_prod, '.pdf'))

# albedo prediction standard deviation binned
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_sd_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "SD", direction=1, labels = labels_sd) +
  facet_wrap(facets~.)+
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
ggsave(paste0('figures/alb_preds_sd_binned_tile_wrap_', alb_prod, '.pdf'))

# albedo prediction standard deviation binned
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_sd_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "SD", direction=1, labels = labels_sd) +
  facet_grid(facets~.)+
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
ggsave(paste0('figures/alb_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))

# albedo prediction standard deviation binned
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_cv_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "CV", direction=1, labels = labels_cv) +
  facet_wrap(facets~.)+
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
ggsave(paste0('figures/alb_preds_cv_binned_tile_wrap_', alb_prod, '.pdf'))

# albedo prediction standard deviation binned
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_cv_bin))) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "CV", direction=1, labels = labels_sd) +
  facet_grid(facets~.)+
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
ggsave(paste0('figures/alb_preds_cv_binned_tile_grid_', alb_prod, '.pdf'))

###############################################################################################################
## plot albedo prediction differences
###############################################################################################################

years_df = data.frame(year = years)

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_sub)+1))
colnames(alb_diff_df) = c(colnames(alb_grid_sub), 'alb_diff')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')

cell_ids = unique(alb_grid_sub$cell_id)
N_cells  = length(cell_ids)
for (i in 1:N_cells){
  
  alb_cell = alb_grid_sub[which(alb_grid_sub$cell_id == cell_ids[i]),] 
  alb_cell = alb_cell[order(alb_cell$year),]
  
  if (nrow(alb_cell) == 1){
    next
  } 
  
  alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
  
  alb_diff_df = rbind(alb_diff_df, data.frame(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
                                              alb_diff = -diff(alb_cell_filled$alb_mean)))
  
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$alb_diff)),]

saveRDS(alb_diff_df, paste0('data/alb_preds_diffs_', alb_prod, '.RDS'))


# labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# labels = c('0.05 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10')
# labels = c('0.05 - 1', '1 - 3', '3 - 5', '5 - 7', '7 - 9', '9 - 11')
# labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')


diff_years = years[-length(years)]
alb_diff_df$facets = labels[match(alb_diff_df$year, diff_years)]
alb_diff_df$facets = factor(alb_diff_df$facets, levels =  labels)

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

values = c(0, 0.4, 0.45, 0.5, 0.55, 0.6, 1)
values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)


# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "RdYlBu",#"BrBG",
#                                         # labels = labels,
#                                         direction=1,
#                                         na.value="grey", 
#                                         name="Albedo change",
#                                         limits = c(-thresh,thresh),
#                                         values = values)


sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
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
                                            direction=1,
                                            # labels = labels,
                                            na.value="transparent",#grey", 
                                            name="Albedo change",
                                            limits = c(-thresh,thresh), 
                                            values = values)

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "BrBG",
                                            # labels = labels,
                                            na.value="transparent",#grey",
                                            name="Albedo change",
                                            limits = c(-thresh,thresh),
                                            values = values)

# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=subset(alb_diff_df, year==4000), aes(x=long, y=lat, colour = alb_diff), size=1, alpha=1)+
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
# # ggsave(paste0('figures/alb_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))
# 
# 
# #change in albedo going back through time 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=alb_diff_df, aes(x=long, y=lat, colour = alb_diff), size=1, alpha=0.6)+
#   sc_colour_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(facets~.)+
#   facet_wrap(~year)+
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
# ggsave(paste0('figures/alb_preds_diff_point_wrap_', alb_prod, '.pdf'))
# 
# ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.pdf'))

#change in albedo going back in time but using tiles 
#a bit easier to interpret than the one above 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  #facet_grid(year~.)+
  facet_wrap(~facets)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()
ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.pdf'))


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  #facet_grid(year~.)+
  facet_grid(facets~.)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  coord_fixed()
ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.pdf'))

pdf(paste0('figures/alb_preds_diff_tile_pages_', alb_prod, '.pdf'))
for (year in diff_years){
  
  diff_sub = alb_diff_df[which(alb_diff_df$year == year),]
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=diff_sub, aes(x=long, y=lat, fill = alb_diff)) +
    sc_fill_diverge + 
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
}
dev.off()

# ###############################################################################################################
# ## plot albedo prediction differences
# ###############################################################################################################
# 
# alb_post = readRDS(paste0('data/paleo_predict_gam_samps_', alb_prod, '.RDS'))
# 
# 
# cell_id <- raster::extract(grid, alb_post[,c('long', 'lat')])
# 
# alb_post <- data.frame(cell_id, alb_post)
# # coords   = xyFromCell(grid, alb_grid$cell_id)
# # colnames(coords) = c('long', 'lat')
# # 
# # alb_grid = cbind(coords, alb_grid[,c('x', 'y', 'cell_id', 'year', 'alb_mean', 'alb_sd')])
# 
# N_iter = length(unique(alb_posts$iter))
# 
# 
# years = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
# #years = c(50, 1000, 3000, 5000, 7000, 9000, 11000)
# # years = c(50, 6000, 11000)
# 
# alb_post_sub = subset(alb_post, year %in% years) 
# 
# 
# years_df = data.frame(year = years)
# 
# alb_post_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_post_sub)+1))
# colnames(alb_post_diff_df) = c(colnames(alb_post_sub), 'alb_diff')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')
# 
# cell_ids = unique(alb_post_sub$cell_id)
# N_cells  = length(cell_ids)
# 
# for (iter in 1:N_iter){
#   
#   print(paste0('Iter: ', iter))
#   
#   alb_post_iter = alb_post_sub[which(alb_post_sub$iter == iter),] 
#   
#   for (i in 1:N_cells){
#     
#     alb_cell = alb_post_iter[which(alb_post_iter$cell_id == cell_ids[i]),] 
#     alb_cell = alb_cell[order(alb_cell$year),]
#     
#     if (nrow(alb_cell) == 1){
#       next
#     } 
#     
#     alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
#     
#     alb_post_diff_df = rbind(alb_post_diff_df, data.frame(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
#                                                 alb_diff = -diff(alb_cell_filled$value)))
#     
#   }
# }
# 
# alb_post_diff_df =  alb_post_diff_df[which(!is.na(alb_post_diff_df$alb_diff)),]
# 
# saveRDS(alb_post_diff_df, paste0('data/alb_preds_post_diffs_', alb_prod, '.RDS'))
# 
# 
# hist(alb_post_diff_df[which((alb_post_diff_df$cell_id == cell_ids[1]) & (alb_post_diff_df$year == years[1])),'alb_diff'])
# 
# alb_post_diff_sig = alb_post_diff_df %>%
#   group_by(year, cell_id, long, lat, x, y, elev, ET, OL, ST) %>%
#   summarize(prob_pos = sum(alb_diff>0),
#             total = length(alb_diff),
#             .groups = 'keep')
# 
# # labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# # labels = c('0.05 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10')
# # labels = c('0.05 - 1', '1 - 3', '3 - 5', '5 - 7', '7 - 9', '9 - 11')
# # labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# 
# 
# diff_years = years[-length(years)]
# alb_diff_df$facets = labels[match(alb_diff_df$year, diff_years)]
# alb_diff_df$facets = factor(alb_diff_df$facets, levels =  labels)
# 
# # thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
# max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
# thresh = ceiling(max_diff*100)/100
# 
# values = c(0, 0.4, 0.45, 0.5, 0.55, 0.6, 1)
# values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)
# 
# 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "RdYlBu",#"BrBG",
# #                                         # labels = labels,
# #                                         direction=1,
# #                                         na.value="grey", 
# #                                         name="Albedo change",
# #                                         limits = c(-thresh,thresh),
# #                                         values = values)
# 
# 
# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "BrBG",
#                                         # labels = labels,
#                                         na.value="grey",
#                                         name="Albedo change",
#                                         limits = c(-thresh,thresh),
#                                         values = values)
# 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "RdYlBu",#"BrBG",
# #                                         # labels = labels,
# #                                         na.value="grey", 
# #                                         name="Percent")##,
# #                                         # limits = c(-thresh,thresh),
# #                                         # values = values)
# 
# sc_colour_diverge <- scale_colour_distiller(type = "div",
#                                             palette = "RdYlBu",#"BrBG",
#                                             direction=1,
#                                             # labels = labels,
#                                             na.value="transparent",#grey", 
#                                             name="Albedo change",
#                                             limits = c(-thresh,thresh), 
#                                             values = values)
# 
# sc_colour_diverge <- scale_colour_distiller(type = "div",
#                                             palette = "BrBG",
#                                             # labels = labels,
#                                             na.value="transparent",#grey",
#                                             name="Albedo change",
#                                             limits = c(-thresh,thresh),
#                                             values = values)
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_point(data=subset(alb_diff_df, year==4000), aes(x=long, y=lat, colour = alb_diff), size=1, alpha=1)+
# #   sc_colour_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   facet_grid(facets~.)+
# #   # facet_wrap(year~.)+
# #   theme_bw()+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=14),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=14),
# #         legend.title = element_text(size=14)) +
# #   coord_fixed()
# # # ggsave(paste0('figures/alb_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))
# # 
# # 
# # #change in albedo going back through time 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_point(data=alb_diff_df, aes(x=long, y=lat, colour = alb_diff), size=1, alpha=0.6)+
# #   sc_colour_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   #facet_grid(facets~.)+
# #   facet_wrap(~year)+
# #   theme_bw()+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=14),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=14),
# #         legend.title = element_text(size=14)) +
# #   coord_fixed()
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # ggsave(paste0('figures/alb_preds_diff_point_wrap_', alb_prod, '.pdf'))
# # 
# # ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.pdf'))
# 
# #change in albedo going back in time but using tiles 
# #a bit easier to interpret than the one above 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(year~.)+
#   facet_wrap(~facets)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.pdf'))
# 
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(year~.)+
#   facet_grid(facets~.)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.pdf'))
# 
# pdf(paste0('figures/alb_preds_diff_tile_pages_', alb_prod, '.pdf'))
# for (year in diff_years){
#   
#   diff_sub = alb_diff_df[which(alb_diff_df$year == year),]
#   p<-ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=diff_sub, aes(x=long, y=lat, fill = alb_diff)) +
#     sc_fill_diverge + 
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
#   print(p)
# }
# dev.off()
# 


###############################################################################################################
## interp
###############################################################################################################

alb_interp_preds = readRDS(paste0('data/paleo_interp_predict_gam_summary_', alb_prod, '.RDS'))
alb_interp_preds$year[which(alb_interp_preds$year == 11500)] = 12000


pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')


breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100
# breaks = c(0, 10, 20, 30, 40, 50, 60, 80, 90, 100)/100
# breaks = c(0, 4, 8, 12, 16, 20, 30, 40, 60, 100)/100
# breaks = c(0, 0.001, 5, 10, 20, 40, 50, 60, 80, 100)/100
# cover_melt2$alb_binned = factor(cut(cover_melt2$value, breaks, include.lowest=TRUE, labels=FALSE), levels=seq(1, 11))
# labels = c("0 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80",  "80 - 90", "90 - 100")
# labels = c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8",  "0.8 - 0.9", "0.9 - 1")

labels = seq(1, length(breaks)-1)


# plot_cuts = c(0, 1e-5, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
# n_plot_cuts = length(plot_cuts) - 1




###############################################################################################################
## COLOUR PALETTES
###############################################################################################################

# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_colour <- scale_colour_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                                 na.value="white", name="Percent", drop=FALSE)


# sc <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), 
# labels=labels, na.value="white", name="Percent", drop=FALSE)  
sc_fill <- scale_fill_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                             na.value="white", name="Percent", drop=FALSE)


sc_fill_seq <- scale_fill_brewer(type = "seq",
                                 palette = "Greens",#"BrBG",#"YlGnBu",#
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
sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        # labels = labels,
                                        direction=1,
                                        na.value="grey", 
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)


sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
                                        # labels = labels,
                                        na.value="grey",
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)



###############################################################################################################
## summary plots of values
###############################################################################################################

alb_interp_preds$alb_bin = cut(alb_interp_preds$alb_mean, breaks, labels=FALSE)

# ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))
alb_interp_preds$ice = NA

# ages_cat_12 = c(ages, 12000)
# ages_cat_12[which(ages == 11500)] = 12000
ages[which(ages == 11500)] = 12000

alb_preds_ages = unique(alb_interp_preds$year)

for (i in 1:length(alb_preds_ages)){
  
  print(i)
  idx_age = which(alb_interp_preds$year == alb_preds_ages[i])
  
  # print(length(idx_age))
  
  # coords_grid  = SpatialPoints(land_grid_NA[,c('x', 'y')], proj4string=CRS(proj_WGS84))
  coords_veg  = SpatialPoints(alb_interp_preds[idx_age,c('x', 'y')], proj4string=CRS(proj_WGS84))
  
  # if (i %in% c(1, 2, 3)){
  #   veg_interp[idx_age, 'ice'] = NA
  # } else {
  idx_ice_match = which.min(abs(alb_preds_ages[i] - ice_years)) 
  
  proj4string(ice[[idx_ice_match]]) = proj_WGS84
  ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))
  
  # ice_status_grid = over(coords_grid, ice[[idx_ice_match]])
  ice_status_veg = over(coords_veg, ice[[idx_ice_match]])
  
  # ice_fort_age = fortify(ice[[idx_ice_match]])
  # 
  # ice_fort = rbind(ice_fort, 
  #                  data.frame(ice_fort_age, 
  #                             ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)), 
  #                             ages = rep(ages_cat_12[i], nrow(ice_fort_age))))
  # 
  # grid_ice = rbind(grid_ice,
  #                  data.frame(land_grid_NA,
  #                             ice = ice_status_grid[,which(substr(colnames(ice_status_grid), 1,4)=='SYMB')],
  #                             age = rep(ages[i], nrow(land_grid_NA))))
  # 
  alb_interp_preds[idx_age, 'ice'] = ice_status_veg[,which(substr(colnames(ice_status_veg), 1,4)=='SYMB')]
  
}

###############################################################################################################
## aggregate to 1 degree by 1 degree tiles
###############################################################################################################
ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

years = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
#years = c(50, 1000, 3000, 5000, 7000, 9000, 11000)
# years = c(50, 6000, 11000)

# breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100
# labels = c("0 - 4", "4 - 8", "8 - 12", "12 - 16", "16 - 20", "20 - 40", "40 - 60",  "60 - 80", "80 - 100")

breaks_alb = c(0, 5, 10, 20, 30, 40, 60, 80, 100)/100
labels_alb = c("0 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 60", "60 - 80", "80 - 100")

grid_NA <- readRDS("data/grid.RDS")
# crs(grid_NA) = CRS(SRS_string = "EPSG:4326")

# grid <- rast(readRDS("data/grid.RDS"))
paleo_interp_sim_gam = readRDS('data/paleo_interp_predict_gam_bluesky.RDS')

cell_id <- raster::extract(grid, alb_interp_preds[,c('x', 'y')])

alb_grid <- data.frame(cell_id, alb_interp_preds)
coords   = xyFromCell(grid, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords, alb_grid[,c('x', 'y', 'cell_id', 'year', 'ice', 'alb_mean', 'alb_sd')])

alb_grid_sub = subset(alb_grid, year %in% years) 

# IF USE LCT AT POINT SCALE
# take sd as well, mean of other vars too 
# alb_grid = aggregate(alb_pred ~ cell_id + long + lat + x + y + year, alb_grid, median)
# alb_grid = aggregate(alb_mean ~ cell_id + long + lat + year, alb_grid, median)

alb_grid_sub$alb_bin = cut(alb_grid_sub$alb_mean, breaks_alb, labels=FALSE)

# breaks_sd = c(0, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09)
# labels_sd = c("0 - 0.04", "0.04 - 0.05", "0.05 - 0.06", "0.06 - 0.07", "0.07 - 0.08", "0.08 - 0.09")
breaks_sd = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
labels_sd = c("0 - 0.01", "0.01 - 0.02", "0.02 - 0.03", "0.03 - 0.04", "0.04 - 0.05", "0.05 - 0.06", "0.06 - 0.07")
alb_grid_sub$alb_sd_bin = cut(alb_grid_sub$alb_sd, breaks_sd, labels=FALSE, include.lowest=TRUE)
alb_grid_sub$alb_sd_bin = factor(alb_grid_sub$alb_sd_bin, 
                                 levels=seq(1, length(labels_sd)),
                                 labels = labels_sd)

alb_grid_sub$alb_cv = alb_grid_sub$alb_sd / alb_grid_sub$alb_mean
breaks_cv = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 10)
labels_cv = c("0 - 0.05", "0.05 - 0.1", "0.1 - 0.15", "0.15 - 0.2", "0.2 - 0.25", "0.25 - 0.3", "0.3 - 10")
alb_grid_sub$alb_cv_bin = cut(alb_grid_sub$alb_cv, breaks_cv, labels=FALSE, include.lowest=TRUE)
alb_grid_sub$alb_cv_bin = factor(alb_grid_sub$alb_cv_bin, 
                                 levels=seq(1, length(labels_cv)),
                                 labels = labels_cv)
# alb_grid_sub = subset(alb_grid, year %in% years) 

labels_year = c('0.05 ka', '0.5 ka', '2 ka', '4 ka', '6 ka', '8 ka', '10 ka', '12 ka')
#labels = c('0.05', '1', '3', '5', '7', '9', '11')


#renaming labels to create facets column in alb_grid_sub 
alb_grid_sub$facets = labels_year[match(alb_grid_sub$year, years)]
alb_grid_sub$facets = factor(alb_grid_sub$facets, levels = labels_year)

alb_grid_sub = alb_grid_sub[which(alb_grid_sub$long < (-60)),]

###############################################################################################################
## Plot albedo predictions and uncertainty
###############################################################################################################

# albedo predictions binned 
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_bin))) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_manual(values=terrain.colors(10), na.value='transparent', name = "Albedo", labels=breaks[-1]) + 
  # sc_fill_seq + 
  # scale_fill_brewer(type='seq', palette='YlOrBr', 
  #                   na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
  scale_fill_brewer(type='seq', 
                    palette='YlOrBr', 
                    name = "Albedo", 
                    direction=1, 
                    na.value = "white",
                    labels = labels_alb) +
  facet_grid(facets~.)+
  # facet_wrap(year~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  # coord_fixed()
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_binned_tile_grid_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_binned_tile_grid_', alb_prod, '.png'))

# albedo predictions binned 
# wrap: by year 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_bin))) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_manual(values=terrain.colors(10), na.value='transparent', name = "Albedo", labels=breaks[-1]) + 
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
  # sc_fill_seq + 
  # facet_grid(facets~variable)+
  facet_wrap(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_binned_tile_wrap_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_binned_tile_wrap_', alb_prod, '.png'))

# albedo predictions binned 
# pages: single year per page 
pdf(paste0('figures/alb_interp_preds_binned_tile_pages_', alb_prod, '.pdf'))
for (year in ages_sub){
  
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, year==year), aes(x=long,y=lat, fill=factor(alb_bin))) +
    geom_polygon(data=subset(ice_sub, ages == year), aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
        # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
    # sc_fill_seq + 
    scale_fill_brewer(type='seq', palette='YlOrBr', 
                      na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
    # facet_grid(facets~.)+
    # facet_wrap(year~.)+
    theme_bw(12)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          # strip.text = element_text(size=14),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  print(p)
}
dev.off()

lims = c(0, round(max(alb_grid_sub$alb_sd),2))

# albedo prediction standard deviation 
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_sd)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_distiller(type='seq', 
                       palette='YlOrBr', 
                       na.value='transparent', 
                       name = "Standard \ndeviation", 
                       direction=1, limits = lims) +
  # sc_fill_seq +
  # facet_grid(facets~variable)+
  facet_wrap(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_sd_tile_wrap_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_sd_tile_wrap_', alb_prod, '.png'))


# albedo prediction standard deviation binned
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_sd_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', 
                    palette='YlOrBr', 
                    na.value='transparent', 
                    name = "Standard \ndeviation", 
                    direction=1, 
                    labels = labels_sd,
                    drop = FALSE) +
  facet_wrap(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_sd_binned_tile_wrap_', alb_prod, '.pdf'))

# albedo prediction standard deviation binned
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_sd_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', 
                    palette='YlOrBr', 
                    na.value='transparent', 
                    name = "Standard \ndeviation", 
                    direction=1, 
                    labels = labels_sd) +
  facet_grid(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_sd_binned_tile_grid_', alb_prod, '.png'))

# albedo prediction standard deviation binned
# wrap: by year
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_cv_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', 
                    palette='YlOrBr', 
                    na.value='transparent', 
                    name = "Coefficient of \nvariation", 
                    direction=1, 
                    labels = labels_cv) +
  facet_wrap(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_cv_binned_tile_wrap_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_cv_binned_tile_wrap_', alb_prod, '.png'))

# albedo prediction standard deviation binned
# grid: year as rows
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_cv_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  # geom_tile(data=alb_grid, aes(x=x,y=y, fill=factor(alb_bin))) +
  # scale_fill_gradientn(colors=terrain.colors(10), na.value='transparent', name = "Albedo") +
  scale_fill_brewer(type='seq', palette='YlOrBr', 
                    na.value='transparent', name = "CV", direction=1, labels = labels_sd) +
  facet_grid(facets~.)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=14),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_cv_binned_tile_grid_', alb_prod, '.pdf'))
ggsave(paste0('figures/alb_interp_preds_cv_binned_tile_grid_', alb_prod, '.png'))

###############################################################################################################
## plot albedo prediction differences
###############################################################################################################

years_df = data.frame(year = ages_sub)

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_sub)+1))
colnames(alb_diff_df) = c(colnames(alb_grid_sub), 'alb_diff')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')

cell_ids = unique(alb_grid_sub$cell_id)
N_cells  = length(cell_ids)
for (i in 1:N_cells){
  
  alb_cell = alb_grid_sub[which(alb_grid_sub$cell_id == cell_ids[i]),] 
  alb_cell = alb_cell[order(alb_cell$year),]
  alb_cell = alb_cell[which(is.na(alb_cell$ice)),]
  
  if (nrow(alb_cell) == 1){
    next
  } 
  
  alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
  
  alb_diff_df = rbind(alb_diff_df, data.frame(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
                                              alb_diff = -diff(alb_cell_filled$alb_mean)))
  
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$alb_diff)),]

saveRDS(alb_diff_df, paste0('data/alb_interp_preds_diffs_', alb_prod, '.RDS'))


# early, middle, late

# alb_diff_coarse_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_sub)+1))
# colnames(alb_diff_coarse_df) = c(colnames(alb_grid_sub), 'alb_diff')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')
# years_coarse_df = data.frame(years = c(50, 2000, 6000, 12000))
# 
# cell_ids = unique(alb_grid_sub$cell_id)
# N_cells  = length(cell_ids)
# for (i in 1:N_cells){
#   
#   alb_cell = alb_grid_sub[which(alb_grid_sub$cell_id == cell_ids[i]),] 
#   alb_cell = alb_cell[order(alb_cell$year),]
#   alb_cell = alb_cell[which(is.na(alb_cell$ice)),]
#   alb_cell = alb_cell[which(alb_cell$year %in% years_coarse_df$year)]
#   
#   if (nrow(alb_cell) == 1){
#     next
#   } 
#   
#   alb_cell_filled = merge(years_coarse_df, alb_cell, all.x=TRUE)
#   
#   alb_diff_coarse_df = rbind(alb_diff_coarse_df, data.frame(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
#                                               alb_diff = -diff(alb_cell_filled$alb_mean)))
#   
# }
# 
# alb_diff_coarse_df =  alb_diff_coarse_df[which(!is.na(alb_diff_coarse_df$alb_diff)),]
# 
# saveRDS(alb_diff_coarse_df, paste0('data/alb_interp_preds_diffs_coarse_', alb_prod, '.RDS'))

# labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# labels = c('0.05 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10')
# labels = c('0.05 - 1', '1 - 3', '3 - 5', '5 - 7', '7 - 9', '9 - 11')
# labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
labels = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

ice_fort_diff_young = data.frame(matrix(NA, nrow=0, ncol=10))
ice_fort_diff_old = data.frame(matrix(NA, nrow=0, ncol=10))
ages_sub = years

ice_fort = ice_fort[,1:9]

for (i in 1:(length(ages_sub)-1)){
  print(ages_sub[i])
  
  # idx_age = which(veg_lct$ages == ages_sub[i])
  # coords  = SpatialPoints(veg_lct[idx_age,c('x', 'y')], proj4string=CRS(proj_WGS84))
  
  age_now = ages_sub[i]
  
  # if (age_now <= 500){
  #   next
  # } else {
    idx_ice_match_young = which.min(abs(ages_sub[i] - ice_years))
    idx_ice_match_old = which.min(abs(ages_sub[i+1] - ice_years))
    
    # ice_status = over(coords, ice[[idx_ice_match]])
    
    ice_fort_age_young = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_young]),]
    
    ice_fort_age_old = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_old]),]
    
    # ice_match = ice_fort[[idx_ice_match]]
    # # ice_match_buffer = gBuffer(ice_match, byid=TRUE, width=0)
    # 
    # ice_match_sf = st_as_sf(ice_match)
    # ice_match_sf = st_make_valid(ice_match_sf)
    # ice_match_clipped = st_crop(ice_match_sf, box_sp)
    # 
    # ice_fort_age = as_Spatial(ice_match_clipped)
    # 
    # ice_fort_age = fortify(ice_fort_age)
    
    print(labels[i])
    
    ice_fort_diff_young = rbind(ice_fort_diff_young,
                                data.frame(ice_fort_age_young,
                                           # ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)),
                                           # ages = rep(ages_sub_diff[i], nrow(ice_fort_age)),
                                           facets = rep(labels[i],  nrow(ice_fort_age_young))
                                ))
    
    ice_fort_diff_old = rbind(ice_fort_diff_old,
                              data.frame(ice_fort_age_old,
                                         # ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)),
                                         # ages = rep(ages_sub_diff[i], nrow(ice_fort_age)),
                                         facets = rep(labels[i],  nrow(ice_fort_age_old))
                              ))
    
  # }
  
}


labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

ice_fort_diff_old$facets = factor(ice_fort_diff_old$facets,
                                  levels = labels_period,
                                  labels = labels_period)

ice_fort_diff_young$facets = factor(ice_fort_diff_young$facets,
                                    levels = labels_period,
                                    labels = labels_period)

saveRDS(ice_fort_diff_young, 'data/ice_fort_diff_young.RDS')
saveRDS(ice_fort_diff_old, 'data/ice_fort_diff_old.RDS')

diff_years = years[-length(years)]
alb_diff_df$facets = labels[match(alb_diff_df$year, diff_years)]
alb_diff_df$facets = factor(alb_diff_df$facets, levels =  labels)

# thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

values = c(0, 0.4, 0.45, 0.5, 0.55, 0.6, 1)
values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)


# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "RdYlBu",#"BrBG",
#                                         # labels = labels,
#                                         direction=1,
#                                         na.value="grey", 
#                                         name="Albedo change",
#                                         limits = c(-thresh,thresh),
#                                         values = values)


sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
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
                                            direction=1,
                                            # labels = labels,
                                            na.value="transparent",#grey", 
                                            name="Albedo change",
                                            limits = c(-thresh,thresh), 
                                            values = values)

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "BrBG",
                                            # labels = labels,
                                            na.value="transparent",#grey",
                                            name="Albedo change",
                                            limits = c(-thresh,thresh),
                                            values = values)

# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=subset(alb_diff_df, year==4000), aes(x=long, y=lat, colour = alb_diff), size=1, alpha=1)+
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
# # ggsave(paste0('figures/alb_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))
# 
# 
# #change in albedo going back through time 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=alb_diff_df, aes(x=long, y=lat, colour = alb_diff), size=1, alpha=0.6)+
#   sc_colour_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(facets~.)+
#   facet_wrap(~year)+
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
# ggsave(paste0('figures/alb_preds_diff_point_wrap_', alb_prod, '.pdf'))
# 
# ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.pdf'))

#change in albedo going back in time but using tiles 
#a bit easier to interpret than the one above 
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
  geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  #facet_grid(year~.)+
  facet_wrap(~facets)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_diff_tile_wrap_', alb_prod, '.png'))
ggsave(paste0('figures/alb_interp_preds_diff_tile_wrap_', alb_prod, '.pdf'))


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
  geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  sc_fill_diverge + 
  # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
  #facet_grid(year~.)+
  facet_grid(facets~.)+
  theme_bw(8)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # strip.text = element_text(size=12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(paste0('figures/alb_interp_preds_diff_tile_grid_', alb_prod, '.png'))
ggsave(paste0('figures/alb_interp_preds_diff_tile_grid_', alb_prod, '.pdf'))

pdf(paste0('figures/alb_interp_preds_diff_tile_pages_', alb_prod, '.pdf'))
for (year in diff_years){
  
  diff_sub = alb_diff_df[which(alb_diff_df$year == year),]
  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=diff_sub, aes(x=long, y=lat, fill = alb_diff)) +
    geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
    sc_fill_diverge + 
    theme_bw(12)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          # strip.text = element_text(size=14),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  print(p)
}
dev.off()

# ###############################################################################################################
# ## plot albedo prediction differences
# ###############################################################################################################
# 
# alb_post = readRDS(paste0('data/paleo_predict_gam_samps_', alb_prod, '.RDS'))
# 
# 
# cell_id <- raster::extract(grid, alb_post[,c('long', 'lat')])
# 
# alb_post <- data.frame(cell_id, alb_post)
# # coords   = xyFromCell(grid, alb_grid$cell_id)
# # colnames(coords) = c('long', 'lat')
# # 
# # alb_grid = cbind(coords, alb_grid[,c('x', 'y', 'cell_id', 'year', 'alb_mean', 'alb_sd')])
# 
# N_iter = length(unique(alb_posts$iter))
# 
# 
# years = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
# #years = c(50, 1000, 3000, 5000, 7000, 9000, 11000)
# # years = c(50, 6000, 11000)
# 
# alb_post_sub = subset(alb_post, year %in% years) 
# 
# 
# years_df = data.frame(year = years)
# 
# alb_post_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_post_sub)+1))
# colnames(alb_post_diff_df) = c(colnames(alb_post_sub), 'alb_diff')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')
# 
# cell_ids = unique(alb_post_sub$cell_id)
# N_cells  = length(cell_ids)
# 
# for (iter in 1:N_iter){
#   
#   print(paste0('Iter: ', iter))
#   
#   alb_post_iter = alb_post_sub[which(alb_post_sub$iter == iter),] 
#   
#   for (i in 1:N_cells){
#     
#     alb_cell = alb_post_iter[which(alb_post_iter$cell_id == cell_ids[i]),] 
#     alb_cell = alb_cell[order(alb_cell$year),]
#     
#     if (nrow(alb_cell) == 1){
#       next
#     } 
#     
#     alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
#     
#     alb_post_diff_df = rbind(alb_post_diff_df, data.frame(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
#                                                 alb_diff = -diff(alb_cell_filled$value)))
#     
#   }
# }
# 
# alb_post_diff_df =  alb_post_diff_df[which(!is.na(alb_post_diff_df$alb_diff)),]
# 
# saveRDS(alb_post_diff_df, paste0('data/alb_preds_post_diffs_', alb_prod, '.RDS'))
# 
# 
# hist(alb_post_diff_df[which((alb_post_diff_df$cell_id == cell_ids[1]) & (alb_post_diff_df$year == years[1])),'alb_diff'])
# 
# alb_post_diff_sig = alb_post_diff_df %>%
#   group_by(year, cell_id, long, lat, x, y, elev, ET, OL, ST) %>%
#   summarize(prob_pos = sum(alb_diff>0),
#             total = length(alb_diff),
#             .groups = 'keep')
# 
# # labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# # labels = c('0.05 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10')
# # labels = c('0.05 - 1', '1 - 3', '3 - 5', '5 - 7', '7 - 9', '9 - 11')
# # labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# labels = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# 
# 
# diff_years = years[-length(years)]
# alb_diff_df$facets = labels[match(alb_diff_df$year, diff_years)]
# alb_diff_df$facets = factor(alb_diff_df$facets, levels =  labels)
# 
# # thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
# max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
# thresh = ceiling(max_diff*100)/100
# 
# values = c(0, 0.4, 0.45, 0.5, 0.55, 0.6, 1)
# values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)
# 
# 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "RdYlBu",#"BrBG",
# #                                         # labels = labels,
# #                                         direction=1,
# #                                         na.value="grey", 
# #                                         name="Albedo change",
# #                                         limits = c(-thresh,thresh),
# #                                         values = values)
# 
# 
# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "BrBG",
#                                         # labels = labels,
#                                         na.value="grey",
#                                         name="Albedo change",
#                                         limits = c(-thresh,thresh),
#                                         values = values)
# 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "RdYlBu",#"BrBG",
# #                                         # labels = labels,
# #                                         na.value="grey", 
# #                                         name="Percent")##,
# #                                         # limits = c(-thresh,thresh),
# #                                         # values = values)
# 
# sc_colour_diverge <- scale_colour_distiller(type = "div",
#                                             palette = "RdYlBu",#"BrBG",
#                                             direction=1,
#                                             # labels = labels,
#                                             na.value="transparent",#grey", 
#                                             name="Albedo change",
#                                             limits = c(-thresh,thresh), 
#                                             values = values)
# 
# sc_colour_diverge <- scale_colour_distiller(type = "div",
#                                             palette = "BrBG",
#                                             # labels = labels,
#                                             na.value="transparent",#grey",
#                                             name="Albedo change",
#                                             limits = c(-thresh,thresh),
#                                             values = values)
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_point(data=subset(alb_diff_df, year==4000), aes(x=long, y=lat, colour = alb_diff), size=1, alpha=1)+
# #   sc_colour_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   facet_grid(facets~.)+
# #   # facet_wrap(year~.)+
# #   theme_bw()+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=14),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=14),
# #         legend.title = element_text(size=14)) +
# #   coord_fixed()
# # # ggsave(paste0('figures/alb_preds_sd_binned_tile_grid_', alb_prod, '.pdf'))
# # 
# # 
# # #change in albedo going back through time 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_point(data=alb_diff_df, aes(x=long, y=lat, colour = alb_diff), size=1, alpha=0.6)+
# #   sc_colour_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   #facet_grid(facets~.)+
# #   facet_wrap(~year)+
# #   theme_bw()+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=14),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=14),
# #         legend.title = element_text(size=14)) +
# #   coord_fixed()
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # ggsave(paste0('figures/alb_preds_diff_point_wrap_', alb_prod, '.pdf'))
# # 
# # ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_diff_subset_point_', alb_prod, '.pdf'))
# 
# #change in albedo going back in time but using tiles 
# #a bit easier to interpret than the one above 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(year~.)+
#   facet_wrap(~facets)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_tile_wrap_', alb_prod, '.pdf'))
# 
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   #facet_grid(year~.)+
#   facet_grid(facets~.)+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_diff_tile_grid_', alb_prod, '.pdf'))
# 
# pdf(paste0('figures/alb_preds_diff_tile_pages_', alb_prod, '.pdf'))
# for (year in diff_years){
#   
#   diff_sub = alb_diff_df[which(alb_diff_df$year == year),]
#   p<-ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=diff_sub, aes(x=long, y=lat, fill = alb_diff)) +
#     sc_fill_diverge + 
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
#   print(p)
# }
# dev.off()
# 
