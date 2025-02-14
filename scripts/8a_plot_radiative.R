library(ggplot2)
library(dplyr)


ages = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)

###############################################################################################################
## read in RF, filter so constant grid cell N through time
###############################################################################################################

RF_holocene = readRDS(paste0('output/forcing/RF_holocene_full.RDS'))
# alb_diff = RF_holocene[which(RF_holocene$year %in% ages_sub),]

RF_holocene = RF_holocene[which(RF_holocene$lat>27),]
RF_holocene = RF_holocene[which(RF_holocene$lat<74),]

RF_check = RF_holocene[,c('cell_id', 'month', 'year')] %>%
  group_by(cell_id, month) %>%
  dplyr::summarize(cell_count = n())
all(RF_check$cell_count == 24)


# RF_holocene$ice_young_thresh = RF_holocene$ice_young
# RF_holocene$ice_old_thresh = RF_holocene$ice_old
# 
# # if ice is NA then no ice
# RF_holocene$ice_young_thresh[which(RF_holocene$ice_young <= 0.5)] = NA
# RF_holocene$ice_old_thresh[which(RF_holocene$ice_old <= 0.5)] = NA

# RF_holocene$ice_young_thresh[which(RF_holocene$ice_young <= 0.5)] = NA
# RF_holocene$ice_old[which(RF_holocene$ice_old <= 0.5)] = NA

# if either or both not NA, then can't determine land to land alb
# RF_holocene$alb_mean[which(!is.na(RF_holocene$ice_young)|!is.na(RF_holocene$ice_old))] = NA

# RF_holocene$rf_value[which(!is.na(RF_holocene$ice_young)|!is.na(RF_holocene$ice_old))] = NA



###############################################################################################################
## plot maps of radiative forcing
###############################################################################################################

labels_period_full = data.frame(end_young = ages[1:(N_times-1)],
                                end_old = ages[2:N_times])/1000
labels_period_full$facets = paste(labels_period_full$end_young, labels_period_full$end_old, sep='-')
# labels_period_full = labels_period_full[!duplicated(labels_period_full), ]
labels_period_full$period_id = seq(1, nrow(labels_period_full))

# # labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
# 
# ages_ice_res_idx = c(1:4, seq(5, 25, by=2))
# ages_ice_res = ages[ages_ice_res_idx]
# # ages_ice_res[length(ages_ice_res)] = 11500
# 
# years_df = data.frame(year = ages_ice_res)
# 
# labels_ages_ice = signif(data.frame(age = ages_ice_res)/1000, 3)
# labels_ages_ice$facets = paste(labels_ages_ice$age)
# # labels_ages_full$period_id = seq(1, nrow(labels_ages_full))
# 
# 
# labels_period_ice = data.frame(end_young = ages_ice_res[1:(length(ages_ice_res)-1)]/1000,
#                                end_old = ages_ice_res[2:length(ages_ice_res)]/1000)
# labels_period_ice$facets = paste(labels_period_ice$end_young, labels_period_ice$end_old, sep='-')
# labels_period_ice$period_id = seq(1, nrow(labels_period_ice))


###############################################################################################################
## add facets
###############################################################################################################

RF_holocene$facets = labels_period_full[match(RF_holocene$year, labels_period_full$end_young*1000), 'facets']
# RF_holocene$period_mid = labels_period_full[match(RF_holocene$year, labels_period_full$end_young*1000), 'period_mid']
RF_holocene$period_id = labels_period_full[match(RF_holocene$year, labels_period_full$end_young*1000), 'period_id']
RF_holocene$period_mid = (labels_period_full[match(RF_holocene$facets, labels_period_full$facets), 'end_young'] +
                            labels_period_full[match(RF_holocene$facets, labels_period_full$facets), 'end_old'])/2
RF_holocene$period_mid = as.numeric(RF_holocene$period_mid)

RF_holocene$facets = factor(RF_holocene$facets, levels = labels_period_full$facets)

# ###############################################################################################################
# ## plot maps of radiative forcing
# ###############################################################################################################
# 
# ages_ice_res_idx = c(1:4, seq(5, 25, by=2))
# ages_ice_res = ages[ages_ice_res_idx]
# # ages_ice_res[length(ages_ice_res)] = 11500
# 
# years_df = data.frame(year = ages_ice_res)
# 
# labels_ages_ice = signif(data.frame(age = ages_ice_res)/1000, 3)
# labels_ages_ice$facets = paste(labels_ages_ice$age)
# # labels_ages_full$period_id = seq(1, nrow(labels_ages_full))
# 
# 
# labels_period_ice = data.frame(end_young = ages_ice_res[1:(length(ages_ice_res)-1)]/1000, 
#                                end_old = ages_ice_res[2:length(ages_ice_res)]/1000)
# labels_period_ice$facets = paste(labels_period_ice$end_young, labels_period_ice$end_old, sep='-')
# labels_period_ice$period_id = seq(1, nrow(labels_period_ice))
# 
# 
# ###############################################################################################################
# ## add facets
# ###############################################################################################################
# 
# RF_holocene$facets = labels_period_ice[match(RF_holocene$year, labels_period_ice$end_young*1000), 'facets']
# RF_holocene$period_mid = labels_period_ice[match(RF_holocene$year, labels_period_ice$end_young*1000), 'period_mid']
# RF_holocene$period_id = labels_period_ice[match(RF_holocene$year, labels_period_ice$end_young*1000), 'period_id']
# RF_holocene$period_mid = (labels_period_ice[match(RF_holocene$facets, labels_period_ice$facets), 'end_young'] + 
#                                  labels_period_ice[match(RF_holocene$facets, labels_period_ice$facets), 'end_old'])/2
# RF_holocene$period_mid = as.numeric(RF_holocene$period_mid)
# 
# RF_holocene$facets = factor(RF_holocene$facets, levels = labels_period_ice$facets)


###############################################################################################################
## read in grid, get area, and merge
###############################################################################################################

#read in grid
grid_NA <- readRDS("data/grid.RDS")
grid_df = as.data.frame(grid_NA, xy=TRUE)

spdf_2 <- as(grid_NA,'SpatialPolygonsDataFrame')

sf_data = st_as_sf(spdf_2)
grid_df$area <- st_area(sf_data) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')
# 
# RF_holocene_merge = merge(RF_holocene, grid_df, by = c('cell_id', 'lat', 'long'))

RF_holocene_merge = RF_holocene

cell_ids = unique(RF_holocene_merge$cell_id)
grid_land = grid_df[which(grid_df$cell_id %in% cell_ids),]
area_land_m2 = as.numeric(sum(grid_land$area))
area_land_km2 = as.numeric(sum(grid_land$area_km2))
N_cells = length(cell_ids)

RF_holocene_merge_veg = RF_holocene_merge[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 'alb_ice',
                                               'year', 'month', 'facets',
                                               'period_mid', 'period_id',
                                               'rf_cack_veg', 'rf_hadgem_veg', 'rf_cam5_veg', 
                                               'ice_frac_old', 'ice_frac_young')]
RF_holocene_merge_veg_ice = RF_holocene_merge[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 'alb_ice',
                                                   'period_mid', 'period_id',
                                                   'year', 'month',  'facets',
                                                   'rf_cack_veg_ice', 'rf_hadgem_veg_ice', 'rf_cam5_veg_ice', 
                                                   'ice_frac_old', 'ice_frac_young')]
RF_holocene_merge_veg_ice_parts = RF_holocene_merge[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 'alb_ice',
                                                     'period_mid', 'period_id',
                                                     'year', 'month',  'facets',
                                                     'rf_cack_veg_ice_parts', 'rf_hadgem_veg_ice_parts', 'rf_cam5_veg_ice_parts', 
                                                     'ice_frac_old', 'ice_frac_young')]

###############################################################################################################
## reformat and plot by land class
###############################################################################################################


rf_long = RF_holocene_merge_veg %>%
  pivot_longer(cols = c('rf_cack_veg', 'rf_hadgem_veg', 'rf_cam5_veg'),
               names_to = 'rf_kernel',
               values_to = 'rf_veg')
rf_long = data.frame(rf_long)
rf_long$rf_kernel = substr(rf_long$rf_kernel, 1, 
                                      nchar(rf_long$rf_kernel)-4)

ggplot() +
  geom_histogram(data=rf_long, aes(x=rf_veg, y=after_stat(density))) +
  facet_grid(rf_kernel~month, scales = 'free_x')
summary(rf_long$rf_veg)
hist(rf_long$rf_veg)


# breaks_rf = c(-150, -5, -2, 0, 2, 5, 150)
# rf_long$rf_veg_bin = cut(rf_long$rf_veg, breaks = breaks_rf, labels = FALSE)


rf_long_ice_filled = RF_holocene_merge_veg_ice %>%
  pivot_longer(cols = c('rf_cack_veg_ice', 'rf_hadgem_veg_ice', 'rf_cam5_veg_ice'),
               names_to = 'rf_kernel',
               values_to = 'rf_veg_ice')
rf_long_ice_filled = data.frame(rf_long_ice_filled)
rf_long_ice_filled$rf_kernel = substr(rf_long_ice_filled$rf_kernel, 1, 
                                      nchar(rf_long_ice_filled$rf_kernel)-8)


rf_long_parts = RF_holocene_merge_veg_ice_parts %>%
  pivot_longer(cols = c('rf_cack_veg_ice_parts', 'rf_hadgem_veg_ice_parts', 'rf_cam5_veg_ice_parts'),
               names_to = 'rf_kernel',
               values_to = 'rf_veg_ice_parts')
rf_long_parts = data.frame(rf_long_parts)
rf_long_parts$rf_kernel = substr(rf_long_parts$rf_kernel, 1, 
                                      nchar(rf_long_parts$rf_kernel)-14)

rf_long = merge(rf_long, 
                rf_long_ice_filled[,c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
                                      'facets', 'period_mid', 'period_id',
                                      'rf_kernel', 'rf_veg_ice')], 
                by = c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
                       'facets', 'rf_kernel'))

rf_long = merge(rf_long, 
                rf_long_parts[,c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
                                      'facets', 'period_mid', 'period_id',
                                      'rf_kernel', 'rf_veg_ice_parts')], 
                by = c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
                       'facets', 'rf_kernel'))


###############################################################################################################
## ice and veg rules
###############################################################################################################


rf_long$ice_frac_mid = (rf_long$ice_frac_old + rf_long$ice_frac_young) / 2

rf_long$rf_veg_part = rf_long$rf_veg

# rf_long$area_km2_veg = as.numeric(rf_long$area_km2) *  (1-rf_long$ice_old)
# 
# rf_long$rf_ice_part = rf_long$rf_ice * rf_long$ice_mid #* as.numeric(rf_long$area_km2) 
# rf_long$rf_land_part = rf_long$rf_value * (1-rf_long$ice_mid) #* as.numeric(rf_long$area_km2) 

# if grid cell is ice in either of period pair assign RF NA
# rf_long[which((!is.na(rf_long$ice_young_thresh))|(!is.na(rf_long$ice_old_thresh))), 
#         'rf_veg'] = NA

# rf_long[which((is.na(rf_long$ice_young_thresh))&(is.na(rf_long$ice_old_thresh))), 
#         'rf_value_ice'] = rf_long[which((is.na(rf_long$ice_young_thresh))&(is.na(rf_long$ice_old_thresh))), 
#                                      'rf_value']

rf_long[which((rf_long$ice_frac_young>0.5)|(rf_long$ice_frac_old>0.5)),
        'rf_veg'] = NA

# rf_long$rf_veg_part = rf_long$rf_veg_part * rf_long$ice_frac_mid 

###############################################################################################################
## ice and veg rules
###############################################################################################################

# total cells
length(unique(rf_long$cell_id))
# length(unique(rf_long[which((is.na(rf_long$ice_young))&(is.na(rf_long$ice_old))), 
#                       'cell_id']))
# length(unique(rf_long[which((!is.na(rf_long$ice_young))&(is.na(rf_long$ice_old))), 
#         'cell_id']))
# length(unique(rf_long[which((is.na(rf_long$ice_young))&(!is.na(rf_long$ice_old))), 
#                       'cell_id']))

rf_summary_class = rf_long %>%
  group_by(facets) %>%
  summarize(N_all = n_distinct(cell_id))

# # list of cells that had ice cover at any time
# cell_id_ice = unique(rf_long[which((!is.na(rf_long$ice_young_thresh))|
#                                      (!is.na(rf_long$ice_old_thresh))), 'cell_id'])
# # list of cells that had ice cover at any time
cell_id_ice = unique(rf_long[which((rf_long$ice_frac_young>0.5)|(rf_long$ice_frac_old>0.5)), 'cell_id'])

length(unique(cell_id_ice))

# list of cells that never had ice cover
cell_id_ice_free = unique(rf_long$cell_id[!(rf_long$cell_id %in% cell_id_ice)])
length(cell_id_ice_free)


# # should this be just those with greater than 0.5 coverage?
# cell_id_icex = unique(rf_long[which((rf_long$ice_young>0.5)|
#                                       (rf_long$ice_old>0.5)), 'cell_id'])
# length(unique(cell_id_icex))
# cell_id_ice = cell_id_icex
# 
# # list of cells that never had ice cover
# cell_id_ice_free = unique(rf_long$cell_id[!(rf_long$cell_id %in% cell_id_ice)])
# length(cell_id_ice_free)

# ##
# ## ONCE ICE
# ##
# 
# 
# # once ice
# # include only cells with no ice in either of period pair
# rf_long_ice = subset(rf_long, cell_id %in% cell_id_ice)
# rf_long_ice = subset(rf_long_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice$cell_id))
# 
# ggplot() +
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=subset(rf_long_ice, facets=='8-8.5'), aes(x = long, y = lat)) +
#   theme_bw(18) 
# 
# rf_holocene_ice = rf_long_ice %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_total_area = as.numeric(sum(rf_value * area, na.rm=TRUE)), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice$type = rep("once ice")
# 
# # XXX how can there be only one cell in the 11.5-11 period?
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean'] = NA
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean_weighted'] = NA
# rf_holocene_ice
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean_weighted, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# # once ice
# # include all cells, so using assigned ice albedo when ice
# rf_long_ice_filled = subset(rf_long, cell_id %in% cell_id_ice)
# # alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice_filled$cell_id))
# 
# ggplot() +
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=subset(rf_long_ice_filled, facets=='11-11.5'), aes(x = long, y = lat)) +
#   theme_bw(18) 
# 
# rf_holocene_ice_filled = rf_long_ice_filled %>%
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE),
#                    forcing_total_area = as.numeric(sum(rf_value_ice * area, na.rm=TRUE)), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE),
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id),
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_filled$type = rep("once ice filled")
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ##
# ## NEVER ICE
# ##
# 
# rf_long_ice_free = subset(rf_long, cell_id %in% cell_id_ice_free)
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# ggplot() +
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_point(data=subset(rf_long_ice_free, facets=='11-11.5'), aes(x = long, y = lat)) +
#   theme_bw(18) 
# 
# 
# rf_holocene_ice_free = rf_long_ice_free %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_total_area = as.numeric(sum(rf_value * area, na.rm=TRUE)), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_free$type = rep("always land")
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)

##
## LAND-LAND (PART)
##


# rf_long$rf_value_ice_part_area = rf_long$rf_value_ice * rf_long$ice_old * as.numeric(rf_long$area_km2) 
# rf_long$rf_value_land_part_area = rf_long$rf_value * (1-rf_long$ice_old) * as.numeric(rf_long$area_km2) 

# rf_long_land_part = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))

rf_long$rf_value_land_part

rf_holocene_land_part = rf_long %>% 
  group_by(facets, month, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_value_land_part, na.rm=TRUE), 
                   forcing_total_area = as.numeric(sum(rf_value_land_part * area_km2, na.rm=TRUE)), 
                   forcing_mean = mean(rf_value_land_part, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_land_part * area_km2, na.rm=TRUE) / sum((1-ice_mid) * area_km2, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum((1-ice_mid) * area_km2, na.rm=TRUE),
                   .groups = 'keep')

rf_holocene_land_part = rf_holocene_land_part %>%
  mutate(pos = forcing_mean > 0)
rf_holocene_land_part = rf_holocene_land_part %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_holocene_land_part$type = rep("land to land part")

# ggplot() +
#   geom_col(data=rf_holocene_land_part, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_holocene_land_part, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)


##
## LAND-LAND (THRESHHOLD)
##

rf_long_land = subset(rf_long, !is.na(rf_veg))
# alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))

rf_land_land_continent = rf_long_land %>% 
  group_by(facets, month, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_veg, na.rm=TRUE), 
                   forcing_total_area = as.numeric(sum(rf_veg * area, na.rm=TRUE)), 
                   forcing_mean = mean(rf_veg, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_veg * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE),
                   .groups = 'keep')

rf_land_land_continent = rf_land_land_continent %>%
  mutate(pos = forcing_mean > 0)
rf_land_land_continent = rf_land_land_continent %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_land_land_continent$type = rep("land to land")

ggplot() +
  geom_col(data=rf_land_land_continent, aes(x = forcing_mean, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_land_land_continent, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_land_land_continent, aes(x = forcing_total, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_land_land_continent, aes(x = forcing_total_area, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

# ##
# ## LAND-LAND
# ##
# 
# rf_long_land_part = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene_land = rf_long_land %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_total_area = as.numeric(sum(rf_value_ice * area, na.rm=TRUE)), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# 
# rf_holocene_land = rf_holocene_land %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_land = rf_holocene_land %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_land$type = rep("land to land")
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 

# ##
# ## LAND-LAND (only for land part at time t)
# ##
# 
# rf_long_land = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene_land = rf_long_land %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_total_area = as.numeric(sum(rf_value_ice * area, na.rm=TRUE)), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# 
# rf_holocene_land = rf_holocene_land %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_land = rf_holocene_land %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_land$type = rep("land to land")
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_land, aes(x = forcing_total_area, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)



##
## ICE-LAND
##

rf_ice_to_veg_filled = subset(rf_long, cell_id %in% cell_id_ice)
# alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
rf_ice_to_veg_filled = subset(rf_ice_to_veg_filled, (ice_frac_young<0.5) & (ice_frac_old>0.5))
length(unique(rf_ice_to_veg_filled$cell_id))

rf_ice_land_continent = rf_ice_to_veg_filled  %>%
  group_by(facets, month, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_veg_ice, na.rm=TRUE),
                   forcing_total_area = as.numeric(sum(rf_veg_ice * area, na.rm=TRUE)), 
                   forcing_mean = mean(rf_veg_ice, na.rm=TRUE),
                   forcing_mean_weighted = as.numeric(sum(rf_veg_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id),
                   total_area = sum(area_km2, na.rm=TRUE),
                   .groups = 'keep')
rf_ice_land_continent = rf_ice_land_continent %>%
  mutate(pos = forcing_mean > 0)
rf_ice_land_continent = rf_ice_land_continent %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_ice_land_continent$type = rep("ice to land filled")

ggplot() +
  geom_col(data=rf_ice_land_continent, aes(x = forcing_mean, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(12) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_ice_land_continent, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(12) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_ice_land_continent, aes(x = forcing_total, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_ice_land_continent, aes(x = forcing_total_area, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

##
## ALL (LAND + ICE FILLED)
##

rf_veg_ice_continent = rf_long %>%
  group_by(facets, month, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_veg_ice, na.rm=TRUE),
                   forcing_total_area = as.numeric(sum(rf_veg_ice * area, na.rm=TRUE)),
                   forcing_mean = mean(rf_veg_ice, na.rm=TRUE),
                   forcing_mean_weighted = as.numeric(sum(rf_veg_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id),
                   total_area = sum(area_km2, na.rm=TRUE),
                   .groups = 'keep')
rf_veg_ice_continent = rf_veg_ice_continent %>%
  mutate(pos = forcing_mean > 0)
rf_veg_ice_continent = rf_veg_ice_continent %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_veg_ice_continent$type = rep("land + ice")


##
## ALL (LAND + ICE PARTS)
##

rf_veg_ice_parts_continent = rf_long %>% 
  group_by(facets, month, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_veg_ice_parts, na.rm=TRUE), 
                   forcing_total_area = as.numeric(sum(rf_veg_ice_parts * area, na.rm=TRUE)), 
                   forcing_mean = mean(rf_veg_ice_parts, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_veg_ice_parts * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE),
                   .groups = 'keep')
rf_veg_ice_parts_continent = rf_veg_ice_parts_continent %>%
  mutate(pos = forcing_mean > 0)
rf_veg_ice_parts_continent = rf_veg_ice_parts_continent %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_veg_ice_parts_continent$type = rep("land + ice (parts)")


# ##
# ## ALL SPLIT WEIGHTED (LAND + ICE)
# ##
# 
# rf_total_ice_to_land = rf_holocene_ice_land_filled
# rf_total_ice_to_land$RF_N_I2L = rf_total_ice_to_land$forcing_total * rf_total_ice_to_land$cells
# rf_total_ice_to_land$cells_I2L = rf_total_ice_to_land$cells
# 
# rf_total_land_to_land = rf_holocene_land
# rf_total_land_to_land$RF_N_L2L = rf_holocene_land$forcing_total * rf_holocene_land$cells
# rf_total_land_to_land$cells_L2L = rf_total_land_to_land$cells
# 
# rf_N_by_type = merge(rf_holocene_continent[,c('facets', 'month', 'rf_kernel', 'cells')], 
#                      rf_total_ice_to_land[,c('facets', 'month', 'rf_kernel', 'RF_N_I2L', 'cells_I2L')], 
#                      by=c('facets', 'month', 'rf_kernel'))
# rf_N_by_type = merge(rf_N_by_type[,c('facets', 'month', 'rf_kernel', 'cells', 'RF_N_I2L', 'cells_I2L')], 
#                      rf_total_land_to_land[,c('facets', 'month', 'rf_kernel', 'RF_N_L2L', 'cells_L2L')], 
#                      by=c('facets', 'month', 'rf_kernel'))
# rf_N_by_type$RF_N_LI = (rf_N_by_type$RF_N_I2L + rf_N_by_type$RF_N_L2L) / (rf_N_by_type$cells_I2L + rf_N_by_type$cells_L2L)
# 
# # rf_holocene_continent_weighted_by_type = rf_long %>% 
# #   group_by(facets, month, rf_kernel) %>%
# #   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
# #                    forcing_total_area = as.numeric(sum(rf_value * area, na.rm=TRUE)), 
# #                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
# #                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
# #                    cells = n_distinct(cell_id), 
# #                    total_area = sum(area_km2, na.rm=TRUE),
# #                    .groups = 'keep')
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_continent$type = rep("land + ice")

ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = forcing_mean, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = forcing_total, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

ggplot() +
  geom_col(data=rf_holocene_continent, aes(x = forcing_total_area, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(rf_kernel~month)

##
## 
##

rf_NA = rbind(rf_holocene_ice, rf_holocene_ice_filled)
# rf_NA = rbind(rf_NA, rf_holocene_ice_filled)
rf_NA = rbind(rf_NA, rf_holocene_ice_free)
rf_NA = rbind(rf_NA, rf_holocene_land)
rf_NA = rbind(rf_NA, rf_holocene_ice_land_filled)
rf_NA = rbind(rf_NA, rf_holocene_continent)
rf_NA$pos = factor(rf_NA$pos, levels = c(TRUE, FALSE))
rf_NA$pos_weighted = factor(rf_NA$pos_weighted, levels = c(TRUE, FALSE))

rf_NA$type = factor(rf_NA$type, levels = c('land + ice', 'land to land', 'ice to land filled', 'once ice', 'once ice filled', 'always land'))

for (month in months){
  rf_month = rf_NA[which(rf_NA$month == month),]
  p = ggplot() +
    geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
    scale_y_discrete(limits=rev) +
    # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
    theme_bw(12) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
    xlab(bquote(radiative~forcing~(W/m^2))) +
    ylab('time period (k years)') +
    facet_grid(type~rf_kernel)
  print(p)
  ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
  ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
}

kernels = unique(rf_NA$rf_kernel)

for (kernel in kernels){
  rf_month = rf_NA[which(rf_NA$rf_kernel == kernel),]
  p = ggplot() +
    geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
    scale_y_discrete(limits=rev) +
    # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
    theme_bw(12) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
    xlab(bquote(radiative~forcing~(W/m^2))) +
    ylab('time period (k years)') +
    facet_grid(type~month)
  print(p)
  ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_facet_', alb_prod, '.png'), width=14, height=6)
  ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_facet_', alb_prod, '.pdf'), width=14, height=6)
}

# land + ice only, for each month
for (kernel in kernels){
  rf_month = rf_NA[which((rf_NA$rf_kernel == kernel) & (rf_NA$type == 'land + ice')),]
  p = ggplot() +
    geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
    scale_y_discrete(limits=rev) +
    # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
    theme_bw(14) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
    xlab(bquote(radiative~forcing~(W/m^2))) +
    ylab('time period (k years)') +
    # facet_grid(month~.)
    facet_wrap(~month, nrow=3, ncol=4)
  print(p)
  ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landice_facet_', alb_prod, '.png'), width=12, height=8)
  ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landice_facet_', alb_prod, '.pdf'), width=12, height=8)
}

rf_NA_wider = rf_NA[, c('facets', 'month', 'forcing_mean_weighted', 'rf_kernel', 'type')] %>% group_by(facets, month) %>% pivot_wider(names_from = rf_kernel, values_from = forcing_mean_weighted)

ggplot() +
  geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_cam5), alpha=0.5) +
  geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_hadgem), colour='dodgerblue', alpha=0.5) +
  facet_wrap(~month) +
  geom_abline(slope = 1, intercept=0)

# average over months
rf_annual = rf_NA %>% 
  group_by(facets, rf_kernel, type) %>%
  summarize(mean_total = mean(forcing_total, na.rm=TRUE),
            mean_total_area = mean(forcing_total_area, na.rm=TRUE),
            forcing_mean = mean(forcing_mean, na.rm=TRUE),
            forcing_mean_weighted = mean(forcing_mean_weighted, na.rm=TRUE),
            .groups='keep')

rf_annual = rf_annual %>%
  mutate(pos = forcing_mean > 0)
rf_annual = rf_annual %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)

rf_annual$pos = factor(rf_annual$pos, levels = c(TRUE, FALSE))
rf_annual$pos_weighted = factor(rf_annual$pos_weighted, levels = c(TRUE, FALSE))

rf_annual$type = factor(rf_annual$type, levels = c('land + ice', 'land to land', 'ice to land filled', 'once ice', 'once ice filled', 'always land'))

rf_annual = rf_annual[which(rf_annual$type %in% c('land + ice', 'land to land', 'ice to land filled')),]

lo_mean = floor(min(rf_annual$forcing_mean, na.rm=TRUE))
hi_mean = ceiling(max(rf_annual$forcing_mean, na.rm=TRUE))

lo_weighted = floor(min(rf_annual$forcing_mean_weighted, na.rm=TRUE))
hi_weighted = ceiling(max(rf_annual$forcing_mean_weighted, na.rm=TRUE))

ggplot() +
  # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  geom_col(data=rf_annual, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(type~rf_kernel)
ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)

ggplot() +
  # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  geom_col(data=rf_annual, aes(x = forcing_mean, y = facets, fill = pos_weighted)) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(type~rf_kernel)
ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)

ggplot() +
  # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  geom_col(data=rf_annual, aes(x = mean_total, y = facets, fill = pos_weighted)) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)') +
  facet_grid(type~rf_kernel)
ggsave(paste0('figures/RF_holocene_total_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_total_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)


# rf_all_bin = rf_all %>% 
#   group_by(rf_kernel, )



labels_period_full$bin = rep(seq(1, 12), each=2)


rf_annual$bin = labels_period_full[match(rf_annual$facets, labels_period_full$facets), 'bin']

rf_annual$period_mid = (labels_period_full[match(rf_annual$facets, labels_period_full$facets), 'end_young'] + 
                          labels_period_full[match(rf_annual$facets, labels_period_full$facets), 'end_old'])/2
rf_annual$period_mid = as.numeric(rf_annual$period_mid)

ggplot() +
  # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  geom_col(data=rf_annual, aes(x = period_mid, y = forcing_mean, fill = pos_weighted)) +
  scale_x_reverse() +
  # scale_y_discrete(limits=rev) +
  # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
  # theme_bw(12) +
  # theme(legend.position = "none",
  #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  # xlab(bquote(radiative~forcing~(W/m^2))) +
  # ylab('time periods (k years)') +
  facet_grid(type~rf_kernel)
# ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)

ggplot() +
  # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
  geom_point(data=rf_annual, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
  geom_line(data=rf_annual, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
  scale_x_reverse() +
  # scale_y_discrete(limits=rev) +
  # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
  # theme_bw(12) +
  # theme(legend.position = "none",
  #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  # xlab(bquote(radiative~forcing~(W/m^2))) +
  # ylab('time periods (k years)') +
  facet_grid(type~.)
ggsave(paste0('figures/RF_holocene_time_series_annual_', alb_prod, '.png'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_time_series_annual_', alb_prod, '.pdf'), width=10, height=6)


#############################################33
rf_coarse = rf_annual %>% 
  group_by(rf_kernel, type, bin) %>%
  summarize(forcing_mean = mean(forcing_mean),
            forcing_mean_weighted = mean(forcing_mean_weighted))

labels_period_coarse = labels_period_full %>%
  group_by(bin) %>%
  summarize(end_young = min(end_young),
            end_old = max(end_old))

labels_period_coarse$facets = paste(labels_period_coarse$end_young, labels_period_coarse$end_old, sep='-')
labels_period_coarse$period_mid = (labels_period_coarse$end_young + labels_period_coarse$end_old)/2


rf_coarse$facets =  labels_period_coarse$facets[match(rf_coarse$bin, labels_period_coarse$bin)]
rf_coarse$period_mid =  labels_period_coarse$period_mid[match(rf_coarse$bin, labels_period_coarse$bin)]


rf_coarse = rf_coarse %>%
  mutate(pos = forcing_mean > 0)
rf_coarse = rf_coarse %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)

rf_coarse$pos = factor(rf_coarse$pos, levels = c(TRUE, FALSE))
rf_coarse$pos_weighted = factor(rf_coarse$pos_weighted, levels = c(TRUE, FALSE))

rf_coarse = rf_coarse[which(rf_coarse$type %in% c('land + ice', 'land to land', 'ice to land filled')),]

rf_coarse$type = factor(rf_coarse$type)


# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_coarse, aes(y = period_mid, x = forcing_mean, fill = pos_weighted)) +
#   # scale_x_reverse() +
#   # scale_y_discrete(limits=rev) +
#   # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   # theme_bw(12) +
#   # theme(legend.position = "none",
#   #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # xlab(bquote(radiative~forcing~(W/m^2))) +
#   # ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_point(data=rf_coarse, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
#   geom_line(data=rf_coarse, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
#   scale_x_reverse() +
#   # scale_y_discrete(limits=rev) +
#   # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   # theme_bw(12) +
#   # theme(legend.position = "none",
#   #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # xlab(bquote(radiative~forcing~(W/m^2))) +
#   # ylab('time periods (k years)') +
#   facet_grid(type~.)
# ggsave(paste0('figures/RF_holocene_coarse_time_series_annual_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_coarse_time_series_annual_', alb_prod, '.pdf'), width=10, height=6)

######################################

# data frame of IPCC forcings
rf_ipcc = data.frame(rf_kernel = NA,
                     facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
                     forcing_mean = c(2.16, 0.54, 0.05, -0.20, -1.06),
                     pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                     # pos_weighted = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                     type = rep("modern", 5))
ggplot() +
  geom_col(data=rf_ipcc, aes(x = forcing_mean, y = facets, fill = !pos)) +
  scale_y_discrete(limits=rev) +
  theme_bw(18) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time periods (k years)')


# rf_all = rbind(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'pos_weighted', 'type')], 
#                rf_ipcc)
# 
# rf_all = rbind(rf_holocene_continent[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'pos_weighted', 'type')], 
#                rf_ipcc)
# rf_annual$pos = as.logical(rf_annual$pos)
# rf_annual$pos_weighted = as.logical(rf_annual$pos_weighted)


rf_annual$pos = as.logical(rf_annual$pos)
rf_all = rbind(data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')]), 
               rf_ipcc)

rf_all$facets = factor(rf_all$facets, levels=c(labels_period_full$facets, c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols")))

# # why do we need this line?!
rf_all$pos = !rf_all$pos
# rf_all$pos_weighted = !rf_all$pos_weighted


rf_all = subset(rf_all, rf_kernel %in% c('rf_cack', NA))
rf_all = subset(rf_all, type %in% c('land to land', 'ice to land filled', 'land + ice', 'modern'))

ggplot() +
  geom_col(data=rf_all, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  theme_bw(16) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_full_radiative_bar_both_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_full_radiative_bar_both_', alb_prod, '.pdf'))



####################3

# data frame of IPCC forcings
rf_ipcc = data.frame(rf_kernel = NA,
                     facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
                     forcing_mean = c(2.16, 0.54, 0.05, -0.20, -1.06),
                     pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                     # pos_weighted = c(TRUE, TRUE, TRUE, FALSE, FALSE),
                     type = rep("modern", 5))

rf_annual$pos = as.logical(rf_annual$pos)
rf_all = rbind(data.frame(rf_annual[,c('rf_kernel', 'facets', 'mean_total', 'pos', 'type')]), 
               rf_ipcc)

rf_all$facets = factor(rf_all$facets, levels=c(labels_period_full$facets, c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols")))

# # why do we need this line?!
rf_all$pos = !rf_all$pos
# rf_all$pos_weighted = !rf_all$pos_weighted


rf_all = subset(rf_all, rf_kernel %in% c('rf_cack', NA))
rf_all = subset(rf_all, type %in% c('land to land', 'ice to land filled', 'land + ice', 'modern'))

ggplot() +
  geom_col(data=rf_all, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  theme_bw(16) +
  theme(legend.position = "none") +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_full_radiative_bar_both_', alb_prod, '.png'))
ggsave(paste0('figures/alb_preds_full_radiative_bar_both_', alb_prod, '.pdf'))









rf_all_class = rbind(data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')]), 
                     rf_ipcc)

rf_annual_weighted = data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean_weighted', 'pos_weighted', 'type')])
colnames(rf_annual_weighted) = c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')

rf_weighted_all_class = rbind(rf_annual_weighted, 
                              rf_ipcc)

# why do we need this line?!
rf_all_class$pos = !rf_all_class$pos

ggplot() +
  geom_col(data=rf_all_class, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  facet_grid(type~rf_kernel, scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_full_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
ggsave(paste0('figures/alb_preds_full_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)



rf_annual_hadgem = subset(rf_annual, rf_kernel == 'rf_hadgem')

lo_mean_hadgem = floor(min(rf_annual_hadgem$forcing_mean, na.rm=TRUE))
hi_mean_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean, na.rm=TRUE))

lo_weighted_hadgem = floor(min(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))
hi_weighted_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))

pdf(paste0('figures/RF_holocene_kernels_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
for (kernel in kernels){
  
  rf_annual_kernel = subset(rf_annual, rf_kernel == kernel)
  
  lo_mean_kernel = floor(min(rf_annual_kernel$forcing_mean, na.rm=TRUE))
  hi_mean_kernel = ceiling(max(rf_annual_kernel$forcing_mean, na.rm=TRUE))
  
  lo_weighted_kernel = floor(min(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
  hi_weighted_kernel = ceiling(max(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
  
  p = ggplot() +
    geom_col(data=subset(rf_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
    scale_y_discrete(limits=rev) +
    # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
    # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
    scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
    theme_bw(12) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
    xlab(bquote(radiative~forcing~(W/m^2))) +
    ylab('forcing agent       time period (k years)') +
    labs(title = paste0(kernel, '; mean forcing')) +
    facet_grid(type~., scales="free_y", space = "free") 
  print(p)
  # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
  # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
  
  p = ggplot() +
    geom_col(data=subset(rf_weighted_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
    scale_y_discrete(limits=rev) +
    # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
    # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
    scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
    theme_bw(12) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
    xlab(bquote(radiative~forcing~(W/m^2))) +
    ylab('forcing agent       time period (k years)') +
    labs(title = paste0(kernel, '; mean weighted forcing')) +
    facet_grid(type~., scales="free_y", space = "free") 
  print(p)
}
dev.off()



pdf(paste0('figures/RF_holocene_kernels_bar_landice_', alb_prod, '.pdf'), width=6, height=6)


rf_annual_landice = subset(rf_annual, type == 'land + ice')

lo_mean_kernel = floor(min(rf_annual_landice$forcing_mean, na.rm=TRUE))
hi_mean_kernel = ceiling(max(rf_annual_landice$forcing_mean, na.rm=TRUE))

lo_weighted_kernel = floor(min(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))
hi_weighted_kernel = ceiling(max(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))

p = ggplot() +
  geom_col(data=rf_annual_landice, aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
  scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  labs(title = 'mean annual forcing') +
  facet_grid(rf_kernel~., scales="free_y", space = "free") 
print(p)
# ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)

p = ggplot() +
  geom_col(data=rf_annual_landice, aes(x = forcing_mean_weighted, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
  # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
  scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('forcing agent       time period (k years)') +
  labs(title = 'mean annual weighted forcing') +
  facet_grid(rf_kernel~., scales="free_y", space = "free") 
print(p)
dev.off()


###############################################################################################################
## summarize rf by region
###############################################################################################################

# alb_diff_merge_sub = subset(alb_diff_merge, )

## CASE: ENA
xlo = -100
xhi = -52
ylo = 48
yhi = 67
xlim = c(xlo, xhi)
ylim = c(ylo, yhi)

boxes_ENA = data.frame(yhi = 67, ylo = 48, xlo = -100, xhi = -52, id="ECAN")
boxes_ENA = transform(boxes_ENA, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)

rf_ENA = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_ENA = alb_diff_merge_ENA %>% 
#   group_by(facets) %>%
#   dplyr::summarize(forcing_total = sum(forcing), 
#                    forcing_mean = mean(forcing), 
#                    forcing_mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_monthly_ENA = rf_ENA %>% 
  group_by(facets, rf_kernel, month) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))

rf_monthly_ENA = rf_monthly_ENA %>%
  mutate(pos = forcing_mean > 0)
rf_monthly_ENA = rf_monthly_ENA %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_monthly_ENA$region = rep("ENA")


rf_annual_ENA = rf_ENA %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))

rf_annual_ENA = rf_annual_ENA %>%
  mutate(pos = forcing_mean > 0)
rf_annual_ENA = rf_annual_ENA %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_annual_ENA$region = rep("ENA")

# CASE: HEMLOCK
xlo = -90
xhi = -60 
ylo = 37
yhi = 48
xlim = c(xlo, xhi)
ylim = c(ylo, yhi)

boxes_HEM = data.frame(yhi = 48, ylo = 37, xlo = -90, xhi = -60, id="NEUS/SEC")
boxes_HEM = transform(boxes_HEM, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)

rf_HEM = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_HEM = alb_diff_merge_HEM %>% 
#   group_by(facets) %>%
#   dplyr::summarize(forcing_total = sum(forcing), 
#                    forcing_mean = mean(forcing), 
#                    forcing_mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_monthly_HEM = rf_HEM %>% 
  group_by(facets, rf_kernel, month) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))


rf_monthly_HEM = rf_monthly_HEM %>%
  mutate(pos = forcing_mean > 0)
rf_monthly_HEM = rf_monthly_HEM %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_monthly_HEM$region = rep("HEM")

rf_annual_HEM = rf_HEM %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))

rf_annual_HEM = rf_annual_HEM %>%
  mutate(pos = forcing_mean > 0)
rf_annual_HEM = rf_annual_HEM %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_annual_HEM$region = rep("HEM")

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

rf_WCAN = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))

# rf_summary_WCAN = alb_diff_merge_WCAN %>% 
#   group_by(facets) %>%
#   dplyr::summarize(forcing_total = sum(forcing), 
#                    forcing_mean = mean(forcing), 
#                    forcing_mean_weighted = sum(forcing * area) / sum(area),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2))

rf_monthly_WCAN = rf_WCAN %>% 
  group_by(facets, rf_kernel, month) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))

rf_monthly_WCAN = rf_monthly_WCAN %>%
  mutate(pos = forcing_mean > 0)
rf_monthly_WCAN = rf_monthly_WCAN %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_monthly_WCAN$region = rep("WCAN")

rf_annual_WCAN = rf_WCAN %>% 
  group_by(facets, rf_kernel) %>%
  dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
                   forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
                   forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
                   cells = n_distinct(cell_id), 
                   total_area = sum(area_km2, na.rm=TRUE))

rf_annual_WCAN = rf_annual_WCAN %>%
  mutate(pos = forcing_mean > 0)
rf_annual_WCAN = rf_annual_WCAN %>%
  mutate(pos_weighted = forcing_mean_weighted > 0)
rf_annual_WCAN$region = rep("WCAN")

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

rf_holocene_NA_landice = subset(rf_holocene_continent, type == 'land + ice')
rf_holocene_NA_landice$region = rep('Continent')
rf_holocene_NA_landice = rf_holocene_NA_landice[,colnames(rf_monthly_ENA)]

rf_monthly_region = rbind(rf_holocene_NA_landice,
                          rf_monthly_ENA,
                          rf_monthly_HEM,
                          rf_monthly_WCAN)

rf_monthly_region$region = factor(rf_monthly_region$region, 
                                  levels = c('Continent', 'ENA', 'HEM', 'WCAN'),
                                  labels = c('Continental', 'ECAN', 'NEUS/SEC', 'WCAN/AK'))

rf_holocene_NA_annual_landice = subset(rf_annual, type == 'land + ice')
rf_holocene_NA_annual_landice$region = rep('Continent')
rf_holocene_NA_annual_landice = rf_holocene_NA_annual_landice[,c('facets', 'rf_kernel', 
                                                                 'forcing_mean', 
                                                                 'forcing_mean_weighted', 'pos', 
                                                                 'pos_weighted', 'region')] 
# rf_holocene_NA_annual_landice = rf_holocene_NA_annual_landice[,colnames(rf_annual_ENA)]

rf_annual_region = rbind(data.frame(rf_holocene_NA_annual_landice),
                         rf_annual_ENA[,colnames(rf_holocene_NA_annual_landice)],
                         rf_annual_HEM[,colnames(rf_holocene_NA_annual_landice)],
                         rf_annual_WCAN[,colnames(rf_holocene_NA_annual_landice)])

rf_annual_region$region = factor(rf_annual_region$region, 
                                 levels = c('Continent', 'ENA', 'HEM', 'WCAN'),
                                 labels = c('Continental', 'ECAN', 'NEUS/SEC', 'WCAN/AK'))

rf_annual_region$pos = as.logical(rf_annual_region$pos)

# rf_region$facets
# rf_region$pos


# ggplot() +
#   geom_col(data=bar, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")

ggplot() +
  geom_col(data=subset(rf_monthly_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
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
  facet_grid(region~month)#, scales="free_x")#, space = "free")
ggsave(paste0('figures/RF_holocene_bar_regions_by_month_facet_', alb_prod, '.pdf'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_regions_by_month_facet_', alb_prod, '.png'), width=10, height=6)

ggplot() +
  geom_col(data=subset(rf_monthly_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
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
  facet_grid(month~region, scales="free_x")#, space = "free")
ggsave(paste0('figures/RF_holocene_bar_month_by_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_month_by_regions_facet_', alb_prod, '.png'), width=10, height=6)

ggplot() +
  geom_col(data=subset(rf_annual_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
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
  facet_grid(region~., scales="free_x")#, space = "free")
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.png'), width=10, height=6)


ggplot() +
  geom_col(data=subset(rf_annual_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean_weighted, y = facets, fill = !pos), width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  scale_x_break(c(13, 29)) +
  # theme(legend.position = "none",
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank(),
  #       # panel.background = element_blank(),
  #       strip.text = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       axis.text = element_text(size=12)) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time period (k years)') +
  facet_grid(region~., scales="free_x")#, space = "free")
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.png'), width=10, height=6)


ggplot() +
  geom_col(data=subset(rf_annual_region, rf_kernel == 'rf_hadgem'), aes(y = forcing_mean_weighted, x = facets, fill = !pos), width=0.9) +
  scale_x_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
  theme_bw(12) +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
  # scale_x_break(c(13, 29)) +
  # theme(legend.position = "none",
  #       # panel.grid.major = element_blank(),
  #       # panel.grid.minor = element_blank(),
  #       # panel.background = element_blank(),
  #       strip.text = element_text(size=14),
  #       axis.title = element_text(size=14),
  #       axis.text = element_text(size=12)) +
  xlab(bquote(radiative~forcing~(W/m^2))) +
  ylab('time period (k years)') +
  facet_grid(region~., scales="free_y")#, space = "free")
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.png'), width=10, height=6)


rf_annual_region$type = 'Holocene'
rf_ipcc$region = 'Northern Hemisphere'

rf_annual_region_both = rbind(rf_annual_region[, colnames(rf_ipcc)], 
                              rf_ipcc)


rf_annual_region_both_hadgem = subset(rf_annual_region_both, rf_kernel %in% c('rf_hadgem', NA))

lo_mean_both = floor(min(rf_annual_region_both_hadgem$forcing_mean, na.rm=TRUE))
hi_mean_both = ceiling(max(rf_annual_region_both_hadgem$forcing_mean, na.rm=TRUE))

# lo_weighted_both = floor(min(rf_annual_region_both$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted_both = ceiling(max(rf_annual_region_both$forcing_mean_weighted, na.rm=TRUE))


ggplot(data=rf_annual_region_both_hadgem) +
  geom_col(data=rf_annual_region_both_hadgem, aes(x = forcing_mean, y = facets, fill = !pos), 
           width=0.9) +
  scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
  scale_x_continuous(breaks = seq(lo_mean_both, hi_mean_both), 
                     limits = c(lo_mean_both, hi_mean_both), expand = c(0,0)) +
  # scale_x_break(c(20,32)) +
  # scale_x_break(c(20,30), space = 0.4, 
  #               ticklabels = seq(lo_mean_both, hi_mean_both),
  #               expand = c(0, 0)) +
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
  facet_grid(region~., scales="free_y", space = "free")
ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.png'), height=6, width=6, dpi=300)
ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.pdf'), height=6, width=6, dpi=300)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# alb_diff_full = readRDS(paste0('output/forcing/RF_holocene.RDS'))
# alb_diff = alb_diff_full[which(alb_diff_full$year %in% ages_sub),]
# 
# diff_years = unique(alb_diff$year)
# alb_diff$facets = labels_period[match(alb_diff$year, diff_years)]
# alb_diff$facets = factor(alb_diff$facets, 
#                          levels = labels_period, 
#                          labels =  labels_period)
# 
# 
# alb_diff_full$facets = labels_period_full[match(alb_diff_full$year, labels_period_full$end_young*1000), 'facets']
# 
# # thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
# max_diff = max(abs(alb_diff$rf_hadgem), na.rm=TRUE)
# thresh = ceiling(max_diff*100)/100
# 
# # values = c(0,0.4, 0.45, 0.5, 0.55, 0.6,1)
# values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)
# 
# sc_fill_diverge <- scale_fill_distiller(type = "div",
#                                         palette = "RdYlBu",#"BrBG",
#                                         # labels = labels,
#                                         na.value="grey", 
#                                         name="RF (W/m^2)",
#                                         limits = c(-thresh,thresh),
#                                         values = values)
# 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "RdBu",#"BrBG",
# #                                         # labels = labels,
# #                                         na.value="grey", 
# #                                         name="RF (W/m^2)",
# #                                         limits = c(-thresh,thresh),
# #                                         values = values)
# # 
# # sc_fill_diverge <- scale_fill_distiller(type = "div",
# #                                         palette = "Spectral",#"BrBG",
# #                                         # labels = labels,
# #                                         na.value="grey", 
# #                                         name="RF (W/m^2)",
# #                                         limits = c(-thresh,thresh),
# #                                         values = values)
# # 
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
#                                             # labels = labels,
#                                             na.value="transparent", 
#                                             name=expression(Forcing (W/m^2)),
#                                             limits = c(-thresh,thresh), 
#                                             values = values)
# 
# # for (year in diff_years){
# #   
# #   alb_diff_sub = alb_diff[which(alb_diff$year == year),]
# #   ggplot()+
# #     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #     geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
# #     sc_colour_diverge + 
# #     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #     facet_grid(facets~.)+
# #     # facet_wrap(year~.)+
# #     theme_bw()+
# #     theme(panel.grid.major = element_blank(),
# #           panel.grid.minor = element_blank(),
# #           panel.background = element_blank(),
# #           strip.text = element_text(size=14),
# #           axis.title = element_blank(),
# #           axis.ticks = element_blank(),
# #           axis.text = element_blank(),
# #           legend.text = element_text(size=14),
# #           legend.title = element_text(size=14)) +
# #     coord_fixed()
# #   # scale_fill_brewer(type = "div", palette = 'Rd
# #   ggsave(paste0('figures/alb_preds_radiative_point_single_', year, 'k.png'))
# #   ggsave(paste0('figures/alb_preds_radiative_point_single_', year, 'k.pdf'))
# # }
# # 
# # 
# # for (year in diff_years){
# #   
# #   alb_diff_sub = alb_diff[which(alb_diff$year == year),]
# #   ggplot()+
# #     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #     geom_tile(data=alb_diff_sub, aes(x=long, y=lat, fill = forcing)) +
# #     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
# #     # sc_colour_diverge + 
# #     sc_fill_diverge + 
# #     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #     facet_grid(facets~.)+
# #     # facet_wrap(year~.)+
# #     theme_bw()+
# #     theme(panel.grid.major = element_blank(),
# #           panel.grid.minor = element_blank(),
# #           panel.background = element_blank(),
# #           strip.text = element_text(size=14),
# #           axis.title = element_blank(),
# #           axis.ticks = element_blank(),
# #           axis.text = element_blank(),
# #           legend.text = element_text(size=14),
# #           legend.title = element_text(size=14)) +
# #     coord_fixed()
# #   
# #   ggsave(paste0('figures/alb_preds_radiative_tile_single_', year, 'k.png'))
# #   ggsave(paste0('figures/alb_preds_radiative_tile_single_', year, 'k.pdf'))
# # }
# 
# alb_diff_sub = alb_diff
# facets = unique(alb_diff_sub$facets)
# 
# pdf(paste0('figures/alb_preds_radiative_tile_pages_', alb_prod, '.pdf'))
# for (i in 1:length(facets)){
#   
#   facet = facets[i]
#   
#   alb_diff_sub = alb_diff[which(alb_diff$facets == facet),]
#   ice_old = ice_fort_diff_old[which(ice_fort_diff_old$facets == facet ),]
#   ice_young = ice_fort_diff_young[which(ice_fort_diff_young$facets == facet ),]
#   
#   p<-ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=alb_diff_sub, aes(x=long, y=lat, fill = rf_hadgem)) +
#     geom_polygon(data=ice_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     geom_polygon(data=ice_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # facet_grid(month~.)+
#     facet_wrap(~month)+
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
#     # coord_fixed()
#     coord_fixed(xlim = xlim, ylim = ylim)
#   print(p)
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.png'))
#   # ggsave(paste0('figures/alb_preds_radiative_subset_tile_', year, 'k.pdf'))
# }
# dev.off()
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_point(data=alb_diff, aes(x=long, y=lat, colour = forcing), size=1)+
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
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # pdf(paste0('figures/alb_preds_radiative_point_grid_', alb_prod, '.pdf'))
# 
# 
# # b = bbox(pbs_ll)
# # bbox(pbs_ll)[1] = -170
# # bbox(pbs_ll)[2] = -45
# # 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_tile(data=alb_diff, aes(x=long, y=lat, fill = rf_hadgem)) +
# #   geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
# #   geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
# #   sc_fill_diverge +
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # facet_grid(facets~.)+
# #   facet_grid(facets~month)+
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
# #   coord_fixed(xlim = xlim, ylim = ylim)
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # ggsave(paste0('figures/alb_preds_radiative_tile_wrap_', alb_prod, '.pdf'))
# # ggsave(paste0('figures/alb_preds_radiative_tile_wrap_', alb_prod, '.png'))
# 
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing)) +
# #   geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
# #   geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
# #   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
# #   # sc_colour_diverge + 
# #   sc_fill_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   facet_grid(facets~.)+
# #   #facet_wrap(~facets)+
# #   theme_bw(12)+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=8),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=12),
# #         legend.title = element_text(size=12)) +
# #   coord_fixed(xlim = xlim, ylim = ylim)
# # # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.pdf'))
# 
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing_ice)) +
# #   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
# #   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
# #   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
# #   # sc_colour_diverge + 
# #   sc_fill_diverge + 
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   facet_grid(facets~.)+
# #   #facet_wrap(~facets)+
# #   theme_bw(12)+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=8),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=12),
# #         legend.title = element_text(size=12)) +
# #   coord_fixed(xlim = xlim, ylim = ylim)
# # # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# # # scale_fill_brewer(type = "div", palette = 'Rd
# # ggsave(paste0('figures/alb_preds_radiative_ice_tile_grid_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_radiative_ice_tile_grid_', alb_prod, '.pdf'))
# 
# 
# sc_fill_seq <- scale_fill_distiller(type = "seq",
#                                     palette = "YlOrBr",#"YlGnBu",#
#                                     # labels = labels,
#                                     na.value="grey", 
#                                     direction=-1,
#                                     name="Radiative \nkernel")
# 
# # ggplot()+
# #   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
# #   geom_tile(data=subset(alb_diff, year == 50), aes(x=long, y=lat, fill = rk)) +
# #   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  
# #   #              colour=ice_colour, fill=ice_fill) +
# #   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  
# #   #              colour=ice_colour_dark, fill=ice_fill_dark) +
# #   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
# #   # sc_colour_diverge + 
# #   sc_fill_seq +
# #   # scale_fill_gradientn(colours = terrain.colors(7)) +
# #   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white')+
# #   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
# #   # facet_grid(facets~.)+
# #   #facet_wrap(~facets)+
# #   theme_bw(12)+
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         strip.text = element_text(size=8),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         legend.text = element_text(size=12),
# #         legend.title = element_text(size=12)) +
# #   coord_fixed(xlim = xlim, ylim = ylim)
# # ggsave(paste0('figures/radiative_kernel_tile_', alb_prod, '.png'))
# # ggsave(paste0('figures/radiative_kernel_tile_', alb_prod, '.pdf'))
# 
# ###############################################################################################################
# ## summarize rf at continental scale
# ###############################################################################################################
# 
# # # labels = c('2 - 0.05', '4 - 2', '4 - 6', '8 - 6', '10 - 8')
# # # labels_period = c('0.05 - 0.5', '0.5 - 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12')
# # # labels_period = c('0.5 - 0.05', '2 - 0.5', '4 - 2', '6 - 4', '8 - 6', '10 - 8', '12 - 10')
# # labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
# # 
# # # labels_period = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', '6 - 4 ka', '8 - 6 ka', 
# # # '10 - 8 ka', '12 - 10 ka')
# # 
# # diff_years = unique(alb_diff$year)
# # alb_diff$facets = labels_period[match(alb_diff$year, diff_years)]
# # alb_diff$facets = factor(alb_diff$facets, 
# #                          levels = labels_period, 
# #                          labels =  labels_period)
# # 
# # # thresh = round_any(max(abs(diff$diff), na.rm=TRUE), 0.01, f=ceiling)
# # max_diff = max(abs(alb_diff$rf_hadgem), na.rm=TRUE)
# # thresh = ceiling(max_diff*100)/100
# # 
# # # values = c(0,0.4, 0.45, 0.5, 0.55, 0.6,1)
# # values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)
# 
# # read in grid
# grid_NA <- readRDS("data/grid.RDS")
# grid_df = as.data.frame(grid_NA, xy=TRUE)
# 
# spdf_2 <- as(grid_NA,'SpatialPolygonsDataFrame')
# 
# sf_data = st_as_sf(spdf_2)
# grid_df$area <- st_area(sf_data) #area of each "square"
# grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
# colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')
# 
# alb_diff_merge = merge(alb_diff_full, grid_df, by = c('cell_id', 'lat', 'long'))
# 
# 
# # drop cells that are not included for all time periods
# cell_id_drop = as.numeric(names(which(table(alb_diff_merge$cell_id)<7)))
# alb_diff_merge = alb_diff_merge[which(!(alb_diff_merge$cell_id %in% cell_id_drop)),]
# 
# 
# alb_diff_merge_full = alb_diff_merge
# 
# alb_diff_merge = alb_diff_merge[which(alb_diff_merge$year %in% ages_sub),]
# 
# p = ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_merge, aes(x=long, y=lat, fill = rf_hadgem)) +
#   geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   # geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~month)+
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
# print(p)
# ggsave(paste0('figures/RF_holocene_month_tile_grid_', alb_prod, '.png'), width=14, height=10)
# ggsave(paste0('figures/RF_holocene_month_tile_grid_', alb_prod, '.pdf'), width=14, height=10)
# 
# months_sub = c('feb', 'may', 'aug', 'nov')
# 
# alb_diff_merge_sub = alb_diff_merge[which(alb_diff_merge$month %in% months_sub),]
# 
# p = ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_merge_sub, aes(x=long, y=lat, fill = rf_hadgem)) +
#   geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   # geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~month)+
#   #facet_wrap(~facets)+
#   theme_bw(12)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed(xlim = xlim, ylim = ylim)
# print(p)
# ggsave(paste0('figures/RF_holocene_month_sub_tile_grid_', alb_prod, '.png'), width=14, height=10)
# ggsave(paste0('figures/RF_holocene_month_sub_tile_grid_', alb_prod, '.pdf'), width=14, height=10)
# 
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_merge, aes(x=long, y=lat, fill = rf_hadgem_ice)) +
#   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~month)+
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
# # ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_radiative_tile_grid_', alb_prod, '.pdf'))
# 
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=alb_diff_merge, aes(x=long, y=lat, fill = alb_mean)) +
#   # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#   # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   facet_grid(facets~month)+
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
# # ggsave(paste0('figures/ALB_holocene_month_tile_grid_', alb_prod, '.png'))
# # ggsave(paste0('figures/ALB_holocene_month_tile_grid_', alb_prod, '.pdf'))
# 
# foo = subset(alb_diff_merge, year==8000)
# foo[which((foo$ice_old == 'ICE') & (foo$forcing_ice < 0)),]
# 
# bar = foo[which((foo$ice_old == 'ICE') & (foo$forcing_ice < 0)),'cell_id']
# 
# ###############################################################################################################
# ## summarize rf at continental scale by land class
# ###############################################################################################################
# 
# labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', 
#                   '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
# labels_period_flip = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', 
#                        '6 - 4 ka', '8 - 6 ka', '10 - 8 ka', '12 - 10 ka')
# 
# alb_diff_merge_keep = alb_diff_merge[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 'facets',
#                                          'rf_cack', 'rf_hadgem', 'rf_cam5', 'ice_old', 
#                                          'ice_young')]
# alb_diff_merge_keep_ice = alb_diff_merge[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 
#                                              'year', 'month',  'facets',
#                                              'rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice', 'ice_old', 
#                                              'ice_young')]
# 
# rf_long = alb_diff_merge_keep %>%
#   pivot_longer(cols = c('rf_cack', 'rf_hadgem', 'rf_cam5'),
#                names_to = 'rf_kernel',
#                values_to = 'rf_value')
# rf_long = data.frame(rf_long)
# 
# ggplot() +
#   geom_histogram(data=rf_long, aes(x=rf_value, y=after_stat(density))) +
#   facet_grid(rf_kernel~month, scales = 'free_x')
# summary(rf_long$rf_value)
# hist(rf_long$rf_value)
# 
# breaks_rf = c(-140, -5, -2, 0, 2, 5, 140)
# rf_long$rf_value_bin = cut(rf_long$rf_value, breaks = breaks_rf, labels = FALSE)
# 
# rf_long_ice_filled = alb_diff_merge_keep_ice %>%
#   pivot_longer(cols = c('rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice'),
#                names_to = 'rf_kernel',
#                values_to = 'rf_value_ice')
# rf_long_ice_filled = data.frame(rf_long_ice_filled)
# rf_long_ice_filled$rf_kernel = substr(rf_long_ice_filled$rf_kernel, 1, 
#                                       nchar(rf_long_ice_filled$rf_kernel)-4)
# 
# rf_long = merge(rf_long, 
#                 rf_long_ice_filled[,c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 'facets', 
#                                       'rf_kernel', 'rf_value_ice')], 
#                 by = c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 'facets', 'rf_kernel'))
# 
# # rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
# #                'alb_diff'] = NA
# rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
#         'rf_value'] = NA
# 
# length(unique(rf_long$cell_id))
# 
# # alb_diff$facets_flip = alb_diff$facets
# # alb_diff$facets_flip = factor(alb_diff$facets_flip, 
# #                               levels = labels_period, labels = labels_period_flip)
# 
# cell_id_ice = unique(rf_long[which((!is.na(rf_long$ice_young))|
#                                      (!is.na(rf_long$ice_old))), 'cell_id'])
# 
# length(unique(cell_id_ice))
# 
# cell_id_ice_free = unique(rf_long$cell_id[!(rf_long$cell_id %in% cell_id_ice)])
# length(cell_id_ice_free)
# 
# rf_long_ice = subset(rf_long, cell_id %in% cell_id_ice)
# rf_long_ice = subset(rf_long_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice$cell_id))
# 
# # ggplot(data=rf_long_ice) +
# #   geom_histogram(aes(x=alb_diff_ice)) +
# #   facet_grid(facets~month)
# 
# for (month in months){
#   rf_month = rf_long_ice[which(rf_long_ice$month == month),]
#   
#   p = ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=rf_month, aes(x=long, y=lat, fill = rf_value)) +
#     # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~rf_kernel)+
#     #facet_wrap(~facets)+
#     theme_bw(12)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=8),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     coord_fixed(xlim = xlim, ylim = ylim)
#   # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_once_ice.png'), width=8, height=12)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_once_ice.pdf'), width=8, height=12)
# }
# 
# rf_holocene_ice = rf_long_ice %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice$type = rep("once ice")
# 
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean'] = NA
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean_weighted'] = NA
# rf_holocene_ice
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean_weighted, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# 
# rf_long_ice_filled = subset(rf_long, cell_id %in% cell_id_ice)
# # alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice_filled$cell_id))
# 
# # ggplot(data=rf_long_ice_filled) +
# #   geom_histogram(aes(x=alb_diff_ice)) +
# #   facet_grid(facets~rf_kernel)
# for (month in months){
#   rf_month = rf_long_ice_filled[which(rf_long_ice_filled$month == month),]
#   
#   p = ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=rf_month, aes(x=long, y=lat, fill = rf_value_ice)) +
#     # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge +
#     sc_fill_diverge +
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~rf_kernel)+
#     #facet_wrap(~facets)+
#     theme_bw(12)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=8),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     coord_fixed(xlim = xlim, ylim = ylim)
#   # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80))
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_once_ice_filled.png'), width=8, height=12)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_once_ice_filled.pdf'), width=8, height=12)
# }
# 
# rf_holocene_ice_filled = rf_long_ice_filled %>%
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE),
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE),
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id),
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_filled$type = rep("once ice filled")
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# rf_long_ice_free = subset(rf_long, cell_id %in% cell_id_ice_free)
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene_ice_free = rf_long_ice_free %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_free$type = rep("never ice")
# 
# for (month in months){
#   rf_month = rf_long_ice_free[which(rf_long_ice_free$month == month),]
#   
#   p = ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=rf_month, aes(x=long, y=lat, fill = rf_value)) +
#     # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~rf_kernel)+
#     #facet_wrap(~facets)+
#     theme_bw(12)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=8),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     coord_fixed(xlim = xlim, ylim = ylim)
#   print(p)
#   # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_never_ice.png'), width=8, height=12)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_never_ice.pdf'), width=8, height=12)
# }
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# rf_long_land = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene = rf_long_land %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# 
# rf_holocene = rf_holocene %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene = rf_holocene %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene$type = rep("land")
# 
# for (month in months){
#   rf_month = rf_long_land[which(rf_long_land$month == month),]
#   
#   p = ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=rf_month, aes(x=long, y=lat, fill = rf_value)) +
#     # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~rf_kernel)+
#     #facet_wrap(~facets)+
#     theme_bw(12)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=8),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     coord_fixed(xlim = xlim, ylim = ylim)
#   print(p)
#   # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_land.png'), width=8, height=12)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_land.pdf'), width=8, height=12)
# }
# 
# ggplot() +
#   geom_col(data=rf_holocene, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# rf_holocene_continent = rf_long %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_continent$type = rep("land + ice")
# 
# for (month in months){
#   rf_month = rf_long[which(rf_long$month == month),]
#   p = ggplot()+
#     geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#     geom_tile(data=rf_month, aes(x=long, y=lat, fill = rf_value_ice)) +
#     # geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
#     # geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
#     #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#     # sc_colour_diverge + 
#     sc_fill_diverge + 
#     # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#     facet_grid(facets~rf_kernel)+
#     #facet_wrap(~facets)+
#     theme_bw(12)+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_blank(),
#           strip.text = element_text(size=8),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     coord_fixed(xlim = xlim, ylim = ylim)
#   # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
#   # scale_fill_brewer(type = "div", palette = 'Rd
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_land_ice_filled.png'), width=8, height=12)
#   ggsave(paste0('figures/RF_holocene_', month, '_tile_grid_', alb_prod, '_land_ice_filled.pdf'), width=8, height=12)
# }
# 
# ggplot() +
#   geom_col(data=rf_holocene_continent, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_continent, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# rf_NA = rbind(rf_holocene, rf_holocene_ice)
# rf_NA = rbind(rf_NA, rf_holocene_ice_free)
# rf_NA = rbind(rf_NA, rf_holocene_continent)
# rf_NA$pos = factor(rf_NA$pos, levels = c(TRUE, FALSE))
# rf_NA$pos_weighted = factor(rf_NA$pos_weighted, levels = c(TRUE, FALSE))
# 
# rf_NA$type = factor(rf_NA$type, levels = c('land + ice', 'land', 'never ice', 'once ice'))
# 
# for (month in months){
#   rf_month = rf_NA[which(rf_NA$month == month),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     facet_grid(type~rf_kernel)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
#   ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# }
# 
# kernels = unique(rf_NA$rf_kernel)
# 
# for (kernel in kernels){
#   rf_month = rf_NA[which(rf_NA$rf_kernel == kernel),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     facet_grid(type~month)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_facet_', alb_prod, '.png'), width=14, height=6)
#   ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_facet_', alb_prod, '.pdf'), width=14, height=6)
# }
# 
# # land + ice only, for each month
# for (kernel in kernels){
#   rf_month = rf_NA[which((rf_NA$rf_kernel == kernel) & (rf_NA$type == 'land + ice')),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(14) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     # facet_grid(month~.)
#     facet_wrap(~month, nrow=3, ncol=4)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landice_facet_', alb_prod, '.png'), width=12, height=8)
#   ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landice_facet_', alb_prod, '.pdf'), width=12, height=8)
# }
# 
# rf_NA_wider = rf_NA[, c('facets', 'month', 'forcing_mean_weighted', 'rf_kernel', 'type')] %>% group_by(facets, month) %>% pivot_wider(names_from = rf_kernel, values_from = forcing_mean_weighted)
# 
# ggplot() +
#   geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_cam5), alpha=0.5) +
#   geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_hadgem), colour='dodgerblue', alpha=0.5) +
#   facet_wrap(~month) +
#   geom_abline(slope = 1, intercept=0)
# 
# 
# 
# rf_annual = rf_NA %>% 
#   group_by(facets, rf_kernel, type) %>%
#   summarize(forcing_mean = mean(forcing_mean, na.rm=TRUE),
#             forcing_mean_weighted = mean(forcing_mean_weighted, na.rm=TRUE),
#             .groups='keep')
# 
# rf_annual = rf_annual %>%
#   mutate(pos = forcing_mean > 0)
# rf_annual = rf_annual %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# 
# rf_annual$pos = factor(rf_annual$pos, levels = c(TRUE, FALSE))
# rf_annual$pos_weighted = factor(rf_annual$pos_weighted, levels = c(TRUE, FALSE))
# 
# rf_annual$type = factor(rf_annual$type, levels = c('land + ice', 'land', 'never ice', 'once ice'))
# 
# rf_annual$forcing_mean
# 
# 
# lo_mean = floor(min(rf_annual$forcing_mean, na.rm=TRUE))
# hi_mean = ceiling(max(rf_annual$forcing_mean, na.rm=TRUE))
# 
# lo_weighted = floor(min(rf_annual$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted = ceiling(max(rf_annual$forcing_mean_weighted, na.rm=TRUE))
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_annual, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_weighted_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_annual, aes(x = forcing_mean, y = facets, fill = pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# 
# # ggplot() +
# #   geom_col(data=rf_NA, aes(x = forcing_mean, y = facets, fill = type), position='dodge') +
# #   scale_y_discrete(limits=rev) +
# #   scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
# #   theme_bw(14) +
# #   theme(panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82'),
# #         panel.grid.minor.x = element_line(linewidth = 0.3, color = 'grey82')) +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('time periods (k years)') +
# #   facet_grid(rf_kernel~.)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.png'), width=8, height=6)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.pdf'), width=8, height=6)
# # 
# # ggplot() +
# #   geom_point(data=rf_NA, aes(x = forcing_mean, y = facets, colour = type)) +
# #   scale_y_discrete(limits=rev) +
# #   theme_bw(18) +
# #   # theme(legend.position = "none") +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('time periods (k years)') +
# #   facet_grid(rf_kernel~.)
# 
# # data frame of IPCC forcings
# rf_ipcc = data.frame(rf_kernel = NA,
#                      facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
#                      forcing_mean = c(2.16, 0.54, 0.05, -0.20, -1.06),
#                      pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
#                      type = rep("modern", 5))
# ggplot() +
#   geom_col(data=rf_ipcc, aes(x = forcing_mean, y = facets, fill = pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)')
# 
# 
# rf_all = rbind(rf_holocene[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')], 
#                rf_ipcc)
# 
# # why do we need this line?!
# rf_all$pos = !rf_all$pos
# 
# ggplot() +
#   geom_col(data=rf_all, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(16) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.pdf'))
# 
# 
# # rf_all_class = rbind(rf_NA[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')], 
# #                      rf_ipcc)
# # 
# # # why do we need this line?!
# # # rf_all_class$pos = !rf_all$pos
# # 
# # ggplot() +
# #   geom_col(data=rf_all_class, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
# #   scale_y_discrete(limits=rev) +
# #   scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
# #   theme_bw(12) +
# #   theme(legend.position = "none",
# #         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('forcing agent       time period (k years)') +
# #   facet_grid(type~., scales="free_y", space = "free")
# # ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# # ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# rf_all_class = rbind(data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')]), 
#                      rf_ipcc)
# 
# rf_annual_weighted = data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean_weighted', 'pos_weighted', 'type')])
# colnames(rf_annual_weighted) = c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')
# 
# rf_weighted_all_class = rbind(rf_annual_weighted, 
#                               rf_ipcc)
# 
# # why do we need this line?!
# # rf_all_class$pos = !rf_all$pos
# 
# ggplot() +
#   geom_col(data=rf_all_class, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~rf_kernel, scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# 
# 
# rf_annual_hadgem = subset(rf_annual, rf_kernel == 'rf_hadgem')
# 
# lo_mean_hadgem = floor(min(rf_annual_hadgem$forcing_mean, na.rm=TRUE))
# hi_mean_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean, na.rm=TRUE))
# 
# lo_weighted_hadgem = floor(min(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))
# 
# pdf(paste0('figures/RF_holocene_kernels_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# for (kernel in kernels){
#   
#   rf_annual_kernel = subset(rf_annual, rf_kernel == kernel)
#   
#   lo_mean_kernel = floor(min(rf_annual_kernel$forcing_mean, na.rm=TRUE))
#   hi_mean_kernel = ceiling(max(rf_annual_kernel$forcing_mean, na.rm=TRUE))
#   
#   lo_weighted_kernel = floor(min(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
#   hi_weighted_kernel = ceiling(max(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
#   
#   p = ggplot() +
#     geom_col(data=subset(rf_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#     # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#     scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('forcing agent       time period (k years)') +
#     labs(title = paste0(kernel, '; mean forcing')) +
#     facet_grid(type~., scales="free_y", space = "free") 
#   print(p)
#   # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
#   # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
#   
#   p = ggplot() +
#     geom_col(data=subset(rf_weighted_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#     # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#     scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('forcing agent       time period (k years)') +
#     labs(title = paste0(kernel, '; mean weighted forcing')) +
#     facet_grid(type~., scales="free_y", space = "free") 
#   print(p)
# }
# dev.off()
# 
# 
# 
# pdf(paste0('figures/RF_holocene_kernels_bar_landice_', alb_prod, '.pdf'), width=6, height=6)
# 
# 
# rf_annual_landice = subset(rf_annual, type == 'land + ice')
# 
# lo_mean_kernel = floor(min(rf_annual_landice$forcing_mean, na.rm=TRUE))
# hi_mean_kernel = ceiling(max(rf_annual_landice$forcing_mean, na.rm=TRUE))
# 
# lo_weighted_kernel = floor(min(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted_kernel = ceiling(max(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))
# 
# p = ggplot() +
#   geom_col(data=rf_annual_landice, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   labs(title = 'mean annual forcing') +
#   facet_grid(rf_kernel~., scales="free_y", space = "free") 
# print(p)
# # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# p = ggplot() +
#   geom_col(data=rf_annual_landice, aes(x = forcing_mean_weighted, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   labs(title = 'mean annual weighted forcing') +
#   facet_grid(rf_kernel~., scales="free_y", space = "free") 
# print(p)
# dev.off()
# 
# 
# ###############################################################################################################
# ## FULL: summarize rf at continental scale by land class
# ###############################################################################################################
# 
# # labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', 
# #                   '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
# # labels_period_flip = c('0.5 - 0.05 ka', '2 - 0.5 ka', '4 - 2 ka', 
# #                        '6 - 4 ka', '8 - 6 ka', '10 - 8 ka', '12 - 10 ka')
# 
# labels_period_full = data.frame(end_young = ages[match(alb_diff_full$year, ages)], 
#                                 end_old = ages[match(alb_diff_full$year, ages)+1])/1000
# labels_period_full$facets = paste(labels_period_full$end_young, labels_period_full$end_old, sep='-')
# labels_period_full = labels_period_full[!duplicated(labels_period_full), ]
# labels_period_full$period_id = seq(1, nrow(labels_period_full))
# 
# 
# alb_diff_merge_full$facets = labels_period_full[match(alb_diff_merge_full$year, labels_period_full$end_young*1000), 
#                                                 'facets']
# alb_diff_merge_full$period_id = labels_period_full[match(alb_diff_merge_full$year, labels_period_full$end_young*1000), 
#                                                    'period_id']
# 
# alb_diff_merge_full$facets = factor(alb_diff_merge_full$facets, levels = unique(labels_period_full$facets))
# 
# alb_diff_merge_full_keep = alb_diff_merge_full[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 
#                                                    'year', 'month', 'facets', 'period_mid', 'period_id',
#                                                    'rf_cack', 'rf_hadgem', 'rf_cam5', 'ice_old', 
#                                                    'ice_young')]
# alb_diff_merge_full_keep_ice = alb_diff_merge_full[, c('cell_id', 'lat', 'long', 'area', 'area_km2', 
#                                                        'year', 'month',  'facets', 'period_mid', 'period_id',
#                                                        'rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice', 'ice_old', 
#                                                        'ice_young')]
# 
# 
# rf_long = alb_diff_merge_full_keep %>%
#   pivot_longer(cols = c('rf_cack', 'rf_hadgem', 'rf_cam5'),
#                names_to = 'rf_kernel',
#                values_to = 'rf_value')
# rf_long = data.frame(rf_long)
# 
# ggplot() +
#   geom_histogram(data=rf_long, aes(x=rf_value, y=after_stat(density))) +
#   facet_grid(rf_kernel~month, scales = 'free_x')
# summary(rf_long$rf_value)
# hist(rf_long$rf_value)
# 
# breaks_rf = c(-150, -5, -2, 0, 2, 5, 150)
# rf_long$rf_value_bin = cut(rf_long$rf_value, breaks = breaks_rf, labels = FALSE)
# 
# rf_long_ice_filled = alb_diff_merge_full_keep_ice %>%
#   pivot_longer(cols = c('rf_cack_ice', 'rf_hadgem_ice', 'rf_cam5_ice'),
#                names_to = 'rf_kernel',
#                values_to = 'rf_value_ice')
# rf_long_ice_filled = data.frame(rf_long_ice_filled)
# rf_long_ice_filled$rf_kernel = substr(rf_long_ice_filled$rf_kernel, 1, 
#                                       nchar(rf_long_ice_filled$rf_kernel)-4)
# 
# rf_long = merge(rf_long, 
#                 rf_long_ice_filled[,c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
#                                       'facets', 'period_mid', 'period_id',
#                                       'rf_kernel', 'rf_value_ice')], 
#                 by = c('cell_id', 'lat', 'long', 'area', 'area_km2', 'year', 'month', 
#                        'period_mid', 'period_id', 'facets', 'rf_kernel'))
# 
# # rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
# #                'alb_diff'] = NA
# rf_long[which((!is.na(rf_long$ice_young))|(!is.na(rf_long$ice_old))), 
#         'rf_value'] = NA
# 
# length(unique(rf_long$cell_id))
# 
# # alb_diff$facets_flip = alb_diff$facets
# # alb_diff$facets_flip = factor(alb_diff$facets_flip, 
# #                               levels = labels_period, labels = labels_period_flip)
# 
# cell_id_ice = unique(rf_long[which((!is.na(rf_long$ice_young))|
#                                      (!is.na(rf_long$ice_old))), 'cell_id'])
# 
# length(unique(cell_id_ice))
# 
# cell_id_ice_free = unique(rf_long$cell_id[!(rf_long$cell_id %in% cell_id_ice)])
# length(cell_id_ice_free)
# 
# rf_long_ice = subset(rf_long, cell_id %in% cell_id_ice)
# rf_long_ice = subset(rf_long_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice$cell_id))
# 
# 
# rf_holocene_ice = rf_long_ice %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice = rf_holocene_ice %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice$type = rep("once ice")
# 
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean'] = NA
# rf_holocene_ice[which(rf_holocene_ice$cells < 5), 'forcing_mean_weighted'] = NA
# rf_holocene_ice
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice, aes(x = forcing_mean_weighted, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# rf_long_ice_filled = subset(rf_long, cell_id %in% cell_id_ice)
# # alb_diff_ice = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# length(unique(rf_long_ice_filled$cell_id))
# 
# rf_holocene_ice_filled = rf_long_ice_filled %>%
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE),
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE),
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id),
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_filled = rf_holocene_ice_filled %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_filled$type = rep("once ice filled")
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_filled, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# rf_long_ice_free = subset(rf_long, cell_id %in% cell_id_ice_free)
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene_ice_free = rf_long_ice_free %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE), 
#                    .groups = 'keep')
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_ice_free = rf_holocene_ice_free %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_ice_free$type = rep("never ice")
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_ice_free, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# rf_long_land = subset(rf_long, is.na(ice_young) & is.na(ice_old))
# # alb_diff_ice_free = subset(alb_diff_ice, is.na(ice_young) & is.na(ice_old))
# 
# rf_holocene = rf_long_land %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# 
# rf_holocene = rf_holocene %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene = rf_holocene %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene$type = rep("land")
# 
# ggplot() +
#   geom_col(data=rf_holocene, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# rf_holocene_continent = rf_long %>% 
#   group_by(facets, month, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE),
#                    .groups = 'keep')
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos = forcing_mean > 0)
# rf_holocene_continent = rf_holocene_continent %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_holocene_continent$type = rep("land + ice")
# 
# ggplot() +
#   geom_col(data=rf_holocene_continent, aes(x = forcing_mean, y = facets, fill = !pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# ggplot() +
#   geom_col(data=rf_holocene_continent, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(rf_kernel~month)
# 
# 
# rf_NA = rbind(rf_holocene, rf_holocene_ice)
# rf_NA = rbind(rf_NA, rf_holocene_ice_free)
# rf_NA = rbind(rf_NA, rf_holocene_continent)
# rf_NA$pos = factor(rf_NA$pos, levels = c(TRUE, FALSE))
# rf_NA$pos_weighted = factor(rf_NA$pos_weighted, levels = c(TRUE, FALSE))
# 
# rf_NA$type = factor(rf_NA$type, levels = c('land + ice', 'land', 'never ice', 'once ice'))
# 
# for (month in months){
#   rf_month = rf_NA[which(rf_NA$month == month),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     facet_grid(type~rf_kernel)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
#   ggsave(paste0('figures/RF_holocene_', month, '_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# }
# 
# kernels = unique(rf_NA$rf_kernel)
# 
# for (kernel in kernels){
#   rf_month = rf_NA[which(rf_NA$rf_kernel == kernel),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     facet_grid(type~month)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_full_', kernel, '_bar_landclass_facet_', alb_prod, '.png'), width=14, height=6)
#   ggsave(paste0('figures/RF_holocene_full_', kernel, '_bar_landclass_facet_', alb_prod, '.pdf'), width=14, height=6)
# }
# 
# # land + ice only, for each month
# for (kernel in kernels){
#   rf_month = rf_NA[which((rf_NA$rf_kernel == kernel) & (rf_NA$type == 'land + ice')),]
#   p = ggplot() +
#     geom_col(data=rf_month, aes(x = forcing_mean, y = facets, fill = pos)) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-1.8, 6.8)) +
#     theme_bw(14) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('time period (k years)') +
#     # facet_grid(month~.)
#     facet_wrap(~month, nrow=3, ncol=4)
#   print(p)
#   ggsave(paste0('figures/RF_holocene_full_', kernel, '_bar_landice_facet_', alb_prod, '.png'), width=12, height=8)
#   ggsave(paste0('figures/RF_holocene_full_', kernel, '_bar_landice_facet_', alb_prod, '.pdf'), width=12, height=8)
# }
# 
# rf_NA_wider = rf_NA[, c('facets', 'month', 'forcing_mean_weighted', 'rf_kernel', 'type')] %>% group_by(facets, month) %>% pivot_wider(names_from = rf_kernel, values_from = forcing_mean_weighted)
# 
# ggplot() +
#   geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_cam5), alpha=0.5) +
#   geom_point(data=rf_NA_wider, aes(x=rf_cack, y=rf_hadgem), colour='dodgerblue', alpha=0.5) +
#   facet_wrap(~month) +
#   geom_abline(slope = 1, intercept=0)
# 
# 
# 
# rf_annual = rf_NA %>% 
#   group_by(facets, rf_kernel, type) %>%
#   summarize(forcing_mean = mean(forcing_mean, na.rm=TRUE),
#             forcing_mean_weighted = mean(forcing_mean_weighted, na.rm=TRUE),
#             .groups='keep')
# 
# rf_annual = rf_annual %>%
#   mutate(pos = forcing_mean > 0)
# rf_annual = rf_annual %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# 
# rf_annual$pos = factor(rf_annual$pos, levels = c(TRUE, FALSE))
# rf_annual$pos_weighted = factor(rf_annual$pos_weighted, levels = c(TRUE, FALSE))
# 
# rf_annual$type = factor(rf_annual$type, levels = c('land + ice', 'land', 'never ice', 'once ice'))
# 
# rf_annual$forcing_mean
# 
# 
# lo_mean = floor(min(rf_annual$forcing_mean, na.rm=TRUE))
# hi_mean = ceiling(max(rf_annual$forcing_mean, na.rm=TRUE))
# 
# lo_weighted = floor(min(rf_annual$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted = ceiling(max(rf_annual$forcing_mean_weighted, na.rm=TRUE))
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_annual, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# ggsave(paste0('figures/RF_holocene_full_weighted_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_full_weighted_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_annual, aes(x = forcing_mean, y = facets, fill = pos_weighted)) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# ggsave(paste0('figures/RF_holocene_full_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_full_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# 
# 
# rf_annual$period_mid = (labels_period_full[match(rf_annual$facets, labels_period_full$facets), 'end_young'] + 
#                           labels_period_full[match(rf_annual$facets, labels_period_full$facets), 'end_old'])/2
# rf_annual$period_mid = as.numeric(rf_annual$period_mid)
# rf_annual$period_mid 
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_col(data=rf_annual, aes(x = period_mid, y = forcing_mean, fill = pos_weighted)) +
#   scale_x_reverse() +
#   # scale_y_discrete(limits=rev) +
#   # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   # theme_bw(12) +
#   # theme(legend.position = "none",
#   #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # xlab(bquote(radiative~forcing~(W/m^2))) +
#   # ylab('time periods (k years)') +
#   facet_grid(type~rf_kernel)
# # ggsave(paste0('figures/RF_holocene_full_bar_landclass_facet_', alb_prod, '.png'), width=10, height=6)
# # ggsave(paste0('figures/RF_holocene_full_bar_landclass_facet_', alb_prod, '.pdf'), width=10, height=6)
# 
# ggplot() +
#   # geom_col(data=rf_NA, aes(x = forcing_mean_weighted, y = facets, fill = pos_weighted)) +
#   geom_point(data=rf_annual, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
#   geom_line(data=rf_annual, aes(x = period_mid, y = forcing_mean, colour = rf_kernel)) +
#   scale_x_reverse() +
#   # scale_y_discrete(limits=rev) +
#   # # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_mean, hi_mean), limits = c(lo_mean, hi_mean)) +
#   # theme_bw(12) +
#   # theme(legend.position = "none",
#   #       panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # xlab(bquote(radiative~forcing~(W/m^2))) +
#   # ylab('time periods (k years)') +
#   facet_grid(type~.)
# ggsave(paste0('figures/RF_holocene_full_time_series_annual_', alb_prod, '.png'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_full_time_series_annual_', alb_prod, '.pdf'), width=10, height=6)
# 
# # ggplot() +
# #   geom_col(data=rf_NA, aes(x = forcing_mean, y = facets, fill = type), position='dodge') +
# #   scale_y_discrete(limits=rev) +
# #   scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
# #   theme_bw(14) +
# #   theme(panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82'),
# #         panel.grid.minor.x = element_line(linewidth = 0.3, color = 'grey82')) +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('time periods (k years)') +
# #   facet_grid(rf_kernel~.)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.png'), width=8, height=6)
# # ggsave(paste0('figures/RF_holocene_bar_landclass_colour_', alb_prod, '.pdf'), width=8, height=6)
# # 
# # ggplot() +
# #   geom_point(data=rf_NA, aes(x = forcing_mean, y = facets, colour = type)) +
# #   scale_y_discrete(limits=rev) +
# #   theme_bw(18) +
# #   # theme(legend.position = "none") +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('time periods (k years)') +
# #   facet_grid(rf_kernel~.)
# 
# # data frame of IPCC forcings
# rf_ipcc = data.frame(rf_kernel = NA,
#                      facets = c("carbon dioxide", "methane", "water vapour", "albedo (land use)", "aerosols"),
#                      forcing_mean = c(2.16, 0.54, 0.05, -0.20, -1.06),
#                      pos = c(TRUE, TRUE, TRUE, FALSE, FALSE),
#                      type = rep("modern", 5))
# ggplot() +
#   geom_col(data=rf_ipcc, aes(x = forcing_mean, y = facets, fill = pos)) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(18) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time periods (k years)')
# 
# 
# rf_all = rbind(rf_holocene[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')], 
#                rf_ipcc)
# 
# # why do we need this line?!
# rf_all$pos = !rf_all$pos
# 
# ggplot() +
#   geom_col(data=rf_all, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   theme_bw(16) +
#   theme(legend.position = "none") +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~., scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.png'))
# ggsave(paste0('figures/alb_preds_radiative_bar_both_', alb_prod, '.pdf'))
# 
# 
# # rf_all_class = rbind(rf_NA[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')], 
# #                      rf_ipcc)
# # 
# # # why do we need this line?!
# # # rf_all_class$pos = !rf_all$pos
# # 
# # ggplot() +
# #   geom_col(data=rf_all_class, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
# #   scale_y_discrete(limits=rev) +
# #   scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
# #   theme_bw(12) +
# #   theme(legend.position = "none",
# #         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('forcing agent       time period (k years)') +
# #   facet_grid(type~., scales="free_y", space = "free")
# # ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# # ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# rf_all_class = rbind(data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')]), 
#                      rf_ipcc)
# 
# rf_annual_weighted = data.frame(rf_annual[,c('rf_kernel', 'facets', 'forcing_mean_weighted', 'pos_weighted', 'type')])
# colnames(rf_annual_weighted) = c('rf_kernel', 'facets', 'forcing_mean', 'pos', 'type')
# 
# rf_weighted_all_class = rbind(rf_annual_weighted, 
#                               rf_ipcc)
# 
# # why do we need this line?!
# # rf_all_class$pos = !rf_all$pos
# 
# ggplot() +
#   geom_col(data=rf_all_class, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   facet_grid(type~rf_kernel, scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# ggsave(paste0('figures/alb_preds_radiative_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# 
# 
# rf_annual_hadgem = subset(rf_annual, rf_kernel == 'rf_hadgem')
# 
# lo_mean_hadgem = floor(min(rf_annual_hadgem$forcing_mean, na.rm=TRUE))
# hi_mean_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean, na.rm=TRUE))
# 
# lo_weighted_hadgem = floor(min(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted_hadgem = ceiling(max(rf_annual_hadgem$forcing_mean_weighted, na.rm=TRUE))
# 
# pdf(paste0('figures/RF_holocene_kernels_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# for (kernel in kernels){
#   
#   rf_annual_kernel = subset(rf_annual, rf_kernel == kernel)
#   
#   lo_mean_kernel = floor(min(rf_annual_kernel$forcing_mean, na.rm=TRUE))
#   hi_mean_kernel = ceiling(max(rf_annual_kernel$forcing_mean, na.rm=TRUE))
#   
#   lo_weighted_kernel = floor(min(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
#   hi_weighted_kernel = ceiling(max(rf_annual_kernel$forcing_mean_weighted, na.rm=TRUE))
#   
#   p = ggplot() +
#     geom_col(data=subset(rf_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#     # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#     scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('forcing agent       time period (k years)') +
#     labs(title = paste0(kernel, '; mean forcing')) +
#     facet_grid(type~., scales="free_y", space = "free") 
#   print(p)
#   # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
#   # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
#   
#   p = ggplot() +
#     geom_col(data=subset(rf_weighted_all_class, rf_kernel %in% c(kernel, NA)), aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#     scale_y_discrete(limits=rev) +
#     # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#     # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#     scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
#     theme_bw(12) +
#     theme(legend.position = "none",
#           panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#     xlab(bquote(radiative~forcing~(W/m^2))) +
#     ylab('forcing agent       time period (k years)') +
#     labs(title = paste0(kernel, '; mean weighted forcing')) +
#     facet_grid(type~., scales="free_y", space = "free") 
#   print(p)
# }
# dev.off()
# 
# 
# 
# pdf(paste0('figures/RF_holocene_kernels_bar_landice_', alb_prod, '.pdf'), width=6, height=6)
# 
# 
# rf_annual_landice = subset(rf_annual, type == 'land + ice')
# 
# lo_mean_kernel = floor(min(rf_annual_landice$forcing_mean, na.rm=TRUE))
# hi_mean_kernel = ceiling(max(rf_annual_landice$forcing_mean, na.rm=TRUE))
# 
# lo_weighted_kernel = floor(min(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))
# hi_weighted_kernel = ceiling(max(rf_annual_landice$forcing_mean_weighted, na.rm=TRUE))
# 
# p = ggplot() +
#   geom_col(data=rf_annual_landice, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   scale_x_continuous(breaks = seq(lo_mean_kernel, hi_mean_kernel), limits = c(lo_mean_kernel, hi_mean_kernel)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   labs(title = 'mean annual forcing') +
#   facet_grid(rf_kernel~., scales="free_y", space = "free") 
# print(p)
# # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.png'), width=6, height=6)
# # ggsave(paste0('figures/RF_holocene_', kernel, '_bar_landclass_both_', alb_prod, '.pdf'), width=6, height=6)
# 
# p = ggplot() +
#   geom_col(data=rf_annual_landice, aes(x = forcing_mean_weighted, y = facets, fill = pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7), limits = c(-2, 7)) +
#   # scale_x_continuous(breaks = seq(lo_weighted, hi_weighted), limits = c(lo_weighted, hi_weighted)) +
#   scale_x_continuous(breaks = seq(lo_weighted_kernel, hi_weighted_kernel), limits = c(lo_weighted_kernel, hi_weighted_kernel)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('forcing agent       time period (k years)') +
#   labs(title = 'mean annual weighted forcing') +
#   facet_grid(rf_kernel~., scales="free_y", space = "free") 
# print(p)
# dev.off()
# 
# 
# ###############################################################################################################
# ## summarize rf by region
# ###############################################################################################################
# 
# # alb_diff_merge_sub = subset(alb_diff_merge, )
# 
# ## CASE: ENA
# xlo = -100
# xhi = -52
# ylo = 48
# yhi = 67
# xlim = c(xlo, xhi)
# ylim = c(ylo, yhi)
# 
# boxes_ENA = data.frame(yhi = 67, ylo = 48, xlo = -100, xhi = -52, id="ECAN")
# boxes_ENA = transform(boxes_ENA, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)
# 
# rf_ENA = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))
# 
# # rf_summary_ENA = alb_diff_merge_ENA %>% 
# #   group_by(facets) %>%
# #   dplyr::summarize(forcing_total = sum(forcing), 
# #                    forcing_mean = mean(forcing), 
# #                    forcing_mean_weighted = sum(forcing * area) / sum(area),
# #                    cells = n_distinct(cell_id), 
# #                    total_area = sum(area_km2))
# 
# rf_monthly_ENA = rf_ENA %>% 
#   group_by(facets, rf_kernel, month) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# rf_monthly_ENA = rf_monthly_ENA %>%
#   mutate(pos = forcing_mean > 0)
# rf_monthly_ENA = rf_monthly_ENA %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_monthly_ENA$region = rep("ENA")
# 
# 
# rf_annual_ENA = rf_ENA %>% 
#   group_by(facets, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# rf_annual_ENA = rf_annual_ENA %>%
#   mutate(pos = forcing_mean > 0)
# rf_annual_ENA = rf_annual_ENA %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_annual_ENA$region = rep("ENA")
# 
# # CASE: HEMLOCK
# xlo = -90
# xhi = -60 
# ylo = 37
# yhi = 48
# xlim = c(xlo, xhi)
# ylim = c(ylo, yhi)
# 
# boxes_HEM = data.frame(yhi = 48, ylo = 37, xlo = -90, xhi = -60, id="NEUS/SEC")
# boxes_HEM = transform(boxes_HEM, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)
# 
# rf_HEM = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))
# 
# # rf_summary_HEM = alb_diff_merge_HEM %>% 
# #   group_by(facets) %>%
# #   dplyr::summarize(forcing_total = sum(forcing), 
# #                    forcing_mean = mean(forcing), 
# #                    forcing_mean_weighted = sum(forcing * area) / sum(area),
# #                    cells = n_distinct(cell_id), 
# #                    total_area = sum(area_km2))
# 
# rf_monthly_HEM = rf_HEM %>% 
#   group_by(facets, rf_kernel, month) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# 
# rf_monthly_HEM = rf_monthly_HEM %>%
#   mutate(pos = forcing_mean > 0)
# rf_monthly_HEM = rf_monthly_HEM %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_monthly_HEM$region = rep("HEM")
# 
# rf_annual_HEM = rf_HEM %>% 
#   group_by(facets, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# rf_annual_HEM = rf_annual_HEM %>%
#   mutate(pos = forcing_mean > 0)
# rf_annual_HEM = rf_annual_HEM %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_annual_HEM$region = rep("HEM")
# 
# # CASE: WCAN
# xlo = -165
# xhi = -105
# ylo = 45
# yhi = 75
# xlim = c(xlo, xhi)
# ylim = c(ylo, yhi)
# 
# xmid = -141
# 
# boxes_WCAN = data.frame(yhi = 75, ylo = 45, xlo = -165, xhi = -105, id="WCAN/AK")
# boxes_WCAN = transform(boxes_WCAN, laby=(yhi + ylo)/2, labx=(xhi + xlo)/2)
# 
# rf_WCAN = subset(rf_long, (long>xlo) & (long<xhi) & (lat>ylo) & (lat<yhi))
# 
# # rf_summary_WCAN = alb_diff_merge_WCAN %>% 
# #   group_by(facets) %>%
# #   dplyr::summarize(forcing_total = sum(forcing), 
# #                    forcing_mean = mean(forcing), 
# #                    forcing_mean_weighted = sum(forcing * area) / sum(area),
# #                    cells = n_distinct(cell_id), 
# #                    total_area = sum(area_km2))
# 
# rf_monthly_WCAN = rf_WCAN %>% 
#   group_by(facets, rf_kernel, month) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# rf_monthly_WCAN = rf_monthly_WCAN %>%
#   mutate(pos = forcing_mean > 0)
# rf_monthly_WCAN = rf_monthly_WCAN %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_monthly_WCAN$region = rep("WCAN")
# 
# rf_annual_WCAN = rf_WCAN %>% 
#   group_by(facets, rf_kernel) %>%
#   dplyr::summarize(forcing_total = sum(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean = mean(rf_value_ice, na.rm=TRUE), 
#                    forcing_mean_weighted = as.numeric(sum(rf_value_ice * area, na.rm=TRUE) / sum(area, na.rm=TRUE)),
#                    cells = n_distinct(cell_id), 
#                    total_area = sum(area_km2, na.rm=TRUE))
# 
# rf_annual_WCAN = rf_annual_WCAN %>%
#   mutate(pos = forcing_mean > 0)
# rf_annual_WCAN = rf_annual_WCAN %>%
#   mutate(pos_weighted = forcing_mean_weighted > 0)
# rf_annual_WCAN$region = rep("WCAN")
# 
# ###############################################################################################################
# ## show regions on a map
# ###############################################################################################################
# 
# 
# 
# ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="darkgrey", fill="grey", alpha=0.4) +
#   geom_rect(data=boxes_ENA, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
#             color="dodgerblue", fill="transparent", lwd=1) + 
#   geom_text(data=boxes_ENA, aes(x=labx, y=laby, label=id), color="dodgerblue", size=8) + 
#   geom_rect(data=boxes_HEM, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
#             color="darkorange", fill="transparent", lwd=1) + 
#   geom_text(data=boxes_HEM, aes(x=labx, y=laby, label=id), color="darkorange", size=8) + 
#   geom_rect(data=boxes_WCAN, aes(xmin=xhi, xmax=xlo, ymin=ylo, ymax=yhi), 
#             color="deeppink", fill="transparent", lwd=1) + 
#   geom_text(data=boxes_WCAN, aes(x=labx, y=laby, label=id), color="deeppink", size=8) + 
#   # geom_tile(data=alb_diff, aes(x=long, y=lat, fill = forcing)) +
#   #geom_point(data=alb_diff_sub, aes(x=long, y=lat, colour = forcing), size=1)+
#   # sc_colour_diverge + 
#   sc_fill_diverge + 
#   # scale_fill_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # scale_colour_gradient2(low = 'blue', high = 'red', mid= 'white',  limits = c(-0.1,0.1))+
#   # facet_grid(facets~.)+
#   #facet_wrap(~facets)+
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         strip.text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size=14),
#         legend.title = element_text(size=14)) +
#   coord_fixed()
# # coord_fixed(xlim=c(-165, -55), ylim = c(20, 80)) 
# # scale_fill_brewer(type = "div", palette = 'Rd
# ggsave(paste0('figures/map_case_study_regions.png'))
# ggsave(paste0('figures/map_case_study_regions.pdf'))
# 
# 
# ###############################################################################################################
# ## merge and plot region rf data frames
# ###############################################################################################################
# 
# # rf_region = rbind(rf_holocene,
# #                   rf_summary_ENA,
# #                   rf_summary_HEM,
# #                   rf_summary_WCAN)
# 
# rf_holocene_NA_landice = subset(rf_holocene_continent, type == 'land + ice')
# rf_holocene_NA_landice$region = rep('Continent')
# rf_holocene_NA_landice = rf_holocene_NA_landice[,colnames(rf_monthly_ENA)]
# 
# rf_monthly_region = rbind(rf_holocene_NA_landice,
#                           rf_monthly_ENA,
#                           rf_monthly_HEM,
#                           rf_monthly_WCAN)
# 
# rf_monthly_region$region = factor(rf_monthly_region$region, 
#                                   levels = c('Continent', 'ENA', 'HEM', 'WCAN'),
#                                   labels = c('Continental', 'ECAN', 'NEUS/SEC', 'WCAN/AK'))
# 
# rf_holocene_NA_annual_landice = subset(rf_annual, type == 'land + ice')
# rf_holocene_NA_annual_landice$region = rep('Continent')
# rf_holocene_NA_annual_landice = rf_holocene_NA_annual_landice[,c('facets', 'rf_kernel', 
#                                                                  'forcing_mean', 
#                                                                  'forcing_mean_weighted', 'pos', 
#                                                                  'pos_weighted', 'region')] 
# # rf_holocene_NA_annual_landice = rf_holocene_NA_annual_landice[,colnames(rf_annual_ENA)]
# 
# rf_annual_region = rbind(data.frame(rf_holocene_NA_annual_landice),
#                          rf_annual_ENA[,colnames(rf_holocene_NA_annual_landice)],
#                          rf_annual_HEM[,colnames(rf_holocene_NA_annual_landice)],
#                          rf_annual_WCAN[,colnames(rf_holocene_NA_annual_landice)])
# 
# rf_annual_region$region = factor(rf_annual_region$region, 
#                                  levels = c('Continent', 'ENA', 'HEM', 'WCAN'),
#                                  labels = c('Continental', 'ECAN', 'NEUS/SEC', 'WCAN/AK'))
# 
# rf_annual_region$pos = as.logical(rf_annual_region$pos)
# 
# # rf_region$facets
# # rf_region$pos
# 
# 
# # ggplot() +
# #   geom_col(data=bar, aes(x = forcing_mean, y = facets, fill = pos), width=0.9) +
# #   scale_y_discrete(limits=rev) +
# #   theme_bw(18) +
# #   theme(legend.position = "none") +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('forcing agent       time period (k years)') +
# #   facet_grid(type~., scales="free_y", space = "free")
# 
# ggplot() +
#   geom_col(data=subset(rf_monthly_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # theme(legend.position = "none",
#   #       # panel.grid.major = element_blank(),
#   #       # panel.grid.minor = element_blank(),
#   #       # panel.background = element_blank(),
#   #       strip.text = element_text(size=14),
#   #       axis.title = element_text(size=14),
#   #       axis.text = element_text(size=12)) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time period (k years)') +
#   facet_grid(region~month)#, scales="free_x")#, space = "free")
# ggsave(paste0('figures/RF_holocene_bar_regions_by_month_facet_', alb_prod, '.pdf'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_bar_regions_by_month_facet_', alb_prod, '.png'), width=10, height=6)
# 
# ggplot() +
#   geom_col(data=subset(rf_monthly_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # theme(legend.position = "none",
#   #       # panel.grid.major = element_blank(),
#   #       # panel.grid.minor = element_blank(),
#   #       # panel.background = element_blank(),
#   #       strip.text = element_text(size=14),
#   #       axis.title = element_text(size=14),
#   #       axis.text = element_text(size=12)) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time period (k years)') +
#   facet_grid(month~region, scales="free_x")#, space = "free")
# ggsave(paste0('figures/RF_holocene_bar_month_by_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_bar_month_by_regions_facet_', alb_prod, '.png'), width=10, height=6)
# 
# ggplot() +
#   geom_col(data=subset(rf_annual_region, rf_kernel == 'rf_hadgem'), aes(x = forcing_mean, y = facets, fill = !pos), width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # theme(legend.position = "none",
#   #       # panel.grid.major = element_blank(),
#   #       # panel.grid.minor = element_blank(),
#   #       # panel.background = element_blank(),
#   #       strip.text = element_text(size=14),
#   #       axis.title = element_text(size=14),
#   #       axis.text = element_text(size=12)) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time period (k years)') +
#   facet_grid(region~., scales="free_x")#, space = "free")
# ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.pdf'), width=10, height=6)
# ggsave(paste0('figures/RF_holocene_bar_annual_regions_facet_', alb_prod, '.png'), width=10, height=6)
# 
# 
# rf_annual_region$type = 'Holocene'
# rf_ipcc$region = 'Northern Hemisphere'
# 
# rf_annual_region_both = rbind(rf_annual_region[, colnames(rf_ipcc)], 
#                               rf_ipcc)
# 
# 
# rf_annual_region_both_hadgem = subset(rf_annual_region_both, rf_kernel %in% c('rf_hadgem', NA))
# 
# lo_mean_both = floor(min(rf_annual_region_both_hadgem$forcing_mean, na.rm=TRUE))
# hi_mean_both = ceiling(max(rf_annual_region_both_hadgem$forcing_mean, na.rm=TRUE))
# 
# # lo_weighted_both = floor(min(rf_annual_region_both$forcing_mean_weighted, na.rm=TRUE))
# # hi_weighted_both = ceiling(max(rf_annual_region_both$forcing_mean_weighted, na.rm=TRUE))
# 
# 
# ggplot(data=rf_annual_region_both_hadgem) +
#   geom_col(data=rf_annual_region_both_hadgem, aes(x = forcing_mean, y = facets, fill = !pos), 
#            width=0.9) +
#   scale_y_discrete(limits=rev) +
#   # scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
#   scale_x_continuous(breaks = seq(lo_mean_both, hi_mean_both), 
#                      limits = c(lo_mean_both, hi_mean_both), expand = c(0,0)) +
#   # scale_x_break(c(20,32)) +
#   # scale_x_break(c(20,30), space = 0.4, 
#   #               ticklabels = seq(lo_mean_both, hi_mean_both),
#   #               expand = c(0, 0)) +
#   theme_bw(12) +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
#   # theme(legend.position = "none",
#   #       # panel.grid.major = element_blank(),
#   #       # panel.grid.minor = element_blank(),
#   #       # panel.background = element_blank(),
#   #       strip.text = element_text(size=14),
#   #       axis.title = element_text(size=14),
#   #       axis.text = element_text(size=12)) +
#   xlab(bquote(radiative~forcing~(W/m^2))) +
#   ylab('time period (k years)') +
#   facet_grid(region~., scales="free_y", space = "free")
# ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.png'), height=6, width=6, dpi=300)
# ggsave(paste0('figures/alb_preds_radiative_bar_regions_facet_', alb_prod, '.pdf'), height=6, width=6, dpi=300)
# 
# # ggplot() +
# #   geom_col(data=rf_region, aes(x = forcing_mean_weighted, y = facets, fill = !pos_weighted), width=0.9) +
# #   scale_y_discrete(limits=rev) +
# #   scale_x_continuous(breaks = seq(-6, 12), limits = c(-6, 12)) +
# #   theme_bw(12) +
# #   theme(legend.position = "none",
# #         panel.grid.major.x = element_line(linewidth = 0.4, color = 'grey82')) +
# #   # theme(legend.position = "none",
# #   #       # panel.grid.major = element_blank(),
# #   #       # panel.grid.minor = element_blank(),
# #   #       # panel.background = element_blank(),
# #   #       strip.text = element_text(size=14),
# #   #       axis.title = element_text(size=14),
# #   #       axis.text = element_text(size=12)) +
# #   xlab(bquote(radiative~forcing~(W/m^2))) +
# #   ylab('time period (k years)') +
# #   facet_grid(type~., scales="free_y", space = "free")
# # ggsave(paste0('figures/alb_preds_radiative_weighted_bar_regions_facet_', alb_prod, '.png'), height=6, width=6, dpi=300)
# # ggsave(paste0('figures/alb_preds_radiative_weighted_bar_regions_facet_', alb_prod, '.pdf'), height=6, width=6, dpi=300)
# # 
# # 
# # ggplot() +
# #   geom_bar(data=rf_region, aes(x = facets, y = forcing_mean, fill = type), stat='identity', position='dodge') +
# #   # scale_y_discrete(limits=rev) +
# #   theme_bw(14) +
# #   # theme(legend.position = "none") +
# #   xlab('time period (k years)') +
# #   ylab(bquote(radiative~forcing~(W/m^2))) #+
# # # facet_grid(type~., scales="free_y", space = "free")
# # ggsave(paste0('figures/alb_preds_radiative_bar_regions_', alb_prod, '.png'))
# # ggsave(paste0('figures/alb_preds_radiative_bar_regions_', alb_prod, '.pdf'))
# # 
