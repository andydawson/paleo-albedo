library(sp)
library(raster)
library(dplyr)
library(terra)
library(sf)

alb_prod = "bluesky"

ages = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)

# ice_years = seq(1, 21)*1000

months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')


breaks_alb = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100

ylim = c(12, 82) 
xlim = c(-166, -50) 

proj_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

alb_interp_preds = readRDS(paste0('output/prediction/paleo_interp_predict_gam_summary_', alb_prod, '.RDS'))
# alb_interp_preds$year[which(alb_interp_preds$year == 11500)] = 12000


alb_interp_preds$alb_bin = cut(alb_interp_preds$alb_mean, breaks_alb, labels=FALSE)

###############################################################################################################
## ICE OLD
###############################################################################################################
# # ice = readRDS('data/map-data/ice/glacier_shapefiles_21-1k.RDS')
# ice = readRDS('data/Dalton_QSR_2020_Ice/paleo_albedo_ice_layers.RDS')
# albedo_age_dalton_ice_match = read.csv('data/albedo_age_dalton_ice_match.csv')
# 
# # ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))
# alb_interp_preds$ice = NA
# 
# # ages_cat_12 = c(ages, 12000)
# # ages_cat_12[which(ages == 11500)] = 12000
# ages[which(ages == 11500)] = 12000
# 
# alb_preds_ages = unique(alb_interp_preds$year)
# 
# for (i in 1:length(alb_preds_ages)){
#   
#   print(i)
#   idx_age = which(alb_interp_preds$year == alb_preds_ages[i])
#   
#   coords_veg  = SpatialPoints(alb_interp_preds[idx_age,c('x', 'y')], 
#                               proj4string=CRS(proj_WGS84))
#   
#   # proj4string(ice[[i]]) = proj_WGS84
#   # ice[[i]] = spTransform(ice[[i]], CRS(proj_WGS84))
#   # 
#   ice_status_veg = over(coords_veg, ice[[i]])
#   
#   alb_interp_preds[idx_age, 'ice'] = ice_status_veg
#   
#   # idx_ice_match = which.min(abs(alb_preds_ages[i] - ice_years)) 
#   # 
#   # proj4string(ice[[idx_ice_match]]) = proj_WGS84
#   # ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))
#   # 
#   # ice_status_veg = over(coords_veg, ice[[idx_ice_match]])
#   
#   # alb_interp_preds[idx_age, 'ice'] = ice_status_veg[,which(substr(colnames(ice_status_veg), 1,4)=='SYMB')]
#   
# }
# 
# # # drop cells that are not included for all time periods
# # cell_id_drop = as.numeric(names(which(table(alb_interp_preds$cell_id)<7)))
# # alb_diff_merge = alb_diff_merge[which(!(alb_diff_merge$cell_id %in% cell_id_drop)),]

###############################################################################################################
## ICE NEW
###############################################################################################################
# # ice = readRDS('data/map-data/ice/glacier_shapefiles_21-1k.RDS')
# ice = readRDS('data/Dalton_QSR_2020_Ice/paleo_albedo_ice_layers.RDS')
# albedo_age_dalton_ice_match = read.csv('data/albedo_age_dalton_ice_match.csv')

ice_dalton_interp = rast('data/Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif')
ice_dalton_years = as.numeric(sapply(names(ice_dalton_interp), function(x) strsplit(x, 'yr|bp')[[1]][2]))

# ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))
alb_interp_preds$ice_frac = NA

# ages_cat_12 = c(ages, 12000)
# ages_cat_12[which(ages == 11500)] = 12000
# ages[which(ages == 11500)] = 12000

alb_preds_ages = unique(alb_interp_preds$year)

for (i in 1:length(alb_preds_ages)){
  
  print(paste0('Albedo year ', alb_preds_ages[i], ' YBP'))
  idx_age = which(alb_interp_preds$year == alb_preds_ages[i])
  
  coords_veg  = SpatialPoints(alb_interp_preds[idx_age,c('x', 'y')], 
                              proj4string=CRS(proj_WGS84))
  
  # proj4string(ice[[i]]) = proj_WGS84
  # ice[[i]] = spTransform(ice[[i]], CRS(proj_WGS84))
  # 
  # ice_status_veg = over(coords_veg, ice_dalton_interp[[i]])
  
  
  idx_ice = match(alb_preds_ages[i], ice_dalton_years)
  ice_layer = project(ice_dalton_interp[[idx_ice]], proj_WGS84)
  
  coords_veg_spat = vect(coords_veg)
  coords_veg_spat_proj = project(coords_veg_spat, crs(ice_layer))
  
  ice_status_veg = terra::extract(ice_layer, coords_veg_spat_proj)
  
  hist(ice_status_veg[,2])
  
  alb_interp_preds[idx_age, 'ice_frac'] = ice_status_veg[,2]
  
  # idx_ice_match = which.min(abs(alb_preds_ages[i] - ice_years)) 
  # 
  # proj4string(ice[[idx_ice_match]]) = proj_WGS84
  # ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))
  # 
  # ice_status_veg = over(coords_veg, ice[[idx_ice_match]])
  
  # alb_interp_preds[idx_age, 'ice'] = ice_status_veg[,which(substr(colnames(ice_status_veg), 1,4)=='SYMB')]
  
}

# 
# plot(alb_interp_preds$x, alb_interp_preds$y)
# ice_missing = alb_interp_preds[which(is.na(alb_interp_preds$ice)), ]
# points(ice_missing$x, ice_missing$y, col='red')


# not sure about this, leave for now
alb_interp_preds = alb_interp_preds[which(alb_interp_preds$x > -170),]

# alb_interp_preds = alb_interp_preds[which(!is.na(alb_interp_preds$ice)), ]

# foo = alb_interp_preds[which(is.na(alb_interp_preds$ice)), ]
# 
# 
# p<-ggplot()+
#   geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
#   geom_tile(data=foo, aes(x=x, y=y, fill=ice)) 
# print(p)

# alb_interp_preds$ice[which(alb_interp_preds$ice == 0)] = NA

# alb_interp_preds$ice[which(alb_interp_preds$ice >0)]

# # drop cells that are not included for all time periods
# cell_id_drop = as.numeric(names(which(table(alb_interp_preds$cell_id)<7)))
# alb_diff_merge = alb_diff_merge[which(!(alb_diff_merge$cell_id %in% cell_id_drop)),]

###############################################################################################################
## 
###############################################################################################################

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

labels = seq(1, length(breaks_alb)-1)

labels_ages_full = signif(data.frame(age = ages)/1000, 3)
labels_ages_full$facets = paste(labels_ages_full$age)
# labels_ages_full$period_id = seq(1, nrow(labels_ages_full))


labels_period_full = data.frame(end_young = ages[1:(length(ages)-1)]/1000, 
                                end_old = ages[2:length(ages)]/1000)
labels_period_full$facets = paste(labels_period_full$end_young, labels_period_full$end_old, sep='-')
# labels_period_full = labels_period_full[!duplicated(labels_period_full), ]
labels_period_full$period_id = seq(1, nrow(labels_period_full))


# labels_period_full = data.frame(end_young = ages[match(alb_diff_full$year, ages)], 
#                                 end_old = ages[match(alb_diff_full$year, ages)+1])/1000
# labels_period_full$facets = paste(labels_period_full$end_young, labels_period_full$end_old, sep='-')
# labels_period_full = labels_period_full[!duplicated(labels_period_full), ]
# labels_period_full$period_id = seq(1, nrow(labels_period_full))

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

years = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)
#years = c(50, 1000, 3000, 5000, 7000, 9000, 11000)
# years = c(50, 6000, 11000)

# breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100
# labels = c("0 - 4", "4 - 8", "8 - 12", "12 - 16", "16 - 20", "20 - 40", "40 - 60",  "60 - 80", "80 - 100")

breaks_alb = c(0, 5, 10, 20, 30, 40, 60, 80, 100)/100
labels_alb = c("0 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 60", "60 - 80", "80 - 100")

grid_NA <- readRDS("data/grid.RDS")
grid_df = as.data.frame(grid_NA, xy=TRUE)

cell_id <- raster::extract(grid_NA, alb_interp_preds[,c('x', 'y')])

alb_grid <- data.frame(cell_id, alb_interp_preds)
coords   = xyFromCell(grid_NA, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords, 
                 alb_grid[,c('x', 'y', 'cell_id', 'year', 'ice_frac', 'alb_mean', 'alb_sd', 'month')])



spdf_2 <- as(grid_NA,'SpatialPolygonsDataFrame')

sf_data = st_as_sf(spdf_2)
grid_df$area <- st_area(sf_data) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')

alb_grid = merge(alb_grid, grid_df, by = c('cell_id', 'lat', 'long'))





alb_grid_full = alb_grid




alb_glacier = read.csv('data/albedo_glacier_monthly.csv', header=TRUE)

# alb_grid_full$ice[which(alb_grid_full$ice == 1)] = 'ICE'

# alb_grid_full$alb_mean_ice = NA
# alb_grid_full$alb_mean_parts = NA
# alb_grid_full$alb_mean_ice[which(is.na(alb_grid_full$ice))] = alb_grid_full$alb_mean[which(is.na(alb_grid_full$ice))]

# weighted average based on fraction of grid cell ice cover
# idx_part_ice = which(alb_grid_full$ice > 0)

# assign monthly glacier albedo
alb_grid_full$ice_albedo = alb_glacier[match(alb_grid_full$month, alb_glacier$month), 'ice_albedo']

# assigned monthly glacier albedo multiplied by the fraction of ice
alb_grid_full$alb_ice_part = alb_grid_full$ice_albedo * alb_grid_full$ice_frac 

# inferred monthly veg albedo mutiplied by the fraction of veg (1 - fraction of ice)
alb_grid_full$alb_land_part = alb_grid_full$alb_mean * (1 - alb_grid_full$ice_frac)

# weighted albedo based on ice and land albedo parts
alb_grid_full$alb_veg_ice_parts = alb_grid_full$alb_ice_part + alb_grid_full$alb_land_part 

# ggplot(data=subset(alb_grid_full, month=='jan')) +
#   geom_point(aes(x=alb_mean, y=alb_veg_ice_parts, colour=ice))


idx_ice = which(alb_grid_full$ice > 0.5)
idx_veg = which(alb_grid_full$ice <= 0.5)

alb_grid_full$alb_veg_thresh = alb_grid_full$alb_mean
alb_grid_full$alb_veg_thresh[idx_ice] = NA

alb_grid_full$alb_veg_ice_thresh = alb_grid_full$alb_mean
alb_grid_full$alb_veg_ice_thresh[idx_ice] = alb_grid_full$ice_albedo[idx_ice]

alb_grid_full$alb_ice_thresh = alb_grid_full$ice_albedo
alb_grid_full$alb_ice_thresh[idx_veg] = NA

alb_grid_full_cells = alb_grid_full %>%
  group_by(year) %>%
  dplyr::summarize(N_all = n_distinct(cell_id))

# alb_grid_full_cells = alb_grid_full %>%
#   dplyr::group_by(month) %>%
#   summarize(N_all = n_distinct(cell_id))

cell_count_summary = alb_grid_full[,c('cell_id', 'month', 'year')] %>%
  group_by(cell_id, month) %>%
  #mutate(cell_count = n()) #%>%
  dplyr::summarize(cell_count = n()) #%>%
  # pivot_wider(id_cols = cell_id, names_from = month, values_from = cell_count)


cell_id_missing = unique(cell_count_summary[which(cell_count_summary$cell_count < 25), 'cell_id'])
cell_id_missing$lat = alb_grid_full[match(cell_id_missing$cell_id, alb_grid_full$cell_id),'lat']

N_missing = nrow(cell_id_missing)
N_missing_south = length(which(cell_id_missing$lat<27))
N_missing_north = length(which(cell_id_missing$lat>74))
N_missing_south + N_missing_north

alb_grid_full_new = alb_grid_full[which(!(alb_grid_full$cell_id %in% cell_id_missing$cell_id)),] 
cell_count_summary_new = alb_grid_full_new[,c('cell_id', 'month', 'year')] %>%
  group_by(cell_id, month) %>%
  #mutate(cell_count = n()) #%>%
  dplyr::summarize(cell_count = n()) #%>%
# pivot_wider(id_cols = cell_id, names_from = month, values_from = cell_count)
cell_id_missing_new = unique(cell_count_summary_new[which(cell_count_summary_new$cell_count < 25), 'cell_id'])

alb_grid_full$alb_bin = cut(alb_grid_full$alb_mean, breaks_alb, labels=FALSE)

breaks_sd = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
labels_sd = c("0 - 0.01", "0.01 - 0.02", "0.02 - 0.03", "0.03 - 0.04", "0.04 - 0.05", "0.05 - 0.06", "0.06 - 0.07")
alb_grid_full$alb_sd_bin = cut(alb_grid_full$alb_sd, breaks_sd, labels=FALSE, include.lowest=TRUE)
alb_grid_full$alb_sd_bin = factor(alb_grid_full$alb_sd_bin, 
                                 levels=seq(1, length(labels_sd)),
                                 labels = labels_sd)

alb_grid_full$alb_cv = alb_grid_full$alb_sd / alb_grid_full$alb_mean
breaks_cv = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 10)
labels_cv = c("0 - 0.05", "0.05 - 0.1", "0.1 - 0.15", "0.15 - 0.2", "0.2 - 0.25", "0.25 - 0.3", "0.3 - 10")
alb_grid_full$alb_cv_bin = cut(alb_grid_full$alb_cv, breaks_cv, labels=FALSE, include.lowest=TRUE)
alb_grid_full$alb_cv_bin = factor(alb_grid_full$alb_cv_bin, 
                                 levels=seq(1, length(labels_cv)),
                                 labels = labels_cv)
# alb_grid_full = subset(alb_grid, year %in% years) 

labels_year = c('0.05 ka', '0.5 ka', '2 ka', '4 ka', '6 ka', '8 ka', '10 ka', '12 ka')
#labels = c('0.05', '1', '3', '5', '7', '9', '11')


#renaming labels to create facets column in alb_grid_full 
alb_grid_full$facets = labels_ages_full[match(alb_grid_full$year/1000, labels_ages_full$age), 'facets']
alb_grid_full$facets = factor(alb_grid_full$facets, levels = labels_ages_full$facets)
alb_grid_full$month = factor(alb_grid_full$month, levels = months)

###############################################################################################################
## plot albedo prediction differences
###############################################################################################################

years_df = data.frame(year = ages)

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_full)+8))
alb_diff_colnames = colnames(alb_grid_full)[which(!(colnames(alb_grid_full) %in% 'ice'))]

colnames(alb_diff_df) = c(alb_diff_colnames, 
                          # 'ice_albedo',
                          'ice_frac_young', 
                          'ice_frac_old', 
                          'alb_diff_veg_thresh',
                          'alb_diff_ice_thresh',
                          'alb_diff_veg_ice_thresh',
                          'alb_diff_veg_ice_parts',
                          'alb_diff_veg_part',
                          'alb_diff_ice_part')

# alb_grid_full$alb_mean_ice = alb_grid_full$alb_mean
# alb_grid_full$alb_mean_ice[which(!is.na(alb_grid_full$ice))] = 0.5

cell_ids = unique(alb_grid_full$cell_id)
N_cells  = length(cell_ids)
for (i in 1:N_cells){
  
  print(paste0('Cell ', i, ' of ', N_cells))
  
  for (month in months){
    
    # print(paste0('>> ', month))
    
    alb_cell = alb_grid_full[which((alb_grid_full$cell_id == cell_ids[i])&(alb_grid_full$month == month)),] 
    alb_cell = alb_cell[order(alb_cell$year),]
    # alb_cell = alb_cell[which(is.na(alb_cell$ice)),]
    
    if (nrow(alb_cell) == 1){
      next
    } 
    
    alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
    
    # colnames(alb_diff_df) = c(alb_diff_colnames, 
                              # 'ice_albedo',
    #                           'ice_frac_young', 
    #                           'ice_frac_old', 
    #                           'alb_diff_veg_thresh',
    #                           'alb_diff_ice_thresh',
    #                           'alb_diff_veg_ice_thresh',
    #                           'alb_diff_veg_ice_parts',
    #                           'alb_diff_veg_part',
    #                           'alb_diff_ice_part')
    
    alb_diff_df = rbind(alb_diff_df, 
                        data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
                                          select=-c(ice)), 
                                   alb_ice = alb_cell_filled[2:nrow(alb_cell_filled), 'alb_ice'],
                                   # alb_mean_ice_young = alb_cell_filled[1:(nrow(alb_cell_filled)-1), 'alb_mean_ice'],
                                   ice_frac_young = alb_cell_filled$ice[1:(nrow(alb_cell_filled)-1)],
                                   ice_frac_old = alb_cell_filled$ice[2:nrow(alb_cell_filled)],
                                   alb_diff_veg_thresh = -diff(alb_cell_filled$alb_mean),
                                   alb_diff_ice_thresh = -diff(alb_cell_filled$alb_ice_thresh),
                                   alb_diff_veg_ice_thresh = -diff(alb_cell_filled$alb_veg_ice_thresh),
                                   alb_diff_veg_ice_parts = -diff(alb_cell_filled$alb_veg_ice_parts),
                                   alb_diff_veg_ice = -diff(alb_cell_filled$alb_veg_ice)))
    
    # alb_diff_df = rbind(alb_diff_df, 
    #                     data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
    #                                       select=-c(ice)), 
    #                                alb_ice = alb_cell_filled[2:nrow(alb_cell_filled), 'alb_ice'],
    #                                # alb_mean_ice_young = alb_cell_filled[1:(nrow(alb_cell_filled)-1), 'alb_mean_ice'],
    #                                ice_frac_young = alb_cell_filled$ice[1:(nrow(alb_cell_filled)-1)],
    #                                ice_frac_old = alb_cell_filled$ice[2:nrow(alb_cell_filled)],
    #                                alb_diff_veg = -diff(alb_cell_filled$alb_mean),
    #                                alb_diff_veg_ice_parts = -diff(alb_cell_filled$alb_veg_ice_parts),
    #                                alb_diff_veg_ice = -diff(alb_cell_filled$alb_veg_ice)))
  }
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]

saveRDS(alb_diff_df, paste0('data/alb_interp_preds_full_diffs_', alb_prod, '.RDS'))

###############################################################################################################
## plot albedo prediction differences
###############################################################################################################

years_df = data.frame(year = ages)

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_full)+6))
alb_diff_colnames = colnames(alb_grid_full)[which(!(colnames(alb_grid_full) %in% 'ice'))]

colnames(alb_diff_df) = c(alb_diff_colnames, 
                          'ice_frac_young', 
                          'ice_frac_old', 
                          'alb_diff_veg',
                          'alb_diff_veg_ice_parts',
                          'alb_diff_veg_ice')


cell_ids = unique(alb_grid_full$cell_id)
N_cells  = length(cell_ids)
for (i in 1:N_cells){
  
  print(paste0('Cell ', i, ' of ', N_cells))
  
  for (month in months){
    
    # print(paste0('>> ', month))
    
    alb_cell = alb_grid_full[which((alb_grid_full$cell_id == cell_ids[i])&(alb_grid_full$month == month)),] 
    alb_cell = alb_cell[order(alb_cell$year),]
    # alb_cell = alb_cell[which(is.na(alb_cell$ice)),]
    
    if (nrow(alb_cell) == 1){
      next
    } 
    
    alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
    
    alb_diff_df = rbind(alb_diff_df, 
                        data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
                                          select=-c(ice)), 
                                   # alb_ice = alb_cell_filled[2:nrow(alb_cell_filled), 'alb_ice'],
                                   # alb_mean_ice_young = alb_cell_filled[1:(nrow(alb_cell_filled)-1), 'alb_mean_ice'],
                                   ice_frac_young = alb_cell_filled$ice[1:(nrow(alb_cell_filled)-1)],
                                   ice_frac_old = alb_cell_filled$ice[2:nrow(alb_cell_filled)],
                                   alb_diff_veg = -diff(alb_cell_filled$alb_mean),
                                   alb_diff_veg_ice_parts = -diff(alb_cell_filled$alb_veg_ice_parts),
                                   alb_diff_veg_ice = -diff(alb_cell_filled$alb_veg_ice)))
  }
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]

saveRDS(alb_diff_df, paste0('data/alb_interp_preds_full_diffs_', alb_prod, '.RDS'))


# ###############################################################################################################
# ## albedo differences for subset of times
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
#                                 end_old = ages_ice_res[2:length(ages_ice_res)]/1000)
# labels_period_ice$facets = paste(labels_period_ice$end_young, labels_period_ice$end_old, sep='-')
# labels_period_ice$period_id = seq(1, nrow(labels_period_ice))
# 
# alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_full)+5))
# alb_diff_colnames = colnames(alb_grid_full)[which(!(colnames(alb_grid_full) %in% 'ice'))]
# 
# colnames(alb_diff_df) = c(alb_diff_colnames, 'alb_mean_ice_old', 'alb_mean_ice_young', 
#                           'ice_young', 'ice_old', 
#                           'alb_diff', 'alb_diff_ice')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')
# 
# alb_grid_sub = alb_grid_full[which(alb_grid_full$year %in% years_df$year),]
# 
# cell_ids = unique(alb_grid_full$cell_id)
# N_cells  = length(cell_ids)
# for (i in 1:N_cells){
#   
#   print(paste0('Cell ', i, ' of ', N_cells))
#   
#   for (month in months){
#     
#     # print(paste0('>> ', month))
#     
#     alb_cell = alb_grid_sub[which((alb_grid_sub$cell_id == cell_ids[i])&(alb_grid_sub$month == month)),] 
#     alb_cell = alb_cell[order(alb_cell$year),]
#     # alb_cell = alb_cell[which(is.na(alb_cell$ice)),]
#     
#     if (nrow(alb_cell) == 1){
#       next
#     } 
#     
#     alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)
#     
#     alb_diff_df = rbind(alb_diff_df, 
#                         data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ], 
#                                           select=-c(ice)), 
#                                    alb_mean_ice_old = alb_cell_filled[2:nrow(alb_cell_filled), 'alb_mean_ice'],
#                                    alb_mean_ice_young = alb_cell_filled[1:(nrow(alb_cell_filled)-1), 'alb_mean_ice'],
#                                    ice_young = alb_cell_filled$ice[1:(nrow(alb_cell_filled)-1)],
#                                    ice_old = alb_cell_filled$ice[2:nrow(alb_cell_filled)],
#                                    alb_diff = -diff(alb_cell_filled$alb_mean),
#                                    alb_diff_ice = -diff(alb_cell_filled$alb_mean_ice)))
#   }
# }
# 
# alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]
# 
# alb_diff_df$facets = labels_period_ice$facets[match(alb_diff_df$year/1000, labels_period_ice$end_young)]
# 
# saveRDS(alb_diff_df, paste0('data/alb_interp_preds_ice_res_diffs_', alb_prod, '.RDS'))
