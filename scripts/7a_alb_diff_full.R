# 7a_alb_diff_full.R: albedo change between consecutive slices, split into vegetation and
# ice parts, for every cell and month
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.
# The per-pair arithmetic is pair_albedo_differences() in R/differences.R.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R (it creates the product
# directories) and starts the run's manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('7a_alb_diff_full')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(sp)
library(raster)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)

# The time slices to pair, from config.R (analysis$ages); they must be slices
# of the prediction table, which is checked once it is read.
ages = cfg$analysis$ages

# The months analysed, from config.R (analysis$months); month names and
# numbers come from the shared calendar, R/calendar.R.
source('R/calendar.R')
months = cfg$analysis$months

proj_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

alb_interp_preds = read_input(run, path_file(paths, 'prediction_summary'))

# CM: every configured slice must exist in the data, or the differences below would be
# taken across a missing slice.
missing_slices = setdiff(ages, alb_interp_preds$year)
if (length(missing_slices)) {
  stop('config.R analysis$ages has slice(s) not in the prediction table: ', paste(missing_slices, collapse = ', '),
       '; the table has ', paste(sort(unique(alb_interp_preds$year)), collapse = ', '))
}

ice_dalton_interp = read_input(run, path_file(paths, 'dalton_ice'), rast)
ice_dalton_years = as.numeric(sapply(names(ice_dalton_interp), function(layer_name) strsplit(layer_name, 'yr|bp')[[1]][2]))

alb_interp_preds$ice_frac = NA

alb_preds_ages = unique(alb_interp_preds$year)

for (i in 1:length(alb_preds_ages)){

  print(paste0('Albedo year ', alb_preds_ages[i], ' YBP'))
  idx_age = which(alb_interp_preds$year == alb_preds_ages[i])

  coords_veg  = SpatialPoints(alb_interp_preds[idx_age,c('x', 'y')],
                              proj4string=CRS(proj_WGS84))

  ice_layer_index = match(alb_preds_ages[i], ice_dalton_years)
  ice_layer = project(ice_dalton_interp[[ice_layer_index]], proj_WGS84)

  coords_veg_spat = vect(coords_veg)
  coords_veg_spat_proj = project(coords_veg_spat, crs(ice_layer))

  ice_status_veg = terra::extract(ice_layer, coords_veg_spat_proj)

  hist(ice_status_veg[,2])

  alb_interp_preds[idx_age, 'ice_frac'] = ice_status_veg[,2]

}

# Cells west of config.R analysis$domain$lon_min are dropped.
alb_interp_preds = alb_interp_preds[which(alb_interp_preds$x > cfg$analysis$domain$lon_min),]

alb_interp_preds = alb_interp_preds[which(!is.na(alb_interp_preds$ice_frac)),]




grid_NA <- read_input(run, path_file(paths, 'grid'))
grid_df = as.data.frame(grid_NA, xy=TRUE)

cell_id <- raster::extract(grid_NA, alb_interp_preds[,c('x', 'y')])

alb_grid <- data.frame(cell_id, alb_interp_preds)
coords   = xyFromCell(grid_NA, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords,
                 alb_grid[,c('x', 'y', 'cell_id', 'year', 'ice_frac', 'alb_mean', 'alb_sd', 'month')])

grid_polygons_sp <- as(grid_NA,'SpatialPolygonsDataFrame')

grid_polygons_sf = st_as_sf(grid_polygons_sp)
grid_df$area <- st_area(grid_polygons_sf) #area of each "square"
grid_df$area_km2 <- units::set_units(grid_df$area, "km^2") # in km2
colnames(grid_df) = c('long', 'lat', 'cell_id', 'area', 'area_km2')

alb_grid = merge(alb_grid, grid_df, by = c('cell_id', 'lat', 'long'))

alb_grid_full = alb_grid

alb_glacier = read_input(run, path_file(paths, 'glacier_albedo'), read.csv, header=TRUE)

# The glacier-albedo columns for the fixed and scaled ice variants: config.R
# analysis$ice$albedo_fixed and albedo_scaled (question C0a).
alb_grid_full$ice_albedo_fixed = alb_glacier[match(alb_grid_full$month, alb_glacier$month), cfg$analysis$ice$albedo_fixed]
alb_grid_full$ice_albedo_sc = alb_glacier[match(alb_grid_full$month, alb_glacier$month), cfg$analysis$ice$albedo_scaled]

alb_grid_full$alb_veg = alb_grid_full$alb_mean

alb_grid_full$alb_ice_part = alb_grid_full$ice_albedo_fixed * alb_grid_full$ice_frac
alb_grid_full$alb_icesc_part = alb_grid_full$ice_albedo_sc * alb_grid_full$ice_frac

alb_grid_full$alb_veg_part = alb_grid_full$alb_veg * (1 - alb_grid_full$ice_frac)

alb_grid_full$alb_veg_ice_parts = alb_grid_full$alb_ice_part + alb_grid_full$alb_veg_part
alb_grid_full$alb_veg_icesc_parts = alb_grid_full$alb_icesc_part + alb_grid_full$alb_veg_part

# A cell is ice when its ice fraction exceeds config.R analysis$ice$threshold (questions
# C0, C2).
ice_threshold = cfg$analysis$ice$threshold
source('R/differences.R')   # pair_albedo_differences(), the per-pair arithmetic
# Per-slice ice/vegetation albedo by the UNADJUSTED ice fraction. Only the
# "legacy_components" method in pair_albedo_differences() reads alb_ice_thresh and
# alb_icesc_thresh; its readvance adjustment can disagree with this classification
# (see R/differences.R).
ice_rows = which(alb_grid_full$ice_frac > ice_threshold)
vegetation_rows = which(alb_grid_full$ice_frac <= ice_threshold)

alb_grid_full$alb_veg_thresh = alb_grid_full$alb_veg
alb_grid_full$alb_veg_thresh[ice_rows] = NA

alb_grid_full$alb_ice_thresh = alb_grid_full$ice_albedo_fixed
alb_grid_full$alb_ice_thresh[vegetation_rows] = NA

alb_grid_full$alb_icesc_thresh = alb_grid_full$ice_albedo_sc
alb_grid_full$alb_icesc_thresh[vegetation_rows] = NA

alb_grid_full$alb_veg_ice_thresh = rowSums(cbind(alb_grid_full$alb_veg_thresh, alb_grid_full$alb_ice_thresh), na.rm=TRUE)
alb_grid_full$alb_veg_icesc_thresh = rowSums(cbind(alb_grid_full$alb_veg_thresh, alb_grid_full$alb_icesc_thresh), na.rm=TRUE)

alb_grid_full$month = factor(alb_grid_full$month, levels = months)

years_df = data.frame(year = ages)

alb_diff_colnames = c('cell_id', 'lat', 'long', 'x', 'y', 'year', 'month', 'area', 'area_km2')

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=length(alb_diff_colnames)+12))

colnames(alb_diff_df) = c(alb_diff_colnames,
                          'ice_frac_young',
                          'ice_frac_old',
                          'alb_diff_veg_thresh',
                          'alb_diff_ice_thresh',
                          'alb_diff_veg_ice_thresh',
                          'alb_diff_icesc_thresh',
                          'alb_diff_veg_icesc_thresh',
                          'alb_diff_veg_part',
                          'alb_diff_ice_part',
                          'alb_diff_veg_ice_parts',
                          'alb_diff_icesc_part',
                          'alb_diff_veg_icesc_parts')

cell_ids = unique(alb_grid_full$cell_id)
N_cells  = length(cell_ids)

for (i in 1:N_cells){

  print(paste0('Cell ', i, ' of ', N_cells))

  for (month in months){

    alb_cell = alb_grid_full[which((alb_grid_full$cell_id == cell_ids[i])&(alb_grid_full$month == month)),]
    alb_cell = alb_cell[order(alb_cell$year),]

    if (nrow(alb_cell) == 1){
      next
    }

    alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)

    # The albedo differences of each consecutive pair of slices (R/differences.R).
    alb_diff_df = rbind(alb_diff_df,
                        data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ],
                                          select=alb_diff_colnames),
                                   pair_albedo_differences(alb_cell_filled, ice_threshold,
                                                           cfg$analysis$ice$difference_method)))

  }
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]

# One row per cell, month and pair of consecutive slices (year is the younger slice); the
# albedo differences are younger minus older (R/differences.R).
write_output(run, alb_diff_df, path_file(paths, 'albedo_differences'))

higher_young_ice_fraction = alb_diff_df[which(alb_diff_df$ice_frac_young > alb_diff_df$ice_frac_old),]

ggplot(data=higher_young_ice_fraction) +
  geom_point(aes(x=ice_frac_old, y=ice_frac_young, size=year, colour=y))

ggplot(data=higher_young_ice_fraction) +
  geom_point(aes(x=ice_frac_old-ice_frac_young, y=year))

run_finish(run)
