# 8_radiative.R: turn albedo change into radiative forcing
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(sp)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)

# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R (issue I07; it creates the product
# directories) and starts the run's manifest; files are then read and written through it
# (issue I05).
source('R/run_manifest.R')
run   = run_begin('8_radiative')
cfg   = run$cfg
paths = run$paths
alb_prod = paths$tag   # the albedo product in file names

# The months analysed, from config.R (analysis$months, issue I04b); month names and
# numbers come from the shared calendar, R/calendar.R (issue I02).
source('R/calendar.R')
months = cfg$analysis$months

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


ice_fort = read_input(run, path_file(paths, 'ice_fort'))
ice_fort_diff_young = read_input(run, path_file(paths, 'ice_fort_diff_young'))
ice_fort_diff_old = read_input(run, path_file(paths, 'ice_fort_diff_old'))

# CM: the period labels (labels_period), the coarse ages (ages_sub) and the tail map window
# (xlim, ylim) were defined here but never used in this script; removed so config.R holds
# their only definition (issue I03). Likewise the fine time slices (ages, N_times; I04b).

ice_fill = 'gainsboro'
ice_colour = 'gray60'

ice_fill_dark = 'ivory3'
ice_colour_dark = 'gray40'

pbs_ll = read_input(run, path_file(paths, 'boundaries_ll'))
pbs = read_input(run, path_file(paths, 'boundaries'))

rk_hadgem = read_input(run, path_file(paths, 'kernel_hadgem'), brick, level=1, varname='albedo_sw_cs')

alb_interp_diff_full = read_input(run, path_file(paths, 'albedo_differences'))

alb_diff = alb_interp_diff_full

alb_diff$long360 = 180 + 180 - abs(alb_diff$long)
alb_diff$lat180 = alb_diff$lat + 90

alb_diff_spatial = SpatialPointsDataFrame(coords = alb_diff[,c('long360', 'lat')],
                                          data = alb_diff,
                                          proj4string = CRS(ll_proj))

alb_diff_spatial_cack = SpatialPointsDataFrame(coords = alb_diff[,c('long360', 'lat180')],
                                               data = alb_diff,
                                               proj4string = CRS(ll_proj))

alb_diff$rk_hadgem = NA
alb_diff$rk_cam5 = NA
alb_diff$rk_cack = NA

for (month in months){

  print(month)
  month_number = month_to_number(month)  # CM: by name, not position in `months` (I02)
  print(month_number)

  idx_month = which(alb_diff$month == month)

  rk_hadgem_month = raster::extract(rk_hadgem[[month_number]], alb_diff_spatial[idx_month,])
  alb_diff$rk_hadgem[idx_month] = rk_hadgem_month

  rk_hadgem_df = as.data.frame(rk_hadgem[[month_number]], xy=TRUE)
  colnames(rk_hadgem_df) = c('x', 'y', 'kernel')

  ggplot() +
    geom_raster(data=rk_hadgem_df, aes(x=x, y=y, fill=kernel)) +
    geom_point(data=data.frame(alb_diff_spatial), aes(x=long360, y=lat), shape=1, alpha=0.1) +
    scale_fill_gradientn(colours = terrain.colors(10))

  rk_cam5 = read_input(run, path_file(paths, 'kernel_cam5'), raster,
                   varname='FSNSC',
                   band=month_number)

  rk_cam5_month = raster::extract(rk_cam5, alb_diff_spatial[idx_month,])
  alb_diff$rk_cam5[idx_month] = rk_cam5_month

  rk_cam5_df = as.data.frame(rk_cam5, xy=TRUE)
  colnames(rk_cam5_df) = c('x', 'y', 'kernel')

  rk_cack = read_input(run, path_file(paths, 'kernel_cack'), raster,
                   varname='CACK',
                   level=month_number,
                   band=cfg$analysis$kernels$cack_year - 2000)   # Year dimension: 2001 = 1 (C3; I04b)

  rk_cack =  t(flip((rk_cack)))

  rk_cack_df = as.data.frame(rk_cack, xy=TRUE)
  colnames(rk_cack_df) = c('x', 'y', 'kernel')

  rk_cack_month = raster::extract(rk_cack, alb_diff_spatial_cack[idx_month,])
  alb_diff$rk_cack[idx_month] = rk_cack_month

}

# Keep cells strictly between config.R analysis$domain$lat_min and lat_max (issue I04b).
alb_diff = alb_diff[which(alb_diff$lat>cfg$analysis$domain$lat_min),]
alb_diff = alb_diff[which(alb_diff$lat<cfg$analysis$domain$lat_max),]

alb_diff$rf_hadgem_veg_thresh = alb_diff$alb_diff_veg_thresh*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_ice_thresh = alb_diff$alb_diff_ice_thresh*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_icesc_thresh = alb_diff$alb_diff_icesc_thresh*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_ice_thresh = alb_diff$alb_diff_veg_ice_thresh*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_icesc_thresh = alb_diff$alb_diff_veg_icesc_thresh*100 * alb_diff$rk_hadgem

alb_diff$rf_hadgem_veg_part = alb_diff$alb_diff_veg_part*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_ice_part = alb_diff$alb_diff_ice_part*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_icesc_part = alb_diff$alb_diff_icesc_part*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts*100 * alb_diff$rk_hadgem
alb_diff$rf_hadgem_veg_icesc_parts = alb_diff$alb_diff_veg_icesc_parts*100 * alb_diff$rk_hadgem

alb_diff$rf_cam5_veg_thresh = alb_diff$alb_diff_veg_thresh*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_ice_thresh = alb_diff$alb_diff_ice_thresh*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_icesc_thresh = alb_diff$alb_diff_icesc_thresh*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_ice_thresh = alb_diff$alb_diff_veg_ice_thresh*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_icesc_thresh = alb_diff$alb_diff_veg_icesc_thresh*100 * alb_diff$rk_cam5

alb_diff$rf_cam5_veg_part = alb_diff$alb_diff_veg_part*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_ice_part = alb_diff$alb_diff_ice_part*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_icesc_part = alb_diff$alb_diff_icesc_part*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts*100 * alb_diff$rk_cam5
alb_diff$rf_cam5_veg_icesc_parts = alb_diff$alb_diff_veg_icesc_parts*100 * alb_diff$rk_cam5

alb_diff$rf_cack_veg_thresh = alb_diff$alb_diff_veg_thresh* (-alb_diff$rk_cack)
alb_diff$rf_cack_ice_thresh = alb_diff$alb_diff_ice_thresh* (-alb_diff$rk_cack)
alb_diff$rf_cack_icesc_thresh = alb_diff$alb_diff_icesc_thresh* (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_ice_thresh = alb_diff$alb_diff_veg_ice_thresh* (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_icesc_thresh = alb_diff$alb_diff_veg_icesc_thresh* (-alb_diff$rk_cack)

alb_diff$rf_cack_veg_part = alb_diff$alb_diff_veg_part* (-alb_diff$rk_cack)
alb_diff$rf_cack_ice_part = alb_diff$alb_diff_ice_part* (-alb_diff$rk_cack)
alb_diff$rf_cack_icesc_part = alb_diff$alb_diff_icesc_part* (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_ice_parts = alb_diff$alb_diff_veg_ice_parts* (-alb_diff$rk_cack)
alb_diff$rf_cack_veg_icesc_parts = alb_diff$alb_diff_veg_icesc_parts* (-alb_diff$rk_cack)

bar = alb_diff[which((alb_diff$rf_hadgem_ice_thresh>100)&(alb_diff$month=='jun')),]

bar[,c('cell_id', 'lat', 'long', 'ice_frac_old', 'ice_frac_young',
       'alb_diff_ice_thresh', 'rk_hadgem', 'rf_hadgem_ice_thresh',
       'rk_cam5', 'rf_cam5_ice_thresh')]

head(bar[,c('cell_id', 'lat', 'long', 'ice_frac_old', 'ice_frac_young',
                   'alb_diff_ice_thresh', 'rk_hadgem', 'rf_hadgem_ice_thresh', 'rf_hadgem_ice_part')])

-0.3326444*-3.297408

ggplot(data=bar) +
  geom_point(aes(x=alb_diff_ice_thresh, y=alb_diff_ice_part)) +
  coord_fixed()

ggplot(data=bar) +
  geom_point(aes(x=ice_frac_old, y=alb_diff_ice_thresh)) +
  geom_point(aes(x=ice_frac_old, y=alb_diff_ice_part), colour='dodgerblue', alpha=0.5)

ggplot(data=bar) +
  geom_histogram(aes(x=alb_diff_ice_thresh - alb_diff_ice_part))

ggplot(data=bar) +
  geom_point(aes(x=ice_frac_old, y=-rk_hadgem*100)) +
  geom_point(aes(x=ice_frac_old, y=rk_cack), colour='dodgerblue', alpha=0.5) +
  geom_point(aes(x=ice_frac_old, y=-rk_cam5*100), colour='pink', alpha=0.5)

ggplot(data=bar) +
  geom_point(aes(x=ice_frac_old, y=-rk_hadgem*100*alb_diff_ice_thresh)) +
  geom_point(aes(x=ice_frac_old, y=rk_cack*alb_diff_ice_thresh), colour='dodgerblue', alpha=0.5) +
  geom_point(aes(x=ice_frac_old, y=-rk_cam5*100*alb_diff_ice_thresh), colour='pink', alpha=0.5)

bar$alb_diff_ice_thresh

write_output(run, alb_diff, path_forcing(paths))

run_finish(run)
