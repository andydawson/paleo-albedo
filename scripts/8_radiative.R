# 8_radiative.R: turn albedo change into radiative forcing
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R (it creates the product
# directories) and starts the run's manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('8_radiative')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(sp)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)

# The months analysed, from config.R (analysis$months); month names and
# numbers come from the shared calendar, R/calendar.R.
source('R/calendar.R')
months = cfg$analysis$months

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# The kernel variables read are set in config.R (analysis$kernels; R/kernels.R says what
# each is: clear-sky or all-sky, TOA or surface; questions C3, C4).
rk_hadgem = read_input(run, path_file(paths, 'kernel_hadgem'), brick, level=1, varname=cfg$analysis$kernels$hadgem_variable)

alb_interp_diff_full = read_input(run, path_file(paths, 'albedo_differences'))

alb_diff = alb_interp_diff_full

# Where each cell is found on the kernel grids. The kernels use longitude 0 to 360, so a
# western longitude (all the cells are west of Greenwich) becomes 360 minus its absolute
# value. CACK is read without geographic coordinates: once turned the right way round
# (t(flip()) below) its 180 x 360 grid is indexed by that longitude and by latitude + 90.
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

  rk_cam5 = read_input(run, path_file(paths, 'kernel_cam5'), raster,
                   varname=cfg$analysis$kernels$cam5_variable,
                   band=month_number)

  rk_cam5_month = raster::extract(rk_cam5, alb_diff_spatial[idx_month,])
  alb_diff$rk_cam5[idx_month] = rk_cam5_month

  if (cfg$analysis$kernels$cack_variable == 'CACK') {
    # One satellite year: CACK is [row, col, Month, Year], Year 2001 = 1 (C3; I04b).
    rk_cack = read_input(run, path_file(paths, 'kernel_cack'), raster,
                     varname='CACK',
                     level=month_number,
                     band=cfg$analysis$kernels$cack_year - 2000)
  } else {
    # The 2001-2016 climatological mean: 'CACK CM' is [row, col, Month] (C3b).
    rk_cack = read_input(run, path_file(paths, 'kernel_cack'), raster,
                     varname='CACK CM',
                     band=month_number)
  }

  # The CACK array comes in transposed and upside down relative to the map.
  rk_cack =  t(flip((rk_cack)))

  rk_cack_month = raster::extract(rk_cack, alb_diff_spatial_cack[idx_month,])
  alb_diff$rk_cack[idx_month] = rk_cack_month

}

# Keep cells strictly between config.R analysis$domain$lat_min and lat_max.
alb_diff = alb_diff[which(alb_diff$lat>cfg$analysis$domain$lat_min),]
alb_diff = alb_diff[which(alb_diff$lat<cfg$analysis$domain$lat_max),]

# Forcing (W/m2, per cell and month) = albedo change x kernel. The albedo changes are the
# younger slice minus the older (R/differences.R), as fractions (0 to 1). HadGEM3 and CAM5
# kernels are in W/m2 per 1% albedo and stored negative (brighter surface, less absorbed
# sunlight), hence x 100; CACK is in W/m2 per unit albedo and stored positive, hence the
# minus sign and no factor of 100. A brighter surface therefore gives a negative forcing
# with every kernel (question C3).
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

# Checks of the largest ice forcings, printed and plotted (to Rplots.pdf), not saved: the
# June cell-pairs whose HadGEM3 ice forcing exceeds 100 W/m2.
june_ice_forcing_over_100 = alb_diff[which((alb_diff$rf_hadgem_ice_thresh>100)&(alb_diff$month=='jun')),]

june_ice_forcing_over_100[,c('cell_id', 'lat', 'long', 'ice_frac_old', 'ice_frac_young',
       'alb_diff_ice_thresh', 'rk_hadgem', 'rf_hadgem_ice_thresh',
       'rk_cam5', 'rf_cam5_ice_thresh')]

head(june_ice_forcing_over_100[,c('cell_id', 'lat', 'long', 'ice_frac_old', 'ice_frac_young',
                   'alb_diff_ice_thresh', 'rk_hadgem', 'rf_hadgem_ice_thresh', 'rf_hadgem_ice_part')])

ggplot(data=june_ice_forcing_over_100) +
  geom_point(aes(x=alb_diff_ice_thresh, y=alb_diff_ice_part)) +
  coord_fixed()

ggplot(data=june_ice_forcing_over_100) +
  geom_point(aes(x=ice_frac_old, y=alb_diff_ice_thresh)) +
  geom_point(aes(x=ice_frac_old, y=alb_diff_ice_part), colour='dodgerblue', alpha=0.5)

ggplot(data=june_ice_forcing_over_100) +
  geom_histogram(aes(x=alb_diff_ice_thresh - alb_diff_ice_part))

ggplot(data=june_ice_forcing_over_100) +
  geom_point(aes(x=ice_frac_old, y=-rk_hadgem*100)) +
  geom_point(aes(x=ice_frac_old, y=rk_cack), colour='dodgerblue', alpha=0.5) +
  geom_point(aes(x=ice_frac_old, y=-rk_cam5*100), colour='pink', alpha=0.5)

ggplot(data=june_ice_forcing_over_100) +
  geom_point(aes(x=ice_frac_old, y=-rk_hadgem*100*alb_diff_ice_thresh)) +
  geom_point(aes(x=ice_frac_old, y=rk_cack*alb_diff_ice_thresh), colour='dodgerblue', alpha=0.5) +
  geom_point(aes(x=ice_frac_old, y=-rk_cam5*100*alb_diff_ice_thresh), colour='pink', alpha=0.5)

june_ice_forcing_over_100$alb_diff_ice_thresh

# One row per cell, month and pair of slices within the domain's latitudes: the albedo
# differences and the forcing of every variant with every kernel.
write_output(run, alb_diff, path_file(paths, 'forcing'))

run_finish(run)
