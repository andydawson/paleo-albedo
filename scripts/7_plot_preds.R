# 7_plot_preds.R: maps of the hindcast albedo, and the coarse slice-to-slice differences
# Interp (spatially complete) path only. Settings come from config.R, and every file is
# read and written through the run manifest; comments marked '# CM:' are notes on
# changes from the original analysis.


# Settings from config.R: the coarse ages and the map window. Slice and
# period labels are made from the ages (R/calendar.R).
# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('7_plot_preds')
cfg   = run$cfg
paths = run$paths   # every file read or written
alb_prod = paths$tag   # the albedo product in file names

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(ggplot2)
library(fields)
library(raster)

pbs_ll = read_input(run, path_file(paths, 'boundaries_ll'))

ages_sub = cfg$analysis$coarse$ages

# The months analysed, from config.R (analysis$months); month names and
# numbers come from the shared calendar, R/calendar.R.
source('R/calendar.R')
months = cfg$analysis$months

xlim = cfg$presentation$map_windows$tail$x
ylim = cfg$presentation$map_windows$tail$y

proj_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

ice = read_input(run, path_file(paths, 'glacier_shapes'))
ice_years = seq(1, 21)*1000

ice_fort = data.frame(matrix(NA, nrow=0, ncol=9))

for (i in 1:length(ages_sub)){

  print(i)

  idx_ice_match = which.min(abs(ages_sub[i] - ice_years))

  proj4string(ice[[idx_ice_match]]) = proj_WGS84
  ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))

  ice_fort_age = fortify(ice[[idx_ice_match]])

  ice_fort = rbind(ice_fort,
                   data.frame(ice_fort_age,
                              ice_year = rep(ice_years[idx_ice_match], nrow(ice_fort_age)),
                              ages = rep(ages_sub[i], nrow(ice_fort_age))))

}

ice_sub = ice_fort[which(ice_fort$ages %in% ages_sub),]

ice_sub$facets = as.character(ice_sub$ages/1000)
ice_sub$facets = factor(ice_sub$facets,
                        levels = ages_ka(ages_sub),
                        labels = paste(ages_ka(ages_sub), 'ka'))

write_output(run, ice_fort, path_file(paths, 'ice_fort'))

ice_fill = 'gainsboro'
ice_colour = 'gray60'

ice_fill_dark = 'ivory3'
ice_colour_dark = 'gray40'

alb_interp_preds = read_input(run, path_file(paths, 'prediction_summary'))
# CM: the slice alias, config.R analysis$coarse$slice_alias (question C7): the
# 11,500 BP slice is treated as 12,000 BP, which also gives it the 12 ka ice polygon below.
slice_alias = cfg$analysis$coarse$slice_alias
if (!is.null(slice_alias)) {
  alb_interp_preds$year[which(alb_interp_preds$year == slice_alias[1])] = slice_alias[2]
}

albedo_bin_breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100

alb_interp_preds$alb_bin = cut(alb_interp_preds$alb_mean, albedo_bin_breaks, labels=FALSE)

alb_interp_preds$ice = NA

alb_preds_ages = unique(alb_interp_preds$year)

for (i in 1:length(alb_preds_ages)){

  print(i)
  idx_age = which(alb_interp_preds$year == alb_preds_ages[i])

  coords_veg  = SpatialPoints(alb_interp_preds[idx_age,c('x', 'y')],
                              proj4string=CRS(proj_WGS84))

  idx_ice_match = which.min(abs(alb_preds_ages[i] - ice_years))

  proj4string(ice[[idx_ice_match]]) = proj_WGS84
  ice[[idx_ice_match]] = spTransform(ice[[idx_ice_match]], CRS(proj_WGS84))

  ice_status_veg = over(coords_veg, ice[[idx_ice_match]])

  alb_interp_preds[idx_age, 'ice'] = ice_status_veg[,which(substr(colnames(ice_status_veg), 1,4)=='SYMB')]

}


years = cfg$analysis$coarse$ages

breaks_alb = c(0, 5, 10, 20, 30, 40, 60, 80, 100)/100
labels_alb = c("0 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 60", "60 - 80", "80 - 100")

grid_NA <- read_input(run, path_file(paths, 'grid'))

cell_id <- raster::extract(grid_NA, alb_interp_preds[,c('x', 'y')])

alb_grid <- data.frame(cell_id, alb_interp_preds)
coords   = xyFromCell(grid_NA, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords,
                 alb_grid[,c('x', 'y', 'cell_id', 'year', 'ice', 'alb_mean', 'alb_sd', 'month')])

alb_grid_sub = subset(alb_grid, year %in% years)

alb_glacier = read_input(run, path_file(paths, 'glacier_albedo'), read.csv, header=TRUE)

# The glacier-albedo column for these maps: config.R analysis$ice$albedo_coarse (question
# C0a; script 7a uses other columns of the same file).
alb_grid_sub$alb_mean_ice = alb_glacier[match(alb_grid_sub$month, alb_glacier$month), cfg$analysis$ice$albedo_coarse]

alb_grid_sub$alb_mean_ice[which(is.na(alb_grid_sub$ice))] = alb_grid_sub$alb_mean[which(is.na(alb_grid_sub$ice))]

alb_grid_sub$alb_mean[which(alb_grid_sub$ice == 'ICE')] = NA

# CM: the two lines below are commented out (2026-10-12). They look meant to drop
# poorly covered cells, but table() counts rows per cell across ages AND months, and a cell
# present at any age has 12 rows per age, so "< 8" dropped no cells at all (0 of 2,870).
# With fewer months selected it would start dropping cells for the wrong reason. Outputs
# are unchanged. What rule was intended is a question for Andria (scientific questions, D4).
# cell_id_drop = as.numeric(names(which(table(alb_grid_sub$cell_id)<8)))
# alb_grid_sub = alb_grid_sub[which(!(alb_grid_sub$cell_id %in% cell_id_drop)),]

alb_grid_sub$alb_bin = cut(alb_grid_sub$alb_mean, breaks_alb, labels=FALSE)

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

labels_year = paste(ages_ka(years), 'ka')

alb_grid_sub$facets = labels_year[match(alb_grid_sub$year, years)]
alb_grid_sub$facets = factor(alb_grid_sub$facets, levels = labels_year)

# Cells east of config.R analysis$domain$lon_max_coarse are dropped.
alb_grid_sub = alb_grid_sub[which(alb_grid_sub$long < cfg$analysis$domain$lon_max_coarse),]

alb_grid_sub$month = factor(alb_grid_sub$month, levels = months)

# CM: February-only map; skipped when February is not selected.
if ('feb' %in% months) {
  ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, month=='feb'), aes(x=long,y=lat, fill=factor(alb_bin))) +
    geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    scale_fill_brewer(type='seq',
                      palette='YlOrBr',
                      name = "Albedo",
                      direction=1,
                      na.value = "white",
                      labels = labels_alb) +
    facet_grid(facets~.)+
    theme_bw(12)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  ggsave(path_figure(paths, paste0('alb_interp_preds_binned_tile_grid_', alb_prod, '.pdf')))#, width=12, height=14)
  ggsave(path_figure(paths, paste0('alb_interp_preds_binned_tile_grid_', alb_prod, '.png')))#, width=12, height=14)
} # CM: end of February-only map

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=factor(alb_bin))) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  scale_fill_brewer(type='seq',
                    palette='YlOrBr',
                    name = "Albedo",
                    direction=1,
                    na.value = "white",
                    labels = labels_alb) +
  facet_grid(facets~month)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_binned_tile_grid_', alb_prod, '.pdf')), width=14, height=12)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_binned_tile_grid_', alb_prod, '.png')), width=14, height=12)

# CM: the four-month panel shows whichever of its months are selected.
months_sub = intersect(c('feb', 'may', 'aug', 'nov'), months)
if (length(months_sub)) {

  ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, month %in% months_sub), aes(x=long,y=lat, fill=factor(alb_bin))) +
    geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    scale_fill_brewer(type='seq',
                      palette='YlOrBr',
                      name = "Albedo",
                      direction=1,
                      na.value = "white",
                      labels = labels_alb) +
    facet_grid(facets~month)+
    theme_bw(20)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  ggsave(path_figure(paths, paste0('alb_interp_preds_month_sub_binned_tile_grid_', alb_prod, '.pdf')), width=14, height=12)
  ggsave(path_figure(paths, paste0('alb_interp_preds_month_sub_binned_tile_grid_', alb_prod, '.png')), width=14, height=12)
} # CM: end of four-month panel

# CM: February-only map; skipped when February is not selected.
if ('feb' %in% months) {
  ggplot() +
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, month=='feb'), aes(x=long,y=lat, fill=factor(alb_bin))) +
    geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    scale_fill_brewer(type='seq', palette='YlOrBr',
                      na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
    facet_wrap(facets~.) +
    theme_bw(12)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  ggsave(path_figure(paths, paste0('alb_interp_preds_binned_tile_wrap_', alb_prod, '.pdf')), width=14, height=12)
  ggsave(path_figure(paths, paste0('alb_interp_preds_binned_tile_wrap_', alb_prod, '.png')), width=14, height=12)
} # CM: end of February-only map

pdf(path_figure(paths, paste0('alb_interp_preds_month_binned_tile_pages_', alb_prod, '.pdf')))
# CM: subset(alb_grid_sub, year==year) below compares the year column with itself, so
# every page draws all ages' tiles; only the ice outline changes per page. Left as is
# pending Andria (questions_for_andria_general.md, section 8).
for (year in ages_sub){

  p = ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=subset(alb_grid_sub, year==year), aes(x=long,y=lat, fill=factor(alb_bin))) +
    geom_polygon(data=subset(ice_sub, ages == year), aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    scale_fill_brewer(type='seq', palette='YlOrBr',
                      na.value='transparent', name = "Albedo", direction=1, labels = labels_alb) +
    facet_wrap(~month, nrow=4, ncol=4)+
    theme_bw(12)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  print(p)
}
dev.off()


ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_sd_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  scale_fill_brewer(type='seq',
                    palette='YlOrBr',
                    na.value='transparent',
                    name = "Standard \ndeviation",
                    direction=1,
                    labels = labels_sd) +
  facet_grid(facets~month)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_sd_binned_tile_grid_', alb_prod, '.pdf')), width=14, height=12)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_sd_binned_tile_grid_', alb_prod, '.png')), width=14, height=12)

# CM: the CV legend below uses labels_sd, not labels_cv, so it shows SD ranges. Left as
# is pending Andria (questions_for_andria_general.md, section 8).
ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_grid_sub, aes(x=long,y=lat, fill=alb_cv_bin)) +
  geom_polygon(data=ice_sub, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  scale_fill_brewer(type='seq', palette='YlOrBr',
                    na.value='transparent', name = "CV", direction=1, labels = labels_sd) +
  facet_grid(facets~month)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_cv_binned_tile_grid_', alb_prod, '.pdf')), width=14, height=12)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_cv_binned_tile_grid_', alb_prod, '.png')), width=14, height=12)

years_df = data.frame(year = ages_sub)

alb_diff_df = data.frame(matrix(NA, nrow=0, ncol=ncol(alb_grid_sub)+5))

alb_diff_colnames = colnames(alb_grid_sub)[which(!(colnames(alb_grid_sub) %in% 'ice'))]

colnames(alb_diff_df) = c(alb_diff_colnames, 'alb_mean_ice_old', 'alb_mean_ice_young',
                          'ice_young', 'ice_old',
                          'alb_diff', 'alb_diff_ice')#c('cell_id', 'long', 'lat', 'x', 'y', 'year', 'alb_pred', 'alb_bin', 'alb_diff')

cell_ids = unique(alb_grid_sub$cell_id)
N_cells  = length(cell_ids)
for (i in 1:N_cells){

  print(paste0('Cell ', i, ' of ', N_cells))

  for (month in months){

    alb_cell = alb_grid_sub[which((alb_grid_sub$cell_id == cell_ids[i])&(alb_grid_sub$month == month)),]
    alb_cell = alb_cell[order(alb_cell$year),]

    if (nrow(alb_cell) == 1){
      next
    }

    alb_cell_filled = merge(years_df, alb_cell, by = 'year', all.x=TRUE)

    alb_diff_df = rbind(alb_diff_df,
                        data.frame(subset(alb_cell_filled[1:(nrow(alb_cell_filled)-1), ],
                                          select=-c(ice)),
                                   alb_mean_ice_old = alb_cell_filled[2:nrow(alb_cell_filled), 'alb_mean_ice'],
                                   alb_mean_ice_young = alb_cell_filled[1:(nrow(alb_cell_filled)-1), 'alb_mean_ice'],
                                   ice_young = alb_cell_filled$ice[1:(nrow(alb_cell_filled)-1)],
                                   ice_old = alb_cell_filled$ice[2:nrow(alb_cell_filled)],
                                   alb_diff = -diff(alb_cell_filled$alb_mean),
                                   alb_diff_ice = -diff(alb_cell_filled$alb_mean_ice)))
  }
}

alb_diff_df =  alb_diff_df[which(!is.na(alb_diff_df$lat)),]

# One row per cell, month and pair of consecutive coarse slices.
write_output(run, alb_diff_df, path_file(paths, 'coarse_differences'))

coarse_period_labels = period_labels(years)

ice_fort_diff_young = data.frame(matrix(NA, nrow=0, ncol=10))
ice_fort_diff_old = data.frame(matrix(NA, nrow=0, ncol=10))
ages_sub = years

ice_fort = ice_fort[,1:9]

for (i in 1:(length(ages_sub)-1)){
  print(ages_sub[i])

  age_now = ages_sub[i]

  idx_ice_match_young = which.min(abs(ages_sub[i] - ice_years))
  idx_ice_match_old = which.min(abs(ages_sub[i+1] - ice_years))

  # CM: ice_fort$ages holds the requested ages (50, 500, ...) but is matched against
  # the nearest ice year (1, 2, ... 21 ka), so 50 and 500 BP find no outline. Left as
  # is pending Andria (questions_for_andria_general.md, section 8).
  ice_fort_age_young = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_young]),]

  ice_fort_age_old = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_old]),]

  print(coarse_period_labels[i])

  ice_fort_diff_young = rbind(ice_fort_diff_young,
                              data.frame(ice_fort_age_young,
                                         facets = rep(coarse_period_labels[i],  nrow(ice_fort_age_young))
                              ))

  ice_fort_diff_old = rbind(ice_fort_diff_old,
                            data.frame(ice_fort_age_old,
                                       facets = rep(coarse_period_labels[i],  nrow(ice_fort_age_old))
                            ))

}

ice_fort_diff_old$facets = factor(ice_fort_diff_old$facets,
                                  levels = coarse_period_labels,
                                  labels = coarse_period_labels)

ice_fort_diff_young$facets = factor(ice_fort_diff_young$facets,
                                    levels = coarse_period_labels,
                                    labels = coarse_period_labels)

write_output(run, ice_fort_diff_young, path_file(paths, 'ice_fort_diff_young'))
write_output(run, ice_fort_diff_old, path_file(paths, 'ice_fort_diff_old'))

diff_years = years[-length(years)]
alb_diff_df$facets = coarse_period_labels[match(alb_diff_df$year, diff_years)]
alb_diff_df$facets = factor(alb_diff_df$facets, levels =  coarse_period_labels)

max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
diff_colour_limit = ceiling(max_diff*100)/100

diff_colour_positions = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        na.value="grey",
                                        name="Albedo change",
                                        limits = c(-diff_colour_limit,diff_colour_limit),
                                        values = diff_colour_positions)

ggplot()+
  geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
  geom_tile(data=alb_diff_df, aes(x=long, y=lat, fill = alb_diff)) +
  geom_polygon(data=ice_fort_diff_old, aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
  geom_polygon(data=ice_fort_diff_young, aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
  sc_fill_diverge +
  facet_grid(month~facets)+
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  coord_fixed(xlim = xlim, ylim = ylim)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_diff_tile_grid_', alb_prod, '.png')), width=12, height=14)
ggsave(path_figure(paths, paste0('alb_interp_preds_month_diff_tile_grid_', alb_prod, '.pdf')), width=12, height=14)

facets = as.vector(unique(alb_diff_df$facets))

pdf(path_figure(paths, paste0('alb_interp_preds_month_diff_tile_pages_', alb_prod, '.pdf')))
for (i in 1:length(facets)){

  facet = facets[i]

  diff_sub = alb_diff_df[which(alb_diff_df$facets == facet),]

  p<-ggplot()+
    geom_polygon(data=pbs_ll, aes(long,lat, group = group), color="grey", fill="grey") +
    geom_tile(data=diff_sub, aes(x=long, y=lat, fill = alb_diff)) +
    geom_polygon(data=subset(ice_fort_diff_old, facets==facet), aes(x=long, y=lat, group=group),  colour=ice_colour, fill=ice_fill) +
    geom_polygon(data=subset(ice_fort_diff_young, facets==facet), aes(x=long, y=lat, group=group),  colour=ice_colour_dark, fill=ice_fill_dark) +
    facet_wrap(~month, nrow=3, ncol=4) +
    sc_fill_diverge +
    theme_bw(12)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    coord_fixed(xlim = xlim, ylim = ylim)
  print(p)
}
dev.off()

run_finish(run)
