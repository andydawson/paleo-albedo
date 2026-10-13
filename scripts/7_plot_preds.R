# 7_plot_preds.R: maps of the hindcast albedo, and the coarse slice-to-slice differences
# Interp (spatially complete) path only: the point-based code and its guards are
# removed. Every remaining line is unchanged from the original.


library(ggplot2)
library(fields)
library(raster)

alb_prod = "bluesky"

dir.create('figures', showWarnings = FALSE)

# Settings from config.R (issue I03): the coarse ages and the map window. Slice and
# period labels are made from the ages (R/calendar.R).
source('R/config.R')
cfg = load_config()

source('R/run_manifest.R')
run_start('7_plot_preds',
          note   = Sys.getenv('RUN_NOTE'),
          inputs = Filter(file.exists, c(
            paste0('output/prediction/paleo_interp_predict_gam_summary_', alb_prod, '.RDS'),
            paste0('output/prediction/paleo_interp_predict_gam_', alb_prod, '.RDS'),
            'data/grid.RDS',
            'data/map-data/ice/glacier_shapefiles_21-1k.RDS',
            'data/albedo_glacier_monthly.csv',
            'data/map-data/geographic/pbs_ll.RDS',
            'data/map-data/geographic/pbs.RDS')),
          config = list(alb_prod = alb_prod, config = config_json(cfg)))

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')

pbs = readRDS('data/map-data/geographic/pbs.RDS')

breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100

labels = seq(1, length(breaks)-1)

ages = c(50, 200, seq(500, 11500, by=500))
N_times = length(ages)
ages_sub = cfg$analysis$coarse$ages

# Month list from the shared calendar (R/calendar.R) rather than typed out in each script
# (issue I02, 2026-10-12).
source('R/calendar.R')
months = MONTHS

xlim = cfg$presentation$map_windows$tail$x
ylim = cfg$presentation$map_windows$tail$y

proj_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

ice = readRDS('data/map-data/ice/glacier_shapefiles_21-1k.RDS')
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

saveRDS(ice_fort, 'data/ice_fort.RDS')

ice_fill = 'gainsboro'
ice_colour = 'gray60'

ice_fill_dark = 'ivory3'
ice_colour_dark = 'gray40'

alb_interp_preds = readRDS(paste0('output/prediction/paleo_interp_predict_gam_summary_', alb_prod, '.RDS'))
alb_interp_preds$year[which(alb_interp_preds$year == 11500)] = 12000

pbs_ll = readRDS('data/map-data/geographic/pbs_ll.RDS')
pbs = readRDS('data/map-data/geographic/pbs.RDS')

breaks = c(0, 4, 8, 12, 16, 20, 40, 60, 80, 100)/100

labels = seq(1, length(breaks)-1)

sc_colour <- scale_colour_manual(values = c(tim.colors(length(breaks)), "grey"), labels = labels,
                                 na.value="white", name="Percent", drop=FALSE)

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

alb_interp_preds$alb_bin = cut(alb_interp_preds$alb_mean, breaks, labels=FALSE)

alb_interp_preds$ice = NA

ages[which(ages == 11500)] = 12000

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

ll_proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

years = cfg$analysis$coarse$ages

breaks_alb = c(0, 5, 10, 20, 30, 40, 60, 80, 100)/100
labels_alb = c("0 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 60", "60 - 80", "80 - 100")

grid_NA <- readRDS("data/grid.RDS")

paleo_interp_sim_gam = readRDS('output/prediction/paleo_interp_predict_gam_bluesky.RDS')

cell_id <- raster::extract(grid_NA, alb_interp_preds[,c('x', 'y')])

alb_grid <- data.frame(cell_id, alb_interp_preds)
coords   = xyFromCell(grid_NA, alb_grid$cell_id)
colnames(coords) = c('long', 'lat')

alb_grid = cbind(coords,
                 alb_grid[,c('x', 'y', 'cell_id', 'year', 'ice', 'alb_mean', 'alb_sd', 'month')])

alb_grid_sub = subset(alb_grid, year %in% years)

alb_glacier = read.csv('data/albedo_glacier_monthly.csv', header=TRUE)

alb_grid_sub$alb_mean_ice = alb_glacier[match(alb_grid_sub$month, alb_glacier$month), 'ice_albedo']

alb_grid_sub$alb_mean_ice[which(is.na(alb_grid_sub$ice))] = alb_grid_sub$alb_mean[which(is.na(alb_grid_sub$ice))]

alb_grid_sub$alb_mean[which(alb_grid_sub$ice == 'ICE')] = NA

# CM: the two lines below are commented out (2026-10-12, issue I02). They look meant to drop
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

alb_grid_sub = alb_grid_sub[which(alb_grid_sub$long < (-60)),]

alb_grid_sub$month = factor(alb_grid_sub$month, levels = months)

# CM: February-only map; skipped when February is not selected (issue I02).
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
ggsave(paste0('figures/alb_interp_preds_binned_tile_grid_', alb_prod, '.pdf'))#, width=12, height=14)
ggsave(paste0('figures/alb_interp_preds_binned_tile_grid_', alb_prod, '.png'))#, width=12, height=14)
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
ggsave(paste0('figures/alb_interp_preds_month_binned_tile_grid_', alb_prod, '.pdf'), width=14, height=12)
ggsave(paste0('figures/alb_interp_preds_month_binned_tile_grid_', alb_prod, '.png'), width=14, height=12)

# CM: the four-month panel shows whichever of its months are selected (issue I02).
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
ggsave(paste0('figures/alb_interp_preds_month_sub_binned_tile_grid_', alb_prod, '.pdf'), width=14, height=12)
ggsave(paste0('figures/alb_interp_preds_month_sub_binned_tile_grid_', alb_prod, '.png'), width=14, height=12)
} # CM: end of four-month panel

# CM: February-only map; skipped when February is not selected (issue I02).
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
ggsave(paste0('figures/alb_interp_preds_binned_tile_wrap_', alb_prod, '.pdf'), width=14, height=12)
ggsave(paste0('figures/alb_interp_preds_binned_tile_wrap_', alb_prod, '.png'), width=14, height=12)
} # CM: end of February-only map

pdf(paste0('figures/alb_interp_preds_month_binned_tile_pages_', alb_prod, '.pdf'))
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

lims = c(0, round(max(alb_grid_sub$alb_sd),2))

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
ggsave(paste0('figures/alb_interp_preds_month_sd_binned_tile_grid_', alb_prod, '.pdf'), width=14, height=12)
ggsave(paste0('figures/alb_interp_preds_month_sd_binned_tile_grid_', alb_prod, '.png'), width=14, height=12)

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
ggsave(paste0('figures/alb_interp_preds_month_cv_binned_tile_grid_', alb_prod, '.pdf'), width=14, height=12)
ggsave(paste0('figures/alb_interp_preds_month_cv_binned_tile_grid_', alb_prod, '.png'), width=14, height=12)

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

    alb_cell_filled = merge(years_df, alb_cell, all.x=TRUE)

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

saveRDS(alb_diff_df, paste0('data/alb_interp_preds_diffs_', alb_prod, '.RDS'))

labels = period_labels(years)

ice_fort_diff_young = data.frame(matrix(NA, nrow=0, ncol=10))
ice_fort_diff_old = data.frame(matrix(NA, nrow=0, ncol=10))
ages_sub = years

ice_fort = ice_fort[,1:9]

for (i in 1:(length(ages_sub)-1)){
  print(ages_sub[i])

  age_now = ages_sub[i]

  idx_ice_match_young = which.min(abs(ages_sub[i] - ice_years))
  idx_ice_match_old = which.min(abs(ages_sub[i+1] - ice_years))

  ice_fort_age_young = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_young]),]

  ice_fort_age_old = ice_fort[which(ice_fort$ages == ice_years[idx_ice_match_old]),]

  print(labels[i])

  ice_fort_diff_young = rbind(ice_fort_diff_young,
                              data.frame(ice_fort_age_young,
                                         facets = rep(labels[i],  nrow(ice_fort_age_young))
                              ))

  ice_fort_diff_old = rbind(ice_fort_diff_old,
                            data.frame(ice_fort_age_old,
                                       facets = rep(labels[i],  nrow(ice_fort_age_old))
                            ))

}

labels_period = period_labels(years)

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

max_diff = max(abs(alb_diff_df$alb_diff), na.rm=TRUE)
thresh = ceiling(max_diff*100)/100

values = c(0, 0.4, 0.45, 0.5, 0.55, 0.6, 1)
values = c(0, 0.45, 0.48, 0.5, 0.52, 0.55, 1)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "BrBG",
                                        na.value="grey",
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)

sc_fill_diverge <- scale_fill_distiller(type = "div",
                                        palette = "RdYlBu",#"BrBG",
                                        na.value="grey",
                                        name="Albedo change",
                                        limits = c(-thresh,thresh),
                                        values = values)

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "RdYlBu",#"BrBG",
                                            direction=1,
                                            na.value="transparent",#grey",
                                            name="Albedo change",
                                            limits = c(-thresh,thresh),
                                            values = values)

sc_colour_diverge <- scale_colour_distiller(type = "div",
                                            palette = "BrBG",
                                            na.value="transparent",#grey",
                                            name="Albedo change",
                                            limits = c(-thresh,thresh),
                                            values = values)

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
ggsave(paste0('figures/alb_interp_preds_month_diff_tile_grid_', alb_prod, '.png'), width=12, height=14)
ggsave(paste0('figures/alb_interp_preds_month_diff_tile_grid_', alb_prod, '.pdf'), width=12, height=14)

facets = as.vector(unique(alb_diff_df$facets))

pdf(paste0('figures/alb_interp_preds_month_diff_tile_pages_', alb_prod, '.pdf'))
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

run_end(outputs = Filter(file.exists, c(
  list.files('figures', pattern = 'alb_interp', full.names = TRUE),
  'data/ice_fort.RDS', 'data/ice_fort_diff_young.RDS', 'data/ice_fort_diff_old.RDS',
  paste0('data/alb_interp_preds_diffs_', alb_prod, '.RDS'))))
