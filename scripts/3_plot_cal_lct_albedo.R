# 3_plot_cal_lct_albedo.R: diagnostic plots of the calibration data (optional)
# Interp (spatially complete) path only. The point-based first half of the original and
# its [run-nointerp] guards are removed (2026-09-30; the other scripts had this done on
# 2026-09-24, this one was left out because nothing downstream reads its output). The
# plotting lines are Andria's, with three changes, each marked "# CM:" where it happens:
# the map boundaries come from R/map_helpers.R, the pie map keeps its legend labels, and
# a block that could not run on the melted table is dropped.
#
# Reads  data/calibration_modern_lct_interp_bluesky.RDS (from script 2)
# Writes figures/LCT_*_interp.{png,pdf}; the albedo scatter plots are printed only, so a
#        non-interactive run leaves them in Rplots.pdf.


# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest; files are then read and written through it.
source('R/run_manifest.R')
run   = run_begin('3_plot_cal_lct_albedo')
cfg   = run$cfg
paths = run$paths

# Packages, loaded after run_begin() so that a missing one is recorded as a failed run.
library(ggplot2)
library(ggtern)
library(scales)
library(dplyr)
library(raster)
library(reshape2)
library(tricolore)
library(scatterpie)

###############################################################################################################
## maps data
###############################################################################################################
pbs_ll = read_input(run, path_file(paths, 'boundaries_ll'))

# CM: boundaries prepared by the shared helper (see R/map_helpers.R for why the raw file
# cannot be drawn as-is). The original drew pbs_ll with geom_polygon and coord_fixed.
# The map window is the calibration window in config.R.
map_window = cfg$presentation$map_windows$calibration
source('R/map_helpers.R')
pbs_land = prepare_boundaries(pbs_ll, map_window)

###############################################################################################################
## calibration data
###############################################################################################################

cal_data = read_input(run, path_file(paths, 'calibration_table'))
cal_long = melt(cal_data, id.vars=c('x', 'y', 'elev', 'ET', 'OL', 'ST'))
colnames(cal_long) = c('x', 'y', 'elev', 'ET', 'OL', 'ST', 'month', 'albedo')
# cal_long$month = as.numeric(substr(cal_long$month, 4, 5))

cal_long = data.frame(long = cal_long$x, lat = cal_long$y, cal_long)

###############################################################################################################
## LCT maps
###############################################################################################################

# make grid for NA (or ENA)
# CM: scripts/make_grid.R is a reconstruction (2026-09-16); the original is not in the repo.
source('scripts/make_grid.R')
grid <- make_grid(cal_data, coord_fun = ~ x + y, projection = '+init=epsg:4326', resolution = 2)

cell_id <- raster::extract(grid, cal_data[,c('x', 'y')])

cal_data_pie <- data.frame(cell_id, cal_data)

cal_data_pie_agg = cal_data_pie %>% 
  group_by(cell_id) %>%
  summarize(ET = mean(ET, na.rm = TRUE), 
            OL = mean(OL, na.rm = TRUE), 
            ST = mean(ST, na.rm = TRUE), 
            .groups='keep')

coords = xyFromCell(grid, cal_data_pie_agg$cell_id)
colnames(coords) = c('x', 'y')


cal_data_pie_agg = cbind(coords, cal_data_pie_agg)

cal_data_pie_agg = cal_data_pie_agg[which(!is.na(cal_data_pie_agg$x)),]

color_values_four = c("#CC79A7", "#009E73", "#0072B2", "#D55E00")
color_values_three = c("#009E73","#0072B2", "#D55E00")

ggplot() +
  boundary_layers(pbs_land, map_window, colour = "grey", fill = "grey") +
  geom_scatterpie(data=cal_data_pie_agg,
                  aes(x=x, y=y),
                  cols=c('OL', 'ST', 'ET'),
                  pie_scale=0.4,
                  alpha=0.7) +
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
  labs(fill='Land cover \ntype') +
  # CM: the original added scale_fill_discrete(labels, reversed guide) and then
  # scale_fill_manual(values); the second replaced the first, so the labels were lost.
  # One scale now carries both.
  scale_fill_manual(values = color_values_three,
                    labels = c("Open land", "Summergreen", "Evergreen"),
                    guide = guide_legend(reverse = TRUE))
ggsave(path_figure(paths, 'LCT_cal_pie_map_ll_interp.png'))
ggsave(path_figure(paths, 'LCT_cal_pie_map_ll_interp.pdf'))

tric_lct <- Tricolore(cal_data_pie,
                      p1 = 'ET', p2 = 'OL', p3 = 'ST', show_data = TRUE)

tric_lct$key + 
  theme_bw(18) + 
  geom_point(data=cal_data_pie, aes(x=ET, y=OL, z=ST), size=1, alpha=0.2)
# tric_lct$key + theme_bw(18) + geom_point(data=cal_data_pie_agg, size=1)

ggsave(path_figure(paths, 'LCT_tricolore_key_interp.png'))
ggsave(path_figure(paths, 'LCT_tricolore_key_interp.pdf'))

# each calibration cell's colour, from its mix of the three land-cover classes
cal_data_pie$lct_rgb <- tric_lct$rgb

plot_lct <-
  # the calibration cells...
  ggplot() +
  boundary_layers(pbs_land, map_window, colour = "grey", fill = "grey") +
  # ...drawn as tiles
  geom_tile(data=cal_data_pie, aes(x, y, fill = lct_rgb)) +
  # ...each coloured by its land-cover mix (lct_rgb)
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
  scale_fill_identity()

plot_lct
ggsave(path_figure(paths, 'LCT_tricolore_map_interp.png'))
ggsave(path_figure(paths, 'LCT_tricolore_map_interp.pdf'))

###############################################################################################################
## gridded land cover maps
###############################################################################################################

cal_lct_melt = melt(cal_long, id.vars = c('long', 'lat', 'x', 'y', 'elev', 'month', 'albedo'))

cal_lct_melt$variable = factor(cal_lct_melt$variable, 
                               levels = c('ET', 'ST', 'OL'),
                               labels = c('ETS', 'STS', 'OVL'))

ggplot() +
  boundary_layers(pbs_land, map_window, colour = "grey", fill = "grey") +
  geom_tile(data=cal_lct_melt, 
            aes(x=long, y=lat, fill=value),
            alpha=0.7) +
  # scale_fill_distiller(type='seq', palette='YlGn') +
  scale_fill_gradientn(colours = rev(terrain.colors(10)), limits=c(0,1)) + 
  theme_bw(12) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(fill='Fractional \nland cover') +
  facet_grid(variable~.)
ggsave(path_figure(paths, 'LCT_gridded_maps_calibration_interp.pdf'))
ggsave(path_figure(paths, 'LCT_gridded_maps_calibration_interp.png'))

###############################################################################################################
## albedo versus other vars
###############################################################################################################

cal_long = cal_lct_melt

ggplot(data=cal_long, aes(x=lat, y=albedo)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)

ggplot(data=cal_long, aes(x=elev, y=albedo)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)

ggplot(data=cal_long, aes(x=value, y=albedo, colour=variable)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)

ggplot(data=subset(cal_long, variable == 'ETS'), aes(x=value, y=albedo)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)

ggplot(data=subset(cal_long, variable == 'STS'), aes(x=value, y=albedo)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)

ggplot(data=subset(cal_long, variable == 'OVL'), aes(x=value, y=albedo)) +
  geom_point(alpha=0.2) +
  facet_wrap(~month)


# albedo versus land cover, by month

# CM: the original re-melted cal_long here and then plotted columns OL, ST and ET, which
# no longer exist once the table is melted (they are 'variable' and 'value'). Those three
# plots duplicate the per-class scatters above, so the block is dropped; cal_lct_melt from
# the gridded-maps section is what the plots below use.

# albedo against each land-cover fraction, one panel per month, with a linear fit
# (Andria's note on May: the relationship is weak but there)
ggplot(data=cal_lct_melt, aes(x=value, y=albedo, colour=variable)) +
  geom_point(alpha=0.6) +
  geom_smooth(se = TRUE, method = lm, fullrange=TRUE)+
  facet_wrap(~month)


# each land-cover fraction against latitude, with a linear fit
ggplot(data=cal_lct_melt, aes(x=lat, y=value, colour=variable)) +
  geom_point(alpha=0.6) +
  geom_smooth(se = TRUE, method = lm, fullrange=TRUE)


corr_lct = cal_lct_melt %>% 
  filter((!(is.na(albedo)))&(!(is.na(value)))) %>%
  group_by(variable, month) %>% 
  summarize(cor = cor(albedo, value))


ggplot(data=corr_lct) + 
  geom_point(aes(y=variable, x=factor(month), size=abs(cor), colour=cor)) +
  scale_colour_gradient2(low = muted("red"),
                         mid = "white",
                         high = muted("blue"),
                         midpoint = 0,
                         limits = c(-0.6, 0.6), 
                         space = "Lab",
                         na.value = "grey50") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.ticks = element_line(size=1),
        axis.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16))

run_finish(run)
