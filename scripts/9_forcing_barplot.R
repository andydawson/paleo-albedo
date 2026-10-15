# 9_forcing_barplot.R
#
# Continental Holocene radiative forcing by period, beside the modern IPCC AR6
# forcing agents. This is an attempt to reproduce the bar chart on slide 18 of
# the EGU talk from output/forcing/RF_holocene_all_cases.RDS.
#
# WRITTEN 2026-09-21. No code for this plot existed in the repository: script 8
# computes per-cell forcing, saves the table and stops. Every aggregation choice
# below is therefore an assumption by Chris/Claude, not something recovered from
# Andria's code. They are listed here so they can be checked and corrected.
#
# WHERE THIS SITS IN THE PIPELINE
#   Step 9 of 9, the last. Script 8 produced a forcing for every cell, month and pair of
#   consecutive slices. This script boils that down to one number per period for the
#   whole continent and puts it next to the modern forcing agents from the IPCC, which is
#   the comparison the talk and the paper make. It was written on 2026-09-21; the original
#   pipeline had no committed code for this step (see the note below and question C5).
#
# WHAT COMES IN
#   output/forcing/RF_holocene_all_cases.RDS      764,640 x 56, from script 8 (anchored)
#   data/alb_interp_preds_diffs_bluesky.RDS       233,880 x 21, from script 7: the coarse
#                                                 7-pair differences the recovered recipe uses
#   data/ipcc-ar6/AR6_ERF_1750-2019{,_pc05,_pc95}.csv   IPCC AR6 forcing by agent and year
#
# WHAT GOES OUT
#   output/forcing/forcing_by_period.csv          7 periods x 3 kernels: domain mean,
#                                                 global equivalent, area   (tracked in git)
#   output/forcing/forcing_by_period_variant_sensitivity.csv    same, 6 variants, HadGEM3
#   output/forcing/forcing_by_period_egu2024recipe_vs_slide.csv the recovered recipe
#   output/forcing/modern_ipcc_ar6_erf.csv        the five IPCC agents used
#   figures/forcing_barplot_*.{pdf,png}           the slide-layout chart, the kernel spread,
#                                                 the domain-mean and variant-sensitivity
#                                                 charts, and the recovered-recipe chart
#
# RUN TIME  Seconds.
#
# ASSUMPTIONS
#
# The settings below (A1 variant, A2 variants, A3 kernels, A4 periods, A8 Earth's
# area) are read from config.R, analysis$forcing and analysis$coarse.
#
# A1. Forcing variant. Uses rf_<kernel>_veg_ice_thresh: the vegetation plus ice
#     albedo change, threshold method. Chosen because it combines the vegetation
#     and the ice contribution and, like the _parts family, has no missing values
#     over the trimmed domain. Threshold rather than area-weighting was preferred
#     because it is closer to the binary ice mask used elsewhere in the pipeline;
#     the choice is question C5(a). The table holds nine other variants
#     (_veg_thresh, _ice_thresh, _icesc_thresh, and the _part / _parts family
#     that mixes vegetation and ice albedo by area fraction rather than by
#     threshold). A2 reports the alternatives, because the choice matters.
#
# A2. Variant sensitivity. Six variants are aggregated the same way and plotted
#     side by side (figures/forcing_barplot_variant_sensitivity.pdf). NB this note
#     said until 2026-10-13 that the six were the ones with complete coverage; they
#     are not. _veg_thresh is missing in 58,416 of the 764,640 rows and _ice_thresh
#     in 706,188 (the cells with no ice), which the aggregation drops, while the
#     complete _veg_part, _ice_part and _icesc_part are not among the six. The
#     list is now analysis$forcing$sensitivity_variants in config.R.
#
# A3. Kernel. HadGEM3 is the headline, matching the talk. CAM5 and CACK are
#     carried through as a spread. They are NOT interchangeable: see C3/C4 in
#     docs/cc/questions_for_andria_scientific.md. CACK is all-sky and gives
#     roughly half the HadGEM3 answer.
#
# A4. Periods. The seven periods are bounded by the coarse ages of config.R
#     (analysis$coarse$ages; by default 50, 500, 2000, 4000, 6000, 8000, 10000, 12000 yr BP).
#     CAVEAT: the oldest age in the data is 11,500 BP, so the bar labelled
#     "10 - 12 ka" actually covers 10 to 11.5 ka.
#
# A5. Combining slices within a period. Each row of the table is the forcing of
#     one consecutive slice-pair. Within a period these are SUMMED, not averaged,
#     because forcing increments are additive: the sum telescopes to the albedo
#     difference between the period's two endpoints. A mean would answer a
#     different question ("typical step") and would not be comparable with a
#     modern 1750-to-2019 forcing.
#
# A6. Months. Averaged with equal weight to an annual mean, after the period sum.
#     Equal weighting ignores that months differ in length and in insolation.
#     NB months whose kernel is NA (CACK, Dec/Jan north of 69 N) drop out of the
#     mean, whereas HadGEM3 and CAM5 store 0 there and stay in; the three kernels
#     are therefore averaged over slightly different month sets at 216 northern
#     cells.
#
# A7. Space. Cells are area-weighted using the `area` column (m^2).
#
# A8. Normalisation, and the one that matters most. Two numbers are produced:
#       - domain mean: W/m^2 averaged over the North American study area. This
#         is what the pipeline naturally produces.
#       - global equivalent: the same flux anomaly spread over the whole Earth,
#         sum(rf * area) / 5.101e14 m^2.
#     IPCC ERF values are global means. Comparing a domain-mean regional forcing
#     with a global-mean ERF overstates the Holocene signal by roughly the ratio
#     of Earth's area to the study area (about 27x: the study area is 1.92e13
#     m^2, 3.8 % of Earth's surface). The global-equivalent column
#     is the like-for-like comparison and is what the headline figure plots.
#
# A9. Sign. Positive = warming (albedo fell). Inherited from script 8.
#
# A10. Missing cells. Cells absent from any slice within a period are dropped for
#      that period, so every period is a complete telescoping sum. The number
#      dropped is reported.
#
# IPCC data: AR6 WG1 Chapter 7, data_output/AR6_ERF_1750-2019.csv from
# github.com/IPCC-WG1/Chapter-7, 1750-2019 effective radiative forcing with
# 5-95% bounds. Downloaded 2026-09-21.


source('R/calendar.R')
# Settings, file locations and this run's manifest: run_begin() reads config.R (or the file
# given with --config), sets up the paths of R/paths.R and starts the run's
# manifest, which records the inputs with checksums and the whole configuration; files are
# then read and written through it.
source('R/run_manifest.R')
run   = run_begin('9_forcing_barplot')
cfg   = run$cfg
paths = run$paths   # every file read or written

# ggplot2 and patchwork draw the charts (patchwork stacks two panels with their own
# y-axis titles); dplyr and tidyr do the aggregation; run_manifest logs provenance.
# Loaded after run_begin() so that a missing package is recorded as a failed run (I06).
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Albedo product tag; Earth's surface area for the global-equivalent normalisation (A8);
# the forcing column to use per kernel, named by kernel (A1, A3); the variants to
# compare (A2). All but the tag come from config.R.
alb_prod   = paths$tag
EARTH_AREA = cfg$analysis$forcing$earth_area_m2          # m^2
KERNELS    = setNames(paste0('rf_', cfg$analysis$forcing$report_kernels, '_', cfg$analysis$forcing$variant),
                      cfg$analysis$forcing$report_kernels)
VARIANTS   = cfg$analysis$forcing$sensitivity_variants

# Period boundaries, the coarse ages in config.R, and labels made from them (A4).
ages_sub      = cfg$analysis$coarse$ages
labels_period = period_labels(ages_sub)

# The per-cell forcing table from script 8.
d = read_input(run, path_file(paths, 'forcing'))

# --- A4/A5: assign each slice-pair to a period -------------------------------
# A row's `year` is the YOUNGER end of the pair; the pair spans year -> next age.
ages      = sort(unique(c(d$year, 11500)))
d$age_old = ages[match(d$year, ages) + 1]
d$period  = cut(d$year, breaks = ages_sub, right = FALSE, labels = labels_period)
d         = d[!is.na(d$period), ]

# How many pairs fall in each period (2, 3, 4, 4, 4, 4, 3).
cat('slice-pairs per period:\n'); print(table(unique(d[, c('year','period')])$period))

# --- A10: keep only cells complete within a period ---------------------------
n_pairs = d %>% group_by(period) %>% summarise(np = n_distinct(year), .groups='drop')
keep = d %>%
  group_by(period, cell_id, month) %>%
  summarise(np = n_distinct(year), .groups = 'drop') %>%
  left_join(n_pairs, by = 'period', suffix = c('', '_full')) %>%
  filter(np == np_full) %>%
  select(period, cell_id, month)
cat('cell-month-period combinations dropped as incomplete:',
    nrow(distinct(d[, c('period','cell_id','month')])) - nrow(keep), '\n')
d = inner_join(d, keep, by = c('period','cell_id','month'))

# --- aggregate ---------------------------------------------------------------
# A5 sum over slice-pairs -> A6 mean over months -> A7 area-weighted over cells
aggregate_forcing <- function(col) {
  d %>%
    group_by(period, cell_id, month, area) %>%
    summarise(rf = sum(.data[[col]], na.rm = FALSE), .groups = 'drop') %>%   # A5
    group_by(period, cell_id, area) %>%
    summarise(rf = mean(rf, na.rm = TRUE), .groups = 'drop') %>%             # A6
    group_by(period) %>%
    summarise(domain_mean  = weighted.mean(rf, area, na.rm = TRUE),          # A7
              global_equiv = sum(rf * area, na.rm = TRUE) / EARTH_AREA,      # A8
              area_m2      = sum(area), .groups = 'drop')
}

# Run the aggregation once per kernel and stack the results; order the periods.
main = bind_rows(lapply(names(KERNELS), function(k)
  aggregate_forcing(KERNELS[[k]]) %>% mutate(kernel = k)))
main$period = factor(main$period, levels = labels_period)

# The study area comes to 3.8% of Earth's surface; print it and the main table.
cat('\nstudy area as a fraction of Earth:',
    signif(unique(main$area_m2)[1] / EARTH_AREA, 3), '\n\n')
print(as.data.frame(main %>% select(kernel, period, domain_mean, global_equiv)), digits = 3)

# --- A2: sensitivity to the forcing variant, HadGEM3 only --------------------
sens = bind_rows(lapply(VARIANTS, function(v) {
  col = paste0('rf_hadgem_', v)
  if (!col %in% names(d)) return(NULL)
  aggregate_forcing(col) %>% mutate(variant = v)
}))
sens$period = factor(sens$period, levels = labels_period)

# --- modern IPCC agents ------------------------------------------------------
read_erf <- function(p) { x = read_input(run, p, read.csv); x[x$year == 2019, ] }
erf    = read_erf(path_file(paths, 'ipcc_erf'))
erf_lo = read_erf(path_file(paths, 'ipcc_erf_pc05'))
erf_hi = read_erf(path_file(paths, 'ipcc_erf_pc95'))

# Agents and labels exactly as on slide 18: CO2, methane, water vapour,
# albedo (land use), aerosols. 'water vapour' on the slide is ~0.05 W/m2, which
# matches AR6 h2o_stratospheric. n2o and the anthropogenic total are not on the
# slide, so they are dropped from the matching figure.
agents = c(co2 = 'carbon dioxide', ch4 = 'methane', h2o_stratospheric = 'water vapour',
           land_use = 'albedo (land use)', aerosol = 'aerosols')
# The five agents on the slide, with best estimate and 5-95% bounds, as a small table.
modern = data.frame(
  agent = unname(agents[names(agents)]),
  erf   = as.numeric(erf[names(agents)]),
  lo    = as.numeric(erf_lo[names(agents)]),
  hi    = as.numeric(erf_hi[names(agents)]))
modern$agent = factor(modern$agent, levels = modern$agent)
cat('\nIPCC AR6 ERF 1750-2019 (W/m2, global mean):\n'); print(modern, digits = 3)

# --- figures -----------------------------------------------------------------
# Layout matched to slide 18 of dawson_EGU.pptx (ppt/media/image37.png):
# horizontal bars, two stacked panels sharing one x axis, panel labels in strips
# on the right, bars coloured by sign with no legend, ggplot's default two-colour
# hue palette. The slide shows a single kernel, so the matching figure uses
# HadGEM3; the kernel spread is kept as a separate figure.

# Bar colours by sign: ggplot's default two-colour palette, matching the slide.
POS = '#F8766D'; NEG = '#00BFC4'   # ggplot default hue palette for two levels

# Holocene panel data (HadGEM3, global-equivalent), and IPCC panel data, in one shape:
# a label, a value, and which panel. Labels are reversed so the first appears at the top.
hol = main %>%
  filter(kernel == 'hadgem') %>%
  transmute(panel = 'Holocene',
            label = sub(' ka$', '', as.character(period)),
            value = global_equiv)
hol$label = factor(hol$label, levels = rev(sub(' ka$', '', labels_period)))

ipcc = modern %>%
  transmute(panel = 'IPCC', label = as.character(agent), value = erf)
ipcc$label = factor(ipcc$label, levels = rev(as.character(modern$agent)))

both = bind_rows(hol, ipcc)
both$panel = factor(both$panel, levels = c('Holocene', 'IPCC'))
both$sign  = ifelse(both$value >= 0, 'pos', 'neg')

# One panel per block so each can carry its own y-axis title, as on the slide.
# Heights are set to the row counts (7 Holocene periods, 5 IPCC agents) so the
# bars are the same thickness in both, which is what facet_grid(space='free_y')
# would have given.
panel_plot <- function(dat, ytitle, striplab, keep_x) {
  ggplot(dat, aes(x = label, y = value, fill = sign)) +
    geom_col(width = 0.75) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = 'grey20') +
    coord_flip() +
    facet_grid(striplab ~ .) +
    scale_fill_manual(values = c(pos = POS, neg = NEG), guide = 'none') +
    scale_y_continuous(limits = rng, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = ytitle, y = if (keep_x) expression('radiative forcing ('*W/m^2*')') else NULL) +
    theme_bw(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'grey85', colour = 'grey20'),
          strip.text.y     = element_text(size = 12),
          axis.text        = element_text(colour = 'grey20'),
          axis.text.x      = if (keep_x) element_text() else element_blank(),
          axis.ticks.x     = if (keep_x) element_line() else element_blank())
}

# Common x range with a little margin; strip labels and sign flags on both tables.
rng = range(c(both$value, 0)) + c(-0.12, 0.12)
hol$striplab  = 'Holocene'
ipcc$striplab = 'IPCC'
hol$sign  = ifelse(hol$value  >= 0, 'pos', 'neg')
ipcc$sign = ifelse(ipcc$value >= 0, 'pos', 'neg')

# Stack the two panels, heights proportional to their row counts, and save.
p_slide_lab = panel_plot(hol,  'time period (k years)', 'Holocene', FALSE) /
              panel_plot(ipcc, 'forcing agent',         'IPCC',     TRUE) +
              plot_layout(heights = c(nrow(hol), nrow(ipcc)))

ggsave(path_figure(paths, 'forcing_barplot_slide18.pdf'), p_slide_lab, width = 8, height = 6)
ggsave(path_figure(paths, 'forcing_barplot_slide18.png'), p_slide_lab, width = 8, height = 6, dpi = 150)

# kernel spread, same layout, Holocene panel only
hol3 = main %>%
  transmute(kernel, label = sub(' ka$', '', as.character(period)), value = global_equiv)
hol3$label = factor(hol3$label, levels = rev(sub(' ka$', '', labels_period)))
# Labels say which kernel variable was read (R/kernels.R). The 'factor of two'
# title holds only when the kernels mix clear-sky and all-sky, as in the original analysis.
kernel_rows = kernels_read(cfg)
mixed_sky = length(unique(kernel_rows$sky)) > 1
p_kern = ggplot(hol3, aes(x = label, y = value, fill = kernel)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c(hadgem='#1b6ca8', cam5='#5fa8d3', cack='#f2a65a'),
                    labels = c(cack=kernel_rows['cack', 'label'], cam5=kernel_rows['cam5', 'label'],
                               hadgem=kernel_rows['hadgem', 'label']), name = NULL) +
  labs(title = if (mixed_sky) 'Kernel choice moves the answer by about a factor of two' else 'Forcing by kernel',
       subtitle = if (mixed_sky) 'see C3 and C4; CACK is the only all-sky kernel of the three' else 'all three kernels all-sky; see C3 and C4',
       x = 'time period (k years)', y = expression('radiative forcing ('*W/m^2*')')) +
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank())
ggsave(path_figure(paths, 'forcing_barplot_kernel_spread.pdf'), p_kern, width = 8, height = 5)

# The domain-mean version, kept separate because it is not comparable with the IPCC
# global means (A8).
p_dom = ggplot(main %>% filter(kernel=='hadgem'), aes(x = period, y = domain_mean)) +
  geom_col(fill = 'grey55') + geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = 'Domain mean over the study area',
       subtitle = 'NOT comparable with global-mean IPCC ERF; see assumption A8',
       x = NULL, y = expression('W'~m^-2)) +
  theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(path_figure(paths, 'forcing_barplot_domain_mean.pdf'), p_dom, width = 9, height = 5)

# Sensitivity of the answer to the forcing variant (A1/A2).
p_sens = ggplot(sens, aes(x = period, y = global_equiv, fill = variant)) +
  geom_col(position = position_dodge(0.85), width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = 'Sensitivity to the forcing variant (HadGEM3 kernel)',
       subtitle = 'Assumption A1 picks veg_ice_thresh; these are the alternatives',
       x = NULL, y = expression('global-equivalent forcing ('*W~m^-2*')')) +
  theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(path_figure(paths, 'forcing_barplot_variant_sensitivity.pdf'), p_sens, width = 10, height = 5)

# --- the recovered EGU 2024 recipe --------------------------------------------
# Andria's own slide 18 code, git 383002d:scripts/8_radiative.R lines 584-628
# (2 April 2024), rerun on today's data. It differs from A1-A8 above: input is
# script 7's 7 coarse slice-pairs (one per period, so nothing to sum); months
# are feb/may/aug/nov only; binary-flagged ice cells are masked; and the bar is
# the PLAIN UNWEIGHTED MEAN over cells x months, i.e. a domain mean. See C5.
d7 = read_input(run, path_file(paths, 'coarse_differences'))
# Kernel and area per cell-month come from `d`, which has already been trimmed to the
# latitudes of config.R analysis$domain (27-74 N by default) and to period-complete cells
# (A4, A10); the join therefore applies the same trim to the recipe, which Andria's
# April-2024 code did not.
d7 = inner_join(d7, distinct(d, cell_id, month, rk_hadgem, area), by = c('cell_id','month'))
d7$facets = sub(' ka$', '', labels_period)[match(d7$year, sort(unique(d7$year)))]
d7$rf = d7$alb_diff * 100 * d7$rk_hadgem
d7$rf[!is.na(d7$ice_young) | !is.na(d7$ice_old)] = NA
# CM: the recipe needs all four of its months; it is skipped, never computed on a subset,
# when any is missing. It is compared with the slide's seven periods, so it is
# skipped too when the coarse ages in config.R are not the slide's.
egu_months = c('feb','may','aug','nov')
slide_ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)   # slide 18's periods, a fact of the slide
if (!all(egu_months %in% unique(d7$month))) {
  message('EGU 2024 recipe skipped: needs ', paste(egu_months, collapse = '/'), ' in the data')
} else if (!identical(ages_sub, slide_ages)) {
  message('EGU 2024 recipe skipped: the coarse ages in config.R are not the periods of slide 18')
} else {
  egu = d7 %>% filter(month %in% c('feb','may','aug','nov')) %>%
    group_by(facets) %>%
    summarise(mean_forcing  = mean(rf, na.rm = TRUE),
              mean_weighted = sum(rf * area, na.rm = TRUE) / sum(area[!is.na(rf)]),
              n_rows = sum(!is.na(rf)), .groups = 'drop')
  egu$slide18 = c(-0.38, -0.17, 0.22, 0.00, 0.36, 0.37, 0.72)[match(egu$facets, sub(' ka$','',labels_period))]
  hol_egu = egu %>% transmute(panel = 'Holocene', label = facets, value = mean_forcing,
                              striplab = 'Holocene', sign = ifelse(mean_forcing >= 0, 'pos', 'neg'))
  hol_egu$label = factor(hol_egu$label, levels = rev(sub(' ka$', '', labels_period)))
  rng = range(c(hol_egu$value, ipcc$value, 0)) + c(-0.3, 0.3)
  p_egu = panel_plot(hol_egu, 'time period (k years)', 'Holocene', FALSE) /
          panel_plot(ipcc,    'forcing agent',         'IPCC',     TRUE) +
          plot_layout(heights = c(nrow(hol_egu), nrow(ipcc))) +
          plot_annotation(title = "Slide 18 recipe recovered from git 383002d, on today's data",
                          subtitle = 'domain mean, feb/may/aug/nov, ice masked, HadGEM3; NOT global-equivalent')
  ggsave(path_figure(paths, 'forcing_barplot_slide18_egu2024recipe.pdf'), p_egu, width = 8, height = 6.5)
  ggsave(path_figure(paths, 'forcing_barplot_slide18_egu2024recipe.png'), p_egu, width = 8, height = 6.5, dpi = 150)
  write_output(run, egu, path_file(paths, 'forcing_egu2024_recipe'), write.csv, row.names = FALSE)
} # CM: end of the EGU 2024 recipe

# --- numbers out -------------------------------------------------------------
write_output(run, main, path_file(paths, 'forcing_by_period'), write.csv, row.names = FALSE)
write_output(run, sens, path_file(paths, 'forcing_variant_sensitivity'), write.csv, row.names = FALSE)
write_output(run, modern, path_file(paths, 'modern_erf'), write.csv, row.names = FALSE)

# Close the run manifest: closes the plot devices and records the figures written.
run_finish(run)
