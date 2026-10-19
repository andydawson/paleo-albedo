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
#   Step 9 (script 10 then breaks its totals down by month and latitude). Script 8 produced a forcing for every cell, month and pair of
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
#   output/forcing/forcing_by_period_variant_sensitivity.csv    same, HadGEM3, for each variant
#                                                 in analysis$forcing$sensitivity_variants
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
# A1. Forcing variant. config.R uses rf_<kernel>_veg_ice_parts: the vegetation plus
#     ice albedo change with partial ice, each cell's albedo being its ice fraction
#     times the fixed glacier albedo plus the rest times its vegetation albedo. Andria's
#     intended headline (her answer to question C1, 2026-10-19). Until then the
#     headline was _veg_ice_thresh, the all-or-nothing version (a cell is all ice
#     above the threshold), which the legacy configuration keeps and A2 shows as a
#     sensitivity. The setting must be one of the four totals (R/config_schema.R):
#     the vegetation-only and ice-only parts appear only in A2.
#
# A2. Variant sensitivity. The variants in analysis$forcing$sensitivity_variants
#     (config.R) are aggregated the same way and plotted side by side
#     (figures/forcing_barplot_variant_sensitivity.pdf): the totals of each ice method,
#     and in config.R also the vegetation and ice parts of the two fixed-albedo totals,
#     Andria's two splits:
#       - all-or-nothing (threshold) split, by kind of step: a slice-pair is an ice step
#         if the cell is ice (above the threshold, readvance-adjusted fractions) at
#         either end, otherwise a vegetation step; _veg_thresh + _ice_thresh =
#         _veg_ice_thresh. This identity holds with difference_method = "endpoint" only.
#       - partial-ice split, by area share: every known pair has both parts, the
#         vegetation change on the share that was ice-free and the change on the share
#         whose ice changed (a signed change: ice retreat or advance);
#         _veg_part + _ice_part = _veg_ice_parts.
#     R/differences.R gives the threshold components NA where they do not apply (a
#     vegetation step has no ice part and vice versa). Until 2026-10-19 those NAs
#     reached the period sum (A5), so a cell with both kinds of step in one period
#     dropped out of BOTH bars: at 8-10 ka, 343 cells carrying 99% of the total
#     (found by Opus and confirmed by Astra on 2026-10-18,
#     docs/cc/2026-10-18_forcing_code_check_codex_gpt6astra.md). Fixed by setting the
#     part that does not apply to 0 before aggregating, the 'by kind of step'
#     definition Chris chose on 2026-10-19 (R/forcing_components.R); genuinely missing
#     pairs (total NA) stay NA. Under the legacy configuration ("legacy_components")
#     the split does not hold (question C2), and Chris chose (2026-10-19) to keep its
#     outputs as they were: there the threshold parts still leave out cells with both
#     kinds of step. The headline (A1) does not use these components and is unaffected.
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
source('R/forcing_periods.R')
source('R/forcing_components.R')
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

# From config.R: Earth's surface area for the global-equivalent normalisation (A8); the
# forcing column to use per kernel, named by kernel (A1, A3); the variants to compare (A2).
EARTH_AREA = cfg$analysis$forcing$earth_area_m2          # m^2
FORCING_COLUMNS    = setNames(paste0('rf_', cfg$analysis$forcing$report_kernels, '_', cfg$analysis$forcing$variant),
                      cfg$analysis$forcing$report_kernels)
VARIANTS   = cfg$analysis$forcing$sensitivity_variants

# Period boundaries, the coarse ages in config.R, and labels made from them (A4).
ages_sub      = cfg$analysis$coarse$ages
labels_period = period_labels(ages_sub)

# The per-cell forcing table from script 8.
forcing_by_cell = read_input(run, path_file(paths, 'forcing'))

# --- A4, A5, A10: the slice-pairs grouped into periods, complete cells only --------
# R/forcing_periods.R (shared with script 10, so its breakdown adds up to these totals).
forcing_by_cell = assign_forcing_periods(forcing_by_cell, ages_sub)

# --- aggregate ---------------------------------------------------------------
# A5 sum over slice-pairs -> A6 mean over months -> A7 area-weighted over cells.
# Takes the per-cell table, the forcing column to aggregate and Earth's area (A8).
aggregate_forcing <- function(forcing_by_cell, forcing_column, earth_area) {
  forcing_by_cell %>%
    group_by(period, cell_id, month, area) %>%
    summarise(rf = sum(.data[[forcing_column]], na.rm = FALSE), .groups = 'drop') %>%   # A5
    group_by(period, cell_id, area) %>%
    summarise(rf = mean(rf, na.rm = TRUE), .groups = 'drop') %>%             # A6
    group_by(period) %>%
    summarise(domain_mean  = weighted.mean(rf, area, na.rm = TRUE),          # A7
              global_equiv = sum(rf * area, na.rm = TRUE) / earth_area,      # A8
              area_m2      = sum(area), .groups = 'drop')
}

# Run the aggregation once per kernel and stack the results; order the periods.
forcing_by_period = bind_rows(lapply(names(FORCING_COLUMNS), function(kernel_id)
  aggregate_forcing(forcing_by_cell, FORCING_COLUMNS[[kernel_id]], EARTH_AREA) %>% mutate(kernel = kernel_id)))
forcing_by_period$period = factor(forcing_by_period$period, levels = labels_period)

# The study area comes to 3.8% of Earth's surface; print it and the main table.
cat('\nstudy area as a fraction of Earth:',
    signif(unique(forcing_by_period$area_m2)[1] / EARTH_AREA, 3), '\n\n')
print(as.data.frame(forcing_by_period %>% select(kernel, period, domain_mean, global_equiv)), digits = 3)

# --- A2: sensitivity to the forcing variant, HadGEM3 only --------------------
# The threshold parts with 0 where they do not apply, so that they add up to their total
# over a period (R/forcing_components.R; unchanged under the legacy configuration).
forcing_by_cell_components = complete_threshold_components(forcing_by_cell, 'hadgem',
                                                           cfg$analysis$ice$difference_method)

variant_sensitivity = bind_rows(lapply(VARIANTS, function(variant_id) {
  forcing_column = paste0('rf_hadgem_', variant_id)
  if (!forcing_column %in% names(forcing_by_cell_components)) return(NULL)
  aggregate_forcing(forcing_by_cell_components, forcing_column, EARTH_AREA) %>% mutate(variant = variant_id)
}))
variant_sensitivity$period = factor(variant_sensitivity$period, levels = labels_period)

# --- modern IPCC agents ------------------------------------------------------
read_erf <- function(path) { erf_history = read_input(run, path, read.csv); erf_history[erf_history$year == 2019, ] }
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
# Plain labels for the forcing variants, in this order: the sensitivity figure's legend,
# and the line that names the headline's ice method on the headline figures (the current
# and legacy configurations report different totals under the same file names). A variant
# not listed keeps its column name. The headline is always one of the four totals
# (R/config_schema.R), all listed here.
variant_labels = c(veg_ice_thresh   = 'total, all-or-nothing ice, fixed glacier albedo',
                   veg_thresh       = 'vegetation part, all-or-nothing ice',
                   ice_thresh       = 'ice part, all-or-nothing ice',
                   veg_ice_parts    = 'total, partial ice, fixed glacier albedo',
                   veg_part         = 'vegetation part, partial ice',
                   ice_part         = 'ice part, partial ice',
                   veg_icesc_thresh = 'total, all-or-nothing ice, month-by-month glacier albedo',
                   veg_icesc_parts  = 'total, partial ice, month-by-month glacier albedo')
headline_line = paste('Ice method:', variant_labels[[cfg$analysis$forcing$variant]])

# Layout matched to slide 18 of dawson_EGU.pptx (ppt/media/image37.png):
# horizontal bars, two stacked panels sharing one x axis, panel labels in strips
# on the right, bars coloured by sign with no legend, ggplot's default two-colour
# hue palette. The slide shows a single kernel, so the matching figure uses
# HadGEM3; the kernel spread is kept as a separate figure.

# Bar colours by sign: ggplot's default two-colour palette, matching the slide.
POS = '#F8766D'; NEG = '#00BFC4'   # ggplot default hue palette for two levels

# Holocene panel data (HadGEM3, global-equivalent), and IPCC panel data, in one shape:
# a label, a value, and which panel. Labels are reversed so the first appears at the top.
holocene_panel = forcing_by_period %>%
  filter(kernel == 'hadgem') %>%
  transmute(panel = 'Holocene',
            label = sub(' ka$', '', as.character(period)),
            value = global_equiv)
holocene_panel$label = factor(holocene_panel$label, levels = rev(sub(' ka$', '', labels_period)))

ipcc = modern %>%
  transmute(panel = 'IPCC', label = as.character(agent), value = erf)
ipcc$label = factor(ipcc$label, levels = rev(as.character(modern$agent)))

forcing_panels = bind_rows(holocene_panel, ipcc)
forcing_panels$panel = factor(forcing_panels$panel, levels = c('Holocene', 'IPCC'))
forcing_panels$sign  = ifelse(forcing_panels$value >= 0, 'pos', 'neg')

# One panel per block so each can carry its own y-axis title, as on the slide.
# Heights are set to the row counts (7 Holocene periods, 5 IPCC agents) so the
# bars are the same thickness in both, which is what facet_grid(space='free_y')
# would have given.
# axis_limits: the forcing axis range, shared by the panels stacked together.
panel_plot <- function(panel_data, ytitle, striplab, keep_x, axis_limits) {
  ggplot(panel_data, aes(x = label, y = value, fill = sign)) +
    geom_col(width = 0.75) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = 'grey20') +
    coord_flip() +
    facet_grid(striplab ~ .) +
    scale_fill_manual(values = c(pos = POS, neg = NEG), guide = 'none') +
    scale_y_continuous(limits = axis_limits, expand = expansion(mult = c(0.02, 0.02))) +
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
forcing_axis_limits = range(c(forcing_panels$value, 0)) + c(-0.12, 0.12)
holocene_panel$striplab  = 'Holocene'
ipcc$striplab = 'IPCC'
holocene_panel$sign  = ifelse(holocene_panel$value  >= 0, 'pos', 'neg')
ipcc$sign = ifelse(ipcc$value >= 0, 'pos', 'neg')

# Stack the two panels, heights proportional to their row counts, and save.
p_slide_lab = panel_plot(holocene_panel,  'time period (k years)', 'Holocene', FALSE, forcing_axis_limits) /
              panel_plot(ipcc, 'forcing agent',         'IPCC',     TRUE, forcing_axis_limits) +
              plot_layout(heights = c(nrow(holocene_panel), nrow(ipcc))) +
              plot_annotation(subtitle = paste0(headline_line, ' (Holocene panel, HadGEM3 kernel)'))

ggsave(path_figure(paths, 'forcing_barplot_slide18.pdf'), p_slide_lab, width = 8, height = 6)
ggsave(path_figure(paths, 'forcing_barplot_slide18.png'), p_slide_lab, width = 8, height = 6, dpi = 150)

# kernel spread, same layout, Holocene panel only
kernel_comparison = forcing_by_period %>%
  transmute(kernel, label = sub(' ka$', '', as.character(period)), value = global_equiv)
kernel_comparison$label = factor(kernel_comparison$label, levels = rev(sub(' ka$', '', labels_period)))
# Labels say which kernel variable was read (R/kernels.R). The 'factor of two'
# title holds only when the kernels mix clear-sky and all-sky, as in the original analysis.
kernel_rows = kernels_read(cfg)
mixed_sky = length(unique(kernel_rows$sky)) > 1
p_kern = ggplot(kernel_comparison, aes(x = label, y = value, fill = kernel)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c(hadgem='#1b6ca8', cam5='#5fa8d3', cack='#f2a65a'),
                    labels = c(cack=kernel_rows['cack', 'label'], cam5=kernel_rows['cam5', 'label'],
                               hadgem=kernel_rows['hadgem', 'label']), name = NULL) +
  labs(title = if (mixed_sky) 'Kernel choice moves the answer by about a factor of two' else 'Forcing by kernel',
       subtitle = paste0(if (mixed_sky) 'see C3 and C4; CACK is the only all-sky kernel of the three' else 'all three kernels all-sky; see C3 and C4',
                         '\n', headline_line),
       x = 'time period (k years)', y = expression('radiative forcing ('*W/m^2*')')) +
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank())
ggsave(path_figure(paths, 'forcing_barplot_kernel_spread.pdf'), p_kern, width = 8, height = 5)

# The domain-mean version, kept separate because it is not comparable with the IPCC
# global means (A8).
p_dom = ggplot(forcing_by_period %>% filter(kernel=='hadgem'), aes(x = period, y = domain_mean)) +
  geom_col(fill = 'grey55') + geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = 'Domain mean over the study area',
       subtitle = paste0('NOT comparable with global-mean IPCC ERF; see assumption A8\n', headline_line),
       x = NULL, y = expression('W'~m^-2)) +
  theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(path_figure(paths, 'forcing_barplot_domain_mean.pdf'), p_dom, width = 9, height = 5)

# Sensitivity of the answer to the forcing variant (A1/A2), with the plain labels above.
sens_plot = variant_sensitivity
sens_plot$label = ifelse(sens_plot$variant %in% names(variant_labels),
                         variant_labels[sens_plot$variant], sens_plot$variant)
sens_plot$label = factor(sens_plot$label, levels = unique(c(variant_labels[names(variant_labels) %in% sens_plot$variant],
                                                            sens_plot$label)))
sens_subtitle = if (cfg$analysis$ice$difference_method == 'endpoint')
  'Totals, and the vegetation and ice parts that add up to them; the headline is one of the totals (A1)' else
  'Totals and parts; with the original arithmetic the all-or-nothing parts leave out cells with both kinds of step (A2)'
p_sens = ggplot(sens_plot, aes(x = period, y = global_equiv, fill = label)) +
  geom_col(position = position_dodge(0.85), width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(title = 'Forcing by ice method, and its vegetation and ice parts (HadGEM3 kernel)',
       subtitle = sens_subtitle, fill = NULL,
       x = NULL, y = expression('global-equivalent forcing ('*W~m^-2*')')) +
  theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(path_figure(paths, 'forcing_barplot_variant_sensitivity.pdf'), p_sens, width = 10, height = 5)

# --- the recovered EGU 2024 recipe --------------------------------------------
# Andria's own slide 18 code, git 383002d:scripts/8_radiative.R lines 584-628
# (2 April 2024), rerun on today's data. It differs from A1-A8 above: input is
# script 7's 7 coarse slice-pairs (one per period, so nothing to sum); months
# are feb/may/aug/nov only; binary-flagged ice cells are masked; and the bar is
# the PLAIN UNWEIGHTED MEAN over cells x months, i.e. a domain mean. See C5.
coarse_differences = read_input(run, path_file(paths, 'coarse_differences'))
# Kernel and area per cell-month come from `d`, which has already been trimmed to the
# latitudes of config.R analysis$domain (27-74 N by default) and to period-complete cells
# (A4, A10); the join therefore applies the same trim to the recipe, which Andria's
# April-2024 code did not.
coarse_differences = inner_join(coarse_differences, distinct(forcing_by_cell, cell_id, month, rk_hadgem, area), by = c('cell_id','month'))
coarse_differences$facets = sub(' ka$', '', labels_period)[match(coarse_differences$year, sort(unique(coarse_differences$year)))]
coarse_differences$rf = coarse_differences$alb_diff * 100 * coarse_differences$rk_hadgem
coarse_differences$rf[!is.na(coarse_differences$ice_young) | !is.na(coarse_differences$ice_old)] = NA
# CM: the recipe needs all four of its months; it is skipped, never computed on a subset,
# when any is missing. It is compared with the slide's seven periods, so it is
# skipped too when the coarse ages in config.R are not the slide's.
egu_months = c('feb','may','aug','nov')
slide_ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)   # slide 18's periods, a fact of the slide
if (!all(egu_months %in% unique(coarse_differences$month))) {
  message('EGU 2024 recipe skipped: needs ', paste(egu_months, collapse = '/'), ' in the data')
} else if (!identical(ages_sub, slide_ages)) {
  message('EGU 2024 recipe skipped: the coarse ages in config.R are not the periods of slide 18')
} else {
  egu = coarse_differences %>% filter(month %in% c('feb','may','aug','nov')) %>%
    group_by(facets) %>%
    summarise(mean_forcing  = mean(rf, na.rm = TRUE),
              mean_weighted = sum(rf * area, na.rm = TRUE) / sum(area[!is.na(rf)]),
              n_rows = sum(!is.na(rf)), .groups = 'drop')
  egu$slide18 = c(-0.38, -0.17, 0.22, 0.00, 0.36, 0.37, 0.72)[match(egu$facets, sub(' ka$','',labels_period))]
  hol_egu = egu %>% transmute(panel = 'Holocene', label = facets, value = mean_forcing,
                              striplab = 'Holocene', sign = ifelse(mean_forcing >= 0, 'pos', 'neg'))
  hol_egu$label = factor(hol_egu$label, levels = rev(sub(' ka$', '', labels_period)))
  forcing_axis_limits = range(c(hol_egu$value, ipcc$value, 0)) + c(-0.3, 0.3)
  p_egu = panel_plot(hol_egu, 'time period (k years)', 'Holocene', FALSE, forcing_axis_limits) /
          panel_plot(ipcc,    'forcing agent',         'IPCC',     TRUE, forcing_axis_limits) +
          plot_layout(heights = c(nrow(hol_egu), nrow(ipcc))) +
          plot_annotation(title = "Slide 18 recipe recovered from git 383002d, on today's data",
                          subtitle = 'domain mean, feb/may/aug/nov, ice masked, HadGEM3; NOT global-equivalent')
  ggsave(path_figure(paths, 'forcing_barplot_slide18_egu2024recipe.pdf'), p_egu, width = 8, height = 6.5)
  ggsave(path_figure(paths, 'forcing_barplot_slide18_egu2024recipe.png'), p_egu, width = 8, height = 6.5, dpi = 150)
  write_output(run, egu, path_file(paths, 'forcing_egu2024_recipe'), write.csv, row.names = FALSE)
} # CM: end of the EGU 2024 recipe

# --- numbers out -------------------------------------------------------------
write_output(run, forcing_by_period, path_file(paths, 'forcing_by_period'), write.csv, row.names = FALSE)
write_output(run, variant_sensitivity, path_file(paths, 'forcing_variant_sensitivity'), write.csv, row.names = FALSE)
write_output(run, modern, path_file(paths, 'modern_erf'), write.csv, row.names = FALSE)

# Close the run manifest: closes the plot devices and records the figures written.
run_finish(run)
