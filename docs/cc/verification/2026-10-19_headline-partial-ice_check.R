# Check for the switch of the headline forcing to partial ice (2026-10-19), current
# configuration: scripts 9 and 10 rerun on the per-cell forcing table of the full run of
# 2026-10-18. Run from the worktree root:
#   Rscript <this file> <run folder>
#  1. the headline table holds the partial-ice totals (veg_ice_parts) for all three kernels:
#     recomputed here from the per-cell table with script 9's aggregation (A5 sum over pairs,
#     A6 mean over months, A7/A8 area weighting), and for HadGEM3 equal to the veg_ice_parts
#     rows of the sensitivity table;
#  2. the headline did change (it is no longer the 18 October all-or-nothing table);
#  3. everything that does not depend on the headline setting is byte-identical: the EGU
#     recipe and IPCC tables to the 18 October run, the sensitivity table to the run that
#     verified the variant-components fix (2026-10-19_variant-components-4);
#  4. script 10's breakdown names the new variant (it stops by itself if its parts do not
#     add up to script 9's totals);
#  5. (added with the second run, after the review's comment fixes and the figure subtitles)
#     every table byte-identical to the first verification run of this change (4672560):
#     comments and figure subtitles must not move a number.
args = commandArgs(TRUE)
run_dir = args[1]
ref_run  = '/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/forcing'
ref_sens = '/home/expert/Documents/contracting/Andria/paleo-RF-verify/2bd4ab5/.rundirs/variant-components-4/output/forcing'
ref_first = '/home/expert/Documents/contracting/Andria/paleo-RF-verify/4672560/.rundirs/headline-partial-ice/output/forcing'
out = file.path(run_dir, 'output/forcing')
fail = character()
check = function(ok, what) { cat(if (ok) 'PASS' else 'FAIL', what, '\n'); if (!ok) fail <<- c(fail, what) }
same_file = function(a, b) identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))

suppressMessages(library(dplyr))
source('R/calendar.R'); source('R/forcing_periods.R')
cfg = source('config.R')$value
check(identical(cfg$analysis$forcing$variant, 'veg_ice_parts'), 'config.R headline is veg_ice_parts')

# 1. Recompute the partial-ice totals from the per-cell table.
f = assign_forcing_periods(readRDS(file.path(out, 'RF_holocene_all_cases.RDS')), cfg$analysis$coarse$ages)
expected = bind_rows(lapply(c('hadgem', 'cam5', 'cack'), function(k)
  f %>% group_by(period, cell_id, month, area) %>% summarise(rf = sum(.data[[paste0('rf_', k, '_veg_ice_parts')]]), .groups = 'drop') %>%
    group_by(period, cell_id, area) %>% summarise(rf = mean(rf, na.rm = TRUE), .groups = 'drop') %>%
    group_by(period) %>% summarise(domain_mean = weighted.mean(rf, area, na.rm = TRUE),
                                   global_equiv = sum(rf * area, na.rm = TRUE) / cfg$analysis$forcing$earth_area_m2,
                                   .groups = 'drop') %>%
    mutate(kernel = k, period = as.character(period))))
headline = read.csv(file.path(out, 'forcing_by_period.csv'))
m = inner_join(headline, expected, by = c('kernel', 'period'), suffix = c('', '_expected'))
check(nrow(headline) == 21 && nrow(m) == 21, 'headline has 3 kernels x 7 periods, all matched')
check(max(abs(m$domain_mean - m$domain_mean_expected)) < 1e-12 && max(abs(m$global_equiv - m$global_equiv_expected)) < 1e-12,
      'headline equals the partial-ice totals recomputed from the per-cell table (all kernels)')
sens = read.csv(file.path(out, 'forcing_by_period_variant_sensitivity.csv')) %>% filter(variant == 'veg_ice_parts')
h = headline %>% filter(kernel == 'hadgem') %>% inner_join(sens, by = 'period', suffix = c('', '_sens'))
check(nrow(h) == 7 && max(abs(h$global_equiv - h$global_equiv_sens)) < 1e-12 && max(abs(h$domain_mean - h$domain_mean_sens)) < 1e-12,
      'HadGEM3 headline equals the veg_ice_parts rows of the sensitivity table')
h810 = headline %>% filter(kernel == 'hadgem', period == '8 - 10 ka')
check(abs(h810$domain_mean - 4.547) < 0.001 && abs(h810$global_equiv - 0.1713) < 0.0001,
      'HadGEM3 8-10 ka as measured on 19 October (study-area mean 4.547, global equivalent 0.1713)')

# 2. The headline did change.
check(!same_file(file.path(out, 'forcing_by_period.csv'), file.path(ref_run, 'forcing_by_period.csv')),
      'headline differs from the 18 October all-or-nothing table')

# 3. Unaffected tables, byte for byte.
for (t in c('forcing_by_period_egu2024recipe_vs_slide.csv', 'modern_ipcc_ar6_erf.csv'))
  check(same_file(file.path(out, t), file.path(ref_run, t)), paste(t, 'byte-identical to the 18 October run'))
check(same_file(file.path(out, 'forcing_by_period_variant_sensitivity.csv'), file.path(ref_sens, 'forcing_by_period_variant_sensitivity.csv')),
      'sensitivity table byte-identical to the variant-components-4 run')

# 4. Script 10.
b = read.csv(file.path(out, 'forcing_by_month_latitude.csv'))
check(nrow(b) > 0 && all(b$variant == 'veg_ice_parts'), "script 10's breakdown is of veg_ice_parts")

# 5. Unchanged from the first verification run of this change, every table.
if (dir.exists(ref_first) && !identical(normalizePath(out), normalizePath(ref_first)))
  for (t in c('forcing_by_period.csv', 'forcing_by_period_variant_sensitivity.csv',
              'forcing_by_period_egu2024recipe_vs_slide.csv', 'modern_ipcc_ar6_erf.csv', 'forcing_by_month_latitude.csv'))
    check(same_file(file.path(out, t), file.path(ref_first, t)), paste(t, 'byte-identical to the first run (4672560)'))

cat('\nNew headline (study-area mean W/m2; global equivalent W/m2):\n')
print(as.data.frame(headline %>% select(kernel, period, domain_mean, global_equiv)), digits = 4)
if (length(fail)) stop(length(fail), ' check(s) failed')
cat('all checks passed\n')
