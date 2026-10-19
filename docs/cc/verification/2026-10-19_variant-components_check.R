# Check for the variant-components fix (2026-10-19), current configuration: script 9 rerun on
# the per-cell forcing table of the full run of 2026-10-18. Run from the worktree root:
#   Rscript <this file> <run folder>
#  1. the headline table is byte-identical to that run's (the fix must not touch it);
#  2. in the sensitivity table each split adds up to its total in every period:
#     veg_thresh + ice_thresh = veg_ice_thresh and veg_part + ice_part = veg_ice_parts;
#  3. every variant counts the same cells in every period (cells whose period forcing is
#     known), so no cell drops out of a part; the reported area_m2 cannot show this, since it
#     sums every cell's area whether or not its forcing is known;
#  4. the HadGEM3 values match two one-off calculations made before the fix, in the same
#     session but from the per-cell table directly: the split by kind of step
#     (scratchpad fullrun/ice_vs_vegetation.R) and by area share (fullrun/parts_split.R).
#     Their values are written out below, so this check does not depend on those files.
args = commandArgs(TRUE)
run_dir = args[1]
ref_run = '/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/forcing'
fail = character()
check = function(ok, what) { cat(if (ok) 'PASS' else 'FAIL', what, '\n'); if (!ok) fail <<- c(fail, what) }
periods = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')

new_headline = file.path(run_dir, 'output/forcing/forcing_by_period.csv')
check(identical(unname(tools::md5sum(new_headline)), unname(tools::md5sum(file.path(ref_run, 'forcing_by_period.csv')))),
      'headline forcing_by_period.csv byte-identical to the 18 October run')

s = read.csv(file.path(run_dir, 'output/forcing/forcing_by_period_variant_sensitivity.csv'))
wide = function(col) {
  w = reshape(s[c('period', 'variant', col)], idvar = 'period', timevar = 'variant', direction = 'wide')
  names(w) = sub(paste0(col, '.'), '', names(w), fixed = TRUE)
  w[match(periods, w$period), ]
}
g = wide('global_equiv'); d = wide('domain_mean')
parts = c('veg_thresh', 'ice_thresh', 'veg_ice_thresh', 'veg_part', 'ice_part', 'veg_ice_parts')
check(!anyNA(g$period) && !anyNA(g[parts]), 'seven periods, no missing component values')
check(max(abs(g$veg_thresh + g$ice_thresh - g$veg_ice_thresh)) < 1e-9, 'veg_thresh + ice_thresh = veg_ice_thresh (global equivalent)')
check(max(abs(g$veg_part + g$ice_part - g$veg_ice_parts)) < 1e-9, 'veg_part + ice_part = veg_ice_parts (global equivalent)')

# 3. Cells counted per variant and period, aggregated as script 9 does (A5 sum over pairs,
#    A6 mean over months); a cell counts if its period value is known.
suppressMessages(library(dplyr))
source('R/calendar.R'); source('R/forcing_periods.R'); source('R/forcing_components.R')
cfg = source('config.R')$value
f = assign_forcing_periods(readRDS(file.path(run_dir, 'output/forcing/RF_holocene_all_cases.RDS')), cfg$analysis$coarse$ages)
f = complete_threshold_components(f, 'hadgem', cfg$analysis$ice$difference_method)
counts = sapply(cfg$analysis$forcing$sensitivity_variants, function(v) {
  f %>% group_by(period, cell_id, month) %>% summarise(rf = sum(.data[[paste0('rf_hadgem_', v)]]), .groups = 'drop') %>%
    group_by(period, cell_id) %>% summarise(rf = mean(rf, na.rm = TRUE), .groups = 'drop') %>%
    group_by(period) %>% summarise(n = sum(is.finite(rf)), .groups = 'drop') %>% arrange(period) %>% pull(n)
})
print(counts)
check(all(apply(counts, 1, function(r) length(unique(r)) == 1)), 'every variant counts the same cells in every period')

# 4. The one-off values (HadGEM3; periods as above, youngest first).
ref = list(
  veg_thresh = list(dm = c(-0.700555545280371, -0.675938448162886, 0.477790790648216, -0.710404574188086, -0.513664490889871, -0.76462214850055, 1.34373068469878),
                    ge = c(-0.026401410688426, -0.0254736811238909, 0.0180062108879645, -0.0267725850497262, -0.0193581612070707, -0.0288158497923966, 0.0506403609255198)),
  ice_thresh = list(dm = c(0, 0.0123075402376368, 0.00943392441253101, 0.0847502375751962, 1.56139890802836, 5.66748950771941, 2.93058495242393),
                    ge = c(0, 0.000463826782283386, 0.000355530570697458, 0.00319393065009419, 0.058843490850991, 0.213587229554741, 0.110443172432959)),
  veg_part   = list(dm = c(-0.69241820894531, -0.674720140877716, 0.473838966195172, -0.695693794069519, -0.511212257408083, -0.750334624027291, 1.34325787347067),
                    ge = c(-0.0260947438438922, -0.0254277675183286, 0.0178572808836896, -0.0262181888279363, -0.0192657453755313, -0.0282774045486458, 0.0506225423763763)),
  ice_part   = list(dm = c(0, 0.00648931903334865, 0.0146132127842187, 0.120626718913705, 1.61319824576618, 5.29686115309329, 3.0860322555639),
                    ge = c(0, 0.000244559018969852, 0.00055071926101036, 0.00454598577870578, 0.0607956209828815, 0.199619584206443, 0.116301420388107)))
for (v in names(ref))
  check(max(abs(d[[v]] - ref[[v]]$dm)) < 1e-9 && max(abs(g[[v]] - ref[[v]]$ge)) < 1e-9,
        paste(v, 'matches the one-off calculation'))

cat('\nHadGEM3, study-area mean (W/m2):\n'); print(data.frame(period = d$period, d[parts]), digits = 3)
cat('\nHadGEM3, global equivalent (mW/m2):\n'); print(data.frame(period = g$period, round(1000 * g[parts], 1)))
if (length(fail)) { cat('\nFAILED:', paste(fail, collapse = '; '), '\n'); quit(status = 1) }
