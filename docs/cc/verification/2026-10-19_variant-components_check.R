# Check for the variant-components fix (2026-10-19): script 9 rerun on the per-cell forcing
# table of the full run of 2026-10-18. Usage: Rscript <this file> <run folder>
#  1. the headline table is byte-identical to that run's (the fix must not touch it);
#  2. in the new sensitivity table each split adds up to its total in every period:
#     veg_thresh + ice_thresh = veg_ice_thresh and veg_part + ice_part = veg_ice_parts;
#  3. the HadGEM3 values match two independent one-off calculations made before the fix:
#     ice_vs_vegetation.csv (split by kind of step) and parts_split.csv (split by area share).
args = commandArgs(TRUE)
run_dir = args[1]
ref_run = '/home/expert/Documents/contracting/Andria/paleo-RF-verify/4ba4d2a/.rundirs/full-current-2026-10-18/output/forcing'
one_off = '/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/fullrun'
fail = character()
check = function(ok, what) { cat(if (ok) 'PASS' else 'FAIL', what, '\n'); if (!ok) fail <<- c(fail, what) }

new_headline = file.path(run_dir, 'output/forcing/forcing_by_period.csv')
check(identical(unname(tools::md5sum(new_headline)), unname(tools::md5sum(file.path(ref_run, 'forcing_by_period.csv')))),
      'headline forcing_by_period.csv byte-identical to the 18 October run')

s = read.csv(file.path(run_dir, 'output/forcing/forcing_by_period_variant_sensitivity.csv'))
wide = function(col) { w = reshape(s[c('period', 'variant', col)], idvar = 'period', timevar = 'variant', direction = 'wide'); names(w) = sub(paste0(col, '.'), '', names(w), fixed = TRUE); w }
g = wide('global_equiv'); d = wide('domain_mean')
check(nrow(g) == 7 && !anyNA(g[c('veg_thresh', 'ice_thresh', 'veg_ice_thresh', 'veg_part', 'ice_part', 'veg_ice_parts')]),
      'seven periods, no missing component values')
check(max(abs(g$veg_thresh + g$ice_thresh - g$veg_ice_thresh)) < 1e-9, 'veg_thresh + ice_thresh = veg_ice_thresh (global equivalent)')
check(max(abs(g$veg_part + g$ice_part - g$veg_ice_parts)) < 1e-9, 'veg_part + ice_part = veg_ice_parts (global equivalent)')
check(length(unique(s$area_m2)) == 1, 'every variant covers the same area')

# Against the one-off calculations (HadGEM3).
compare = function(ref, part, variant, what) {
  r = ref[ref$part == part, ]
  m = merge(r, data.frame(period = d$period, new_dm = d[[variant]], new_ge = g[[variant]]), by = 'period')
  check(nrow(m) == 7 && max(abs(m$domain_mean - m$new_dm)) < 1e-9 && max(abs(m$global_equiv - m$new_ge)) < 1e-9, what)
}
kind = read.csv(file.path(one_off, 'ice_vs_vegetation.csv')); kind = kind[kind$kernel == 'hadgem', ]
compare(kind, 'vegetation', 'veg_thresh', 'veg_thresh matches the by-kind-of-step one-off (vegetation)')
compare(kind, 'ice', 'ice_thresh', 'ice_thresh matches the by-kind-of-step one-off (ice)')
share = read.csv(file.path(one_off, 'parts_split.csv'))
compare(share, 'vegetation', 'veg_part', 'veg_part matches the area-share one-off (vegetation)')
compare(share, 'ice', 'ice_part', 'ice_part matches the area-share one-off (ice)')

cat('\nHadGEM3, study-area mean (W/m2) and global equivalent (mW/m2):\n')
out = data.frame(period = d$period,
                 veg_thresh = d$veg_thresh, ice_thresh = d$ice_thresh, veg_ice_thresh = d$veg_ice_thresh,
                 veg_part = d$veg_part, ice_part = d$ice_part, veg_ice_parts = d$veg_ice_parts)
print(out, digits = 3)
print(data.frame(period = g$period, round(1000 * g[c('veg_thresh', 'ice_thresh', 'veg_ice_thresh', 'veg_part', 'ice_part', 'veg_ice_parts')], 1)))
if (length(fail)) { cat('\nFAILED:', paste(fail, collapse = '; '), '\n'); quit(status = 1) }
