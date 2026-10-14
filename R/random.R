# Random draws in the pipeline: gratia's simulate() in scripts 5 and 6 (issue I04a).
#
# Why: the draws were unseeded, so no run could be repeated exactly. config.R's
# analysis$draws$seed is now either a whole number, which makes the draws repeatable, or
# "random", in which case a new base seed is made at the start of the run and recorded in
# its manifest, so that run too can be repeated by copying the number into the config.
#
# Each simulate() call gets its own seed, derived from the base seed, the script, the
# month and the call. So a month's draws do not depend on which other months are run or in
# what order, and adding a call does not shift the draws of the others. The seed is given
# through simulate()'s own `seed` argument, which gratia applies for that call only and
# then restores R's random-number state; nothing calls set.seed() globally.
#
# Repeating a seeded run gives identical draws on the same machine and thread count; on
# another machine the predictions they are drawn around can differ in the last bit.

# The base seed for this run: the configured number, or for "random" one made from the
# clock and the process id.
base_seed = function(setting) {
  if (!identical(setting, 'random')) return(as.integer(setting))
  derive_seed(format(Sys.time(), '%Y%m%d%H%M%OS6'), Sys.getpid())
}

# A seed from any values (base seed, script, month, call): a fixed hash of their text, a
# whole number from 0 to 2147483646. Base R only, so it is the same on every system.
derive_seed = function(...) {
  h = 0
  for (code in utf8ToInt(paste(..., sep = '|'))) h = (h * 31 + code) %% 2147483647
  as.integer(h)
}

# The draw number of each simulated value, from the column names simulate()'s draws get
# in a data frame ("V1" ... or "X1" ...). Replaces substr(iter, 2, 4), which read draw
# 1000 as 100.
draw_index = function(names) as.numeric(sub('^[^0-9]+', '', as.character(names)))
