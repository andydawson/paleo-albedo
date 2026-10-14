# The radiative-kernel variables the pipeline can read (script 8), with what each one is.
#
# Why: which variable of each kernel file is read decides whether the forcing is clear-sky
# or all-sky, and at the top of the atmosphere or the surface (questions C3, C4). config.R
# names the variable (analysis$kernels); this table gives the legal names, and the labels
# script 9 puts on its kernel figure, so the labels always match what was read (issue I16).
KERNEL_VARIABLES = data.frame(stringsAsFactors = FALSE, rbind(
  #  kernel    variable        sky      level      label
  c('hadgem', 'albedo_sw_cs', 'clear', 'TOA',     'HadGEM3 (clear, TOA)'),
  c('hadgem', 'albedo_sw',    'all',   'TOA',     'HadGEM3 (all-sky, TOA)'),
  c('cam5',   'FSNSC',        'clear', 'surface', 'CAM5 (clear, surface)'),
  c('cam5',   'FSNS',         'all',   'surface', 'CAM5 (all-sky, surface)'),
  c('cam5',   'FSNTC',        'clear', 'TOA',     'CAM5 (clear, TOA)'),
  c('cam5',   'FSNT',         'all',   'TOA',     'CAM5 (all-sky, TOA)'),
  c('cack',   'CACK',         'all',   'TOA',     'CACK (all-sky)'),
  c('cack',   'CACK CM',      'all',   'TOA',     'CACK (all-sky, 2001-2016 mean)')))
names(KERNEL_VARIABLES) = c('kernel', 'variable', 'sky', 'level', 'label')

# The rows of the table for the variables config.R names, one per kernel, by kernel id.
kernels_read = function(cfg) {
  k = cfg$analysis$kernels
  chosen = c(hadgem = k$hadgem_variable, cam5 = k$cam5_variable, cack = k$cack_variable)
  rows = KERNEL_VARIABLES[match(paste(names(chosen), chosen), paste(KERNEL_VARIABLES$kernel, KERNEL_VARIABLES$variable)), ]
  rownames(rows) = rows$kernel
  rows
}
