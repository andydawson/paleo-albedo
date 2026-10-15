# I16c elevation (current): script 1 under config.R reads the land-mean elevation table. No
# anchor exists for the new values: the run must complete (script 1 stops if any cell lacks
# an elevation), and the comparison with the anchored tables (report only) must show the elev
# column changed and the land-cover columns identical.
list(
  name    = 'i16c-elevation-current-2',
  commit  = 'i16c-elevation',
  config  = 'config.R',
  scripts = 'scripts/1_veg_lct_prep.R',
  checks  = list(c('tools/compare_tables.R', 'tests/anchors/interp-allmonths-2026-09-20/lct_modern_reveals_interp.RDS', '.rundirs/i16c-elevation-current-2/data/lct_modern_reveals_interp.RDS'),
                 c('tools/compare_tables.R', 'tests/anchors/interp-allmonths-2026-09-20/lct_paleo_reveals_interp.RDS',  '.rundirs/i16c-elevation-current-2/data/lct_paleo_reveals_interp.RDS')),
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '4', CORES = '12-15'))
