# prepare_centre_elevation.R: the original analysis's elevation, one value at each 1-degree
# cell's centre point, from the AWS terrain tiles. Run once, from the repository root, only
# if data/elev_interp_cells.RDS is missing (it is committed, so normally never):
#
#   Rscript tools/prepare_centre_elevation.R
#
# Script 1 reads the file when config.R has analysis$elevation$method = "centre_point" (the
# legacy setting). This lookup used to sit inside script 1 and ran when the file was absent;
# it moved here (Astra's simplicity review, 2026-10-15) so that the pipeline only reads fixed
# inputs and never goes to the network. The query is the original one: elevatr's
# get_elev_point() with src = "aws" at the default zoom, once per distinct cell (the original
# queried once per cell-slice, which gives the same values). Needs the network and the
# elevatr package.

library(elevatr)

OUT_FILE = 'data/elev_interp_cells.RDS'
posts = readRDS('data/veg_posts_interp_ice.RDS')
cells = unique(posts[, c('x', 'y')])
lookup = get_elev_point(cells, prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", src = "aws")
out = data.frame(cells, elev = lookup$elevation)
saveRDS(out, OUT_FILE)
cat(OUT_FILE, ':', nrow(out), 'cells, queried', format(Sys.time(), '%Y-%m-%d %H:%M %Z'),
    'with elevatr', as.character(packageVersion('elevatr')), '\n',
    'Record this date and version in the README input table (elev_interp_cells.RDS).\n')
