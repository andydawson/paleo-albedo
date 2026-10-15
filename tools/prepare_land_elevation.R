# prepare_land_elevation.R: elevation of each 1-degree cell as the mean over its non-ocean
# area, from the GMTED2010 30-arc-second mean-elevation grid and the MODIS MOD44W water
# mask. Run once, from the repository root:
#
#   Rscript tools/prepare_land_elevation.R
#
# It writes data/elev_land_mean_cells.csv (committed), which script 1 reads when config.R
# has analysis$elevation$method = "land_mean"; the original centre-point elevation
# (data/elev_interp_cells.RDS) stays the legacy setting.
#
# Why this method (Chris, 2026-10-14 and 2026-10-15; question B9). The land-cover fractions
# describe a whole cell, so the elevation beside them should too, not the one point under
# the cell's centre (which for coastal cells can be sea floor). Published practice averages
# a fine DEM over a cell's non-ocean pixels and keeps lake surfaces:
#   - New, Lister, Hulme & Makin (2002), Climate Research 21:1-25, doi:10.3354/cr021001, p. 7:
#     each land cell's elevation is the average of 100-400 GLOBE 1 km points.
#   - ISLSCP II HYDRO1k elevation products (Verdin 2011, doi:10.3334/ORNLDAAC/1007, sec.
#     9.2.1): "mean elevation value within each cell" over "the non-ocean pixels".
# So the ocean is left out and lakes stay in (no precedent removes lakes from elevation).
# The record of the literature check is docs/cc/2026-10-15_land_mean_elevation_precedents.md.
#
# Inputs (not in git; see data/gmted2010/README.md and data/mod44w/README.md):
#   - GMTED2010 30-arc-second mean elevation, mn30_grd (Danielson & Gesch 2011, USGS Open-File
#     Report 2011-1073, doi:10.3133/ofr20111073), unzipped into data/gmted2010/mn30_grd/.
#   - MOD44W v6.1 2015 water mask and its QA layer (Carroll et al. 2017,
#     doi:10.1080/17538947.2016.1232756), in data/mod44w/2015/.
#   - The cells: the (x, y) centres in data/elev_interp_cells.RDS, the cells script 1 uses.
#
# The steps:
#   1. Ocean at 250 m: a MOD44W pixel is ocean if it is water (water_mask = 1) and its QA
#      class is 4, "Ocean Mask". Lakes are water in other QA classes and stay in.
#      (Checked 2026-10-15: Hudson Bay, Chesapeake, Puget Sound, San Francisco and Florida
#      Bays and the Alaska coast are almost all class 4; Lake Erie has no class 4.)
#   2. Ocean share of each 30-arc-second GMTED pixel: the 250 m ocean indicator averaged
#      onto the GMTED grid.
#   3. Weight of each GMTED pixel = its area x (1 - ocean share). No cut-off is needed:
#      a pixel that is half sea counts half.
#   4. Elevation of a 1-degree cell = the weighted mean of its GMTED pixels; the cell's
#      non-ocean share = summed weight / summed area.
# A cell with no non-ocean area would have no elevation: the tool stops and lists it.

library(terra)

GMTED_DIR   = 'data/gmted2010/mn30_grd'
MOD44W_DIR  = 'data/mod44w/2015'
CELLS_FILE  = 'data/elev_interp_cells.RDS'
OUT_FILE    = 'data/elev_land_mean_cells.csv'
OCEAN_QA    = 4        # MOD44W QA lookup: 4 = "Ocean Mask"
CELL_SIZE   = 1        # degrees; the land-cover grid

started = Sys.time()
terraOptions(memfrac = 0.5, progress = 0)

# The cells and the box that holds them, with whole cells around each centre.
cells = readRDS(CELLS_FILE)[, c('x', 'y')]
box = ext(min(cells$x) - CELL_SIZE / 2, max(cells$x) + CELL_SIZE / 2,
          min(cells$y) - CELL_SIZE / 2, max(cells$y) + CELL_SIZE / 2)

# GMTED2010 over the box. Its pixels nest exactly in whole degrees (120 x 120 per degree);
# stop if not, because step 4 relies on it.
gmted = crop(rast(GMTED_DIR), box, snap = 'near')
pixels_per_cell = CELL_SIZE / res(gmted)
stopifnot(all(abs(pixels_per_cell - round(pixels_per_cell)) < 1e-6),
          abs(xmin(gmted) - xmin(box)) < 1e-6, abs(ymax(gmted) - ymax(box)) < 1e-6)

# Step 1: the 250 m ocean indicator, in MOD44W's own sinusoidal grid, over the box.
water = rast(file.path(MOD44W_DIR, 'MOD44W.061_water_mask_doy2015001000000_aid0001.tif'))
qa    = rast(file.path(MOD44W_DIR, 'MOD44W.061_water_mask_QA_doy2015001000000_aid0001.tif'))
box_sinusoidal = project(box, 'EPSG:4326', crs(water))
water = crop(water, box_sinusoidal)
qa    = crop(qa, box_sinusoidal)
ocean = (water == 1) & (qa == OCEAN_QA)

# Step 2: ocean share of each GMTED pixel. MOD44W was downloaded for the study area only
# (data/mod44w/paleo_rf_area.geojson), so pixels outside it come out NA. Rather than guess
# what they are, measure how much of each cell the mask covers (step 4) and stop if a cell
# is not fully covered.
ocean_share = project(ocean, gmted, method = 'average')
covered     = !is.na(ocean_share)
ocean_share = ifel(covered, ocean_share, 0)

# Step 3: weights. Pixel areas shrink towards the pole, so each pixel counts by its area.
area   = cellSize(gmted, unit = 'm')
weight = area * (1 - ocean_share)
weight = ifel(is.na(gmted), 0, weight)          # GMTED voids (none expected south of 84 N)

# Step 4: sums over each 1-degree cell, then the weighted mean.
fact = round(pixels_per_cell[1])
sum_elev_weight = aggregate(ifel(is.na(gmted), 0, gmted) * weight, fact = fact, fun = 'sum')
sum_weight      = aggregate(weight, fact = fact, fun = 'sum')
sum_area        = aggregate(area,   fact = fact, fun = 'sum')
sum_covered     = aggregate(area * covered, fact = fact, fun = 'sum')
at = as.matrix(cells)
elev_land_mean  = extract(sum_elev_weight, at)[, 1] / extract(sum_weight, at)[, 1]
non_ocean_share = extract(sum_weight, at)[, 1] / extract(sum_area, at)[, 1]
mask_coverage   = extract(sum_covered, at)[, 1] / extract(sum_area, at)[, 1]

# Every cell must lie wholly inside the downloaded mask (tolerance: one GMTED pixel's
# worth of edge, 1/14,400 of a cell, for reprojection at the polygon's edge).
uncovered = which(mask_coverage < 1 - 1 / 14400)
if (length(uncovered))
  stop('cells not fully covered by the MOD44W download (coverage in brackets): ',
       paste(sprintf('(%s, %s) [%.3f]', cells$x[uncovered], cells$y[uncovered],
                     mask_coverage[uncovered]), collapse = ' '))

no_land = which(!is.finite(elev_land_mean))
if (length(no_land))
  stop('cells with no non-ocean area, so no land elevation: ',
       paste(sprintf('(%s, %s)', cells$x[no_land], cells$y[no_land]), collapse = ' '))

out = data.frame(x = cells$x, y = cells$y,
                 elev = round(elev_land_mean, 1),
                 non_ocean_share = round(non_ocean_share, 4))
write.csv(out, OUT_FILE, row.names = FALSE)

cat(sprintf('%s: %d cells; elevation %.0f to %.0f m; non-ocean share below 0.5 in %d cells; %.1f min\n',
            OUT_FILE, nrow(out), min(out$elev), max(out$elev), sum(out$non_ocean_share < 0.5),
            as.numeric(difftime(Sys.time(), started, units = 'mins'))))
