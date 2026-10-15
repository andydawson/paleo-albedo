# prepare_land_elevation.R: elevation of each 1-degree cell as the mean over its non-ocean
# area, from the GMTED2010 30-arc-second mean-elevation grid. Run once, from the repository
# root:
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
# a fine elevation model over a cell's land points, leaving the sea out and keeping lakes:
#   - New, Lister, Hulme & Makin (2002), Climate Research 21:1-25, doi:10.3354/cr021001, p. 7:
#     each land cell's elevation is the average of 100-400 GLOBE 1 km points (the elevation
#     model's own land points: GLOBE marks the sea as no data).
#   - ISLSCP II HYDRO1k elevation products (Verdin 2011, doi:10.3334/ORNLDAAC/1007, sec.
#     9.2.1): "mean elevation value within each cell" over "the non-ocean pixels".
# The record of the literature check is docs/cc/2026-10-15_land_mean_elevation_precedents.md.
#
# The sea is taken from the elevation model itself, as in New et al.: GMTED2010 records the
# sea as 0 m (checked 2026-10-15: 99.6-100% of sampled open-sea pixels), so a pixel at exactly
# 0 m is sea and is left out; lakes keep their surface heights (the Great Lakes about 183 m)
# and stay in. Chris chose this on 2026-10-15 over an earlier version that took the sea from
# the MODIS MOD44W water mask: both reviewers found that mask's ocean class starts about 1 km
# offshore (so the coastal strip of sea counted as land, at 0 m) and that it labels one large
# lake, Nettilling on Baffin Island, as ocean. Against that mask 637 cells rise (median 4 m)
# and 35 fall. Limitations, which Chris chose to state rather than fix (2026-10-15; Astra
# preferred fixing the coast with the GSHHG shoreline database, Fable judged them acceptable):
#   - where GMTED's source data carry land heights over sea inlets, that sea counts as land:
#     four cells come out 31 to 179 m low, (-64.5, 67.5) and (-65.5, 67.5) on eastern Baffin
#     Island, (-89.5, 65.5) at Wager Bay and (-145.5, 60.5) at Prince William Sound, and 14
#     Arctic cells 3 to 13 m low. (Nettilling's cell also falls, by 21 m, as intended: its lake
#     was wrongly ocean before.) New et al.'s GLOBE coded the sea separately from land at 0 m,
#     so this limitation is not shared by that precedent.
#   - land whose mean is exactly 0 m is treated as sea: at most about 7% of a cell's area
#     (Mackenzie delta), 12 cells above 5%;
#   - a coastal 1 km pixel that is part sea holds the mean of land and 0 m.
#
# Input (not in git): GMTED2010 30-arc-second mean elevation, mn30_grd (Danielson & Gesch 2011,
# USGS Open-File Report 2011-1073, doi:10.3133/ofr20111073), downloaded as mn30_grd.zip from
# https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip
# (277,229,566 bytes, SHA-256 dfd0d6c6486f4da22109be6107c93a70ef9917a5b6f954cd82d87cf8b920149d)
# and unzipped into data/gmted2010/. The cells: the (x, y) centres in
# data/elev_interp_cells.RDS, the cells script 1 uses (keep that file even when using the
# land mean).
#
# The steps: each GMTED pixel weighs its area if it is above or below 0 m and nothing if it is
# at 0 m; a cell's elevation is the weighted mean of its 120 x 120 pixels, and its non-ocean
# share (a diagnostic, not read by the pipeline) is the summed weight over the summed area.
# A cell with no non-ocean pixel would have no elevation: the tool stops and lists it.

library(terra)

GMTED_DIR  = 'data/gmted2010/mn30_grd'
CELLS_FILE = 'data/elev_interp_cells.RDS'
OUT_FILE   = 'data/elev_land_mean_cells.csv'
SEA_LEVEL  = 0         # GMTED2010's value for the sea, metres
CELL_SIZE  = 1         # degrees; the land-cover grid

started = Sys.time()
terraOptions(memfrac = 0.5, progress = 0)

# The cells and the box that holds them, with whole cells around each centre.
cells = readRDS(CELLS_FILE)[, c('x', 'y')]
box = ext(min(cells$x) - CELL_SIZE / 2, max(cells$x) + CELL_SIZE / 2,
          min(cells$y) - CELL_SIZE / 2, max(cells$y) + CELL_SIZE / 2)

gmted = rast(GMTED_DIR)
# GMTED2010 keeps the "pixel centre" convention of its 1-arc-second SRTM source (Danielson &
# Gesch 2011, pp. 8-9, fig. 8, table 3), so its grid edges sit half a source arc-second
# (1/7200 degree: about 15 m north-south, less east-west) west and south of whole degrees:
# -180.000139 to 179.999860 E, -90.000139 to 83.999860 N for 43,200 x 20,880 pixels. Placing
# the grid on whole degrees moves it by those 15 m, far less than a 1 km pixel, and lets
# 120 x 120 pixels make exactly one cell.
stopifnot(ncol(gmted) == 43200, nrow(gmted) == 20880,
          abs(xmin(gmted) + 180) < 1 / 3600, abs(ymax(gmted) - 84) < 1 / 3600)
ext(gmted) = ext(-180, 180, -90, 84)
gmted = crop(gmted, box, snap = 'near')
fact = round(CELL_SIZE / res(gmted))
stopifnot(all(abs(CELL_SIZE / res(gmted) - fact) < 1e-6), ncol(gmted) %% fact[1] == 0, nrow(gmted) %% fact[2] == 0,
          abs(xmin(gmted) - xmin(box)) < 1e-6, abs(ymax(gmted) - ymax(box)) < 1e-6)
if (global(gmted, 'isNA')[1, 1] > 0) stop('GMTED2010 has voids inside the study box')

# Weights: a pixel's area if it is not sea, nothing if it is. Pixel areas shrink towards the
# pole, so each pixel counts by its area.
area   = cellSize(gmted, unit = 'm')
weight = area * (gmted != SEA_LEVEL)

# Sums over each 1-degree cell, then the weighted mean.
sum_elev_weight = aggregate(gmted * weight, fact = fact, fun = 'sum')
sum_weight      = aggregate(weight, fact = fact, fun = 'sum')
sum_area        = aggregate(area,   fact = fact, fun = 'sum')
at = as.matrix(cells)
elev_land_mean  = extract(sum_elev_weight, at)[, 1] / extract(sum_weight, at)[, 1]
non_ocean_share = extract(sum_weight, at)[, 1] / extract(sum_area, at)[, 1]

no_land = which(!is.finite(elev_land_mean))
if (length(no_land))
  stop('cells with no non-ocean pixel, so no land elevation: ',
       paste(sprintf('(%s, %s)', cells$x[no_land], cells$y[no_land]), collapse = ' '))

out = data.frame(x = cells$x, y = cells$y,
                 elev = round(elev_land_mean, 1),
                 non_ocean_share = round(non_ocean_share, 4))
write.csv(out, OUT_FILE, row.names = FALSE)

cat(sprintf('%s: %d cells; elevation %.0f to %.0f m; non-ocean share below 0.5 in %d cells; %.1f min\n',
            OUT_FILE, nrow(out), min(out$elev), max(out$elev), sum(out$non_ocean_share < 0.5),
            as.numeric(difftime(Sys.time(), started, units = 'mins'))))
