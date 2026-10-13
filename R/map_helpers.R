# Map helpers shared by the plotting scripts.
#
# The political-boundary file data/map-data/geographic/pbs_ll.RDS covers the whole
# hemisphere (1,831 polygons, -180 to 180 E), includes 23 water polygons whose edges
# run along parallels, and has two Russia polygons that cross the dateline, one of them
# topologically invalid. Drawn as-is with geom_path(aes(long, lat, group = group)) and
# no axis limits, the result is straight lines across every panel and a panel 360
# degrees wide: the "weirdness" Andria noted in script 2 on 2026-09-24. Scripts 7, 7a
# and 8 draw the same layer the same way.
#
# Usage, in any plotting script:
#   source('R/map_helpers.R')
#   window   = cfg$presentation$map_windows$calibration      # from config.R
#   pbs_land = prepare_boundaries(readRDS('data/map-data/geographic/pbs_ll.RDS'), window)
#   ggplot() + geom_spatraster(...) + boundary_layers(pbs_land, window) + ...
# boundary_layers() returns the boundary outline AND the coordinate window as one list,
# which ggplot2 accepts with a single "+". Written 2026-09-25.
#
# The window, list(x = c(lon_min, lon_max), y = c(lat_min, lat_max)), is always passed in
# by the calling script and has no default here: the maps use three different windows
# (config.R, presentation$map_windows), and a helper that picked one itself would hide
# which (issue I03, 2026-10-13).

library(sf)

# Turn the raw sp boundary object into a clean sf layer for drawing: land features only,
# geometries repaired, dateline-crossing features (longitude span over 300 degrees)
# dropped, and everything cropped to the map window. Spherical geometry is switched
# off during the crop because the invalid polygon fails on the sphere; the previous
# setting is restored afterwards.
prepare_boundaries = function(pbs_sp, window) {
  s2_was_on = sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(s2_was_on), add = TRUE)
  land = st_as_sf(pbs_sp)
  land = land[land$COUNTRY != "water/agua/d'eau", ]
  land = st_make_valid(land)
  span = vapply(st_geometry(land), function(g) { b = st_bbox(g); unname(b["xmax"] - b["xmin"]) }, numeric(1))
  land = land[span <= 300, ]
  suppressWarnings(st_crop(land, xmin = window$x[1], xmax = window$x[2],
                                 ymin = window$y[1], ymax = window$y[2]))
}

# The two ggplot layers every map needs: the outline, and a coordinate system fixed to
# the map window (coord_sf keeps lon/lat with a 1:1 aspect and clips to the window).
# fill = NA draws an outline only (script 2); script 3 fills the land grey, as its
# original geom_polygon did, by passing fill = "grey".
boundary_layers = function(pbs_land, window, colour = "grey50", linewidth = 0.2, fill = NA) {
  list(
    geom_sf(data = pbs_land, fill = fill, colour = colour, linewidth = linewidth),
    coord_sf(xlim = window$x, ylim = window$y, expand = FALSE)
  )
}
