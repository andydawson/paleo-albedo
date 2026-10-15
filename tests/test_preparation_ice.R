# Tests for cells_in_polygons() in R/preparation.R (issue I16). Run from the repository root:
# Rscript tests/test_preparation_ice.R
source("R/preparation.R")

# Two unit squares, one with no coordinate system (as the glacier shapes are).
square = function(x0, y0) terra::vect(sprintf("POLYGON ((%d %d, %d %d, %d %d, %d %d, %d %d))",
                                               x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0))
polys = rbind(square(0, 0), square(10, 10))
inside = cells_in_polygons(x = c(0.5, 10.5, 5, -0.5), y = c(0.5, 10.5, 5, 0.5), polygons = polys)
stopifnot(identical(inside, c(TRUE, TRUE, FALSE, FALSE)))

# The real 1 ka ice polygons and the modern cells: the count found on 2026-09-25 (question B8).
ice_1ka = readRDS("data/map-data/ice/glacier_shapefiles_21-1k.RDS")[[1]]
lct = readRDS("data/lct_modern_reveals_interp.RDS")
n = sum(cells_in_polygons(lct$x, lct$y, ice_1ka))
cat("modern cells under the 1 ka ice:", n, "of", nrow(lct), "\n")
stopifnot(n == 48)

cat("test_preparation_ice: all tests passed\n")
