# Tests for the arithmetic of tools/prepare_bluesky_albedo.R, worked by hand (cases from the build
# reviews of 20 October 2026). Run from the repository root: Rscript tests/test_bluesky_albedo.R
suppressMessages(source("tools/prepare_bluesky_albedo.R"))
near <- function(a, b, tol = 1e-12) isTRUE(all(abs(a - b) < tol))

# Blue-sky mix: f = 0 gives black-sky, f = 1 white-sky, and values in between stay between them.
stopifnot(
  near(blue_sky(0.10, 0.30, 0), 0.10), near(blue_sky(0.10, 0.30, 1), 0.30),
  near(blue_sky(0.10, 0.30, 0.25), 0.15),
  all(blue_sky(0.10, 0.30, seq(0, 1, 0.1)) >= 0.10 & blue_sky(0.10, 0.30, seq(0, 1, 0.1)) <= 0.30)
)

# Equation of time: about -6 min in mid-July and +16 min in early November; leap years use 366 days.
eot_by_hand <- function(doy, n) { g <- 2 * pi / n * (doy - 1)
  229.18 * (0.000075 + 0.001868 * cos(g) - 0.032077 * sin(g) - 0.014615 * cos(2 * g) - 0.040849 * sin(2 * g)) }
stopifnot(
  abs(equation_of_time(as.Date("2005-07-15")) - (-5.78)) < 0.01,
  abs(equation_of_time(as.Date("2005-11-03")) - 16.4) < 0.1,
  near(equation_of_time(as.Date("2004-12-31")), eot_by_hand(366, 366)),
  near(equation_of_time(as.Date("2005-12-31")), eot_by_hand(365, 365))
)

# Land: more than half of the days with a class say land; blank days do not count; a tie is not
# land. With the 'land_or_shore' rule, shoreline days count with land days.
stopifnot(identical(is_land(c(10, 6, 5, 0, 0), 0, c(10, 10, 10, 3, 0)), c(TRUE, TRUE, FALSE, FALSE, FALSE)),
          identical(is_land(c(3, 3, 0), c(0, 3, 6), c(10, 10, 10)), c(FALSE, FALSE, FALSE)),
          identical(is_land(c(3, 3, 0), c(0, 3, 6), c(10, 10, 10), 'land_or_shore'), c(FALSE, TRUE, TRUE)))

# Low-sun rule: a missing angle passes only when there is no rule.
stopifnot(identical(zenith_ok(c(60, 75, 80, NA), 75), c(TRUE, TRUE, FALSE, FALSE)),
          identical(zenith_ok(c(60, NA), Inf), c(TRUE, TRUE)))

# One day's totals: snowy, snow-free and unknown snow status; a day not kept adds nothing.
d <- day_totals(keep = c(TRUE, TRUE, TRUE, FALSE), snow = c(1, 0, NA, 1), blue = c(0.8, 0.2, 0.5, 0.9))
stopifnot(
  identical(unname(d[, "n_all"]), c(1, 1, 1, 0)), identical(unname(d[, "n_snow"]), c(1, 0, 0, 0)),
  identical(unname(d[, "n_free"]), c(0, 1, 0, 0)),
  near(d[, "s_all"], c(0.8, 0.2, 0.5, 0)), near(d[, "s_snow"], c(0.8, 0, 0, 0)), near(d[, "s_free"], c(0, 0.2, 0, 0)),
  # all = snowy + snow-free + unknown
  identical(unname(d[, "n_all"] - d[, "n_snow"] - d[, "n_free"]), c(0, 0, 1, 0))
)

# Unequal coverage (Astra): cell A has pixel p (tile 1) with 0.15 and 0.25 in years 1 and 2, and
# pixel q (tile 2) with 0.55 in year 1 only, one day each; cell B has 0.30 in both years. Pooling
# pixel-days within each year and weighting the years equally gives A = (0.35 + 0.25) / 2 = 0.300.
pixel_days <- data.frame(cell = c("A", "A", "A", "B", "B"), year = c(1, 2, 1, 1, 2), blue = c(0.15, 0.25, 0.55, 0.30, 0.30))
per_year <- rowsum(cbind(n = 1, s = pixel_days$blue), paste(pixel_days$cell, pixel_days$year))
cell_of <- sub(" .*", "", rownames(per_year))
cl <- climatology(cell_of, per_year[, "n"], 0, per_year[, "n"], per_year[, "s"], 0, per_year[, "s"])
stopifnot(identical(cl$group, c("A", "B")), near(cl$all, c(0.300, 0.300)), identical(cl$years, c(2, 2)))

# Snow mixture (Astra's counterexample): one year, pixel A 10 snowy days at 0.8 and 10 snow-free
# at 0.2, pixel B 2 snowy at 0.6 and 18 snow-free at 0.2. All days 14.8 / 40 = 0.37, and the mix of
# snowy and snow-free by the snow share must give exactly that.
cl <- climatology("c", n_all = 40, n_snow = 12, n_free = 28, s_all = 14.8, s_snow = 9.2, s_free = 5.6)
stopifnot(near(cl$all, 0.37), near(cl$share_snowy, 0.3),
          near(cl$share_snowy * cl$snowy + cl$share_snowfree * cl$snowfree, 0.37))

# The same identity over several years with unequal days and some days of unknown snow status.
n_all <- c(40, 7, 15); n_snow <- c(12, 0, 9); n_free <- c(25, 7, 5)
s_snow <- c(9.2, 0, 5.4); s_free <- c(5.0, 1.1, 0.9); s_unknown <- c(1.5, 0, 0.2)
cl <- climatology(rep("c", 3), n_all, n_snow, n_free, s_snow + s_free + s_unknown, s_snow, s_free)
w <- 1 / n_all
unknown_mean <- sum(s_unknown * w) / sum((n_all - n_snow - n_free) * w)
share_unknown <- mean((n_all - n_snow - n_free) * w)
stopifnot(near(cl$all, mean((s_snow + s_free + s_unknown) / n_all)),
          near(cl$share_snowy + cl$share_snowfree + share_unknown, 1),
          near(cl$share_snowy * cl$snowy + cl$share_snowfree * cl$snowfree + share_unknown * unknown_mean, cl$all))

# No qualifying day at all gives no value, not zero; a year without data does not count.
cl <- climatology(c("x", "x", "y"), n_all = c(0, 0, 0), n_snow = 0, n_free = 0, s_all = 0, s_snow = 0, s_free = 0)
stopifnot(all(is.na(cl$all)), all(is.na(cl$snowy)), identical(cl$years, c(0, 0)))
cl <- climatology(c("x", "x"), n_all = c(4, 0), n_snow = 0, n_free = c(4, 0), s_all = c(0.8, 0), s_snow = 0, s_free = c(0.8, 0))
stopifnot(near(cl$all, 0.2), identical(cl$years, 1), is.na(cl$snowy))

# Collecting: an interrupted move (some files already in "<month>.partial") is finished by running
# collect again; a damaged file stops it; the checksum list and request files are kept in records.
tmp <- file.path(tempdir(), "collect_test"); unlink(tmp, recursive = TRUE)
DOWNLOADS <- file.path(tmp, "Downloads"); APPEEARS_DIR <- file.path(tmp, "appeears"); RECORDS_DIR <- file.path(tmp, "records")
dir.create(DOWNLOADS, recursive = TRUE)
names_tif <- sprintf("MCD43A3.061_Albedo_BSA_shortwave_doy2005%03d000000_aid0001.tif", 182:184)
for (i in seq_along(names_tif)) writeLines(paste("day", i), file.path(DOWNLOADS, names_tif[i]))
sha <- vapply(file.path(DOWNLOADS, names_tif), function(f) as.character(openssl::sha256(file(f))), "")
writeLines(paste(sha, names_tif), file.path(DOWNLOADS, "paleoRFMCD432005-07-sha256sums.txt"))
writeLines("granules", file.path(DOWNLOADS, "paleoRFMCD432005-07-granule-list.txt"))
dir.create(file.path(APPEEARS_DIR, "2005-07.partial"), recursive = TRUE)
invisible(file.rename(file.path(DOWNLOADS, names_tif[1]), file.path(APPEEARS_DIR, "2005-07.partial", names_tif[1])))
writeLines("damaged", file.path(DOWNLOADS, names_tif[2]))
damaged_stops <- inherits(try(capture.output(collect_downloads()), silent = TRUE), "try-error")
writeLines("day 2", file.path(DOWNLOADS, names_tif[2]))
invisible(capture.output(collect_downloads()))
stopifnot(damaged_stops,
          setequal(list.files(file.path(APPEEARS_DIR, "2005-07")), names_tif),
          !dir.exists(file.path(APPEEARS_DIR, "2005-07.partial")),
          setequal(list.files(file.path(RECORDS_DIR, "2005-07")),
                   c("paleoRFMCD432005-07-sha256sums.txt", "paleoRFMCD432005-07-granule-list.txt")),
          check_checksums(file.path(RECORDS_DIR, "2005-07", "paleoRFMCD432005-07-sha256sums.txt"),
                          file.path(APPEEARS_DIR, "2005-07")) == 3)

# Delivery: every GeoTIFF must be one of our layers on a day of the month, and every needed layer
# must have at least one file; a day missing only the zenith is used by the no-rule version only,
# a day missing another layer is skipped, and both are counted.
deliv <- file.path(tmp, "delivery"); dir.create(deliv)
july <- month_days(as.Date("2005-07-01"))
make_day_files <- function(dir, days, layers = LAYERS$file)
  for (l in layers) invisible(file.create(file.path(dir, sprintf("%s_doy%s000000_aid0001.tif", l, format(days, "%Y%j")))))
make_day_files(deliv, july)
invisible(file.remove(file.path(deliv, sprintf("%s_doy%s000000_aid0001.tif", LAYERS$file[LAYERS$name == "sza"], "2005190"))))
invisible(file.remove(file.path(deliv, sprintf("%s_doy%s000000_aid0001.tif", LAYERS$file[LAYERS$name == "snow"], "2005200"))))
summary_lines <- delivery_summary(delivery_table(deliv, july, LAYERS$name), LAYERS$name)
stopifnot(length(july) == 31,
          grepl("with every needed layer: 29$", summary_lines[1]),
          grepl("zenith .*: 1 \\(09\\)", summary_lines[2]),
          grepl("skipped.*: 1 \\(19\\)", summary_lines[3]))
invisible(file.create(file.path(deliv, "MCD43A3.061_Albedo_BSA_shortwave_doy2005213000000_aid0001.tif")))  # 1 August
stray_stops <- inherits(try(delivery_table(deliv, july, LAYERS$name), silent = TRUE), "try-error")
invisible(file.remove(file.path(deliv, "MCD43A3.061_Albedo_BSA_shortwave_doy2005213000000_aid0001.tif")))
invisible(file.create(file.path(deliv, "MCD43A3.061_Albedo_BSA_v2_doy2005190000000_aid0001.tif")))  # renamed layer
renamed_stops <- inherits(try(delivery_table(deliv, july, LAYERS$name), silent = TRUE), "try-error")
invisible(file.remove(file.path(deliv, "MCD43A3.061_Albedo_BSA_v2_doy2005190000000_aid0001.tif")))
invisible(file.remove(list.files(deliv, pattern = "Snow_BRDF_Albedo", full.names = TRUE)))  # a whole layer missing
missing_layer_stops <- inherits(try(delivery_table(deliv, july, LAYERS$name), silent = TRUE), "try-error")
stopifnot(stray_stops, renamed_stops, missing_layer_stops)

# The request file must ask for the month's days and our layers.
WORK_DIR <- tmp; rec <- file.path(tmp, "rec_req"); dir.create(rec)
invisible(write_request(as.Date("2005-07-01"), request_area(data.frame(x = c(-100.5, -99.5), y = c(40.5, 41.5)))))
stopifnot(grepl("as expected", check_request(as.Date("2005-07-01"), rec)))
wrong <- jsonlite::fromJSON(file.path(tmp, "requests", "paleoRF_MCD43_2005-07.json"), simplifyVector = FALSE)
wrong$params$layers <- wrong$params$layers[1:5]
jsonlite::write_json(wrong, file.path(rec, "paleoRFMCD432005-07-request.json"), auto_unbox = TRUE)
stopifnot(inherits(try(check_request(as.Date("2005-07-01"), rec), silent = TRUE), "try-error"))
WORK_DIR <- "data/modis_albedo"

# A monthly file is accepted only if it lies on its tile grid, has our layers and tag, reads in
# full, and its counts and sums are consistent: one pixel per row of the table below, every
# version alike (nested versions may be equal).
tpl <- rast(nrows = 40, ncols = 40, crs = SIN_CRS, extent = ext(0, 40 * 463.3127, 0, 40 * 463.3127))
consistent <- function() {
  set.seed(1)
  n_all <- sample(0:31, ncell(tpl), replace = TRUE); n_snow <- floor(n_all / 3); n_free <- n_all - n_snow - (n_all > 5)
  a <- runif(ncell(tpl), 0.05, 0.9)
  vals <- list(n_all = n_all, n_snow = n_snow, n_free = n_free, s_all = n_all * a, s_snow = n_snow * a, s_free = n_free * a)
  layers <- c(unlist(lapply(VERSIONS$name, function(v) setNames(vals, paste(v, SUMS, sep = "_"))), recursive = FALSE),
              list(land_days = floor(n_all / 2), shore_days = floor(n_all / 4), read_days = n_all))
  r <- rast(lapply(TILE_LAYERS, function(n) setValues(tpl, layers[[n]]))); names(r) <- TILE_LAYERS; r
}
write_tile <- function(r, name, tag = SETTINGS_TAG) {   # the tag is set last: changing a layer drops it
  metags(r) <- c(paleoRF_build = tag); f <- file.path(tmp, name)
  writeRaster(r, f, overwrite = TRUE, gdal = "COMPRESS=DEFLATE", datatype = "FLT4S"); f
}
good <- write_tile(consistent(), "good.tif")
old_tag <- write_tile(consistent(), "old.tif", tag = "reduction 1")
r <- consistent(); r[["full_75_n_snow"]] <- r[["full_75_n_all"]] + 1; bad_layer <- write_tile(r, "bad_layer.tif")   # not the last layer
r <- consistent(); r[["full_70_n_all"]] <- r[["full_70_n_all"]] + 1; r[["full_70_n_free"]] <- r[["full_70_n_free"]] + 1
not_nested <- write_tile(r, "not_nested.tif")
r <- consistent(); r[["any_75_s_free"]] <- r[["any_75_n_free"]] + 0.5; too_bright <- write_tile(r, "too_bright.tif")
cut_short <- file.path(tmp, "cut.tif"); writeBin(readBin(good, "raw", file.size(good))[1:floor(0.9 * file.size(good))], cut_short)
other_grid <- rast(nrows = 40, ncols = 40, crs = SIN_CRS, extent = ext(463.3127, 41 * 463.3127, 0, 40 * 463.3127))
# The July 2005 shortcut (zenith taken as 0) has a tag of its own, so its files are never reused
# by a build with the real zenith, nor accepted as such.
july_tag <- settings_tag(FALSE)
july_file <- write_tile(consistent(), "july.tif", tag = july_tag)
stopifnot(july_tag != SETTINGS_TAG, !valid_output(july_file, template = tpl), valid_output(july_file, tag = july_tag, template = tpl))

problem <- function(f, ...) paste(output_problems(f, ...), collapse = "; ")
stopifnot(valid_output(good, template = tpl),
          problem(old_tag, template = tpl) == "was made with other settings",
          grepl("full_75 snowy plus snow-free days exceed all days", problem(bad_layer, template = tpl)),
          problem(not_nested, template = tpl) == "the versions do not nest",
          problem(too_bright, template = tpl) == "any_75 albedo sums outside 0 to their number of days",
          !valid_output(cut_short, template = tpl),       # GDAL loses the trailing names or the data
          problem(good, template = other_grid) == "is not on its tile grid")

cat("test_bluesky_albedo: all tests passed\n")
