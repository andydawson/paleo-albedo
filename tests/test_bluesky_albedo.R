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

# Diffuse fraction from made-up ERA5 values: everywhere f = 0.5 (ssrd 1000, fdir 500), except at
# the grid point and hour each case should pick, where fdir = 750 gives f = 0.25. ERA5 accumulates
# over the hour ending at its time stamp, so the stamp is the hour after local solar noon.
era5 <- list(lon = c(-172.25, -172, -100.25, -100, -99.75), lat = c(45.25, 45, 44.75),
             time = c(seq(as.POSIXct("2005-07-15 00:00", tz = "UTC"), by = "hour", length.out = 25),
                      seq(as.POSIXct("2005-12-31 00:00", tz = "UTC"), by = "hour", length.out = 26)))
era5$ssrd <- array(1000, c(5, 3, length(era5$time))); era5$fdir <- array(500, dim(era5$ssrd))
at <- function(lon, lat, stamp) cbind(match(lon, era5$lon), match(lat, era5$lat), match(as.POSIXct(stamp, tz = "UTC"), era5$time))
era5$fdir[at(-100, 45, "2005-07-15 19:00")] <- 750      # noon at -100 on 15 July is 18:46 UTC
era5$fdir[at(-172, 45, "2006-01-01 00:00")] <- 750      # noon at -172 on 31 December is 23:30 UTC, next day's first stamp
era5$ssrd[at(-99.75, 45, "2005-07-15 19:00")] <- 0      # no sunlight: no fraction
stopifnot(
  near(diffuse_fraction(era5, -100, 45, as.Date("2005-07-15")), 0.25),
  near(diffuse_fraction(era5, -100.1, 45.1, as.Date("2005-07-15")), 0.25),   # nearest quarter-degree point
  near(diffuse_fraction(era5, -100.2, 45.1, as.Date("2005-07-15")), 0.5),    # nearer to -100.25
  near(diffuse_fraction(era5, -172, 45, as.Date("2005-12-31")), 0.25),
  is.na(diffuse_fraction(era5, -99.75, 45, as.Date("2005-07-15"))),
  near((1000 - 750) / 1000, 0.25)
)

# Lost retrievals: a warning only for tiles where more than 0.1% found no diffuse fraction.
w <- no_diffuse_warnings(c("h01v01", "h02v01", "h03v01", "h04v01"), c(0, 1, 50, 0), c(1e5, 1e5, 1e4, 0))
stopifnot(length(w) == 1, grepl("^WARNING tile h03v01: 50 of 10000 retrievals \\(0.50%\\)", w))

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
stopifnot(near(cl$all, mean((s_snow + s_free + s_unknown) / n_all)),
          near(cl$unknown, sum(s_unknown * w) / sum((n_all - n_snow - n_free) * w)),
          near(cl$share_unknown, mean((n_all - n_snow - n_free) * w)),
          near(cl$share_snowy + cl$share_snowfree + cl$share_unknown, 1),
          near(cl$share_snowy * cl$snowy + cl$share_snowfree * cl$snowfree + cl$share_unknown * cl$unknown, cl$all))

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
# AppEEARS's other files for the task use hyphens (task paleoRF_MCD43_2005-07), as on the real downloads.
side_names <- c("paleoRF-MCD43-2005-07-granule-list.txt", "paleoRF-MCD43-2005-07-request.json",
                "paleoRF-MCD43-2005-07-MCD43A2-061-metadata.xml", "paleoRF-MCD43-2005-07-MCD43A3-061-metadata.xml")
for (f in side_names) writeLines("side", file.path(DOWNLOADS, f))
writeLines("other task", file.path(DOWNLOADS, "paleoRF-MCD43-2005-08-request.json"))   # another month: left alone
dir.create(file.path(APPEEARS_DIR, "2005-07.partial"), recursive = TRUE)
invisible(file.rename(file.path(DOWNLOADS, names_tif[1]), file.path(APPEEARS_DIR, "2005-07.partial", names_tif[1])))
writeLines("damaged", file.path(DOWNLOADS, names_tif[2]))
damaged_stops <- inherits(try(capture.output(collect_downloads()), silent = TRUE), "try-error")
writeLines("day 2", file.path(DOWNLOADS, names_tif[2]))
invisible(capture.output(collect_downloads()))
stopifnot(damaged_stops,
          setequal(list.files(file.path(APPEEARS_DIR, "2005-07")), names_tif),
          !dir.exists(file.path(APPEEARS_DIR, "2005-07.partial")),
          setequal(list.files(file.path(RECORDS_DIR, "2005-07")), c("paleoRFMCD432005-07-sha256sums.txt", side_names)),
          file.exists(file.path(DOWNLOADS, "paleoRF-MCD43-2005-08-request.json")),
          check_checksums(file.path(RECORDS_DIR, "2005-07", "paleoRFMCD432005-07-sha256sums.txt"),
                          file.path(APPEEARS_DIR, "2005-07")) == 3)

# Collecting, the ordinary case: a fresh month with nothing half done (no "<month>.partial" at
# all), then an idle run with nothing left to collect. Neither may fail.
fresh <- file.path(tempdir(), "collect_fresh"); unlink(fresh, recursive = TRUE)
saved <- list(DOWNLOADS, APPEEARS_DIR, RECORDS_DIR)
DOWNLOADS <- file.path(fresh, "Downloads"); APPEEARS_DIR <- file.path(fresh, "appeears"); RECORDS_DIR <- file.path(fresh, "records")
dir.create(DOWNLOADS, recursive = TRUE)
names_sep <- sprintf("MCD43A3.061_Albedo_BSA_shortwave_doy2005%03d000000_aid0001.tif", 244:245)
for (i in seq_along(names_sep)) writeLines(paste("sep", i), file.path(DOWNLOADS, names_sep[i]))
sha <- vapply(file.path(DOWNLOADS, names_sep), function(f) as.character(openssl::sha256(file(f))), "")
writeLines(paste(sha, names_sep), file.path(DOWNLOADS, "paleoRFMCD432005-09-sha256sums.txt"))
invisible(capture.output(collect_downloads()))
idle <- capture.output(collect_downloads())
stopifnot(setequal(list.files(file.path(APPEEARS_DIR, "2005-09")), names_sep),
          any(grepl("no request checksum lists", idle)))
DOWNLOADS <- saved[[1]]; APPEEARS_DIR <- saved[[2]]; RECORDS_DIR <- saved[[3]]

# Collecting, interrupted at the last moment: the files are all in "<month>.partial" and the
# checksum list is already in the records, but the folder was not renamed. Collect finishes it.
dir.create(file.path(APPEEARS_DIR, "2005-08.partial"))
names_aug <- sprintf("MCD43A3.061_Albedo_BSA_shortwave_doy2005%03d000000_aid0001.tif", 213:214)
for (i in seq_along(names_aug)) writeLines(paste("aug", i), file.path(APPEEARS_DIR, "2005-08.partial", names_aug[i]))
sha <- vapply(file.path(APPEEARS_DIR, "2005-08.partial", names_aug), function(f) as.character(openssl::sha256(file(f))), "")
dir.create(file.path(RECORDS_DIR, "2005-08"))
writeLines(paste(sha, names_aug), file.path(RECORDS_DIR, "2005-08", "paleoRFMCD432005-08-sha256sums.txt"))
invisible(capture.output(collect_downloads()))
stopifnot(setequal(list.files(file.path(APPEEARS_DIR, "2005-08")), names_aug), !dir.exists(file.path(APPEEARS_DIR, "2005-08.partial")))

# Delivery: every GeoTIFF must be one of our layers on a day of the month, and every needed layer
# must have at least one file; a day missing only the zenith is used by the no-rule version only,
# a day missing another layer is skipped, and both are counted.
deliv <- file.path(tmp, "delivery"); dir.create(deliv)
july <- month_days(as.Date("2005-07-01"))
make_day_files <- function(dir, days, layers = LAYERS$file)
  for (l in layers) invisible(file.create(file.path(dir, sprintf("%s_doy%s000000_aid0001.tif", l, format(days, "%Y%j")))))
day_file <- function(name, days) sprintf("%s_doy%s000000_aid0001.tif", LAYERS$file[LAYERS$name == name], format(days, "%Y%j"))
make_day_files(deliv, july)
invisible(file.remove(file.path(deliv, sprintf("%s_doy%s000000_aid0001.tif", LAYERS$file[LAYERS$name == "sza"], "2005190"))))
invisible(file.remove(file.path(deliv, sprintf("%s_doy%s000000_aid0001.tif", LAYERS$file[LAYERS$name == "snow"], "2005200"))))
summary_lines <- delivery_summary(delivery_table(deliv, july, LAYERS$name), LAYERS$name)
stopifnot(length(july) == 31,
          grepl("used by every version: 29$", summary_lines[1]),
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
# A month in which every layer has files but no day has them all would build empty tiles: it
# stops (Chris, 2026-10-21). One complete day is enough to go on.
partial <- file.path(tmp, "partial_days"); dir.create(partial)
make_day_files(partial, july, LAYERS$file[!LAYERS$name %in% c("bsa", "wsa")])
make_day_files(partial, july[1:15], LAYERS$file[LAYERS$name == "bsa"])
make_day_files(partial, july[16:31], LAYERS$file[LAYERS$name == "wsa"])
no_usable_msg <- tryCatch(delivery_table(partial, july, LAYERS$name), error = conditionMessage)
make_day_files(partial, july[1], LAYERS$file[LAYERS$name == "wsa"])
one_day <- delivery_summary(delivery_table(partial, july, LAYERS$name), LAYERS$name)
stopifnot(is.character(no_usable_msg), grepl("no day has every albedo, quality, snow and land layer \\(bsa, wsa, qa, snow, land\\)", no_usable_msg),
          grepl("used by every version: 1$", one_day[1]), grepl("skipped.*: 30 ", one_day[3]))
# A month in which no usable day has the noon zenith stops as well (Chris, 2026-10-21): the versions
# with a low-sun rule would be empty. First no zenith file at all; then one only on 1 July, a day
# skipped for want of its snow file. One usable day with the zenith is enough to go on, and the
# July 2005 test, which does not need the zenith, is not stopped by its absence.
no_zen <- file.path(tmp, "no_zenith"); dir.create(no_zen)
make_day_files(no_zen, july, LAYERS$file[LAYERS$name != "sza"])
zenith_nowhere_msg <- tryCatch(delivery_table(no_zen, july, LAYERS$name), error = conditionMessage)
zenith_not_needed <- delivery_table(no_zen, july, setdiff(LAYERS$name, "sza"))
invisible(file.remove(file.path(no_zen, day_file("snow", july[1]))))
make_day_files(no_zen, july[1], LAYERS$file[LAYERS$name == "sza"])
zenith_skipped_only_msg <- tryCatch(delivery_table(no_zen, july, LAYERS$name), error = conditionMessage)
make_day_files(no_zen, july[2], LAYERS$file[LAYERS$name == "sza"])
one_zenith_day <- delivery_summary(delivery_table(no_zen, july, LAYERS$name), LAYERS$name)
stopifnot(is.character(zenith_nowhere_msg), grepl("no file at all for sza", zenith_nowhere_msg), is.data.frame(zenith_not_needed),
          is.character(zenith_skipped_only_msg), grepl("no usable day has the noon solar zenith layer", zenith_skipped_only_msg),
          grepl("used by every version: 1$", one_zenith_day[1]), grepl("zenith .*: 29 \\(", one_zenith_day[2]),
          grepl("skipped.*: 1 \\(01\\)", one_zenith_day[3]))

# Keeping day files: a month built with "keep" keeps them, and a later build without names skips
# it (its files and first build record survive); naming it rebuilds it and still keeps the files.
# A month built without "keep" has its day files deleted. The tiles themselves are not built here:
# build_month is replaced by a stand-in that only reports, as the real one does when all tiles pass.
# The warnings an expression gives, and its value.
with_warnings <- function(expr) { w <- character(0)
  value <- withCallingHandlers(expr, warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  list(value = value, warnings = w) }
# AppEEARS's copy of our request file, as it comes back: our request plus its own fields, named
# with hyphens, in `dir`; `change` alters what it asks for.
write_appeears_copy <- function(ours, dir, change = identity) {
  req <- change(jsonlite::fromJSON(ours, simplifyVector = FALSE))
  req$task_id <- "abc"; req$status <- "done"; req$params$dates[[1]]$recurring <- FALSE
  jsonlite::write_json(req, file.path(dir, paste0(gsub("_", "-", sub("\\.json$", "", basename(ours))), "-request.json")),
                       auto_unbox = TRUE, digits = NA)
}
test_ring <- request_area(data.frame(x = c(-100.5, -99.5), y = c(40.5, 41.5)))
# A collected month for build_collected: day files of every layer and day (but those named in
# `drop`), their checksum list in the records, our request file and, unless appeears_copy = FALSE,
# AppEEARS's copy of it.
collected_month <- function(m, appeears_copy = TRUE, drop = character(0)) {
  dd <- file.path(APPEEARS_DIR, m); dir.create(dd, recursive = TRUE)
  rd <- file.path(RECORDS_DIR, m); dir.create(rd, recursive = TRUE)
  make_day_files(dd, month_days(as.Date(paste0(m, "-01"))))
  invisible(file.remove(file.path(dd, drop)))
  tifs <- list.files(dd)
  writeLines(paste(vapply(file.path(dd, tifs), function(f) as.character(openssl::sha256(file(f))), ""), tifs),
             file.path(rd, paste0("paleoRFMCD43", m, "-sha256sums.txt")))
  ours <- write_request(as.Date(paste0(m, "-01")), test_ring)
  if (appeears_copy) write_appeears_copy(ours, rd)
  dd
}
kb <- file.path(tempdir(), "keep_test"); unlink(kb, recursive = TRUE)
saved <- list(APPEEARS_DIR, RECORDS_DIR, WORK_DIR, build_month)
APPEEARS_DIR <- file.path(kb, "appeears"); RECORDS_DIR <- file.path(kb, "records"); WORK_DIR <- kb
build_month <- function(tiles, month, day_dir, out_dir, ...) paste("tile", tiles, ": made in this run")
for (m in c("2005-03", "2005-04")) collected_month(m)
record <- function(m) readLines(file.path(RECORDS_DIR, m, "build.txt"))
invisible(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-03", keep = TRUE)))
first_record <- record("2005-03")
out <- capture.output(build_collected("h00v00", file.path(kb, "monthly")))   # unnamed, no "keep"
stopifnot(dir.exists(file.path(APPEEARS_DIR, "2005-03")), file.exists(file.path(APPEEARS_DIR, "2005-03", KEEP_MARKER)),
          identical(record("2005-03"), first_record),                       # skipped: record untouched
          any(grepl("2005-03 : marked \"keep\" by an earlier build; skipped", out)),
          !dir.exists(file.path(APPEEARS_DIR, "2005-04")),                  # an ordinary month: built, files deleted
          any(grepl("day files: deleted after this record", record("2005-04"))),
          any(grepl("day files: kept", first_record)))
invisible(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-03")))   # named, no "keep"
stopifnot(dir.exists(file.path(APPEEARS_DIR, "2005-03")),                  # still kept
          sum(grepl("^=== built:", record("2005-03"))) == 2,               # a second section, the first kept
          identical(record("2005-03")[seq_along(first_record)], first_record))
# The KEEP file is written before the month is built (Chris, 2026-10-21): a build with "keep" that
# fails part-way leaves it, so a later build without names skips the month and its day files survive.
dd <- collected_month("2005-06")
build_month <- function(...) stop("worker crashed")
crash_msg <- tryCatch(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-06", keep = TRUE)),
                      error = conditionMessage)
build_month <- function(tiles, month, day_dir, out_dir, ...) paste("tile", tiles, ": made in this run")
out <- capture.output(build_collected("h00v00", file.path(kb, "monthly")))   # unnamed, no "keep"
stopifnot(identical(crash_msg, "worker crashed"), file.exists(file.path(dd, KEEP_MARKER)),
          any(grepl("2005-06 : marked \"keep\" by an earlier build; skipped", out)),
          length(list.files(dd, pattern = "\\.tif$")) == 30 * nrow(LAYERS))   # 30 days in June
# A build that will delete the day files needs AppEEARS's copy of the request (Chris, 2026-10-21):
# without it the build stops before building or deleting anything; with "keep" it only warns.
dd <- collected_month("2005-05", appeears_copy = FALSE)
no_copy_msg <- tryCatch(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-05")), error = conditionMessage)
stopifnot(grepl("^2005-05: AppEEARS's copy of the request is missing", no_copy_msg),
          grepl("stopped before building or deleting anything", no_copy_msg),
          length(list.files(dd, pattern = "\\.tif$")) == 31 * nrow(LAYERS), !file.exists(file.path(RECORDS_DIR, "2005-05", "build.txt")))
kept_build <- with_warnings(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-05", keep = TRUE)))
stopifnot(length(kept_build$warnings) == 1, grepl("AppEEARS's copy of the request is missing", kept_build$warnings),
          length(list.files(dd, pattern = "\\.tif$")) == 31 * nrow(LAYERS),
          any(grepl("our own request file only.*WARNING", record("2005-05"))))
# Two copies of AppEEARS's request in the records (e.g. a browser download saved twice, as
# "...-request (1).json", which collect files with the others) also stop a deleting build: which one
# AppEEARS worked from cannot be told.
dd <- collected_month("2005-07")
their_copy <- file.path(RECORDS_DIR, "2005-07", "paleoRF-MCD43-2005-07-request.json")
invisible(file.copy(their_copy, sub("\\.json$", " (1).json", their_copy)))
two_copies_msg <- tryCatch(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-07")), error = conditionMessage)
stopifnot(grepl("^2005-07: AppEEARS's copy of the request is not unique", two_copies_msg),
          grepl("stopped before building or deleting anything", two_copies_msg),
          length(list.files(dd, pattern = "\\.tif$")) == 31 * nrow(LAYERS), !file.exists(file.path(RECORDS_DIR, "2005-07", "build.txt")))
# A month in which no usable day has the noon zenith stops the build before any tile is built or
# anything deleted (Chris, 2026-10-21): August 2005 with the zenith only on 1 August, which has no
# snow file.
aug <- month_days(as.Date("2005-08-01"))
dd <- collected_month("2005-08", drop = c(day_file("sza", aug[-1]), day_file("snow", aug[1])))
no_zenith_msg <- tryCatch(capture.output(build_collected("h00v00", file.path(kb, "monthly"), named = "2005-08")), error = conditionMessage)
stopifnot(grepl("no usable day has the noon solar zenith layer", no_zenith_msg),
          length(list.files(dd, pattern = "\\.tif$")) == 31 * nrow(LAYERS) - 31, !file.exists(file.path(RECORDS_DIR, "2005-08", "build.txt")))
APPEEARS_DIR <- saved[[1]]; RECORDS_DIR <- saved[[2]]; WORK_DIR <- saved[[3]]; build_month <- saved[[4]]

# The request: before a build deletes day files, AppEEARS's copy must be in the records and ask
# for the same dates, layers and area as our request file (Chris, 2026-10-21); a build with "keep"
# only warns. Either way, what was asked must be the month's days and our layers.
WORK_DIR <- tmp; rec <- file.path(tmp, "rec_req"); dir.create(rec); jul <- as.Date("2005-07-01")
ours <- write_request(jul, test_ring)
only_ours_msg <- tryCatch(check_request(jul, rec), error = conditionMessage)
only_ours <- with_warnings(check_request(jul, rec, deleting = FALSE))
stopifnot(grepl("AppEEARS's copy of the request is missing", only_ours_msg),
          grepl("as expected", only_ours$value), grepl("our own request file only", only_ours$value),
          length(only_ours$warnings) == 1)
write_appeears_copy(ours, rec)
their_line <- with_warnings(check_request(jul, rec))
stopifnot(grepl("as expected", their_line$value), grepl("the same dates, layers and area as our request file", their_line$value),
          grepl("paleoRF-MCD43-2005-07-request.json", their_line$value), !length(their_line$warnings))
# AppEEARS's copy asks for another area (shifted a degree east): a deleting build stops, a build
# with "keep" warns and goes on.
shift_east <- function(req) {
  ring <- req$params$geo$features[[1]]$geometry$coordinates[[1]]
  req$params$geo$features[[1]]$geometry$coordinates[[1]] <- lapply(ring, function(p) list(p[[1]] + 1, p[[2]])); req }
write_appeears_copy(ours, rec, shift_east)
area_msg <- tryCatch(check_request(jul, rec), error = conditionMessage)
area_keep <- with_warnings(check_request(jul, rec, deleting = FALSE))
stopifnot(grepl("differs from ours in area", area_msg), grepl("stopped before building or deleting anything", area_msg),
          length(area_keep$warnings) == 1, grepl("differs from ours in area", area_keep$warnings),
          grepl("AppEEARS's copy.*WARNING", area_keep$value))
# Other dates, or fewer layers: a deleting build stops on the difference; a build with "keep" too,
# since what was asked is then not the month's days and our layers.
one_day_short <- function(req) { req$params$dates[[1]]$endDate <- "07-30-2005"; req }
five_layers <- function(req) { req$params$layers <- req$params$layers[1:5]; req }
write_appeears_copy(ours, rec, one_day_short)
dates_msg <- tryCatch(check_request(jul, rec), error = conditionMessage)
dates_keep_stops <- inherits(try(suppressWarnings(check_request(jul, rec, deleting = FALSE)), silent = TRUE), "try-error")
write_appeears_copy(ours, rec, five_layers)
layers_msg <- tryCatch(check_request(jul, rec), error = conditionMessage)
layers_keep_stops <- inherits(try(suppressWarnings(check_request(jul, rec, deleting = FALSE)), silent = TRUE), "try-error")
stopifnot(grepl("differs from ours in dates", dates_msg), dates_keep_stops,
          grepl("differs from ours in layers", layers_msg), layers_keep_stops)
WORK_DIR <- "data/modis_albedo"

# A monthly file is accepted only if it lies on its tile grid, has our layers and tag, reads in
# full, and its counts and sums are consistent: one pixel per row of the table below, every
# version alike (nested versions may be equal).
tpl <- rast(nrows = 40, ncols = 40, crs = SIN_CRS, extent = ext(0, 40 * 463.3127, 0, 40 * 463.3127))
consistent <- function() {
  set.seed(1)
  n_all <- sample(0:30, ncell(tpl), replace = TRUE)     # 0-30: room for one more day within 31
  n_snow <- floor(n_all / 3); n_free <- n_all - n_snow - (n_all > 5)
  a <- runif(ncell(tpl), 0.05, 0.9)
  vals <- list(n_all = n_all, n_snow = n_snow, n_free = n_free, s_all = n_all * a, s_snow = n_snow * a, s_free = n_free * a)
  layers <- c(unlist(lapply(VERSIONS$name, function(v) setNames(vals, paste(v, SUMS, sep = "_"))), recursive = FALSE),
              list(land_days = floor(n_all / 2), shore_days = floor(n_all / 4), read_days = n_all))
  r <- rast(lapply(TILE_LAYERS, function(n) setValues(tpl, layers[[n]]))); names(r) <- TILE_LAYERS; r
}
write_tile <- function(r, name, tag = SETTINGS_TAG, sha = SCRIPT_SHA) {   # tags set last: changing a layer drops them
  metags(r) <- c(paleoRF_build = tag, paleoRF_script_sha256 = sha); f <- file.path(tmp, name)
  writeRaster(r, f, overwrite = TRUE, gdal = "COMPRESS=DEFLATE", datatype = "FLT4S"); f
}
good <- write_tile(consistent(), "good.tif")
old_tag <- write_tile(consistent(), "old.tif", tag = "reduction 1")
old_script <- write_tile(consistent(), "old_script.tif", sha = "0123abcd")
r <- consistent(); r[["full_75_n_snow"]] <- r[["full_75_n_all"]] + 1; bad_layer <- write_tile(r, "bad_layer.tif")   # not the last layer
r <- consistent(); r[["full_70_n_all"]] <- r[["full_70_n_all"]] + 1; r[["full_70_n_free"]] <- r[["full_70_n_free"]] + 1
not_nested <- write_tile(r, "not_nested.tif")
r <- consistent(); r[["any_75_s_free"]] <- r[["any_75_n_free"]] + 0.5; too_bright <- write_tile(r, "too_bright.tif")
# Snowy sums at their largest possible value (albedo 1 on every snowy day): each sum is within its
# own bounds, but the "all" sum no longer leaves room for them, so the unknown-snow part is negative.
r <- consistent(); r[["full_nocut_s_snow"]] <- r[["full_nocut_n_snow"]]
unknown_negative <- write_tile(r, "unknown_negative.tif")
r <- consistent(); r[["read_days"]] <- r[["read_days"]] + 10; too_many_days <- write_tile(r, "h00v00_2005_07.tif")  # 41 > 31 days
wrong_crs <- { r <- consistent(); crs(r) <- "EPSG:4326"; write_tile(r, "wrong_crs.tif") }
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
          problem(old_script, template = tpl) == "was made by another version of the build script",
          SCRIPT_SHA != "unknown", nchar(SCRIPT_SHA) == 64,
          grepl("full_75 snowy plus snow-free days exceed all days", problem(bad_layer, template = tpl)),
          problem(not_nested, template = tpl) == "the versions do not nest",
          grepl("any_75 albedo sums outside 0 to their number of days", problem(too_bright, template = tpl)),   # and its unknown part goes negative
          !valid_output(cut_short, template = tpl),       # GDAL loses the trailing names or the data
          problem(good, template = other_grid) == "is not on its tile grid",
          problem(unknown_negative, template = tpl) == "full_nocut unknown-snow sum outside 0 to its number of days",
          problem(too_many_days, template = tpl) == "land, shoreline or class days not whole, non-negative and at most 31 days",
          days_of_file("h11v04_2000_03.tif") == 27, days_of_file("h11v04_2000_03.tmp.tif") == 27,
          days_of_file("h11v04_2004_02.tif") == 29, days_of_file("good.tif") == 31,
          problem(wrong_crs, template = tpl) == "is not on the MODIS sinusoidal projection")

# Reuse: an existing monthly file is taken as done only if its settings tag and script SHA-256 both
# match; otherwise the build stops (before reading any data) and names the file.
reuse_dir <- file.path(tmp, "reuse"); dir.create(reuse_dir)
invisible(file.copy(old_script, file.path(reuse_dir, "h00v00_2005_07.tif")))
stale_msg <- tryCatch(process_tile_month("h00v00", as.Date("2005-07-01"), tmp, reuse_dir), error = conditionMessage)
invisible(file.copy(good, file.path(reuse_dir, "h00v01_2005_07.tif")))
stopifnot(grepl("is stale: it was made by another version of this script", stale_msg),
          identical(process_tile_month("h00v01", as.Date("2005-07-01"), tmp, reuse_dir), file.path(reuse_dir, "h00v01_2005_07.tif")))

# Without its own SHA-256 the script refuses to build or combine monthly files (Chris,
# 2026-10-21): each entry point stops before doing anything else. The folders point at an empty
# scratch folder meanwhile, so that a guard lost by a later edit cannot reach the real data. Being
# empty, it also leaves build_collected no month to hand to build_month (which has its own guard):
# only build_collected's guard can make it stop here.
saved <- list(SCRIPT_SHA, APPEEARS_DIR, RECORDS_DIR, WORK_DIR)
no_sha_dir <- file.path(tmp, "no_sha"); dir.create(no_sha_dir)
SCRIPT_SHA <- "unknown"; APPEEARS_DIR <- file.path(no_sha_dir, "appeears"); RECORDS_DIR <- file.path(no_sha_dir, "records")
WORK_DIR <- no_sha_dir
no_sha <- list(build_collected = tryCatch(build_collected("h00v00", file.path(tmp, "no_such_dir")), error = conditionMessage),
               build_month = tryCatch(build_month("h00v00", as.Date("2005-07-01"), tmp, file.path(tmp, "no_such_dir")), error = conditionMessage),
               cell_table = tryCatch(cell_table("h00v00", data.frame(x = 0.5, y = 0.5), file.path(tmp, "no_such_dir"), as.Date("2005-07-01")),
                                     error = conditionMessage))
SCRIPT_SHA <- saved[[1]]; APPEEARS_DIR <- saved[[2]]; RECORDS_DIR <- saved[[3]]; WORK_DIR <- saved[[4]]
# (a list, and each entry checked, so that an entry point that returns without stopping fails here)
stopifnot(length(no_sha) == 3,
          all(vapply(no_sha, function(m) is.character(m) && grepl("^cannot tell which version of this script is running", m), TRUE)),
          !dir.exists(file.path(tmp, "no_such_dir")), identical(list.files(no_sha_dir), character(0)))

# The "test" step checks for the SHA-256 first, before hashing the July 2005 download. Fed to Rscript
# on standard input, the script cannot hash itself; the tile named is not one of ours, so a test step
# without that check stops on the tile instead, before touching any data.
test_step <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("-", "test", "h99v99"),
                                      stdin = "tools/prepare_bluesky_albedo.R", stdout = TRUE, stderr = TRUE))
stopifnot(!is.null(attr(test_step, "status")), any(grepl("cannot tell which version of this script is running", test_step)))

# Publishing: a new monthly file is checked under its temporary name and renamed only if it passes;
# a file that fails is left as ".tmp.tif" and the tile fails.
pub <- file.path(tmp, "publish"); dir.create(pub)
tagged <- function(r) { metags(r) <- c(paleoRF_build = SETTINGS_TAG, paleoRF_script_sha256 = SCRIPT_SHA); r }   # as process_tile_month does
ok_out <- publish_tile(tagged(consistent()), file.path(pub, "h00v00_2005_07.tif"), SETTINGS_TAG, tpl)
r <- consistent(); r[["full_75_n_snow"]] <- r[["full_75_n_all"]] + 1
bad_msg <- tryCatch(publish_tile(tagged(r), file.path(pub, "h00v01_2005_07.tif"), SETTINGS_TAG, tpl), error = conditionMessage)
stopifnot(identical(ok_out, file.path(pub, "h00v00_2005_07.tif")), file.exists(ok_out),
          !file.exists(file.path(pub, "h00v00_2005_07.tmp.tif")),
          grepl("failed its checks .*snowy plus snow-free days exceed all days", bad_msg),
          !file.exists(file.path(pub, "h00v01_2005_07.tif")), file.exists(file.path(pub, "h00v01_2005_07.tmp.tif")))

# End to end, cell step: small monthly files for two "tiles" and two years -> land vote -> tile
# totals -> cell table. Cell A (10.5, 0.5) spans both tiles: pixel p (tile 1) has 0.15 in 2005 and
# 0.25 in 2006, pixel q (tile 2) 0.55 in 2005 only, one day each (Astra's unequal-coverage case:
# A = (0.35 + 0.25) / 2 = 0.300). Cell B (11.5, 0.5): pixel b has, in 2005, 1 snowy day at 0.8,
# 2 snow-free at 0.2 and 1 of unknown snow status at 0.5, and in 2006 2 snow-free days at 0.3
# (Opus's case: all 0.3625 = 0.125 x 0.8 + 0.75 x 0.26667 + 0.125 x 0.5); its pixel t is land on
# 1 of its 2 days with a class (a tie, so not land) and shoreline on the other, with 0.9 in 2005.
deg <- function(d) R_EARTH * d * pi / 180
tpl1 <- rast(nrows = 2, ncols = 2, crs = SIN_CRS, extent = ext(deg(10), deg(10.5), 0, deg(1)))
tpl2 <- rast(nrows = 2, ncols = 4, crs = SIN_CRS, extent = ext(deg(10.5), deg(11.5), 0, deg(1)))
templates <- list(h01v01 = tpl1, h02v01 = tpl2)
cdir <- file.path(tmp, "monthly_e2e"); dir.create(cdir)
pixel <- function(n_all = 0, n_snow = 0, n_free = 0, s_all = 0, s_snow = 0, s_free = 0, land = 0, shore = 0, read = 0)
  c(n_all = n_all, n_snow = n_snow, n_free = n_free, s_all = s_all, s_snow = s_snow, s_free = s_free, land_days = land, shore_days = shore, read_days = read)
# pixels: one pixel() per cell of the tile, row by row from the top left; sha NA writes no script
# SHA-256, as files made before the files recorded it.
write_monthly <- function(tile, year, pixels, dir = cdir, sha = SCRIPT_SHA) {
  m <- do.call(rbind, pixels)
  layers <- lapply(TILE_LAYERS, function(n) { sum_name <- sub("^(full|any)_[^_]+_", "", n)
    setValues(templates[[tile]], m[, if (n %in% c("land_days", "shore_days", "read_days")) n else sum_name]) })
  r <- rast(layers); names(r) <- TILE_LAYERS
  metags(r) <- c(paleoRF_build = SETTINGS_TAG, if (!is.na(sha)) c(paleoRF_script_sha256 = sha))
  writeRaster(r, file.path(dir, sprintf("%s_%d_07.tif", tile, year)), overwrite = TRUE)
}
none <- pixel()
write_monthly("h01v01", 2005, list(pixel(1, 0, 1, 0.15, 0, 0.15, land = 1, read = 1), none, none, none))
write_monthly("h01v01", 2006, list(pixel(1, 0, 1, 0.25, 0, 0.25, land = 1, read = 1), none, none, none))
write_monthly("h02v01", 2005, list(pixel(1, 0, 1, 0.55, 0, 0.55, land = 1, read = 1), none,
                                   pixel(4, 1, 2, 1.7, 0.8, 0.4, land = 4, read = 4), pixel(1, 0, 1, 0.9, 0, 0.9, land = 1, read = 1),
                                   none, none, none, none))
write_monthly("h02v01", 2006, list(none, none, pixel(2, 0, 2, 0.6, 0, 0.6, land = 2, read = 2), pixel(shore = 1, read = 1),
                                   none, none, none, none))
cells2 <- data.frame(x = c(10.5, 11.5), y = c(0.5, 0.5))
yrs <- as.Date(c("2005-07-01", "2006-07-01"))
ct <- cell_table(names(templates), cells2, cdir, yrs, template_of = function(t) templates[[t]])
A <- ct[ct$x == 10.5, ]; B <- ct[ct$x == 11.5, ]
mix <- function(row, v) { part <- function(p) ifelse(row[[paste0(v, "_share_", p)]] == 0, 0, row[[paste0(v, "_share_", p)]] * row[[paste0(v, "_", p)]])
  part("snowy") + part("snowfree") + part("unknown") }
stopifnot(nrow(ct) == 2, ct$month == c("jul", "jul"),
          near(A$full_75, 0.300, 1e-6), A$full_75_years == 2, near(A$full_75_coverage, 0.75, 1e-6), A$land_pixels == 2,
          B$land_pixels == 1, near(B$full_75, 0.3625, 1e-6),
          near(B$full_75_share_snowy, 0.125, 1e-6), near(B$full_75_snowy, 0.8, 1e-6),
          near(B$full_75_share_snowfree, 0.75, 1e-6), near(B$full_75_snowfree, 0.4 / 1.5, 1e-6),
          near(B$full_75_share_unknown, 0.125, 1e-6), near(B$full_75_unknown, 0.5, 1e-6),
          near(mix(B, "full_75"), B$full_75, 1e-5), near(mix(A, "any_75"), A$any_75, 1e-5),
          is.na(A$full_75_snowy), A$full_75_share_snowy == 0)
LAND_RULE <- "land_or_shore"                              # the tie pixel now counts: B in 2005 is 2.6 / 5
ct_shore <- cell_table(names(templates), cells2, cdir, yrs, template_of = function(t) templates[[t]])
LAND_RULE <- "land"
stopifnot(ct_shore$land_pixels[ct_shore$x == 11.5] == 2, near(ct_shore$full_75[ct_shore$x == 11.5], (2.6 / 5 + 0.3) / 2, 1e-6))
# A missing month, a file with another tag, a dead worker or an error stop the cell step.
missing_month_stops <- inherits(try(cell_table(names(templates), cells2, cdir, c(yrs, as.Date("2007-07-01")),
                                               template_of = function(t) templates[[t]]), silent = TRUE), "try-error")
wrong_tag_stops <- inherits(try(cell_table(names(templates), cells2, cdir, yrs, tag = "reduction 1",
                                           template_of = function(t) templates[[t]]), silent = TRUE), "try-error")
dead_stops <- inherits(try(combine_parts(list(data.frame(a = 1), NULL), c("h01v01", "h02v01")), silent = TRUE), "try-error")
error_stops <- inherits(try(combine_parts(list(data.frame(a = 1), "FAILED h02v01 boom"), c("h01v01", "h02v01")), silent = TRUE), "try-error")
stopifnot(missing_month_stops, wrong_tag_stops, dead_stops, error_stops,
          identical(combine_parts(list(data.frame(a = 1), data.frame()), c("h01v01", "h02v01")), data.frame(a = 1)))

# Only files made by this version of the script are accepted at the cell step (Chris, 2026-10-21):
# the same four files, but one without a script SHA-256 (made before the files recorded it) or
# one with another SHA-256 among them, stop it and name the file; the tagged ones alone pass.
script_check <- function(sha) {
  d <- file.path(tmp, paste0("monthly_sha_", if (is.na(sha)) "none" else sha)); unlink(d, recursive = TRUE); dir.create(d)
  invisible(file.copy(list.files(cdir, full.names = TRUE), d))
  write_monthly("h02v01", 2006, list(none, none, pixel(2, 0, 2, 0.6, 0, 0.6, land = 2, read = 2), pixel(shore = 1, read = 1),
                                     none, none, none, none), dir = d, sha = sha)
  tryCatch(cell_table(names(templates), cells2, d, yrs, template_of = function(t) templates[[t]]), error = conditionMessage)
}
untagged_msg <- script_check(NA)
other_msg <- script_check("0123abcd")
stopifnot(is.character(untagged_msg),
          grepl("another version of this script, not one listed in ACCEPTED_OLDER_SHAS, or do not say which, e.g. .*h02v01_2006_07.tif \\(SHA-256 none recorded, now ",
                untagged_msg),
          is.character(other_msg), grepl("h02v01_2006_07.tif \\(SHA-256 0123abcd, now ", other_msg),
          is.data.frame(script_check(SCRIPT_SHA)))
# An older version listed in ACCEPTED_OLDER_SHAS, checked to compute the same numbers, is accepted
# too (Chris, 2026-10-21), with the same result; one not listed, or a file with no SHA-256, is still
# refused. The note beside the table counts each version's files and names those of older ones.
older_sha <- strrep("ab", 32)
ACCEPTED_OLDER_SHAS <- older_sha
listed <- script_check(older_sha)
unlisted_msg <- script_check(strrep("cd", 32))
untagged_listed_msg <- script_check(NA)
versions <- script_versions(list.files(file.path(tmp, paste0("monthly_sha_", older_sha)), full.names = TRUE))
ACCEPTED_OLDER_SHAS <- c()
stopifnot(is.data.frame(listed), isTRUE(all.equal(listed, ct)),
          is.character(unlisted_msg), grepl(paste0("\\(SHA-256 ", strrep("cd", 32), ", now "), unlisted_msg),
          is.character(untagged_listed_msg), grepl("or do not say which", untagged_listed_msg),
          length(versions) == 2,
          any(grepl(paste0(SCRIPT_SHA, " - 3 files \\(this version\\)$"), versions)),
          any(grepl(paste0(older_sha, " - 1 files \\(an accepted older version\\): h02v01_2006_07.tif$"), versions)))

cat("test_bluesky_albedo: all tests passed\n")
