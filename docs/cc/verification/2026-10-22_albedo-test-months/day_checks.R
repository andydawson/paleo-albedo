# Read-only checks on one test month's AppEEARS day files (22 Oct 2026).
# 1. Does a blank land/water class coincide with no albedo? Counts pixel-days by
#    (class blank or not) x (black-sky albedo present or not), and the classes of
#    pixel-days that have a class but no albedo.
# 2. For March 2005 only: white-sky minus black-sky albedo, by noon solar zenith band and by
#    snow flag, over pixel-days with both albedos in 0-1.
suppressMessages(library(terra))
month = commandArgs(TRUE)[1]
dir = file.path('data/modis_albedo/appeears', month)
files = list.files(dir, pattern = '\\.tif$', full.names = TRUE)
doy = sub('.*_doy(\\d{7}).*', '\\1', basename(files))
layer_of = function(p) files[grepl(p, basename(files), fixed = TRUE)]
L = list(bsa = 'Albedo_BSA_shortwave', wsa = 'Albedo_WSA_shortwave', land = 'LandWaterType',
         sza = 'LocalSolarNoon', snow = 'Snow_BRDF_Albedo')
fill = c(bsa = 32767, wsa = 32767, land = 255, sza = 255, snow = 255)
days = sort(unique(doy))
cat(month, ': ', length(days), ' days, ', min(days), ' to ', max(days), '\n', sep = '')
open = function(f) { r = rast(f); scoff(r) = cbind(1, 0); r }
read = function(r, name, row, nrows) { v = readValues(r, row, nrows); v[v == fill[name]] = NA; v }
want_diff = month == '2005-03'
cross = c(blank_with_albedo = 0, blank_no_albedo = 0, class_with_albedo = 0, class_no_albedo = 0)
class_no_albedo = class_with_albedo = setNames(numeric(8), 0:7)   # classes 0-7
breaks = seq(-1, 1, by = 0.005)
diff_hist = list()
for (d in days) {
  r = lapply(L, function(p) open(files[doy == d & grepl(p, basename(files), fixed = TRUE)]))
  e = sapply(r, function(x) as.vector(ext(x)))
  if (max(abs(e - e[, 1])) > 0.01) stop(d, ': layers do not share one extent')
  for (x in r) readStart(x)
  b = blocks(r$bsa, n = 8)
  for (i in seq_len(b$n)) {
    v = lapply(names(L), function(n) read(r[[n]], n, b$row[i], b$nrows[i])); names(v) = names(L)
    has = !is.na(v$bsa); cls = !is.na(v$land)
    cross = cross + c(sum(!cls & has), sum(!cls & !has), sum(cls & has), sum(cls & !has))
    class_no_albedo = class_no_albedo + tabulate(v$land[cls & !has] + 1, 8)
    class_with_albedo = class_with_albedo + tabulate(v$land[cls & has] + 1, 8)
    if (want_diff) {
      bsa = v$bsa * 0.001; wsa = v$wsa * 0.001
      ok = !is.na(bsa) & !is.na(wsa) & bsa >= 0 & bsa <= 1 & wsa >= 0 & wsa <= 1
      zb = cut(v$sza[ok], c(0, 30, 45, 60, 70, 75, 80, 90), include.lowest = TRUE)
      zb = addNA(zb)
      sn = factor(ifelse(is.na(v$snow[ok]), 'unknown', ifelse(v$snow[ok] == 1, 'snow', 'free')))
      key = paste(zb, sn)
      dd = wsa[ok] - bsa[ok]
      for (k in unique(key)) {
        h = tabulate(findInterval(dd[key == k], breaks, all.inside = TRUE), length(breaks) - 1)
        diff_hist[[k]] = if (is.null(diff_hist[[k]])) h else diff_hist[[k]] + h
      }
    }
  }
  for (x in r) readStop(x)
}
cat('pixel-days by land/water class (blank or not) and black-sky albedo (present or not):\n')
print(cross)
cat('classes of pixel-days with a class but no albedo:\n'); print(class_no_albedo)
cat('classes of pixel-days with an albedo:\n'); print(class_with_albedo)
if (want_diff) {
  mids = head(breaks, -1) + 0.0025
  q = function(h, p) mids[which(cumsum(h) >= p * sum(h))[1]]
  out = do.call(rbind, lapply(sort(names(diff_hist)), function(k) {
    h = diff_hist[[k]]
    data.frame(group = k, n = sum(h), mean = round(sum(h * mids) / sum(h), 4),
               p05 = q(h, 0.05), median = q(h, 0.5), p95 = q(h, 0.95), share_negative = round(sum(h[mids < 0]) / sum(h), 3))
  }))
  all = Reduce(`+`, diff_hist)
  out = rbind(out, data.frame(group = 'ALL', n = sum(all), mean = round(sum(all * mids) / sum(all), 4),
              p05 = q(all, 0.05), median = q(all, 0.5), p95 = q(all, 0.95), share_negative = round(sum(all[mids < 0]) / sum(all), 3)))
  cat('white-sky minus black-sky albedo, by noon zenith band (degrees) and snow flag (binned to 0.005):\n')
  print(out, row.names = FALSE)
}
