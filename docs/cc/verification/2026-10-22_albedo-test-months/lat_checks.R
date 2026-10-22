# Read-only: northernmost land pixel with at least one qualifying day, per test month and
# version, from the monthly tiles; and the latitude where the noon sun reaches each cut-off on
# the month's most favourable day (cut-off + highest solar declination of the days used).
suppressMessages(library(terra))
R = 6371007.181
months = c('2005-12', '2005-03', '2000-03', '2005-07')
first = c('2005-12' = '2005-12-01', '2005-03' = '2005-03-01', '2000-03' = '2000-03-05', '2005-07' = '2005-07-01')
decl = function(d) { n = as.numeric(format(d, '%j')); 23.44 * sin(2 * pi * (284 + n) / 365) }  # Cooper 1969
versions = c(full_75 = 75, any_75 = 75, full_70 = 70, full_nocut = Inf)
rows = list()
for (m in months) {
  f = list.files('data/modis_albedo/monthly', pattern = paste0('_', sub('-', '_', m), '\\.tif$'), full.names = TRUE)
  stopifnot(length(f) == 38)
  d1 = as.Date(first[m]); days = seq(d1, seq(as.Date(paste0(m, '-01')), by = 'month', length.out = 2)[2] - 1, by = 'day')
  top = setNames(rep(-Inf, length(versions)), names(versions)); n_north60 = setNames(numeric(length(versions)), names(versions))
  for (tf in f) {
    r = rast(tf)
    lat = yFromRow(r, 1:nrow(r)) / R * 180 / pi
    land = values(r[['land_days']]) > values(r[['read_days']]) / 2 & values(r[['read_days']]) > 0
    rowlat = rep(lat, each = ncol(r))
    for (v in names(versions)) {
      has = values(r[[paste0(v, '_n_all')]]) > 0 & land
      if (any(has)) top[v] = max(top[v], max(rowlat[has]))
      n_north60[v] = n_north60[v] + sum(has & rowlat > 60)
    }
  }
  for (v in names(versions))
    rows[[length(rows) + 1]] = data.frame(month = m, version = v, northernmost_land_pixel = round(top[v], 2),
      sun_limit = if (is.finite(versions[v])) round(versions[v] + max(decl(days)), 1) else NA,
      land_pixels_north_of_60N = n_north60[v])
}
print(do.call(rbind, rows), row.names = FALSE)
