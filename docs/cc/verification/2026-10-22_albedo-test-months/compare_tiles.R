# Read-only: the scratch deleting build's July 2005 tiles against the real kept build's.
suppressMessages(library(terra))
a = sort(list.files('/home/expert/Documents/contracting/Andria/paleo-RF/data/modis_albedo/monthly', pattern = '_2005_07\\.tif$', full.names = TRUE))
b = sort(list.files(commandArgs(TRUE)[1], pattern = '_2005_07\\.tif$', full.names = TRUE))
stopifnot(length(a) == 38, identical(basename(a), basename(b)))
diffs = 0; layers = 0
for (i in seq_along(a)) {
  ra = rast(a[i]); rb = rast(b[i])
  stopifnot(identical(names(ra), names(rb)), compareGeom(ra, rb))
  va = values(ra); vb = values(rb)
  layers = layers + ncol(va)
  same = (is.na(va) & is.na(vb)) | (!is.na(va) & !is.na(vb) & va == vb)
  if (!all(same)) { diffs = diffs + 1; cat(basename(a[i]), ': ', sum(!same), ' values differ\n') }
  ta = metags(ra); tb = metags(rb)
  keep = !grepl('created', ta[, 1])
  if (!identical(ta[keep, , drop = FALSE], tb[!grepl('created', tb[, 1]), , drop = FALSE])) cat(basename(a[i]), ': tags other than the creation time differ\n')
}
cat(length(a), 'tiles,', layers, 'layers compared;', diffs, 'tiles with any differing value\n')
