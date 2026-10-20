# Check of the AppEEARS version of tools/prepare_bluesky_albedo.R (Chris, 2026-10-20): tile
# h11v04, July 2005, built from the AppEEARS download ("test h11v04") must equal, layer by
# layer and pixel by pixel, the same tile built on 2026-10-17 from NASA's original files by the
# first version of the script (commit e97ec44). That version applied the noon solar zenith rule
# and the AppEEARS July download has no zenith layer, so equality also shows that the rule
# removes nothing in July. Run from the repository root of the main checkout, where data/ is.
library(terra)
new = rast('data/modis_albedo/test_appeears/h11v04_2005_07.tif')
ref = rast('data/modis_albedo/monthly/h11v04_2005_07.tif')
stopifnot(identical(names(new), names(ref)), compareGeom(new, ref))
ok = TRUE
for (l in names(ref)) {
  a = values(new[[l]], mat = FALSE); b = values(ref[[l]], mat = FALSE)
  same_na = identical(is.na(a), is.na(b))
  differ = sum(a != b, na.rm = TRUE)
  cat(sprintf('%-18s values %8d  NA same: %-5s  differing: %d\n', l, sum(!is.na(b)), same_na, differ))
  ok = ok && same_na && differ == 0
}
if (!ok) stop('the AppEEARS tile differs from the reference tile')
cat('PASS: all', nlyr(ref), 'layers identical\n')
