# Open questions for Andria: general

Maintained by Chris with Claude Code; kept current as work proceeds (see
AGENTS.md). Technical and scientific questions are in
`questions_for_andria_scientific.md`. Updated 2026-09-18. Only questions
that are still open are listed; answered or superseded items are removed
(e.g. the REVEALS and interpolation code, now agreed to be out of scope;
the point / non-interp path, now dropped; the interpolated posteriors and
ice shapefiles, received 2026-09-17).

## 1. Time slices in the interpolated land-cover file

The interpolated land-cover file you sent has 25 time slices: 50, 200,
500, then every 500 years to 11,500, and beyond. The scripts in the repo
use a shorter list, 50, 200, 500 to 11,500 in 500-year steps, and the
draft methods describe a different set again, with irregular bins in the
last two millennia (0-100, 100-350, 350-700, 700-1,500) and 1,000-year
bins before that. Which set of slices should the paper use, and are the
slice boundaries in the file the ones the REVEALS reconstruction was
actually run on? Also, does each slice value in the file represent a bin
centre or a bin edge?

## 2. Content of `veg_posts_interp_ice.RDS`

- The `ice` column is NA for the rows we looked at. What values does it
  take, and does it mean the cell was masked for that slice, or partly
  ice-covered?
- Are the 200 `iter` values posterior draws from the interpolation model
  (so that each draw is a coherent map), or independent per cell?
- `cell_area`: units (km²?) and on which projection?
- Cells: 2,870. Is that the full land mask of the study region, and is
  the 1-degree grid the same as `data/grid.RDS`?

## 3. Data files still needed for scripts 7a and 8

- `data/Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif`: the Dalton et
  al. 2020 ice-fraction raster interpolated to the time slices (script 7a).
  Checked 2026-09-19: the Dalton margins are paywalled with no open
  repository found, and this file is a derived interpolation specific to
  this project, so it cannot be downloaded. See C0b.
- `data/albedo_glacier_monthly.csv`: the monthly glacier albedo assigned
  to ice-covered cells (scripts 7 and 7a). Checked 2026-09-19: this is
  not a published dataset but a set of chosen values (columns `month`,
  `ice_albedo`, `ice_albedo_fixed`, `ice_albedo_sc`), so we cannot
  obtain it independently. See C0a in the scientific questions.
- `data/radiative-kernels/CACKv1.0/CACKv1.0.nc` (script 8): the other two
  kernels were downloaded on 2026-09-19 from Zenodo (HadGEM3
  doi:10.5281/zenodo.3594673, CAM5 doi:10.5065/D6F47MT6) and verified to
  contain the variables the script reads. CACK is published through the
  Environmental Data Initiative (`edi.396.1`), whose portal is behind a
  human-verification check, so it needs a manual download. Do you still
  have the file? Also: did you preprocess any of the three, or use them
  as downloaded? And which CACK band is band 3?

## 4. Provenance to record in the data documentation

- The blue-sky albedo GeoTIFF (`blue_sky_monthly_2000-2009.tif`): the
  MODIS MCD43A3 v061 plus ERA5 construction described in the methods. Is
  the code for that step available anywhere, and who ran it?
- `grid.RDS`, the political-boundary polygons and `taxon2LCT_translation_v2.0.csv`:
  origin and version.
- The R version and package versions used for the results in the talk
  (there is no renv lock or sessionInfo in the repo).

## 5. Method choices to confirm for the write-up

- The manuscript says 1,000 posterior samples per cell and month; the
  scripts draw 100. Which is intended?
- Snow is never modelled: the 2000-2009 snow climatology enters through
  the location and elevation terms and is held fixed for every slice. You
  said the snow / water-budget branch was dropped deliberately. Should the
  paper state the fixed-modern-snow assumption explicitly in methods and
  limitations?

## 6. Repository logistics

- The 334 MB posteriors file is now in the repo via Git LFS. LFS storage
  and download quota count against the repo owner's GitHub account (free
  tier: 1 GB each). Is that acceptable, or should large files live
  elsewhere (e.g. Zenodo / OSF with a download script)?
- Are the archived scripts (`scripts/archive/`, the Thornthwaite,
  ClimateNA and GCM snow work) safe to remove from the working tree and
  keep only in git history?

## 7. Small code questions raised by the configuration migration (2026-10-08)

Found while designing the move to a single config file. None is urgent;
answers let us delete code rather than carry it.

- **Three map windows.** Script 7 draws its maps on -166..-50 E, 12..82 N
  (scripts 7a and 8 also set that window but never drew with it; the unused
  copies were removed in migration issue I03, 2026-10-13); scripts 2 and 3
  use -172..-50, 15..80; script 1's diagnostics use -172..-50, 17..79.
  Inherited, not chosen; on the migration branch they are now the three
  `presentation$map_windows` in config.R. One window for the paper, or keep
  the two?
- **Model 8 is fitted three times** in script 4 (lines 98, 154 and 234, the
  same formula each time, saved under three names). Can the second and
  third fits go, with the selected model read from one file everywhere?
- **The model-structure sensitivity check** (`6_prediction_model_spatial_eval.R`)
  cannot run at present: it needs the `SemiPar` and `pwiser` packages, which
  are not installed, uses an undefined variable at line 278, and its tail
  from line 326 reads point-flavour files that no longer exist. Is this
  analysis still wanted for the paper? If yes we will rebuild it on the
  shared functions; if no we will retire it.
