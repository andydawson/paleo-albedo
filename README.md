# paleo-RF

Estimating Holocene radiative forcing from land-cover change in North
America, using land cover inferred from fossil pollen, a modern
satellite-albedo calibration, and radiative kernels.

**Status: research code under active development.** The pipeline runs end
to end as of 2026-09-21 and its outputs are frozen as regression anchors.
Since 2026-09-24 the trunk carries the interp flavour only (see below).
This README was last brought up to date on 2026-09-24. The authoritative
descriptions of the scripts and data are in `docs/cc/`; see the index at
the end.

## What the pipeline does

1. Land-cover fractions per 1-degree cell and time slice, three classes:
   evergreen trees (ET), summergreen trees (ST), open land (OL). These
   come from the REVEALS pollen-vegetation model followed by a Bayesian
   spatial interpolation with an ice mask, both done outside this
   repository (Dawson et al., Climate of the Past); the product is a
   fixed input here.
2. A modern blue-sky albedo climatology (MODIS MCD43A3 + ERA5, 2000-2009,
   monthly) is sampled at the modern cells.
3. Twelve monthly calibration models (beta-regression GAMs, `mgcv::bam`)
   relate albedo to location, elevation and the three cover fractions.
4. The models hindcast albedo for every Holocene slice from the paleo
   cover fractions, with posterior draws.
5. Albedo differences between consecutive slices are split into
   vegetation and ice-sheet contributions and multiplied by a radiative
   kernel to give forcing in W/m² per cell and month.
6. The per-cell forcing is aggregated to continental values by period and
   set beside the modern IPCC forcing agents.

Diagram: `docs/cc/2026-09-17_methodology_schematic.png`.

## Repository layout

| Path | Holds |
|---|---|
| `scripts/` | the R pipeline, numbered in run order |
| `R/` | helpers sourced by the scripts (`run_manifest.R`) |
| `tools/` | shell and Python helpers that are not part of the pipeline |
| `data/` | inputs, plus some intermediates the scripts still write here |
| `output/` | fitted models, predictions, forcing (git-ignored except small summary tables) |
| `figures/` | figures (git-ignored; regenerable) |
| `runs/` | one provenance manifest per script run |
| `tests/anchors/` | frozen outputs a refactor must reproduce |
| `docs/cc/` | reviews, plans, question lists and other generated documents |
| `writing/` | the draft manuscript, the EGU talk and the reference papers |

## The point flavour, and where it went

The pipeline originally ran on the ~505 cells that contain pollen sites
(the point, or non-interp, flavour). Shortly before the EGU talk it was
switched to the spatially complete, interpolated maps (the interp
flavour, files carrying `_interp`), which is the analysis in the talk
and the draft paper. By agreement with Andria (2026-09-17) the point
flavour is not developed further, and on **2026-09-24 it was removed
from `chris-dev`**: each script now contains its interp code only, every
line unchanged, with the point blocks, the guards that skipped them and
the commented-out dead code gone. Script 1 went from 258 lines to 37.

The point code is preserved and reachable: tag `v0-legacy` (as
received), tag `v1-both-flavours` (the last trunk commit with both
flavours, immediately before the removal), branch `legacy`, and
`origin/run-nointerp` / `origin/run-may` (the two runs made of it). Its
outputs are anchored in `tests/anchors/nointerp-*` and its scripts and
data are documented in `docs/cc/README_nointerp.md`, whose line numbers
refer to `v1-both-flavours`.

One thing the removal did not change: the scripts loop over all twelve
months and cannot run a single month without editing the loop. Making
the month set a setting is on the roadmap for the config stage
(the staged plan, archived 2026-09-25).

## Scripts (`scripts/`)

Files are named as the scripts write them, with `<month>` standing for
`jan` ... `dec`. Line numbers (`script:line`) refer to the scripts on
`chris-dev` as of 2026-09-24 and give the statement that loads the file. Everything the
pipeline writes goes under `output/` except where a script still writes
into `data/`.

| Script | Does |
|---|---|
| `1_veg_lct_prep.R` | mean over the 200 posterior draws; ET/ST/OL per cell and slice; elevation from a committed cache (first built from AWS terrain tiles); modern (age 50) vs paleo split. With `diagnostic = TRUE` in the run section of `config.R` it also reports on the discarded ice flag, the absent cell-slices and mean-versus-median, and maps the ice flag at every slice into `figures/diagnostics/`
| `2_calibration_lct_bluesky.R` | monthly blue-sky albedo at the modern cells, native pixel and 1-degree mean |
| `3_plot_cal_lct_albedo.R` | diagnostic plots of the calibration data (optional) |
| `4_calibration_model.R` | model ladder mod1-mod8 for every month; AIC table; spatial-effects experiment |
| `5_calibration_eval.R` | fit diagnostics; saves the selected model (config.R `calibration$model_id`, model 8 by default) per month |
| `6_prediction_model.R` | hindcast albedo per slice and month with 100 draws |
| `6_prediction_model_spatial_eval.R` | sensitivity of hindcasts to model structure (optional) |
| `7_plot_preds.R` | albedo, uncertainty and difference maps with ice overlays; coarse (7-period) albedo differences |
| `7a_alb_diff_full.R` | consecutive-slice albedo differences split into vegetation and ice parts |
| `8_radiative.R` | forcing = albedo change x kernel, three kernels, ten variants |
| `9_forcing_barplot.R` | continental forcing by period beside the IPCC AR6 agents, plus kernel-spread and variant-sensitivity figures; every aggregation choice is an explicit assumption in its header |

What each script reads and writes (`script:line` is the load statement):

| Script | Reads | Writes |
|---|---|---|
| `1_veg_lct_prep.R` | `data/veg_posts_interp_ice.RDS` (`1:12`) | `data/lct_modern_reveals_interp.RDS`<br>`data/lct_paleo_reveals_interp.RDS` |
| `2_calibration_lct_bluesky.R` | `data/blue_sky_monthly_2000-2009.tif` (`2:38`)<br>`data/grid.RDS` (`2:15`)<br>`data/lct_modern_reveals_interp.RDS` (`2:19`)<br>`pbs_ll.RDS` (`2:36-37`) | `data/calibration_modern_lct_interp_bluesky.RDS`<br>`..._coarse.RDS`<br>albedo maps |
| `3_plot_cal_lct_albedo.R` | `data/calibration_modern_lct_interp_bluesky.RDS` (`3:38`)<br>`scripts/make_grid.R` (`3:51`, a reconstruction; the original is not in the repo) | figures (`figures/LCT_*_interp.*`); the albedo scatter plots print to `Rplots.pdf` |
| `4_calibration_model.R` | `data/calibration_modern_lct_interp_bluesky.RDS` (`4:16`) | `output/calibration/calibration_mod{1..8}_interp_<month>_bluesky.RDS`<br>`AIC_table.csv`<br>spatial-experiment fits |
| `5_calibration_eval.R` | the model files above (`5:19`) | `output/calibration/calibration_mod_interp_selected_<month>_bluesky.RDS`<br>`calibration_model_stats.csv`<br>figures |
| `6_prediction_model.R` | selected models (`6:24`)<br>`data/lct_paleo_reveals_interp.RDS` (`6:17`) | `output/prediction/paleo_interp_predict_gam{_samps,_summary}_<month>_bluesky.RDS` and the merged `..._bluesky.RDS` files |
| `6_prediction_model_spatial_eval.R` | spatial-experiment fits<br>`data/lct_paleo_reveals_interp.RDS` | `output/prediction/*spatial_eval*`<br>figures |
| `7_plot_preds.R` | `output/prediction/paleo_interp_predict_gam[_summary]_bluesky.RDS` (`7:84`)<br>`data/grid.RDS` (`7:148`)<br>`data/map-data/ice/glacier_shapefiles_21-1k.RDS` (`7:46`)<br>`data/albedo_glacier_monthly.csv` (`7:163`)<br>`pbs*.RDS` (`7:27-28`) | figures<br>`data/ice_fort*.RDS`<br>`data/alb_interp_preds_diffs_bluesky.RDS` |
| `7a_alb_diff_full.R` | `output/prediction/paleo_interp_predict_gam_summary_bluesky.RDS` (`7a:40`)<br>`data/Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif` (`7a:44`)<br>`data/albedo_glacier_monthly.csv` (`7a:121`)<br>`data/grid.RDS` (`7a:98`)<br>`pbs*.RDS` (`7a:77-78`) | `data/ALB_diffs_bluesky.RDS` |
| `8_radiative.R` | `data/ALB_diffs_bluesky.RDS` (`8:59`)<br>`data/ice_fort*.RDS` (`8:38-40`)<br>kernels (`8:57`, `8:97`, `8:107`)<br>`pbs*.RDS` (`8:54-55`) | `output/forcing/RF_holocene_all_cases.RDS` |
| `9_forcing_barplot.R` | `output/forcing/RF_holocene_all_cases.RDS` (`9:141`)<br>`data/alb_interp_preds_diffs_bluesky.RDS` (`9:325`)<br>`data/ipcc-ar6/*.csv` (`9:199-201`) | `output/forcing/forcing_by_period*.csv`<br>`modern_ipcc_ar6_erf.csv`<br>figures |

Not part of the pipeline: `GCM_snow_prob.R`, `thornthwaite.R`,
`beta_veg_lct_modern.R` and `scripts/archive/` (abandoned climate/snow
branch and earlier versions).

Run order is 1 -> 2 -> 4 -> 5 -> 6 -> 7 -> 7a -> 8 -> 9; 3 and
6-spatial-eval are optional, and 7 and 7a are independent of each other.
Every script logs a provenance manifest (see below).

Diagnostics: script 1 has a diagnostic switch (`run$diagnostic` in
`config.R`, off by default) that adds checks and plots without changing
any output; the plots go to `figures/diagnostics/`. The
pattern is meant to be extended to the other scripts.

Measured runtimes on this machine (48 cores, capped as noted, one R thread
in the loops): script 1 ~4 min (mostly the elevation lookup); 2 ~20 min;
4 ~27 h on 8 cores, four fifths of it model 7 (the Gaussian-process cover
smooth) at ~2 h per month, every other model 1 to 5 min; 5 ~4 min; 6
~15 min; 7 62 min; 7a 52 min; 8 20 s; 9 seconds. Scripts 7 and 7a are
dominated by a per-cell `rbind` loop that is quadratic in output rows,
which is a planned consolidation.

Known stop: script 5 reads the spatial-experiment models from
`output/calibration/spatial_experiment/` but script 4 writes them to
`output/calibration/` (known issue 13). Its essential outputs are written
before that point, so the pipeline continues.

## Data assets, interp flavour (`data/`, `output/`)

Legend: <mark>unknown</mark> = provenance not yet confirmed with Andria.
Roles: **input** (external, no script here produces it), **derived**
(produced by a numbered script), **result** (end product). No required
input is missing as of 2026-09-21.

### External inputs

| File | Used by | Status |
|---|---|---|
| `data/veg_posts_interp_ice.RDS` (334 MB, Git LFS) | input to 1 (`1:12`) | present |
| `data/blue_sky_monthly_2000-2009.tif` | input to 2 (`2:38`) | present |
| `data/grid.RDS` | input to 2, 7, 7a (`2:15`, `7:148`, `7a:98`) | present |
| `data/map-data/geographic/pbs.RDS`, `pbs_ll.RDS`, `PoliticalBoundaries/` | input to 2 (`pbs_ll` only), 3, 7, 7a, 8 (`2:36-37`, `3:39-40`, `7:27-28`, `7a:77-78`, `8:54-55`) | present |
| `data/elev_interp_cells.RDS` | input to 1 (`1:57`) | present (16 KB, committed) |
| `data/map-data/ice/glacier_shapefiles_21-1k.RDS` | input to 7 (`7:46`) | present |
| `data/albedo_glacier_monthly.csv` | input to 7, 7a (`7:163`, `7a:121`) | present |
| `data/Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif` | input to 7a (`7a:44`) | present |
| `data/radiative-kernels/HadGEM3-GA7.1_TOA_kernel_L19.nc` | input to 8 (`8:57`) | present (157 MB, Git LFS) |
| `data/radiative-kernels/CAM5/alb.kernel.nc` | input to 8 (`8:97`) | present (21 MB) |
| `data/radiative-kernels/CACKv1.0/CACKv1.0.nc` | input to 8 (`8:107`) | present (126.5 MB, Git LFS) |
| `data/ipcc-ar6/AR6_ERF_1750-2019{,_pc05,_pc95}.csv` | input to 9 (`9:199-201`) | present |
| `scripts/make_grid.R` | sourced by 3 (`3:57`, `3:345`) | present (reconstructed) |

What each input is and where it came from:

| File | What | Provenance |
|---|---|---|
| `veg_posts_interp_ice.RDS` | interpolated land-cover posteriors: 2,860 cells x 25 slices x 200 draws x 3 classes, plus `cell_area` and a binary `ice` flag (7.2% of rows). Script 1 averages the draws and **drops `cell_area` and `ice`**, so neither reaches the rest of the pipeline | Andria, 2026-09-17; REVEALS + Bayesian spatial interpolation with ice mask, from the Climate of the Past land-cover paper |
| `blue_sky_monthly_2000-2009.tif` | 12-band monthly blue-sky albedo, 0.25 degree, 2000-2009 mean | MODIS MCD43A3 v061 + ERA5 as described in manuscript §2.1; <mark>unknown</mark> who built it and with what code |
| `grid.RDS` | 1-degree lon/lat raster with cell ids (-172 to 127 E, 17 to 79 N) | <mark>unknown</mark> (committed 2023-05-16, no generating code) |
| `pbs.RDS` | political boundaries, projected and lon/lat; includes ocean polygons | <mark>unknown</mark> |
| `elev_interp_cells.RDS` | point elevation (m) at each of the 2,870 cell centres: x, y, elev | fetched once from AWS terrain tiles by `elevatr` on 2026-09-25 and committed, so no network is needed; delete the file to force a fresh lookup |
| `glacier_shapefiles_21-1k.RDS` | 21 ice-margin polygon sets, 1,000-year steps, 21 to 1 ka, lon/lat. **The project's ice chronology**: a point-in-polygon test reproduces the `ice` flag above exactly (69/69 cells at 6 ka, 213/213 at 8 ka, 596/596 at 10 ka, 780/780 at 11 ka), so the flag was derived from these | Andria, 2026-09-17; <mark>unknown</mark> original source of the margins, but pre-dates Dalton 2020 |
| `albedo_glacier_monthly.csv` | monthly albedo assigned to ice-covered cells; three columns offering alternative conventions (`ice_albedo` seasonal 0.6-0.8, `ice_albedo_fixed` constant 0.68, `ice_albedo_sc` smoothly varying 0.56-0.80) | Andria, 2026-09-20; <mark>unknown</mark> literature source for the values and which column is preferred |
| `dalton_interpolated_LC6k.tif` | continuous ice **fraction** per cell and slice, used to mix vegetation and ice albedo by area; needed because the polygons above are binary. 26 layers named `yr<n>bp`, 12,000 to 50 BP, 116 x 62 cells, lon/lat WGS84, values 0 to 1; every one of the 25 pipeline ages matches a layer | Dalton et al. 2020 margins interpolated to the slices; Andria, 2026-09-20; <mark>unknown</mark> who did the interpolation and by what method |
| `HadGEM3-GA7.1_TOA_kernel_L19.nc` | HadGEM3 albedo kernel, clear-sky, top of atmosphere, W/m² per 1% | Smith (2019), Zenodo doi:10.5281/zenodo.3594673, CC-BY-4.0 |
| `alb.kernel.nc` | CAM5 albedo kernel; the script reads `FSNSC`, a **surface** flux (question C3) | Pendergrass (2017), doi:10.5065/D6F47MT6, CC-BY-4.0 |
| `CACKv1.0.nc` | CACK all-sky TOA albedo kernel, 180 x 360 x 12 months x 16 years, W/m² per unit albedo. The code's `band=3` selects **year 2003** (the file's Year dimension starts at 2001), not a sky condition; a climatological mean `CACK CM` and uncertainty layers are in the same file (question C3) | Bright and O'Halloran (2019), EDI doi:10.6073/pasta/d77b84b11be99ed4d5376d77fe0043d8; downloaded by hand 2026-09-21 |
| `AR6_ERF_1750-2019{,_pc05,_pc95}.csv` | IPCC AR6 WG1 Chapter 7 effective radiative forcing 1750-2019, best estimate and 5-95% bounds, by agent | github.com/IPCC-WG1/Chapter-7 `data_output/`, downloaded 2026-09-21 |
| `make_grid.R` | helper building a 2-degree grid for the diagnostic maps | original not in repo; the copy present is a reconstruction (2026-09-16) |

### A note on ice

The pipeline needs to know where the ice was at each time slice, and it
answers that question from two files rather than one.

`glacier_shapefiles_21-1k.RDS` is the chronology. It supplies the mask
that stopped the vegetation interpolation inventing plants under ice
(the `ice` flag inside `veg_posts_interp_ice.RDS`, verified identical to
a point-in-polygon test of the polygons), and it supplies the ice
outlines that `7_plot_preds.R` draws on the maps. That part is
self-consistent.

The Dalton raster is used by `7a_alb_diff_full.R` for one narrow reason:
to split a cell's albedo change into a vegetation part and an ice part,
that script needs a *fraction* of the cell covered by ice, and the
polygons give only yes or no. Dalton supplies a fraction.

Rasterising the polygons already in the repository onto the 1-degree
grid with fractional coverage would produce the same quantity and leave
the pipeline with a single source of truth for ice. The trade-off is
chronology rather than method: Dalton et al. 2020 updates the older
reconstruction the polygons appear to come from, so deriving the
fraction ourselves means using the older margins consistently instead of
newer margins for the fraction and older ones for everything else. That
choice is with Andria, as question C0 in
`docs/cc/questions_for_andria_scientific.md`.

A second, separate point: `1_veg_lct_prep.R` discards the `ice` and
`cell_area` columns when it averages the posterior draws, which is why
the later scripts have to go looking for ice again at all.

### Derived files and results

All produced on this machine between 2026-09-19 and 2026-09-21 and frozen
in `tests/anchors/`; the copies in `data/` and `output/` are regenerable
and git-ignored.

| File | Written by | Read by | Anchor |
|---|---|---|---|
| `data/lct_modern_reveals_interp.RDS`<br>`data/lct_paleo_reveals_interp.RDS` | 1 | 2 (`2:19`), 6 (`6:17`) | `interp-allmonths-2026-09-20` |
| `data/calibration_modern_lct_interp_bluesky.RDS`<br>`..._coarse.RDS` | 2 | 3 (`3:323`), 4 (`4:16`), 5 (`5:19`) | `interp-allmonths-2026-09-20` |
| `output/calibration/calibration_mod{1..8}_interp_<month>_bluesky.RDS`<br>`AIC_table.csv`<br>spatial-experiment fits | 4 | 4, 5, 6-spatial-eval | AIC table and stats only (models are 3.6 GB) |
| `output/calibration/calibration_mod_interp_selected_<month>_bluesky.RDS`<br>`calibration_model_stats.csv` | 5 | 6 (`6:24`) | stats only |
| `output/prediction/paleo_interp_predict_gam{_samps,_summary}_<month>_bluesky.RDS`; merged `..._bluesky.RDS`<br>`..._summary_bluesky.RDS` | 6 | 7 (`7:84`), 7a (`7a:40`) | summaries in `interp-allmonths-2026-09-20` |
| `data/ice_fort.RDS`<br>`ice_fort_diff_young.RDS`<br>`ice_fort_diff_old.RDS`<br>`data/alb_interp_preds_diffs_bluesky.RDS` | 7 | 8 (`8:38-40`), 9 (`9:325`) | `interp-tail-2026-09-21` |
| `data/ALB_diffs_bluesky.RDS` (result: albedo differences) | 7a | 8 (`8:59`) | `interp-tail-2026-09-21`, reproduced byte for byte on re-run |
| `output/forcing/RF_holocene_all_cases.RDS` (result: forcing) | 8 | 9 (`9:141`) | `interp-tail-2026-09-21` (Git LFS) |
| `output/forcing/forcing_by_period*.csv`<br>`modern_ipcc_ar6_erf.csv` | 9 |  | tracked in git (small) |

### Other files in `data/`

Everything else in `data/` belongs to the point flavour (see
`docs/cc/README_nointerp.md`) or to abandoned branches: the `_albclim`,
`calibration_model*`, `cal_data`, `calibration-albedo-climate*`,
`lct_albedo_snow_modern_*`, `lct_paleo.RDS`, `pollen-modern-slice`,
`*_CRU.csv`, `*_GCM.csv` and `climate_CRU.csv` files are read by no
current script and are candidates for removal once confirmed.

Large files: anything over 50 MB is tracked with Git LFS; install
`git-lfs` before cloning or you will get pointer files. `.lfsconfig`
excludes `tests/anchors/**` from fetches, so a fresh clone downloads the
three data inputs (617 MB) and leaves the 139 MB anchored forcing table
as a pointer; fetch it when needed with
`git lfs pull --include="tests/anchors/**"`. The two
downloadable kernels can also be re-fetched with
`bash tools/download_kernels.sh`; CACK must be downloaded by hand from
the EDI portal (link in the table above) and unzipped into
`data/radiative-kernels/CACKv1.0/`.

## Environment

R 4.5 with `mgcv`, `terra`, `raster`, `sp`, `sf`, `dplyr`, `tidyr`,
`reshape2`, `ggplot2`, `patchwork`, `gratia`, `tidyterra`, `scico`,
`ggtern`, `tricolore`, `scatterpie`, `fields`, `brms`, `elevatr` (needs
network). On the development machine there is no system R; a user-space
environment is used:

```
micromamba create -n paleo-rf -c conda-forge r-base r-terra r-raster r-sp r-sf \
  r-dplyr r-tidyr r-ggplot2 r-reshape2 r-mgcv r-gam r-scales r-ggally r-rastervis \
  r-scico r-scatterpie r-ggtern r-brms r-fields r-units r-ggbreak r-ncdf4 r-maps \
  r-geosphere r-patchwork
micromamba run -n paleo-rf Rscript -e 'install.packages(c("gratia","tidyterra","elevatr","tricolore"))'
```

For the albedo rebuild (reading MODIS HDF4 and ERA5 netCDF files; added 2026-10-15, existing
packages frozen, pipeline outputs checked unchanged against the regression anchors):

```
micromamba install -n paleo-rf -c conda-forge --freeze-installed libgdal-hdf4 libgdal-netcdf \
  r-getpass r-keyring r-rstudioapi r-filelock r-yaml
micromamba run -n paleo-rf Rscript -e 'install.packages("ecmwfr", dependencies = FALSE)'
```

Run scripts from the repository root. **Cap the BLAS threads to the cores
you give the job** or the fits oversubscribe the machine: an uncapped run
on 2026-09-17 sat for 2.5 hours producing nothing. A typical launch:

```
OPENBLAS_NUM_THREADS=8 OMP_NUM_THREADS=8 MKL_NUM_THREADS=8 \
  nice taskset -c 0-7 micromamba run -n paleo-rf Rscript scripts/7_plot_preds.R
```

The thread settings are recorded in the run manifest (they can change results in the
last bit). They are the only environment variables involved, and the pipeline only
records them: every setting it reads is in `config.R` (issue I06). A package lockfile
(`renv`) is planned.

## Run provenance (`runs/`)

Every run of a script that uses `R/run_manifest.R` writes
`runs/<script>/<start time>_<process id>.md` (in the run directory, for a
run with `run$dir` set) as soon as it starts, and updates it as the run
goes: status `started`, then `completed` or `failed` with the error (a run
that is killed stays `started`). It records the full commit and any
uncommitted changes to `config.R`, `scripts/` or `R/`, the whole
configuration, every file read and written with its md5, the figures
written, and R, package and thread settings. The top of the file is a
readable summary; the full record is JSON at the end.

Before a script reads a file made by an earlier script it checks that
script's latest manifest: the file must be the one that run wrote, and the
run must have used the same `analysis` settings. If not, the script stops
and lists what differs; rerun the earlier script, or set
`run$allow_stale = TRUE` in the config to go on regardless (each such file
is listed in the manifest). A script uses it with `run_begin()` at the top,
`read_input()` and `write_output()` for its files, and `run_finish()` at
the end; every numbered script has it (issues I05, I04a, I06), with
`run_begin()` before the packages are loaded so that a missing package is
recorded as a failed run. Older manifests in `runs/` (flat, named by minute) are kept as they
are; three were written by hand after the fact and are marked
`RECONSTRUCTED`.

## Regression anchors (`tests/anchors/`)

Frozen outputs that a refactor must still reproduce, each set with its
md5s and a README entry saying how it was made and what it does not
cover. Four sets: `nointerp-mar-2026-09-16` and `nointerp-may-2026-09-17`
(the point flavour; reproducible only from `v1-both-flavours` or earlier
now that the trunk is interp only), `interp-allmonths-2026-09-20` (scripts 1 to 6) and
`interp-tail-2026-09-21` (scripts 7, 7a, 8; 187 MB, determinism
demonstrated by a byte-identical re-run of 7a). Andria's own outputs are
preserved at the `v0-legacy` tag rather than copied;
`bash tools/restore_original_outputs.sh` brings them back.

## Working practices

See `CONTRIBUTING.md` (branches, commits, merges, where generated
material lives) and `AGENTS.md` (notes for AI-assisted sessions).
Branches: `main` (upstream, merged only by pull request), `legacy`
(frozen copy of the code as received, also tagged `v0-legacy`),
`chris-dev` (working trunk, interp only since 2026-09-24; Andria reviews
and edits here directly), `annotated-interp` (a teaching copy of the
same code with a comment on every line; read it, do not merge it), and
topic branches off `chris-dev` that are deleted when merged. Tags:
`v0-legacy`, `v1-both-flavours`.

## Documents (`docs/cc/`)

- `questions_for_andria_scientific.md`, `questions_for_andria_general.md`: living lists of open questions, kept current.
- `2026-09-16_known_issues_missing_data_and_code.md`: defects, missing data, missing code (items 37-41 are the ones found in September 2026).
- `README_nointerp.md`: the point flavour's scripts and data (line numbers as of `v1-both-flavours`).

Further working documents (code map, code review, staged plan, glossary,
walkthrough, run logs) were removed from the repository on 2026-09-25 to
reduce clutter; they remain in git history before that date and in a
local archive on Chris's workstation.
