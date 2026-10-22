# Albedo build: checks on the four test months and on the deletion safeguards (22 October 2026)

Stand-alone checks, as agreed for this branch (Chris, 20 October: verification by recorded
exception; reason in the merge message). Code checked: `tools/prepare_bluesky_albedo.R` at
commit 6444179 (script SHA-256 2ee39123...ba1aa5). All checks read data only, except the deletion
test, which ran in a scratch copy. The scripts beside this report are the ones that ran; they are
run from the repository root with the paleo-rf R environment (`PROJ_LIB` and `PROJ_DATA` set to its
`share/proj`).

## The four test months

Built with `build <month> keep` (day files kept, `KEEP` marker): March 2000, March 2005, July 2005,
December 2005. Each: 38 tiles; all six layers on every day; no albedo outside 0-1; no retrieval
without a diffuse fraction; the build record names AppEEARS's own copy of the request and finds it
equal to ours.

| Month | Days | Build time | Peak memory |
|---|---|---|---|
| March 2000 | 27 (5-31 March) | 27.1 min | 12.1 GB |
| March 2005 | 31 | 30.9 min | 12.0 GB |
| July 2005 | 31 | 30.9 min | 11.9 GB |
| December 2005 | 31 | 30.9 min | 11.7 GB |

The build runs one month at a time with 6 tiles in parallel, so about 12 GB is the build's total
memory use (the workstation has 125 GB).

**March 2000 starts on 5 March.** AppEEARS's copy of the request runs 03-05-2000 to 03-31-2000 and
the day files run from day 2000065 (5 March, a leap year) to 2000091 (31 March): the start of the
validated record in the MODIS user guide (`FIRST_DAY`).

**A blank land/water class means no albedo** (`day_checks.R`), in every season: pixel-days with a
blank class but a black-sky albedo were 0 in all four months, out of 12.5-15.2 billion blank
pixel-days each (December 2005: 15,154,095,170). The land rule's assumption that blank days can be
ignored holds.

**The 70- and 75-degree sun-angle cut-offs end where the noon sun allows** (`lat_checks.R`).
Northernmost land pixel with at least one qualifying day:

| Month | 75 degrees | 70 degrees | no cut-off | sun limit 75 / 70 (cut-off + highest declination) |
|---|---|---|---|---|
| December 2005 | 53.9 N | 48.9 N | 58.0 N | 53.3 / 48.3 N |
| March 2005 | 79 N (area edge) | 74.5 N | 79 N | 78.9 / 73.9 N |
| March 2000 | 79 N (area edge) | 74.9 N | 79 N | 79.0 / 74.0 N |
| July 2005 | 79 N | 79 N | 79 N | beyond the area |

(Declination from Spencer's formula; `lat_checks.R` printed Cooper's, about 0.4 degrees lower in
December.) The data reach about 0.6 degrees beyond the limit, consistent with the zenith being stored
in whole degrees. With no cut-off nothing lies north of 58 N in December: the product makes no
retrievals in the polar dark.

North of 60 N in March 2005, full-quality retrievals cover about half the land pixels that any
usable retrieval covers (5.9 against 12.1 million; July 2005 10.3 against 12.7 million). Both versions
are kept; noted for Andria (chris-dev 37bc34b).

**White-sky minus black-sky albedo, March 2005** (`day_checks.R`), mean by noon zenith:

| Noon zenith | snow-free | snowy |
|---|---|---|
| 0-30 | +0.011 | +0.014 (9,302 pixel-days) |
| 30-45 | +0.008 | +0.008 |
| 45-60 | +0.002 | +0.000 |
| 60-70 | -0.006 | -0.014 |
| 70-75 | -0.019 | -0.033 |
| 75-80 | -0.040 | -0.051 |

Black-sky albedo rises as the sun gets lower and crosses white-sky albedo at about 50-60 degrees, as
expected for these surfaces. (The script's all-pixel-days row overflowed an integer sum in its
percentiles; that row is not used. The per-band rows are below the limit.)

**Shoreline pixels** (`shoreline.R`, the script's own `cell_table` under both land rules). Counting
shoreline pixels as land darkens cells with much shoreline by about 0.006 on average (shoreline
20-50% of the cell: -0.0056; over 50%: -0.0061, 5-95% range -0.042 to +0.027), mostly in July, hardly
in December, and adds an albedo to 52 of 7,078 cell-months. Chris chose land only (22 October; the
script's `LAND_RULE` already is `land`).

## Deletion safeguards (scratch copy)

A scratch project folder held hard links to July 2005's 186 day files (no `KEEP` marker), copies of
its records and request file, and links to the ERA5 data and the cell file. Each case ran
`Rscript tools/prepare_bluesky_albedo.R build` there.

1. AppEEARS's request copy removed: stopped with "AppEEARS's copy of the request is missing ...;
   stopped before building or deleting anything". No tile written; all 186 files present.
2. AppEEARS's request copy with the end date changed to 30 July: stopped with "... differs from ours
   in dates ...; stopped before building or deleting anything". No tile written; all 186 present.
3. Everything in place: 38 tiles built in 33.3 min (12.1 GB peak), build record written ("day files:
   deleted after this record"), then the scratch day folder deleted. The real July 2005 folder kept
   all its files, and all 186 still match AppEEARS's SHA-256 list. The 38 scratch tiles equal the
   real July 2005 tiles in all 1,026 layers (`compare_tiles.R`); only the creation time differs.
