# Verification i16c-elevation-current-4: PASS

- verdict: **PASS**
- commit: 75945ee0258d3f60a5c896e5dd608f04897b3b51
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/verification/2026-10-15_i16c-elevation-current-4.R`
- started 2026-10-15 21:20:17 UTC, finished 2026-10-15 21:21:03 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/75945ee` at 75945ee0258d3f60a5c896e5dd608f04897b3b51; 0 frozen inputs linked from the main checkout
- run folder `.rundirs/i16c-elevation-current-4`; configuration `.rundirs/i16c-elevation-current-4/config_used.R` (from `config.R`); 0 files copied in
- runner `tools/rscript.sh` with THREADS=4 CORES=12-15

## Scripts

- scripts/1_veg_lct_prep.R: exit status 0, 0.4 min, run record `20261015T212030.199Z_1191673.md`, 2 outputs

## Anchors


## Checks

- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_modern_reveals_interp.RDS .rundirs/i16c-elevation-current-4/data/lct_modern_reveals_interp.RDS`: exit status 0 (log `check_01.log`)
- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_paleo_reveals_interp.RDS .rundirs/i16c-elevation-current-4/data/lct_paleo_reveals_interp.RDS`: exit status 0 (log `check_02.log`)

## Failures

- none
