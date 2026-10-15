# Verification i16c-elevation-current: PASS

- verdict: **PASS**
- commit: 334afa3379e9086a1e7aad7d4961acab243539d2
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/verification/2026-10-15_i16c-elevation-current.R`
- started 2026-10-15 19:06:28 UTC, finished 2026-10-15 19:07:14 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/334afa3` at 334afa3379e9086a1e7aad7d4961acab243539d2; 0 frozen inputs linked from the main checkout
- run folder `.rundirs/i16c-elevation-current`; configuration `.rundirs/i16c-elevation-current/config_used.R` (from `config.R`); 0 files copied in
- runner `tools/rscript.sh` with THREADS=4 CORES=12-15

## Scripts

- scripts/1_veg_lct_prep.R: exit status 0, 0.4 min, run record `20261015T190640.846Z_1145386.md`, 2 outputs

## Anchors


## Checks

- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_modern_reveals_interp.RDS .rundirs/i16c-elevation-current/data/lct_modern_reveals_interp.RDS`: exit status 0 (log `check_01.log`)
- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_paleo_reveals_interp.RDS .rundirs/i16c-elevation-current/data/lct_paleo_reveals_interp.RDS`: exit status 0 (log `check_02.log`)

## Failures

- none
