# Verification i16c-elevation-current-2: PASS

- verdict: **PASS**
- commit: 95fedfa1874d231db7a0c031cad618d48de5c5d6
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/verification/2026-10-15_i16c-elevation-current-2.R`
- started 2026-10-15 20:09:20 UTC, finished 2026-10-15 20:10:07 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/95fedfa` at 95fedfa1874d231db7a0c031cad618d48de5c5d6; 0 frozen inputs linked from the main checkout
- run folder `.rundirs/i16c-elevation-current-2`; configuration `.rundirs/i16c-elevation-current-2/config_used.R` (from `config.R`); 0 files copied in
- runner `tools/rscript.sh` with THREADS=4 CORES=12-15

## Scripts

- scripts/1_veg_lct_prep.R: exit status 0, 0.4 min, run record `20261015T200932.982Z_1169141.md`, 2 outputs

## Anchors


## Checks

- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_modern_reveals_interp.RDS .rundirs/i16c-elevation-current-2/data/lct_modern_reveals_interp.RDS`: exit status 0 (log `check_01.log`)
- `tools/compare_tables.R tests/anchors/interp-allmonths-2026-09-20/lct_paleo_reveals_interp.RDS .rundirs/i16c-elevation-current-2/data/lct_paleo_reveals_interp.RDS`: exit status 0 (log `check_02.log`)

## Failures

- none
