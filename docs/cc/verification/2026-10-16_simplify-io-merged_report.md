# Verification simplify-io-merged: PASS

- verdict: **PASS**
- commit: 300c2a680fd8c4cc8cf9daaee7236f5cf8420146
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-io/docs/cc/verification/2026-10-16_simplify-io-merged.R`
- started 2026-10-15 23:37:31 UTC, finished 2026-10-15 23:38:46 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/300c2a6` at 300c2a680fd8c4cc8cf9daaee7236f5cf8420146; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/simplify-io-merged`; configuration `.rundirs/simplify-io-merged/config_used.R` (from `configs/legacy_config.R`); 1 files copied in
- runner `tools/rscript.sh` with THREADS=4 CORES=16-19

## Scripts

- scripts/1_veg_lct_prep.R: exit status 0, 0.4 min, run record `20261015T233747.364Z_1224679.md`, 2 outputs
- scripts/8_radiative.R: exit status 0, 0.4 min, run record `20261015T233809.189Z_1225027.md`, 1 outputs

## Anchors


## Checks

- `tools/check_anchors.R tests/anchors/interp-allmonths-2026-09-20 .rundirs/simplify-io-merged`: exit status 0 (log `check_01.log`)
- `tools/check_anchors.R tests/anchors/interp-tail-2026-09-21 .rundirs/simplify-io-merged`: exit status 0 (log `check_02.log`)
- `tests/test_paths.R`: exit status 0 (log `check_03.log`)
- `tests/test_config.R`: exit status 0 (log `check_04.log`)
- `tests/test_tools.R`: exit status 0 (log `check_05.log`)

## Failures

- none
