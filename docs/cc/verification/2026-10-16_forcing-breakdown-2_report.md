# Verification forcing-breakdown-2: PASS

- verdict: **PASS**
- commit: c0098cb0b780fb48a6fabf7b9e885dc576238202
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/docs/cc/verification/2026-10-16_forcing-breakdown-2.R`
- started 2026-10-16 07:42:38 UTC, finished 2026-10-16 07:43:56 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/c0098cb` at c0098cb0b780fb48a6fabf7b9e885dc576238202; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/forcing-breakdown-2`; configuration `.rundirs/forcing-breakdown-2/config_used.R` (from `configs/legacy_config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=8 CORES=16-23

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.6 min, run record `20261016T074253.007Z_1333558.md`, 4 outputs
- scripts/10_forcing_breakdown.R: exit status 0, 0.3 min, run record `20261016T074326.818Z_1333796.md`, 1 outputs

## Anchors


## Checks

- `tools/check_anchors.R tests/anchors/interp-forcing-2026-10-08 .rundirs/forcing-breakdown-2 --all`: exit status 0 (log `check_01.log`)
- `tests/test_paths.R`: exit status 0 (log `check_02.log`)
- `tests/test_config.R`: exit status 0 (log `check_03.log`)
- `tests/test_forcing_breakdown.R`: exit status 0 (log `check_04.log`)

## Failures

- none
