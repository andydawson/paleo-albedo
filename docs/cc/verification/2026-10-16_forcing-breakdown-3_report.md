# Verification forcing-breakdown-3: PASS

- verdict: **PASS**
- commit: 2ca41719f8a8bbe361b29131d331ee96848d5f7d
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/docs/cc/verification/2026-10-16_forcing-breakdown-3.R`
- started 2026-10-16 09:09:26 UTC, finished 2026-10-16 09:10:44 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/2ca4171` at 2ca41719f8a8bbe361b29131d331ee96848d5f7d; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/forcing-breakdown-3`; configuration `.rundirs/forcing-breakdown-3/config_used.R` (from `configs/legacy_config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=8 CORES=16-23

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.6 min, run record `20261016T090942.428Z_1350785.md`, 4 outputs
- scripts/10_forcing_breakdown.R: exit status 0, 0.3 min, run record `20261016T091015.577Z_1350920.md`, 1 outputs

## Anchors


## Checks

- `tools/check_anchors.R tests/anchors/interp-forcing-2026-10-08 .rundirs/forcing-breakdown-3 --all`: exit status 0 (log `check_01.log`)
- `tests/test_paths.R`: exit status 0 (log `check_02.log`)
- `tests/test_config.R`: exit status 0 (log `check_03.log`)
- `tests/test_forcing_breakdown.R`: exit status 0 (log `check_04.log`)

## Failures

- none
