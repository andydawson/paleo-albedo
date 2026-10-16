# Verification forcing-breakdown: PASS

- verdict: **PASS**
- commit: d7662609633659cec4ea9b3a50773e4a89abe361
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-breakdown/docs/cc/verification/2026-10-16_forcing-breakdown.R`
- started 2026-10-16 05:51:53 UTC, finished 2026-10-16 05:53:12 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/d766260` at d7662609633659cec4ea9b3a50773e4a89abe361; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/forcing-breakdown`; configuration `.rundirs/forcing-breakdown/config_used.R` (from `configs/legacy_config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=4 CORES=16-19

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.6 min, run record `20261016T055210.002Z_1300498.md`, 4 outputs
- scripts/10_forcing_breakdown.R: exit status 0, 0.3 min, run record `20261016T055243.371Z_1300589.md`, 1 outputs

## Anchors


## Checks

- `tools/check_anchors.R tests/anchors/interp-forcing-2026-10-08 .rundirs/forcing-breakdown --all`: exit status 0 (log `check_01.log`)
- `tests/test_paths.R`: exit status 0 (log `check_02.log`)
- `tests/test_config.R`: exit status 0 (log `check_03.log`)

## Failures

- none
