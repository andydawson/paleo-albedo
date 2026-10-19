# Verification headline-partial-ice-legacy: PASS

- verdict: **PASS**
- commit: 9ca2ed8b9f7779fed8214b5c3b7fd2e8765fa5b5
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-partial/docs/cc/verification/2026-10-19_headline-partial-ice-legacy.R`
- started 2026-10-19 15:25:55 UTC, finished 2026-10-19 15:27:30 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/9ca2ed8` at 9ca2ed8b9f7779fed8214b5c3b7fd2e8765fa5b5; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/headline-partial-ice-legacy`; configuration `.rundirs/headline-partial-ice-legacy/config_used.R` (from `configs/legacy_config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=2 CORES=46-47

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.7 min, run record `20261019T152613.388Z_2404423.md`, 4 outputs
- scripts/10_forcing_breakdown.R: exit status 0, 0.3 min, run record `20261019T152656.954Z_2404517.md`, 1 outputs

## Anchors

- tests/anchors/interp-forcing-2026-10-08: every file MATCH
```
forcing_by_period.csv                            MATCH
forcing_by_period_egu2024recipe_vs_slide.csv     MATCH
forcing_by_period_variant_sensitivity.csv        MATCH
modern_ipcc_ar6_erf.csv                          MATCH
```

## Checks

- `tests/test_config.R`: exit status 0

## Failures

- none
