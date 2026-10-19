# Verification headline-partial-ice-legacy-2: PASS

- verdict: **PASS**
- commit: dd6e0fc433c00b2f79169c892133b44a4f072cad
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-partial/docs/cc/verification/2026-10-19_headline-partial-ice-legacy-2.R`
- started 2026-10-19 16:01:05 UTC, finished 2026-10-19 16:02:26 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/dd6e0fc` at dd6e0fc433c00b2f79169c892133b44a4f072cad; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/headline-partial-ice-legacy-2`; configuration `.rundirs/headline-partial-ice-legacy-2/config_used.R` (from `configs/legacy_config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=2 CORES=46-47

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.6 min, run record `20261019T160122.712Z_2416941.md`, 4 outputs
- scripts/10_forcing_breakdown.R: exit status 0, 0.3 min, run record `20261019T160157.894Z_2417144.md`, 1 outputs

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
