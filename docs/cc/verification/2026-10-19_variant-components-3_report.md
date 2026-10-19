# Verification variant-components-3: FAIL

- verdict: **FAIL**
- commit: d8b6ab10e1bc62271fbc27a38512bae0b96e181f
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-s9fix/docs/cc/verification/2026-10-19_variant-components-3.R`
- started 2026-10-19 14:25:43 UTC, finished 2026-10-19 14:27:53 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/d8b6ab1` at d8b6ab10e1bc62271fbc27a38512bae0b96e181f; 261 frozen inputs linked from the main checkout
- run folder `.rundirs/variant-components-3`; configuration `.rundirs/variant-components-3/config_used.R` (from `config.R`); 2 files copied in
- runner `tools/rscript.sh` with THREADS=2 CORES=46-47

## Scripts

- scripts/9_forcing_barplot.R: exit status 0, 0.9 min, run record `20261019T142605.440Z_2385058.md`, 4 outputs

## Anchors


## Checks

- `docs/cc/verification/2026-10-19_variant-components_check.R .rundirs/variant-components-3`: exit status 0
- `tests/test_forcing_components.R`: exit status 0
- `tests/test_differences.R`: exit status 0
- `tests/test_config.R`: exit status 1

## Failures

- check failed: tests/test_config.R (log: /home/expert/Documents/contracting/Andria/paleo-RF-verify/d8b6ab1/.rundirs/variant-components-3/checks.log)
