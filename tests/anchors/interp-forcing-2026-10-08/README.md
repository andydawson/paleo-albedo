# Interp forcing summary anchor, 2026-10-08

Script 9's four summary tables, frozen as the regression reference for the last stage of
the pipeline. Made for issue I01 of the single-config migration (#2), because script 9's
outputs had no anchor; the same four files are also tracked in git under
`output/forcing/`, and are byte-identical to them.

Produced by the I01 baseline run on `feature/single-config` at commit 1b9d5a9 (clean
tree, 8 threads), from inputs that themselves reproduce the `interp-tail-2026-09-21`
anchor byte for byte: manifests `runs/2026-10-08_1718_8_radiative.md` and
`runs/2026-10-08_1719_9_forcing_barplot.md`.

| File | What | Exactly reproducible? |
|---|---|---|
| `forcing_by_period.csv` | radiative forcing per period, main variant, three kernels | yes, deterministic |
| `forcing_by_period_variant_sensitivity.csv` | the same for the sensitivity variants | yes |
| `forcing_by_period_egu2024recipe_vs_slide.csv` | the recovered EGU 2024 recipe against the slide | yes |
| `modern_ipcc_ar6_erf.csv` | IPCC AR6 effective radiative forcing, 2019 | yes (copied from the input data) |

Check with `Rscript tools/check_anchors.R tests/anchors/interp-forcing-2026-10-08 [run-dir]`.
