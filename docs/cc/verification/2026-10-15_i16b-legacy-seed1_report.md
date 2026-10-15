# Verification i16b-legacy-seed1: PASS

- verdict: **PASS**
- commit: 5deabac7df11daa289698a41ec85373a7ab72ef3
- spec: `/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/i16b/spec_legacy_seed1.R`
- started 2026-10-15 06:48:17 UTC, finished 2026-10-15 07:10:22 UTC
- worktree `/home/expert/Documents/contracting/Andria/paleo-RF-verify/5deabac` at 5deabac7df11daa289698a41ec85373a7ab72ef3; 0 frozen inputs linked from the main checkout
- run folder `.rundirs/i16b-legacy-seed1`; configuration `.rundirs/i16b-legacy-seed1/config_used.R` (from `configs/legacy_config.R`); 195 files copied in
- runner `tools/rscript.sh` with THREADS=8 CORES=8-15

## Scripts

- scripts/5_calibration_eval.R: exit status 0, 8.1 min, run record `20261015T064833.110Z_888114.md`, 26 outputs
- scripts/6_prediction_model.R: exit status 0, 13.3 min, run record `20261015T065641.109Z_894018.md`, 38 outputs

## Anchors

- tests/anchors/interp-seed1-2026-10-14: every file MATCH
```
calibration_model_stats.csv                      MATCH
paleo_interp_predict_gam_summary_bluesky.RDS     MATCH
paleo_interp_predict_gam_samps_apr_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_aug_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_dec_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_feb_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_jan_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_jul_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_jun_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_mar_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_may_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_nov_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_oct_bluesky.RDS   MATCH
paleo_interp_predict_gam_samps_sep_bluesky.RDS   MATCH
paleo_interp_predict_gam_summary_apr_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_aug_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_dec_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_feb_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_jan_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_jul_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_jun_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_mar_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_may_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_nov_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_oct_bluesky.RDS MATCH
paleo_interp_predict_gam_summary_sep_bluesky.RDS MATCH
```

## Checks


## Failures

- none
