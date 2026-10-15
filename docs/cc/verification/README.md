# Verification specs and reports

Every change to the code is verified with `tools/verify_change.R` before it is merged
(decided by Chris, 2026-10-15). Git enforces it: `tools/check_merge_evidence.R` runs as the
pre-merge-commit hook and refuses a merge that changes code (`R/`, `scripts/`, `tools/`,
`tests/`, `config.R`, `configs/`) unless a PASS report here covers the merged code.

- **Spec:** `<date>_<name>.R`, a short R file whose value is a list: what to run and what
  must hold. Written for each verification, committed with its report.
- **Report:** `<date>_<name>_report.md`, written by the tool: verdict, commit, the worktree
  and run folder, each script's exit status and time, anchor comparisons, checks, failures.

Run, from the repository root (on this machine through `tools/rscript.sh`):

    Rscript tools/verify_change.R docs/cc/verification/<date>_<name>.R

The tool runs the scripts in a separate worktree at the spec's commit
(`../paleo-RF-verify/<sha7>`), never in the main checkout, so implementation can go on
meanwhile. The fitted models and data not in git are linked from the main checkout; the
run writes only into its own run folder in the worktree. Worktrees are not deleted by the
tool: remove them with `git worktree remove` once their reports are committed.

Example spec:

```r
# I16b, legacy configuration: scripts 5 and 6 with seed 1 must reproduce the seed-1 anchor.
months = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
list(
  name    = 'i16b-legacy-seed1',
  commit  = 'i16b-calibration',                 # resolved to a SHA when the tool starts
  config  = 'configs/legacy_config.R',
  settings = NULL,                              # e.g. list(analysis = list(months = 'jan'))
  seed    = c('calibration_table', 'calibration_ladder', 'calibration_spatial',
              'calibration_spatial_elev_cover', 'aic_table', 'lct_paleo'),
  copy    = lapply(months, function(m) c(sprintf('output/calibration/calibration_mod8_interp_%s_bluesky.RDS', m),
                                         sprintf('output/calibration/calibration_prediction_fit_%s_bluesky.RDS', m))),
  scripts = c('scripts/5_calibration_eval.R', 'scripts/6_prediction_model.R'),
  anchors = 'tests/anchors/interp-seed1-2026-10-14',
  checks  = NULL,                               # e.g. list(c('tools/compare_tables.R', ...))
  rscript = 'tools/rscript.sh',
  env     = c(THREADS = '8', CORES = '8-15'))
```

A merge without a report is possible only as a recorded exception: `git merge --no-verify`,
with the reason in the merge message.
