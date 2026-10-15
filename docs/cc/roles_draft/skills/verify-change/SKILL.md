---
name: verify-change
description: Verify a paleo-RF code change before merging, with tools/verify_change.R from its own worktree. Use whenever a branch that changes R/, scripts/, tools/, tests/, config.R or configs/ is ready to merge, or a change needs a pipeline run as evidence.
---

# Verify a change (paleo-RF)

Never write an ad hoc driver. Never run pipeline scripts in the main checkout for evidence,
and never switch branches in a checkout a run is reading.

1. **Choose the checks** from the regression contract
   (`docs/cc/2026-10-08_single_config_regression_contract.md`) and the change:
   - deterministic outputs: the anchors they must MATCH byte for byte;
   - stochastic outputs: seed 1 against `tests/anchors/interp-seed1-2026-10-14`, or
     tolerances via `tools/compare_tables.R` as a check;
   - calibration code: the one-month refit (`tools/check_calibration_month.R`) as a check,
     never the 27-hour script 4 except at the final gate;
   - a new method setting: the legacy configuration (`configs/legacy_config.R`) must
     reproduce the anchors; the new setting runs cleanly, and its effect is measured.
   - tools or tests only: no scripts; the test files as checks.
2. **Write the spec** `docs/cc/verification/<YYYY-MM-DD>_<name>.R` (format and example in
   that folder's README): commit (the branch tip), config, settings, seed/copy, scripts,
   anchors, checks, `rscript = 'tools/rscript.sh'`, `env = c(THREADS = '8', CORES = ...)`.
   Cores: at most 24 across all running jobs; check `pgrep -fa Rscript` first.
3. **Run** in the background from the repository root, the exit status taken from the tool:
   `tools/rscript.sh tools/verify_change.R docs/cc/verification/<date>_<name>.R`.
   Wait for the completion notice; do not poll.
4. **Read the report** (`docs/cc/verification/<date>_<name>_report.md`). A FAIL is
   reported to Chris as it is, with the failure lines; fix on the branch, commit, run again
   under a new name. Never edit a report.
5. **Commit** the spec and the report on the branch (docs only, so the code they cover is
   unchanged). The merge check accepts the branch only with a PASS report for its code.
6. Repeat the key outputs to Chris: verdict, what matched, measured runtimes, anything
   unexpected. Questions go through the question interface.
