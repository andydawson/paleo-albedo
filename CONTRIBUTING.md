# Contributing / version-control protocol

Recorded 2026-09-16. This file is the source of truth for how work in this
repository is branched, committed, merged and backed up. Agent sessions read
`AGENTS.md`, which points here.

## Branches

| Branch      | Role                                                                 |
|-------------|----------------------------------------------------------------------|
| `main`      | Upstream line from `andydawson/paleo-RF`. Not committed to directly. |
| `legacy`    | Frozen snapshot of the code as received (commit `c9d525e`, 2026-09-13). **Never commit to or rebase this branch.** |
| `chris-dev` | Development trunk for Chris McWilliams' work. All new work goes here. |

## Working rules (confirmed by Chris, 2026-09-16)

- Trunk: `chris-dev`. Topic branches off `chris-dev`, one per logical task
  (e.g. `fix-missing-dirs`, `replace-rgeos`); merge into `chris-dev` when the
  task is finished.
- Commit each atomic (single logical) change, frequently. Concise imperative
  subject line plus a body explaining *what* changed and *why*. Do not squash
  granular history unless asked.
- Never commit to `legacy` or `main`. Work reaches `main` only via a pull
  request on GitHub reviewed and approved by Andria; agent sessions never
  merge to `main` themselves.
- Remote: `origin` (`andydawson/paleo-RF`). Push `chris-dev`, `legacy` and
  any open topic branches at natural checkpoints and before ending each
  session, without asking each time.
- The code map that used to live in `docs/cc/` was archived on 2026-09-25 (git history before that date); the README's script table is now the current map and is kept up to date when scripts are added, renamed or retired.
- Andria's code and data as received are tagged `v0-legacy`. That tag is
  the store of her original pipeline outputs; recover them with
  `bash tools/restore_original_outputs.sh`. Never commit regenerated run
  outputs over the committed versions in `data/`: a run overwrites them
  in the working tree, so check `git status` before staging, and prefer
  `git add <path>` to `git add -A`.
- Frozen outputs used to prove a refactor changed nothing live in
  `tests/anchors/`; see the README there for what is anchored and why.
- Verification (Chris, 2026-10-15): every code change is verified with
  `tools/verify_change.R` before it is merged, from a separate worktree at the
  exact commit, never in a checkout a run is reading; the PASS/FAIL report and
  its spec are committed under `docs/cc/verification/` (README there). Git
  refuses a merge that changes code without a PASS report for that code (the
  pre-merge-commit hook; install once per clone with
  `Rscript tools/check_merge_evidence.R install`). `git merge --no-verify` is a
  recorded exception, with the reason in the merge message.
- Everything Claude Code generates that is not a script, a result or a data
  file with its own home (code maps, issue lists, schematics, reports,
  generators) lives under `docs/cc/`, with dated, descriptive file names
  (`YYYY-MM-DD_what_and_why.ext`); living documents carry no date. The
  repo root holds only README, CONTRIBUTING, AGENTS, the git and LFS
  settings (`.gitignore`, `.gitattributes`, `.lfsconfig`), `config.R` (once
  the single-config migration adds it), `scripts/` (the pipeline), `R/`
  (shared functions), `data/` (inputs and intermediates), `docs/`, `tools/`
  (helpers that are not pipeline), `tests/` (anchors and checks) and `runs/`
  (run manifests) and `output/` (results; a few small tables are tracked,
  the bulk is ignored). Not tracked, but present on a working machine:
  `figures/`, `writing/` and `.rundirs/` (run directories of alternative
  and verification runs, set by `run$dir` in `config.R`).
- `scripts/` is the R analysis pipeline and nothing else. Helper code that
  is not part of the pipeline (data download, diagram generators) lives in
  `tools/`.
- Questions for Andria are collected continuously in
  `docs/cc/questions_for_andria_general.md` and
  `docs/cc/questions_for_andria_scientific.md`; add to them as questions
  arise, remove them when answered.

## Coding practice

Research code, but written to good practice: no duplicated code (shared
routines are functions in `R/`, sourced by the scripts); small functions with
a stated purpose; comments say why; every change parsed and, where a run is
feasible, checked against `tests/anchors/`; version control as above. Chris,
2026-09-25. `AGENTS.md` carries the same rules for agent sessions.

## Decisions on record

- **2026-09-17.** The point (non-interp) flavour is not developed further;
  the interp flavour is the analysis (agreed with Andria).
- **2026-09-24.** The point flavour is removed from `chris-dev`: every
  script now holds its interp code only, lines unchanged. Preserved at
  tags `v0-legacy` and `v1-both-flavours`, branch `legacy`, and
  `origin/run-nointerp` / `origin/run-may`; documented in
  `docs/cc/README_nointerp.md`; outputs in `tests/anchors/nointerp-*`.
- **2026-09-24.** Andria reviews the method on `chris-dev` and commits her
  comments and changes there directly. While that review is open, Chris and
  agent sessions do not edit `scripts/` on `chris-dev`; work goes on topic
  branches and is rebased onto her changes before merging.
- **2026-09-30.** The 2026-09-24 removal covered the scripts the interp run
  executes (1, 2, 4, 5, 6, 7, 7a, 8, 9) and left the two optional scripts as
  they were, without saying so. `3_plot_cal_lct_albedo.R` was cleaned on this
  date on topic branch `clean-script-3`, run on the interp data for the first
  time, merged to `chris-dev` and cherry-picked onto `annotated-interp` so
  every live branch holds one version. `origin/run-interp` and
  `origin/run-nointerp` are run records and keep the old file.
  `6_prediction_model_spatial_eval.R` (the model-structure sensitivity
  check, optional) is Andria's original, untouched: it never had point-flavour
  code, has not been run by us, and is the same file on every branch.
- **2026-09-30.** `annotated-interp` is kept as a reference for the
  annotations only and is no longer kept identical to `chris-dev`: script 2
  there lacks the 2026-09-25 boundary fix and Andria's edits, and only script
  3 was carried across. The branch will be retired once every script has been
  worked through on `chris-dev` (Chris, 2026-09-30).
- **2026-10-08.** Migration to a single configuration file, decided by Chris
  after two design reviews (`docs/cc/2026-10-07_design_review_*` and
  `..._adversarial_review_*`): `config.R` at the repo root with `analysis`
  and `presentation` sections, loaded by a validating helper; scientific
  decisions live there as selectors among implemented alternatives, with
  open questions flagged by their id; one plain-text manifest per script
  invocation; a simple consistency check (whole `analysis` section of each
  input's manifest compared with the current config, stop on any difference,
  `RUN_ALLOW_STALE=1` override recorded; on 2026-10-13 Chris made the
  override the config setting `run$allow_stale`, with no run note); output
  paths unchanged so the
  anchors keep working; stages become functions in `R/` with the numbered
  scripts as wrappers (superseded 2026-10-13/14: the scripts stay flat so Andria can run
  them line by line, and the extraction issues were closed; see the migration changelog);
  the ensemble driver is parked. The work happens on
  one long-lived feature branch off `chris-dev`, topic branches per issue,
  because Andria's review continues on `chris-dev`. Issues designed in
  `docs/cc/2026-10-08_migration_issues_single_config_codex_gpt6astra.md`,
  reviewed in `..._adversarial_review_migration_issues_fable.md`, revised
  in `..._migration_issues_single_config_v2.md` (the version to file).
  Exception to the rebase rule above: the feature branch is built from
  `--no-ff` merges, so rebasing it would flatten the history; instead
  `chris-dev` is merged into the feature branch at each stage boundary and
  once before the final merge (Chris, 2026-10-08). The mean/median
  reduction switch is in scope; the model-structure sensitivity script
  (`6_prediction_model_spatial_eval.R`) is out of scope pending Andria's
  answer (general question 7). The random seed for the simulations in
  scripts 5 and 6 is a config option, `"random"` by default with the drawn
  seed recorded in every manifest (Chris, 2026-10-08; issues #7, #6, #11,
  #15, #16). On 2026-10-14 Chris kept the calibration settings in
  `config.R` to the choice of model (`calibration$model_id`): formulas,
  basis sizes and `maxit` live in `R/model_registry.R`, changed only by a
  reviewed code change.
- **2026-09-23.** Branch `annotated-interp` is a teaching copy of the same
  code with a comment on every line. It is read, not merged; its code was
  kept identical to `chris-dev`'s by the comment-stripped comparison
  described in its commit messages until 2026-09-25 (see the 2026-09-30
  entry). Chris intends to prune it into package documentation later.
- **2026-09-24.** `.lfsconfig` excludes `tests/anchors/**` from LFS
  fetches so that a clone downloads only the data inputs (617 MB) and stays
  inside GitHub's 1 GB/month LFS bandwidth allowance on Andria's account.
  Fetch anchors with `git lfs pull --include="tests/anchors/**"`.
- **Manifest string.** `8_radiative.R` records `cack_band_meaning = 'year
  2003'`; the file's Year dimension says 2001 = 1 (known issue 37).
