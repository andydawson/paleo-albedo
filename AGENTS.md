# Notes for agent sessions

Read `CONTRIBUTING.md` for the version-control protocol before making changes.
Summary: trunk is `chris-dev`; one topic branch per task off it, merged back
when done; never touch `legacy` (frozen snapshot of the original code) or
`main`; commit each atomic change with a why-focused message; push to
`origin` at checkpoints and session end; `main` changes only via a PR that
Andria approves.

`docs/cc/2026-09-16_code_map_original_scripts.md` (archived 2026-09-25; in git history) gives a high-level description of every script and its data
inputs/outputs.

Two living documents must be kept current in every session:
`docs/cc/questions_for_andria_general.md` (simple and logistical) and
`docs/cc/questions_for_andria_scientific.md` (technical and scientific).
Whenever a question for the PI arises during the work, add it to the
relevant file at once with a one-line note of where it came from; remove
questions once answered, recording the answer in the relevant document.

File naming under `docs/cc/`: dated and descriptive, e.g.
`2026-09-18_code_review_original_code.md`, so the purpose is clear from
the name. Living documents that are updated continuously (the two question
files) carry no date.

## Coding practice (Chris, 2026-09-25)

This is a research project, but it is written to good software practice, and
that includes version control:

- **Do not duplicate code.** A fix or a routine needed in more than one place
  becomes a function, in `R/` if more than one script uses it (e.g.
  `R/map_helpers.R`), sourced from the scripts. Pasting the same lines into
  several plots or several scripts is not acceptable, even as a quick fix.
- Prefer small functions with a stated purpose over long inline blocks; comment
  *why*, not what the line obviously does.
- Every change keeps the pipeline runnable and is checked: parse the file, and
  compare regenerated outputs with `tests/anchors/` where a run is feasible.
- Version control per `CONTRIBUTING.md`: topic branch off `chris-dev`, one
  atomic change per commit with a why-focused message, merge when finished,
  push at checkpoints, never commit to `legacy` or `main`.
- Verify with `tools/verify_change.R` (a spec under `docs/cc/verification/`), never with
  an ad hoc driver; it runs from its own worktree. Never switch branches in a checkout a
  run is reading; use a worktree for other work.
- Never edit a script while `Rscript` is executing it (R reads the file
  incrementally; the run is corrupted). Wait for the run to finish.
- Never remove a `library()` call without a run proving nothing depended on it
  (`gratia` registers `simulate()` for GAMs; `terra` and `sf` set up PROJ).

## Review in proportion to risk (Chris, 2026-10-14)

| Change | Review |
|---|---|
| Documentation, comments, local naming | The implementer checks it; an independent check only where useful |
| Paths, manifests, config plumbing, tools, tests | One independent reviewer (the `change-reviewer` role: Fable, maximum effort) plus targeted executable checks |
| Arithmetic, units, signs, stochastic behaviour, scientific options | Independent reviews by Fable (`science-reviewer`, maximum effort) and Astra (Codex `gpt-6-astra`, maximum effort, read-only sandbox; the `astra-review` skill), each given the same code and requirements before seeing the other's view; one reconciliation pass; open scientific questions go to Chris or Andria as questions, not forced consensus |
| Final integration (end of the migration) | Astra's review of all the migration code (`gpt-6-astra`, maximum effort), and the full-run gate |

Findings carry file and line, consequence, evidence and a proposed check. Before designing
anything, one sentence on its benefit to the paper or to Andria's use of the code; drop
speculative generality early. Verification of every code change: `tools/verify_change.R`
(see CONTRIBUTING.md).
