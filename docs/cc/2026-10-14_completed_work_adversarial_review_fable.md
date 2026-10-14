# Is the completed migration work net negative for Andria? Adversarial review

**What this is.** After the extraction issues were dropped as net-negative for readability,
Chris asked (2026-10-14) whether the work already done is net negative as well. A second
adversarial review, under the same priority (AGENTS.md: readability before elegance).
Verdict: no; nothing to roll back. The path catalogue (I07) is the one real readability
cost, best answered by flattening and tracing rather than removal. Chris's decisions on it
are in the change log.

**Who wrote it.** A Claude Code subagent (Fable), maximum effort, read-only, on branch
i06-wiring (58b47c9), comparing the scripts with the pre-migration trunk and reading `R/`,
`config.R`, tools, tests, the change log, the contract and the 2026-10-14 reviews.

---

**Short answer: no.** The completed issues changed what Andria reads at the *boundaries* of each script (where files are read and written, where a literal became a setting) and left the science lines in place; the dropped extraction issues would have moved the science itself. Verdicts: I01 positive, I02 positive, I04b positive, I04a positive, I03 mixed-positive, I05/I06 mixed, I07 **mixed, the one real readability cost**, tools/tests positive but invisible. Nothing should be rolled back. Andria has not touched a script on `chris-dev` since the merge base, so the reconciliation cost of every changed line is still prospective; every anchored output has been byte-identical at every merge.

## 1. Piece by piece

- **I01 regression contract: positive, invisible.** It is why every later verdict can say "byte-identical". Niggle: `check_calibration_month.R:22` assigns `month` globally (already I15).
- **I02 months by name: positive.** `month_to_number()` replaced a silently wrong positional lookup in script 8. Cost: unindented `if ('nov' %in% months) {` guards (already I15).
- **I03 config + schema: mixed, leaning positive.** `config.R` is the best document Andria will meet: every inherited choice with its question id. Plain `source()` was the right reversal. Cost: `R/config_schema.R` (171 lines) and the exact-key rule mean a new setting needs two files, and a stray key stops every script; the error messages are precise, which is what the rule buys. Add two lines to `config.R`: "to add a setting, add it here and in R/config_schema.R".
- **I07 path catalogue + run directories: mixed, the main readability cost.** Legacy `readRDS('data/lct_paleo_reveals_interp.RDS')` became `read_input(run, path_file(paths, 'lct_paleo'))`; `path_prediction(paths, 'point', month)` introduces the new word "point". The file name is gone from the line and the console (about 140 sites). Three family wrappers add vocabulary on top of `path_file`; `producer()` recovers the writer by matching a path back against templates; `read_input()` refuses uncatalogued paths. What it buys: every verification run without touching the baseline, I16's legacy-vs-new runs, I13b's full run, the manifests' producer lookup, and the script 5 fix. Run directories need a path layer; the surface is heavier than needed.
- **I05/I06 manifests + consistency check: mixed.** For scripts 7-9 a gain over the old hand-typed (and wrong) input/output lists; for 1-6, five lines plus a comment each. It buys the paper's provenance: commit, config, inputs, seed, including failed runs. Costs: `R/run_manifest.R` (308 lines) is never read but its errors are met; the stale check compares the whole `analysis` section, so on Andria's machine the first run stops for every existing product, and `allow_stale = TRUE` risks being left on; stepping through in RStudio writes a `started` manifest per abandoned session; `seed_base = run_record(...)$base` hides a calculation; `alb_prod = paths$tag` is dead in scripts 4, 5, 6, 7a, 8 and 9.
- **I04a registry, reduction, draws, seeds: positive.** Script 4 went from 243 to 98 lines; fifteen identical `bam()` calls became one readable table. Costs: script 4 no longer shows the family and method, reached through a hidden `paths.R` -> registry dependency; `eval(call('~', ...))` is obscure (I15: `reformulate`).
- **I04b settings: positive.** Each literal replaced in place by a self-describing setting; 7a's slice check is a genuine safety gain.
- **Tools and tests: positive for the project, invisible to Andria.** `test_run_manifest.R` is heavy but caught the side-by-side figure misattribution.
- **Cross-cutting:** the accumulated preamble (script 8 has 33 lines before the first science line, eight of them "X was removed (issue I03)" tombstones), about 80 "(issue I0x)" tags, false "every remaining line is unchanged" headers, redundant `source('R/calendar.R')` lines, and scripts 4 and 5 lacking the one `source()` that matters (`R/model_registry.R`).

## 2. Simplifications that keep the value

1. **Print what is read and written:** `read_input()`/`write_output()` emit `reading <path>` / `wrote <path>`. Stepping through shows the real file name at every site; batch logs become a trace. One line each; no output risk.
2. **Flatten the path vocabulary to `path_file` ids only:** retire `path_prediction`, `path_calibration_model`, `path_forcing`; about 50 call sites; paths asserted identical by `tests/test_paths.R`; scripts 8 and 9 as a quick run.
3. **Interactive sessions:** create the manifest at the first write or at `run_finish()` rather than at `run_begin()`, so stepping through leaves no stray `started` manifests; batch runs unchanged.
4. **Stale check: keep "stop"**, fix the two messages; a per-script key list was rightly refused and a warning is what `allow_stale = TRUE` already is.
5. **I15 additions:** delete dead `alb_prod` lines and the tombstone comments; strip "(issue I0x)" tags (the change log keeps the trail); correct the false headers; split `seed_base` from `run_record`; add `source('R/model_registry.R')` to scripts 4 and 5; two "how to add a setting" lines in `config.R`.

## 3. Roll back anything?

No. The only candidate is I07, and removing it removes run directories, on which every verification, I16's legacy/new runs and I13b depend. Flatten it; do not remove it.

## 4. Decisions

A. Flatten to `path_file` ids only (recommended) or keep the family helpers.
B. Interactive sessions: manifest at begin, at first write/finish, or none; and whether `runs/<script>/` is committed or ignored.
C. Stale check: stop (recommended) or warn.
D. Strip issue tags from script comments at I15 (recommended), keeping them in the change log.
E. Keep the schema's exact-key rule (recommended) or allow unknown keys with a warning.
