# Readability review of the pipeline code

**What this is.** At Chris's request (2026-10-14, after recording the decision that
readability comes before elegance), an independent review of where the code is now hard
for Andria and her colleagues (R-literate scientists, not software engineers) to read or
understand, covering the original scripts and the infrastructure added by the migration.
Items already in I15 (#18) were excluded.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `max`,
2026-10-14, from the code of branch i04b-settings b88813b inlined (scripts 1-9, R/, tools/,
config.R, AGENTS.md, the manifest tests). Finding 1 (scripts 1, 2 and 4 without manifests)
is resolved by I06, written after this snapshot.

---

The main problems are hidden execution rules and missing explanations at calculation boundaries. Several shared helpers are useful; the largest simplification opportunities are in run management and path handling.

This is a static review of the supplied snapshot. Effort estimates below distinguish local edits from coordinated infrastructure changes.

1. **The manifest migration leaves an unexplained break between producers and consumers.** Scripts 1, 2 and 4 use ordinary reads/writes and never start manifests, while script 5 requires their producer records. With no existing manifests, “rerun the producer” cannot resolve the failure. See `scripts/1_veg_lct_prep.R:13–16,85–89`, `scripts/2_calibration_lct_bluesky.R:13–16,147–148`, `scripts/4_calibration_model.R:12–15,36–37,67`, `scripts/5_calibration_eval.R:44–54`, `R/run_manifest.R:144–170`.
   
   Use the same explicit `run_begin` / recorded I/O / `run_finish` convention for these producers. During transition, errors should identify an uninstrumented producer rather than prescribe an ineffective rerun. **Hurts:** debugging a failed run. **Origin:** migration integration. **Cost/risk:** medium; changes provenance behavior, with little numerical risk.

2. **Starting a run silently changes the RStudio session.** `run_begin()` creates a mutable environment and installs a global error handler; path setup changes the default graphics device; `run_finish()` closes *every* open graphics device. Only the error option is restored on successful completion, and `run_failed()` restores neither option. See `R/run_manifest.R:38,65–74,118–134`, `R/paths.R:111–120`.
   
   Keep automatic error handling confined to batch execution; explicitly restore previous options and close only devices owned by the run. Document that recording functions modify `run` in place. **Hurts:** stepping through scripts and debugging subsequent errors. **Origin:** migration. **Cost/risk:** medium; requires interactive-session checks, not scientific changes.

3. **“Stale input” conflates several different situations.** The checker compares the *entire* analysis configuration and consults only the producer’s latest run. Consequently, changing script 9’s reporting variant rejects script 8’s unchanged table, although script 8 computes every variant. A later producer run that omitted a file also obscures its earlier provenance. See `R/run_manifest.R:139–164`, `scripts/8_radiative.R:116–150`, `scripts/9_forcing_barplot.R:129–138`, `tests/test_run_manifest.R:117–121`.
   
   Immediately distinguish “file changed”, “producer record missing” and “configuration differs” in messages. Longer term, associate each output with its producing manifest and compare explicitly documented relevant settings, including upstream dependencies. **Hurts:** changing settings and understanding rerun requirements. **Origin:** migration. **Cost/risk:** small for messages; substantial for dependency-aware checking, because incomplete dependency lists would weaken protection.

4. **The central calculations lack local explanations of sign, units and missing values.** The reader must reconstruct why differences are negated, how young/old rows align, and why some missing components become zero while missing ice remains `NA`. Kernel coordinate adjustments, transpose/flip operations, factors of 100 and CACK’s sign reversal likewise lack explanations. See `scripts/7a_alb_diff_full.R:245–315`, `scripts/8_radiative.R:53–62,97–108,116–150`. The zero replacement also has an unexplained numerical choice at `scripts/2_calibration_lct_bluesky.R:141–142`.
   
   Add short contracts beside these operations: ascending years BP; change means younger minus older; meanings of zero and `NA`; native kernel coordinates, units and sign conventions. Name young/old indices once, retaining existing arithmetic. **Hurts:** tracing a reported number. **Origin:** inherited analysis. **Cost/risk:** small for documentation; no numerical change. Establish explanations from existing data documentation without reconsidering the method.

5. **A “seeded” run directory has surprising, platform-dependent behavior.** Seeding may create a symlink or copy, records filenames before checking success, and does not carry producer manifests. The anchor checker then treats every recorded filename as seeded, without checking whether the run subsequently rewrote it. See `R/paths.R:244–258`, `R/run_manifest.R:99–104,145–150`, `tools/check_anchors.R:20–28`.
   
   Prefer explicit copies for isolated runs, record successful reuse with its source provenance, and distinguish reused files from newly written outputs. A symlink can also route a write back to the baseline, contrary to the isolation promise in `config.R:121–128`. **Hurts:** changing run directories and interpreting validation. **Origin:** migration. **Cost/risk:** medium; copying costs disk space, but makes behavior much easier to explain.

6. **Paths are constructed, then reverse-engineered to recover information already in the catalogue.** A prediction read passes through family helpers, template substitution and root handling; provenance then matches the resulting filename against generated regular expressions. See `R/paths.R:131–170,193–220`, `R/run_manifest.R:82–94`, `scripts/7_plot_preds.R:81`.
   
   Keep the catalogue. Flatten its resolution into concrete paths plus producer metadata, avoiding the filename-to-template round trip. A cheap improvement at call sites is:
   ```r
   prediction_file = path_prediction(paths, "summary")
   alb_interp_preds = read_input(run, prediction_file, reader = readRDS)
   ```
   Both the filename and reader become inspectable. **Hurts:** reading scripts and locating files. **Origin:** migration. **Cost/risk:** small locally; medium for resolver changes, requiring path-equivalence checks.

7. **Configuration loading implements a small programming language.** A scientist sees an R list but must understand a restricted grammar, recursive syntax inspection, a specially populated environment, and a separate recursive schema. Ordinary R expressions can therefore fail despite being valid R. See `R/config.R:21–60,80–94`, `R/config_schema.R:111–149`.
   
   If self-contained configuration expressions are optional, ordinary evaluation followed by the existing validation is simpler:
   ```r
   cfg = source(path, local = new.env(parent = baseenv()))$value
   # Apply the existing key, value and combination checks here.
   ```
   This loses the guarantee that settings depend only on the configuration’s text. The manifest already records evaluated settings (`R/run_manifest.R:55`). **Hurts:** changing settings and maintaining validation. **Origin:** migration. **Cost/risk:** medium; this is an explicit contract change, not merely cleanup.

8. **Model selection uses code evaluation where ordinary lookup would suffice.** The spatial evaluation constructs variable names and evaluates them; the registry constructs a formula through calls, symbols and expression indexing. See `scripts/5_calibration_eval.R:232–265`, `R/model_registry.R:58–64`.
   
   Store experiment fits in a named list and use `experiment_fits[[model]]`. Keep existing experiment labels and seed identifiers. For formula construction, use the existing terms text with:
   ```r
   stats::reformulate(calibration_terms(model_id), response = month)
   ```
   Retain the registry’s visible scientific formulas. **Hurts:** following model selection and inspecting a fit. **Origin:** inherited lookup; migration formula construction. **Cost/risk:** small to medium; check formula environments and the existing one-month regression comparison.

9. **Table structure is inferred from column positions and implicit join keys.** Renaming the first prediction-input column, replacing complete column-name vectors, and merging without `by` require the reader to remember earlier layouts. See `scripts/6_prediction_model.R:34–35,61–62`, `scripts/7a_alb_diff_full.R:124–126,252`, `scripts/2_calibration_lct_bluesky.R:42–44`.
   
   Select and rename by existing column names; specify joins explicitly:
   ```r
   lct_interp_paleo = dplyr::rename(lct_interp_paleo, year = ages)
   alb_cell_filled = merge(years_df, alb_cell,
                           by = "year", all.x = TRUE, sort = TRUE)
   ```
   Add “one row per …” comments at saved-table boundaries. **Hurts:** reading transformations and debugging unexpected columns or rows. **Origin:** inherited analysis. **Cost/risk:** small; preserve row order and output schemas.

10. **Script 9’s function arguments conceal important inputs.** `aggregate_forcing(col)` also depends on the surrounding table and Earth area. `panel_plot()` reads a global plotting range that is reassigned for the EGU figure. See `scripts/9_forcing_barplot.R:164–174,246–265,349–351`.
    
    Pass the table, Earth area and plot limits explicitly. This makes an interactive function call explainable from its arguments. **Hurts:** stepping through and rerunning sections. **Origin:** added analysis, explicitly identified as new code at `scripts/9_forcing_barplot.R:7–10`. **Cost/risk:** small; preserve aggregation order and plotting values.

11. **Comments sometimes describe an earlier pipeline rather than the running code.** Script 1 repeatedly says the pipeline uses the mean despite its configurable reduction; the registry says each ladder model adds to its predecessor, although models 6–8 replace/change smooth terms. Several script headers claim remaining lines are unchanged despite migrated settings and I/O. See `scripts/1_veg_lct_prep.R:25,40–52,195–200`, `R/model_registry.R:16–32`, `scripts/7_plot_preds.R:2–18`, `scripts/8_radiative.R:2–25`.
    
    State current behavior and distinguish defaults from guarantees. Replace migration chronology at use sites with purpose and configuration references; retain history in version control. **Hurts:** reading scripts and changing settings. **Origin:** migration/addition comments. **Cost/risk:** small; no numerical change.

12. **Repeated evaluation blocks obscure which outputs use which simulation.** Script 5 twice loads, saves, predicts and evaluates the selected model, then saves November again. The simulations have deliberately different seed labels. See `scripts/5_calibration_eval.R:53–87,123–165,361–408`.
    
    Separate deterministic model preparation from the named simulation purposes; save the selected model once per month. Label the sections by their products. Preserve each simulation call and seed label when extracting shared preparation. **Hurts:** tracing statistics versus figures. **Origin:** inherited repetition with migration seed handling. **Cost/risk:** medium; combining simulations would change results, so avoid doing that incidentally.

13. **Optional plotting work interrupts production of required numerical outputs.** Script 7 computes its coarse-difference table only after extensive plotting; script 8 performs exploratory calculations and plots before saving forcing. Script 1 embeds a long optional diagnostic report. See `scripts/7_plot_preds.R:208–434`, `scripts/8_radiative.R:152–186`, `scripts/1_veg_lct_prep.R:91–241`.
    
    Put numerical preparation and writes together, followed by clearly optional diagnostics with explicit inputs. Extract repeated presentation code into a few purpose-specific helpers, keeping scientific equations visible. **Hurts:** finding the computational path and diagnosing plotting failures. **Origin:** inherited analysis plus later diagnostics. **Cost/risk:** medium; preserve calculation order and check that plotting blocks do not supply downstream values.

14. **Large guarded blocks are deliberately left unindented.** The November block explicitly prioritizes a small historical diff over readable scope; similar guards surround maps and the EGU recipe. See `scripts/5_calibration_eval.R:338–424`, `scripts/7_plot_preds.R:208–234,261–289`, `scripts/9_forcing_barplot.R:339–358`.
    
    Indent the complete bodies and add short purpose comments above the guards. **Hurts:** stepping through scripts with selected months. **Origin:** migration guards around existing/additional analysis. **Cost/risk:** very small; formatting only.

15. **Seed creation is hidden inside a recording expression.** Obtaining a number requires reading through `run_record(... list(... base_seed(...)))$base`, although recording and calculation are separate actions. See `scripts/5_calibration_eval.R:34–36`, `scripts/6_prediction_model.R:24–26`.
    
    Split them:
    ```r
    seed_base = base_seed(cfg$analysis$draws$seed)
    run_record(run, "random_seed",
               list(setting = cfg$analysis$draws$seed, base = seed_base))
    ```
    **Hurts:** inspecting and reproducing random draws. **Origin:** migration. **Cost/risk:** very small; retain exactly one `base_seed()` call.

16. **The calibration checker has an ambiguous target.** Its documented CLI reads a selected-model file, refits that stored model’s specification, and compares against AIC row 8. It does not invoke the current registry by default. Its legacy compatibility helper also assigns `month` globally. See `tools/check_calibration_month.R:19–27,30–40,46–54`.
    
    Make the target explicit: a model-8 regression check using current fitting code, or a stored-fit reproducibility check. Report the model ID and choose its matching anchor row. Confine legacy formula compatibility to a helper that restores any session state. **Hurts:** understanding what a passing check establishes. **Origin:** migration tooling. **Cost/risk:** medium; verify the revised checker on one month.

17. **Validation results can sound stronger than the checks performed.** The anchor checker can return success with missing or entirely skipped files. The table checker prints “within tolerance” even without numerical tolerances. It also identifies columns by storage type, so “identifier” is not an explicit data contract. See `tools/check_anchors.R:21–37`, `tools/compare_tables.R:47–64,81–89`.
    
    Report counts of compared, missing, reused and untested items; say “report only—no numerical tolerances supplied” when applicable. Declare identifier columns explicitly where type inference is insufficient. **Hurts:** interpreting a validation run. **Origin:** migration tooling. **Cost/risk:** small for reporting; modest for explicit column contracts.

For the infrastructure specifically, my judgement is:

| Component | What to keep, flatten or remove—and what would be lost |
|---|---|
| `R/config.R`, `R/config_schema.R` | **Keep validation.** The schema mirrors the configuration and provides useful key-specific errors. The custom interpreter is the optional complexity; removing it loses text-only configuration independence. `R/config.R:24–60,80–94`; `R/config_schema.R:111–169`. |
| `R/paths.R` | **Keep one catalogue and run-root handling.** Flatten the tiny profile constructors and forcing alias; replace reverse regex matching with direct metadata lookup. Those layers add no scientific information. Replacing symlink seeding with copies costs storage. `R/paths.R:104–112,173–174,202–220,244–258`. |
| `R/run_manifest.R` | **Keep hashes, resolved settings, session information and failure records.** Simplify global hooks and output ownership first. An environment is defensible for flat scripts: replacing it with lists would require returning and reassigning state throughout. Markdown plus embedded JSON is also a deliberate single-file convenience, though it couples rendering and parsing. `R/run_manifest.R:38–76,82–134,211–250,281–298`. |
| `R/model_registry.R` | **Proportionate and useful.** Visible formulas plus one fitting function are easier to maintain than repeated fits. Simplify formula construction; do not replace the formulas with a more generic model-building system. `R/model_registry.R:15–48,58–74`. |
| `R/random.R` | **Proportionate.** The short hash supports draws independent of month order and other calls. A single global seed would lose those stated properties. Clarify call sites rather than removing this helper. `R/random.R:8–12,19–35`. |
| `R/preparation.R` | **Proportionate.** It exposes the mean/median choice and states normalization behavior. A comment on row-wise recycling, or a simple loop over class columns, would clarify `fractions / total`; no framework change is needed. `R/preparation.R:9–48`. |
| Anchor/table/calibration tools | **Keep separate purposes.** Byte identity, numerical tolerance and model refitting answer different questions. Clarify their contracts and results; combining them into a generalized testing framework would add indirection. `tools/check_anchors.R:1–14`; `tools/compare_tables.R:1–22`; `tools/check_calibration_month.R:1–15`. |
| `tools/frozen.R` | **Reasonable temporary migration tooling.** Explain that the octal operations preserve other permission bits while changing write bits. Retiring lock/unlock later would remove accidental-overwrite protection, so do that only when the frozen-input workflow is retired. `tools/frozen.R:3–13,22–28`. |

The five best readability returns for Andria are:

1. **Document sign, units and `NA`/zero meanings beside the equations.** `scripts/7a_alb_diff_full.R:245–315`; `scripts/8_radiative.R:53–62,97–150`.
2. **Indent the large conditional blocks.** `scripts/5_calibration_eval.R:341–424`; `scripts/7_plot_preds.R:209–234,263–289`; `scripts/9_forcing_barplot.R:339–358`.
3. **Replace constructed-variable lookup with a named list of models.** `scripts/5_calibration_eval.R:249–265`.
4. **Separate seed calculation from manifest recording.** `scripts/5_calibration_eval.R:34–36`; `scripts/6_prediction_model.R:24–26`.
5. **Correct comments that contradict configurable behavior or formulas.** `scripts/1_veg_lct_prep.R:25,40–52,195–200`; `R/model_registry.R:16–32`.