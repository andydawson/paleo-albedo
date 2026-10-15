# Is the codebase over-engineered? Review by Astra (Codex gpt-6-astra, max effort)

Asked by Chris (2026-10-15): "ensure that the codebase and scripts are not becoming overcomplicated or over engineered. We can add new separate specific scripts if we need to, for building rasters etc." Reviewed branch i16c-elevation at bdf8a5e (one uncommitted edit to the elevation tool appeared during the review and was excluded). Astra's text, unedited. Nothing here is a decision.

---

**The migration has mostly simplified the scientific code. The main sources of excessive complexity are the rules for reusing previous results, the mixture of routine analysis with optional investigations, and the expanding plans for future tools.** I would preserve the numbered scripts and shared scientific functions, while making preparation and development work visibly separate.

The numbered pipeline has fallen from 6,774 to 2,637 lines. Including its shared helpers and configurations, it is still substantially shorter than the original. However, across all reviewed code, nonblank, non-comment lines have increased by about 49%. Smaller scripts therefore do not mean less total machinery.

The configuration, model definitions, month handling, random seeds, regression anchors and basic run records have clear benefits for the paper. Their removal would recreate documented problems. The blanket rule that every intermediate must have exactly the same analysis configuration is less proportionate: it can demand upstream reruns after changes that cannot affect those outputs.

The new elevation tool follows the right architectural pattern: prepare a fixed input separately, then select it in script 1. Its committed delivery is incomplete: the new default requires a table absent from this checkout, and its promised input documentation is missing.

The immediate priority is to finish that delivery, correct the entry documentation and remove a few concrete duplications. For the planned albedo work, agree one scientifically justified preparation recipe before implementing alternatives. The argument and methods tools should remain small experiments outside the analysis pipeline.

I reviewed committed `bdf8a5e`, including the two elevation commits, against `legacy`, using source, history and documentation. An uncommitted elevation-tool edit appeared during the review; it is excluded from the counts and assessment below. I made no changes, opened none of the prohibited material, and did not execute R or the verification suite.

**The size comparison shows where the additional work has gone.**

Counts cover tracked `.R`, `.py` and `.sh` files. “Code lines” below means nonblank lines that do not begin with a comment marker; it is a consistent approximation, not a count of executable statements.

| Area | Legacy lines | Current lines | Legacy code lines | Current code lines |
|---|---:|---:|---:|---:|
| Top-level `scripts/` | 7,861 | 3,736 | 2,983 | 2,267 |
| `scripts/archive/` | 2,444 | 2,444 | 1,302 | 1,302 |
| `R/` | 0 | 1,286 | 0 | 792 |
| `config.R` | 0 | 182 | 0 | 43 |
| `configs/` | 0 | 172 | 0 | 43 |
| `tools/` | 0 | 1,566 | 0 | 1,142 |
| `tests/` code | 0 | 1,054 | 0 | 779 |
| **Total** | **10,305** | **10,440** | **4,285** | **6,368** |

There are now 67 code files rather than 27. Physical lines increased only 1.3%, partly because substantial commented and superseded code disappeared. Tooling and tests account for 2,620 lines, including 668 lines generating historical diagrams.

For the numbered pipeline specifically:

| Script | Legacy lines | Current lines |
|---|---:|---:|
| 1 | 246 | 271 |
| 2 | 420 | 182 |
| 3 | 622 | 254 |
| 4 | 830 | 105 |
| 5 | 1,067 | 421 |
| 6 | 304 | 111 |
| 7 | 2,085 | 524 |
| 7a | 853 | 212 |
| 8 | 347 | 184 |
| 9 | — | 373 |

Adding `R/`, both configurations and `make_grid.R` gives 4,289 lines around the current numbered pipeline, versus 6,774 originally. This is not a like-for-like functionality comparison: the point branch disappeared, while script 9 and scientific options were added.

For a newcomer, I count **six operational concepts** before confidently running script 1: repository root and run order; packages and input availability; current versus legacy configuration; analysis/presentation/run settings; input versus output locations; and run records with the rules for accepting previous results. The original required roughly three: location/order, dependencies/data, and settings embedded in scripts. Modifying the current pipeline adds three more: catalogue identifiers, the read/write wrappers, and schema validation. Andria should not need to understand verification specifications or merge hooks to run an analysis.

The following is the evidence behind my assessment; recommendations follow separately.

| Mechanism and evidence | Reader cost | Benefit and proportionality |
|---|---|---|
| Configuration: [config.R:20](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/config.R:20), `R/config.R:56`, [R/config_schema.R:1](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/config_schema.R:1). Ordinary R lists, a loader and validation; the legacy configuration is a complete separate list. | Adding a setting requires coordinated edits to current configuration, legacy configuration, validation and its consuming script. | Proportionate. Choices become visible and invalid combinations fail early. The explicit legacy snapshot protects it from changes to current defaults. Its repeated values are useful documentation. |
| Path catalogue: [R/paths.R:42](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/paths.R:42), 254 lines. Identifiers map to filenames, producers and alternative output directories. | A reader follows an identifier to discover a filename. The file also contains directory creation, graphics-device settings and copying of baseline products. | Mostly proportionate. It fixes the real disagreement between script 4’s model output paths and script 5’s input paths. It also prevents sensitivity runs overwriting the baseline. |
| Run records and wrappers: [R/run_manifest.R:94](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/run_manifest.R:94), 317 lines. Files are hashed; writes and run status are recorded; session settings are restored. | Introduces a run object, wrapper calls, error handling and rules for reusing outputs. | Provenance serves Andria and the paper directly. The basic mechanism is justified; the configuration compatibility rule is too broad for routine experimentation. |
| Model definitions: [R/model_registry.R:14](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/model_registry.R:14), 79 lines; script 4 is now 105 lines. | Model names must be looked up once. Some identical formulas remain under different names. | A substantial improvement over repeated fitting blocks. This is a named list of formulas, not an elaborate extension system. The ML comparison fits and REML prediction fits have different purposes. |
| `R/calendar.R:1` (40 lines), `R/random.R:1` (35), `R/kernels.R:1` (26). | Three small shared vocabularies. | Strongly proportionate: correct month indexing, reproducible draws independent of month order, and explicit kernel meanings. |
| `R/preparation.R:1` (81), `R/map_helpers.R:1` (56), `scripts/make_grid.R:1` (12), [R/differences.R:1](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/differences.R:1) (139). | Scientific operations require occasional movement between script and helper. | Appropriate extraction of shared or consequential operations. Fraction checks, boundary handling and tested ice/vegetation arithmetic warrant named functions. |
| Preparation and plotting: [script 1:117](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/scripts/1_veg_lct_prep.R:117); script 2:91; script 3:1. | Optional diagnostics occupy more than half of script 1. Script 2 mixes extraction with extensive plotting, although script 3 already exists for diagnostics. | The checks are useful; their placement makes the essential preparation harder to see. |
| Evaluation and prediction: [script 5:124](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/scripts/5_calibration_eval.R:124), script 6:1. | Script 5 repeats selected-model evaluation and saves the selected fit at lines 56, 128 and 360. Script 6 is comparatively direct. | Script 5 still carries avoidable duplication. Script 6’s present structure is proportionate. |
| Maps and forcing: [script 7:365](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/scripts/7_plot_preds.R:365), script 7a:188, script 8:104. | A script called “plot predictions” also produces numerical differences. Script 8 repeats related conversions across output columns. | The main calculations remain recognisable. The confusing responsibilities and remaining repetition deserve small changes, not another pipeline framework. |
| Final reporting: [script 9:322](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/scripts/9_forcing_barplot.R:322). The recovered EGU recipe reads script 7’s coarse differences before the main tables are saved. | Historical reproduction is an obligatory dependency even when only the current summary is wanted. | Valuable historical evidence, but poorly placed in the routine reporting path. |

Three concrete problems illustrate the practical cost.

First, [config.R:41](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/config.R:41) defaults to `land_mean`, and [script 1:75](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/scripts/1_veg_lct_prep.R:75) requires `data/elev_land_mean_cells.csv`. That table is neither present nor tracked. The two raw-input README files named by the elevation tool are also absent. Furthermore, [tests/test_paths.R:13](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tests/test_paths.R:13) compares the catalogue against the frozen inventory, with an exception only for new prediction fits. The elevation entry therefore violates its assertion by inspection.

Second, [R/run_manifest.R:177](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/R/run_manifest.R:177) compares the **whole analysis configuration**. Changing the forcing variant used by script 9 rejects previously produced forcing data, although script 8 already computes all variants. Changing the requested months also rejects upstream preparation, despite script 2 deliberately preparing all twelve months. The prescribed alternatives are rerunning or allowing all stale inputs. This is an explicitly chosen policy in `CONTRIBUTING.md:109`, rather than an accidental implementation error, but it imposes unnecessary work.

Third, the entry documentation contradicts the implementation. [README.md:71](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/README.md:71) says month subsets are unavailable; line 135 describes a model-path failure already fixed; line 42 describes `tools/` as shell and Python helpers. Its principal launch example is machine-specific. `CONTRIBUTING.md:114` still describes converting stages into wrapper functions, although the changelog records the decision to retain flat scripts. These contradictions increase the learning burden more than another small helper would.

**The development machinery has mixed purposes and should be treated accordingly.**

| Evidence | What it buys | Assessment |
|---|---|---|
| [tools/verify_change.R:1](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/verify_change.R:1), 284 lines. Its introduction records two misleading ad hoc verification drivers and the danger of changing a checkout during execution. | One repeatable verification route, isolated outputs, recorded exit status and completed-run checks. | Justified development infrastructure. Its Git worktrees and symbolic links are additional requirements; being written in R does not establish cross-platform portability. |
| [tools/check_merge_evidence.R:8](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/check_merge_evidence.R:8), 91 lines. A matching PASS report is required for the merged code. | Enforces evidence during concurrent development. | Reasonable during migration. It verifies that a report exists for the code, not that the chosen checks were scientifically sufficient. It need not become part of Andria’s ordinary operating procedure. |
| [tools/check_anchors.R:21](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/check_anchors.R:21) leaves success true for absent or copied outputs. `verify_change.R:186` consequently interprets its printed output to require every result to be `MATCH`. | Distinguishes reproduced outputs from reused ones. | Useful distinction, unnecessarily split between two success rules and text parsing. |
| `tools/compare_tables.R:1`, `tools/check_calibration_month.R:1`, anchor documentation and `tools/frozen.R:1`. The frozen inventory contains 94 `data/` and 237 `output/` paths. | Numerical comparisons, prediction equivalence and protection of the migration baseline. | Keep the comparisons. Distinguish permanent source-input integrity from freezing an entire development cache. |
| [tools/check_comments_only.R:1](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/check_comments_only.R:1), 54 lines. | Proves that comment/layout edits preserve parsed R expressions. | Proportionate; it avoids expensive numerical reruns for editorial changes. |
| Twelve test files, 1,054 lines. [tests/test_run_manifest.R:232](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tests/test_run_manifest.R:232) includes a file larger than 2 GB. | Small scientific examples plus error, concurrency and large-file checks. | Keep meaningful coverage, but distinguish fast checks from platform and stress tests. |
| `tools/methodology_schematic.py:1`, `tools/nointerp_pipeline.py:1`, `tools/restore_original_outputs.sh:1`, `tools/rscript.sh:1`, `tools/download_kernels.sh:1`. | Historical documentation, recovery, a development-machine launcher and acquisition. | These have different audiences. The historical diagram generators are not a second analysis pipeline. The Linux launcher is already explicitly optional; acquisition needs an R route for hand-over. |

The 14 archived scripts are unchanged from `legacy`. So are `GCM_snow_prob.R`, `thornthwaite.R`, `beta_veg_lct_modern.R` and `6_prediction_model_spatial_eval.R`, together another 1,087 lines. This is inherited clutter, not migration growth. The spatial-evaluation script still uses old paths and dependencies; [the existing general questions:97](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/questions_for_andria_general.md:97) already ask whether that analysis is wanted.

**My preparation recommendation is to make each published input reproducible without making every analysis run rebuild it.**

Use one simple convention:

- Put entry scripts in `tools/preparation/prepare_<product>.R`. Separate downloading only where it materially helps authentication, restarting or storage.
- Give each product a plain R recipe configuration, a short README and checksums. Record source versions and references, grid, units, masking, averaging, missing-value rules and the code version.
- Keep large raw downloads outside Git with retrieval instructions. Preserve small final cell tables, or provide an explicitly versioned download for larger outputs.
- Have the pipeline select the documented product/recipe through configuration and read its fixed output through the existing catalogue. Preparation should never start implicitly because an input is missing.
- Check cell identifiers or coordinates, uniqueness, coverage and expected columns at the boundary. Introduce shared preparation helpers only when more than one script actually needs them.

MODIS/ERA5 climatology construction, static elevation aggregation and static land-fraction calculation belong here. Posterior reduction, calibration masking and calculations that vary with an analysis configuration still belong in the numbered pipeline.

The elevation tool already follows the central separation: [tools/prepare_land_elevation.R:44](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/tools/prepare_land_elevation.R:44) prepares a small table; script 1 selects it or the legacy input. It needs the missing product, documentation and checksum/provenance record. Conversely, script 1’s centre-point fallback at line 83 still downloads and writes a nominally fixed input; move that acquisition into preparation.

Keep the mask meanings explicit: the elevation tool’s **non-ocean area includes lakes**. That quantity is not automatically the land fraction required for vegetation-only albedo. Applying a land fraction to forcing also changes the scientific area calculation and must remain an explicit, reviewed method choice.

The planned work needs the following limits.

| Work | Evidence | Simplest proportionate design |
|---|---|---|
| **MCD43A3/A2 + ERA5 rebuild** | The reconciliations still disagree. [Astra:213](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/2026-10-15_albedo_rebuild_reconciliation_codex_gpt6astra.md:213) favours all processed qualities; [Fable:324](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/2026-10-15_albedo_rebuild_reconciliation_fable.md:324) favours full inversions. Coverage thresholds and centre-versus-overlap aggregation also differ. | Resolve one primary recipe after a small pilot. Use a download script, preparation script and checking script, with ordinary shared R functions. Restartable downloads and bounded memory are justified. Implement only sensitivities tied to a paper question; avoid a matrix of every quality rule, time reducer, mask, zenith threshold and radiation definition. |
| **Jia product** | The published product provides actual and snow-free climatologies, but uses 2001–2020 and an aerosol-based treatment of diffuse illumination. | Evaluate it before committing to the full rebuild. A small importer into the same calibration-table format may save substantial work. Adoption requires accepting its different period, construction and land/water treatment; otherwise retain it as a comparison product. Do not describe it as equivalent to rebuilding the original recipe. |
| **Snow sensitivity** | Existing plans already distinguish snow states; the original pipeline holds modern snow influences within the calibration. | Start with two named configurations using the same accepted product: actual conditions and snow-free conditions. Refit the affected calibration and predictions, compare common supported cells/months, and report lost coverage. Do not revive the abandoned climate/water-budget branch. This tests dependence on the calibration response; it does not reconstruct Holocene snow change. |
| **Argument and methods tools** | [The proposal:284](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i16c-elev/docs/cc/2026-10-15_argument_structure_and_methods_tools_codex_gpt6astra.md:284) includes two skills, another role, Python adapters, a project collector and configuration. Lines 78–124 prescribe records at four document levels. | Begin with one Markdown outline and 5–10 methods statements from one calibration run. Retain source locations and the distinctions between configured, executed, recomputed and approved claims. Compare correction effort and useful errors found against a simple manual/prompt baseline. Defer document importers, dependency tracking and approval-state machinery until that pilot demonstrates a repeated need. Keep reusable tooling outside this repository. |

The precedent for evaluating the alternative is Jia, A., Wang, D., Liang, S., Peng, J. and Yu, Y. (2022), *Global daily actual and snow-free blue-sky land surface albedo climatology from 20-year MODIS products*, **JGR Atmospheres 127**, e2021JD035987, DOI [10.1029/2021JD035987](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021JD035987). Its [public data record](https://zenodo.org/records/6350821) offers a roughly 79 MB coarse product for initial inspection.

A later snow-mixture analysis has precedent in Hasler et al. (2024), *Accounting for albedo change to identify climate-positive tree cover restoration*, **Nature Communications 15**, 2275, DOI [10.1038/s41467-024-46577-1](https://www.nature.com/articles/s41467-024-46577-1). It combines snow/illumination states using corresponding fractions; it does not justify treating the fraction of snowy retrieval dates as ground snow-cover area. The project’s formal reference-checker audit remains recorded as pending in the albedo reconciliation.

**I would rank the concrete changes as follows.** All code changes should use the existing verification tool with checks appropriate to the affected behaviour; there is no need for another driver.

| Priority | Do now | Benefit | Risk and check |
|---|---|---|---|
| **1** | Complete the elevation delivery: `config.R:47`, script 1:75, elevation tool’s final write, `tests/test_paths.R:13`. Supply the documented product and provenance; distinguish new inputs from the historical frozen inventory. | Makes the default executable and removes confusing preparation obligations. | Scientific risk lies in product construction. Check grid alignment, cell matching, mask meaning and coverage; verify the legacy option separately and review expected changes under the new option. |
| **2** | Correct `README.md:41,71,135,254,277` and the superseded instruction at `CONTRIBUTING.md:114`. Provide one short R/RStudio starting route and a current input list. | Largest immediate reduction in newcomer confusion. | Low risk. Walk through the instructions from a fresh session; check every named file and package. |
| **3** | Give `check_anchors.R:21` one structured result, consumed directly by `verify_change.R:186`. Explicitly distinguish inspection from required reproduction. | Removes duplicated success rules and parsing of printed messages. | Check missing, copied, matching and differing outputs. Required missing outputs must fail. |
| **4** | Remove repeated selected-model writes in script 5:128,360, the unused prediction read in script 7:123, and unused boundary reads in script 8:31. Consolidate repeated calculations only into small named functions. | Less work and fewer irrelevant dependencies. | Preserve simulation call identifiers and seeds. Compare numerical outputs; expect provenance records to lose only unnecessary entries. |
| **5** | Move script 1’s elevation acquisition fallback at line 83 into preparation. Extract its diagnostic block at line 117 into one named helper. | Makes ordinary preparation short and predictable. | Compare legacy tables with the existing cache, and diagnostic results with diagnostics enabled. Avoid opportunistic package removal. |

| Priority | Do at hand-over | Benefit | Risk and check |
|---|---|---|---|
| **6** | Separate optional investigations: script 9:322’s EGU reproduction; script 5:226’s spatial comparisons; diagnostics mixed into scripts 1–2. Retire unused historical scripts only after the existing Andria questions are resolved. | Clarifies the minimum route to the paper’s results. | Preserve required analyses and historical reproduction commands. Compare their outputs before and after separation. Do not merge ML and REML fits simply because their formulas match. |
| **7** | Reconsider `R/run_manifest.R:177`’s whole-configuration rule. Use a small explicit list of settings affecting each product, including inherited upstream choices, while retaining hashes and provenance. | Allows valid reuse after reporting-only changes. | This revises an agreed policy. Test that irrelevant changes permit reuse and consequential changes still reject it. Avoid automatic dependency inference. |
| **8** | Organise `tools/preparation/` and `tools/development/`; distinguish fast tests, integration/stress tests and migration records. Keep supported tools alongside their code. | Makes purpose and maintenance responsibility visible. | Update paths and verification specifications together. Test representative verification and preparation commands. |
| **9** | Preserve the migration’s frozen cache and reports as historical evidence, while maintaining a smaller permanent source-input inventory and meaningful result anchors. Make the merge hook an explicit maintainer facility. Provide R acquisition commands where required. | Reduces hand-over obligations without losing reproducibility. | Check that a clean checkout can obtain required inputs and run the documented R workflow. Do not silently relax the current migration rules. |

**Leave these mechanisms in place:** the flat numbered scripts; explicit current and legacy configurations; schema validation; the central path catalogue; named model formulas; shared calendar, seed, preparation, mapping and difference functions; basic run manifests; regression anchors; and the single verification tool. The comments explaining units, signs and scientific choices are also valuable.

Keep the universal stage-wrapper conversion and ensemble driver parked. The next substantial investment should produce an accepted, documented scientific input or answer a specific paper question—not expand the number of ways the pipeline can be configured.