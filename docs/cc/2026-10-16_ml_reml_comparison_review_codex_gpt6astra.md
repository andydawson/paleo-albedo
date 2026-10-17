# ML or REML for the model comparisons: review by Astra (Codex gpt-6-astra, with reconciliation)

**Verdict.** Do not merge on the stated rationale: restore REML for the seven experiments, correct the explanation of mgcv’s AIC, and put the ladder choice back to Chris with that correction.

**Benefit.** Consistent fitting makes the spatial-structure diagnostics easier to interpret; I found no demonstrated benefit to the paper from changing these seven fits to ML.

**Hand check.**

The relevant AIC arithmetic is
\[
\mathrm{AIC}=-2\ell_{\mathrm{conditional}}+2d,
\]
where \(d\) includes the applicable effective-degrees-of-freedom correction and nuisance parameters. For hypothetical fitted models:

| Model | Conditional log likelihood | Total \(d\) | AIC |
|---|---:|---:|---:|
| A | 12 | 3 | −18 |
| B | 13 | 5 | −16 |

B fits better by likelihood but has worse AIC. AIC therefore need not decrease as terms are added. Neither model’s REML score enters this calculation. This agrees with script 4’s `AIC()` call and mgcv 1.9-4’s `logLik.gam` implementation, which uses `edf2` when available. [Versioned source](https://raw.githubusercontent.com/cran/mgcv/1.9-4/R/mgcv.r)

For a prediction invariant, take two cells, two slices and one month. Hold location and elevation constant through time and let a fitted spatial-only model have linear predictors \(-\log 4\) and \(-\log(3/2)\). Its albedos are \(0.2\) and \(0.4\) at **both** slices, whatever valid cover fractions change between them. Consecutive point-prediction differences are exactly zero. `R/model_registry.R:39` and `scripts/6_prediction_model_spatial_eval.R:66` agree structurally. Finite simulated means need not agree exactly: the optional script uses 20 unseeded draws.

These are arithmetic and structural checks, not newly fitted examples.

**Findings.**

1. **Blocker — the stated reason for this change is factually false.**  
   [scripts/4_calibration_model.R:75](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/scripts/4_calibration_model.R:75); `config.R:52`.

   The seven are not compared by AIC in script 5. I found no AIC, BIC, likelihood-ratio or `anova` comparison consuming these seven experiment files in the searched current or legacy code. Changing their smoothing method changes their fitted responses and simulated diagnostics without the claimed requirement.

   **Check:** trace the read sites below; any contrary claim needs an actual call whose arguments are these experiment fits. Restore an explicit `method = "REML"` at script 4’s experiment fit call; no additional setting or abstraction is needed.

2. **Blocker for the scientific justification — REML scores and AIC from REML-fitted GAMs have been conflated.**  
   `R/model_registry.R:68`; `config.R:54`; [questions_for_andria_scientific.md:264](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/docs/cc/questions_for_andria_scientific.md:264).

   The installed `logLik.gam` help distinguishes marginal AIC from conditional AIC. Its different-fixed-effects REML caveat belongs to the **marginal-likelihood discussion**. The subsequent conditional-AIC correction applies, when available, to **both ML and REML smoothing selection**. Consequently, different unpenalized terms do not impose a blanket prohibition on comparing these GAMs with `AIC()`. Same observations and suitable fitted models remain necessary. [mgcv documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/logLik.gam.html)

   This removes the stated necessity for the 14 October ladder change. It does **not** establish that fitting the ladder by ML is wrong or authorize silently reversing Chris’s decision.

   **Check:** inspect actual fits’ likelihood, AIC degrees of freedom, `edf2`, optimizer and retained observations; verify the calculation against `logLik.gam`.

3. **Should-fix — the use audit omitted a scientifically relevant consumer.**  
   [scripts/6_prediction_model_spatial_eval.R:59](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/scripts/6_prediction_model_spatial_eval.R:59), and its six experiment reads listed below.

   This optional script makes Holocene predictions and contrasts between model structures. Under the proposed change, its reference is the selected **REML** fit, while the six alternatives become **ML** fits. Their differences would then combine changes of structure and smoothing method.

   It also assumes the selected fit represents `SP_ELEV_LC`; that label becomes false if `model_id` changes away from `mod8`. This is inherited and the script is already parked, but “script 5 only simulates them for figures” is incomplete.

   **Check:** before reviving it, verify formula, method, inputs and provenance for every loaded model. Keep this separate from the small REML restoration.

4. **Should-fix — the proposed verification does not exercise the changed fitting call.**  
   [2026-10-15_spatial-ml.R:8](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/docs/cc/verification/2026-10-15_spatial-ml.R:8).

   `scripts = character(0)` runs configuration/path/preparation/manifest tests. Those cannot establish convergence, method routing, prediction changes or scientific appropriateness. The spec refers to a full current-configuration run, but I did not verify that run.

   **Check:** add targeted fitting evidence through `tools/verify_change.R`, as described below.

5. **Should-fix, inherited — changing to ML does not validate all ladder chi-square comparisons.**  
   `scripts/4_calibration_model.R:70`; `scripts/5_calibration_eval.R:351`; `R/model_registry.R:27`.

   The sequence includes replacements of additive smooths and changes between GP and thin-plate bases. It is not established as a nested sequence. ML does not supply that missing condition. mgcv also describes multi-model GAM tests as approximate and potentially misleading. These tables should not be presented as general significance tests supporting the ladder’s ordering. [anova.gam documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/anova.gam.html)

   **Check:** identify each intended hypothesis and demonstrate nesting and identical response rows before interpreting its test; otherwise use the justified model-comparison criterion.

6. **Should-fix — B4 contains incorrect claims and an incomplete decision record.**  
   `questions_for_andria_scientific.md:250`, `:260`, `:272`.

   B4 says AIC increases cannot occur for correctly fitted nested models. The hand calculation disproves that. Its “monotone … every month” description is also contradicted by the anchor: June changes from −11796 for model 5 to −11790 for model 6.

   B4 still ends with an unanswered request to refit by ML; it does not contain the October decisions attributed to it. The migration changelog records the ladder change elsewhere.

   **Check:** correct B4 and record the actual decisions with their sources. Chris’s “It feels confusing that we sometimes use ML, and sometimes REML” is a concern, not a decision to revert.

The complete read-site audit is below. Column headings identify the files; numbers are source lines.

| Experiment | Current `scripts/5_calibration_eval.R` | `origin/legacy:scripts/5_calibration_eval.R` | `scripts/6_prediction_model_spatial_eval.R`, current and legacy |
|---|---:|---:|---:|
| `spatial_elev_cover` | 245 | 342 — selected-fit substitute | 59 — selected-fit substitute |
| `cover_elev` | 246 | 344 | 94 |
| `cover` | 247 | 345 | 87 |
| `spatial_cover` | 248 | 346 | 79 |
| `spatial` | 249 | 347 | 65 |
| `spatial_elev` | 250 | 348 | 72 |
| `elev` | 251 | 349 | 100 |

- **Current script 5:** simulations at `:257`; summaries at `:278`; scatter/interval plots at `:286`, `:299`, `:312`; simulated-minus-observed errors at `:325` and histograms at `:327`. These use the calibration data, so they are in-sample diagnostics.
- **Legacy script 5:** corresponding operations at `:355`, `:375`, `:383`, `:401`, `:417`, `:433`, `:435`. Its active reads use `output/calibration/spatial_experiment/`, whereas script 4 writes elsewhere. These are coded uses, not evidence that the legacy block completed. The separately written `spatial_elev_cover` file has no consumer under its own filename in these legacy scripts.
- **Optional spatial evaluation:** predictions immediately follow each read; their table is saved at `:117`. Simulations occur at `:122`, `:127`, `:132`, `:137`, `:142`, `:147`, `:152`; summaries at `:178` and `:190`; aggregation/reloads at `:210`, `:217`, `:225`; inter-model differences begin at `:237`; comparisons are plotted at `:252`, `:288`, `:317`. The file is unchanged from `origin/legacy`.
- **Shared helpers:** `R/paths.R:65`, `:66`, `:159` define and route these filenames; `R/run_manifest.R:94` reads and records them. No model comparison occurs there.
- **Other pipeline consumers:** script 6 reads the selected fit at `:42`; scripts 7–9 consume its downstream products. I found no additional experiment-fit consumers in `scripts/` or `R/`.

The actual likelihood-based comparisons use the **ladder**: current script 4 at `:64` and `:70`, script 5 at `:348` and `:351`; legacy script 4 at `:326` and `:332`, script 5 at `:732` and `:735`. Legacy script 5 also compares the old point-flavour ladder at `:904`, `:906`, `:909`, `:912`, `:915`, `:918`. None consumes the seven experiment files. Three experiment formulas duplicate `mod1`, `mod2` and `mod8`; that is not a seven-experiment comparison.

The citable basis supports **REML as a defensible choice**, not a claim that ML predictions are invalid:

- **Wood, S. N. (2011), “Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models,” JRSS B 73, 3–36, DOI: 10.1111/j.1467-9868.2010.00749.x.** The abstract reports advantages of both REML and ML over GCV/AIC smoothing selection. It does not establish universal REML superiority over ML. [Publisher abstract](https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-9868.2010.00749.x)
- **Wood, S. N., Pya, N. and Säfken, B. (2016), “Smoothing Parameter and Model Selection for General Smooth Models,” JASA 111, 1548–1563, DOI: 10.1080/01621459.2016.1180986.** Section 5 develops corrected conditional AIC; Section 6 evaluates it, including beta responses. This is directly relevant to the objection about differing unpenalized terms. [Article, Sections 5–6](https://www.tandfonline.com/doi/full/10.1080/01621459.2016.1180986)
- **Pedersen, E. J., Miller, D. L., Simpson, G. L. and Ross, N. (2019), “Hierarchical generalized additive models in ecology: an introduction with mgcv,” PeerJ 7:e6876, DOI: 10.7717/peerj.6876.** The examples use REML, recommend either REML or ML over GCV, and demonstrate AIC comparisons. “Comparing different HGAM specifications” separately warns about likelihood-ratio tests and recommends predictive assessment beyond training-data fit. [Article](https://pmc.ncbi.nlm.nih.gov/articles/PMC6542350/)
- **Installed mgcv 1.9-4:** `gam`’s `method` documentation identifies REML as the default beyond the exponential family. `gam.selection`, “Interactive term selection,” warns against comparing **REML scores** across different fixed-effect structures. Neither statement makes ML compulsory for conditional AIC. [gam](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.html), [gam.selection](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html)

“ML for fixed-effect comparisons, REML for final estimation” is a conventional mixed-model workflow. The `nlme::anova.lme` Note explicitly rejects restricted-likelihood comparisons with different fixed effects, and `lme4` automatically refits REML models by ML for multi-model `anova`. That supports the conventional rule **for those likelihood comparisons**; it should not be transferred indiscriminately to mgcv’s conditional AIC. [nlme documentation](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/anova.lme.html), [lme4 documentation](https://lme4.github.io/lme4/reference/merMod-class.html)

I would replace the explanation to Chris with:

> ML and REML choose the amount of smoothing. REML accounts for estimating the model’s unpenalized coefficients; its own fitting score cannot be compared directly when that unpenalized structure changes. But mgcv’s `AIC()` uses a different quantity—the conditional data likelihood with an effective-complexity correction—so REML-fitted GAMs can be compared that way. The seven experiments compare predictions, making REML a reasonable, consistent choice alongside the selected prediction model. Their plots do not establish performance on unseen data.

**What the proposed verification misses.**

- Fit the seven for one month through the actual script route and assert their recorded method, convergence and finite response predictions in \([0,1]\). Hold the calibration table and all other settings fixed.
- With `model_id = "mod8"` and REML throughout, compare point predictions from `spatial_elev_cover` with the separate prediction fit. They have the same formula and inputs; agreement is a useful routing invariant.
- Assert the two-slice invariant above using **point predictions** for models without cover terms.
- For any AIC comparison, verify identical retained observations—not merely equal row counts—and inspect whether `edf2` is present. `na.omit` can otherwise make differing formulas use differing data.
- Confirm that changing only the seven does not change the selected prediction fit or its downstream seeded outputs.

`R/random.R:26` derives seeds by script, month and call; script 5’s experiment keys at `:259` are unchanged. Draw counts remain 100 in the configured pipeline. Method changes can change the simulated distributions despite identical seeds.

The seed-1 anchor covers selected-model statistics and script 6’s draws/summaries; it contains no numerical artifact for these seven-model diagnostics. Historical tolerances measure simulation variability, not validity of ML or REML. Passing either check would not establish the new method’s scientific justification.

**Questions for Chris or Andria.**

- **B4:** after correcting the AIC explanation, retain the ML ladder with REML prediction refits, or use REML fits and conditional AIC throughout? Both require appropriate diagnostics. Meanwhile this checkout uses ML for the ladder and seven experiments, and REML for prediction.
- **General question 7:** retain and repair the optional spatial-evaluation script, or retire it? Meanwhile it remains outside the migrated pipeline, uses hard-coded paths and unseeded simulations, and assumes the selected model is `mod8`.
- **B3:** are the seven intended as descriptive fit diagnostics or evidence about transferable land-cover effects? Meanwhile script 5 evaluates them on their fitting data.

**Not checked.**

No fits, simulations or verification suite were run; no numerical ML–REML effect size is established here. Actual stored fits’ corrected degrees of freedom and convergence remain unchecked. A read-only R source-inspection attempt failed because R could not create its temporary file; the supplied help and versioned public source were used instead.

Wood (2017) and Zuur et al. (2009) identities were checked, but their specific book passages were not verified, so I have not used them as decisive evidence. Kernel units, ice arithmetic and forcing aggregation were outside this change. No reviewer conclusions, `writing/` content, PDF, DOCX or PPTX files were opened.

Files read: packet diff/help; CONTRIBUTING; regression contract, both question files and relevant changelog entries; configs; relevant R helpers, scripts 2/4/5/6/spatial-eval and legacy counterparts; verification spec, test and anchor text — tools: 24 read-only command calls, including one failed R inspection, and 10 web calls; no edits.
## Reconciliation reply

**Verdict — AGREE:** do not merge the spatial ML change as proposed; restore REML for the seven experiments. Keeping the ladder on ML remains defensible, but its justification needs correction and its eventual scientific disposition remains a question for Chris and Andria.

**Benefit — AGREE:** restoring REML preserves consistency in the spatial diagnostics and, when `model_id = "mod8"`, with the prediction model. Neither report establishes a benefit from switching these seven fits to ML.

I re-checked code at `6c9cb68` and the question files at `origin/chris-dev` (`9903077`). References to those question files below mean that branch.

1. **Finding 1 and packet answer 1 — AGREE: the asserted AIC comparison does not exist in the audited code.**

   I repeated the likelihood-call search and traced the consumers. Script 4’s comparisons at lines 64 and 70 consume ladder fits. Script 5 reads the seven at lines 244–251, simulates at 257, and produces summaries and plots at 278–331; its later AIC/ANOVA calls consume ladder fits. The optional spatial-evaluation script reads the selected fit and six alternatives at lines 59–100, predicts, simulates, and calculates differences at 237–242. The legacy code agrees with that account.

   Thus [script 4’s explanation](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/scripts/4_calibration_model.R:75) and `config.R:52–55` misdescribe the computation. The optional consumer also means “used only to draw calibration figures” is incomplete.

   My “blocker” and Fable’s “should-fix” labels lead to the same disposition for this patch. A contrary finding would require an actual likelihood-comparison call consuming these experiment fits. Our searches establish absence in the audited code, not every historical or external analysis.

2. **Finding 2 — AGREE, conditional on the selected formula.**

   With the current `model_id = "mod8"`, the proposed change gives the diagnostic reference an ML fit while prediction uses a separate REML fit of the same formula. The optional script would combine a REML reference with six ML alternatives, confounding structure and fitting method.

   However, different methods do not prove a particular numerical prediction difference without examining the fits. Also, the proposed equality check is appropriate only when the selected model has the reference formula.

   **Check:** with `model_id = "mod8"` and identical inputs, compare response predictions within `1e-6`. Because the figure displays simulated responses, also compare fitted beta precision: matching means alone does not establish matching response distributions.

3. **Finding 3 and packet answer 3 — AGREE on the AIC distinction; DISAGREE with an unrestricted reading of the ML recommendation.**

   I re-read the supplied mgcv help, including the REML examples. `AIC()` uses the conditional data likelihood and effective degrees of freedom; the smoothing-parameter uncertainty correction applies when available under either ML or REML. The prohibition concerning differing fixed effects concerns the restricted likelihood criterion. It is not a blanket prohibition on `AIC()` for REML-fitted GAMs. Actual objects still need checking for correction availability. [mgcv `logLik.gam`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/logLik.gam.html)

   The conventional mixed-model ML workflow is supported by `nlme::anova.lme` and lme4’s default ML refitting for comparisons. That does not transfer unchanged to mgcv’s conditional AIC. [nlme documentation](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/anova.lme.html), [lme4 documentation](https://lme4.github.io/lme4/reference/merMod-class.html)

   Specifically, `gam.selection`’s recommendation that ML is probably best occurs in its discussion of **backward selection using approximate p-values**. Reading the preceding paragraph settles that scope; it is not a direct instruction that this conditional-AIC ladder must use ML. [mgcv `gam.selection`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html)

   I therefore retain my earlier position: correct the rationale and put the informed choice to Chris. Neither silently reversing the ladder nor treating the correction as scientifically inconsequential is warranted.

4. **Finding 4 — DISAGREE with the missing-record finding; I withdraw that part of my own report.**

   `origin/chris-dev:docs/cc/questions_for_andria_scientific.md` already contains:

   - The 14 October ladder decision at line 388.
   - The 15 October spatial decision at lines 364–368.
   - The mistaken-premise correction and reopening at lines 369–375.
   - January sensitivity results at lines 390–404.

   The settling check was the requested `git show origin/chris-dev:docs/cc/questions_for_andria_scientific.md`.

   **DISAGREE** with recording a completed reversal before Chris makes that decision. The record currently says **reopened**, which is accurate. Our recommendation can be recorded as a recommendation.

   Some substantive corrections remain: line 355 still incorrectly says correctly fitted nested models cannot show increasing AIC, and lines 377–386 retain the misleading likelihood argument. The correction at lines 358–362 already addresses the earlier claim of monotonic ordering.

5. **Finding 5 — DISAGREE with unconditional removal of `spatial_elev_cover`; UNRESOLVED as a presentation decision.**

   [Script 5’s existing comment](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/scripts/5_calibration_eval.R:237) explains why it reads a fixed formula: the selected model is configurable. `R/config_schema.R:123` permits any ladder model.

   Replacing this panel unconditionally with `calibration_prediction_fit` would change its meaning when another model is selected. For example, selecting `mod1` would put a spatial-only model under `SP_ELEV_LC`.

   **Settling check:** exercise routing with `model_id = "mod1"` and inspect the panel’s formula and label. Reuse is reasonable when the formula, method and inputs match; unconditional catalogue removal needs a decision about the figure’s intended meaning.

6. **Finding 6 — AGREE with reporting sensitivity; DISAGREE that it is absent from B4.**

   I re-checked the changelog and current B4. They report January model 8 ahead by 13 under REML and model 7 ahead by 13 under ML on the full table. The ice-excluded ML result narrows the lead to 7. I also checked the REML anchor table; I did not recompute the ML results.

   The comparison on the same full table supports the reported method sensitivity. Comparing the ice-excluded ML table directly with the all-cell REML table would confound two changes. The paper should identify the fitting method and acknowledge this sensitivity if these results support model selection. Which table governs selection remains unresolved.

**Hand check — AGREE with the response-simulation interpretation, with two corrections.**

The documented simulation route holds fitted parameters fixed and draws responses. For beta regression, mgcv’s `rd` uses the fitted precision stored in the family; `object$sig2` is not the beta precision. This reinforces the need to check precision as well as predicted means. [mgcv 1.9-4 beta-family source](https://raw.githubusercontent.com/cran/mgcv/1.9-4/R/efam.r)

The count is **192 fitted objects produced by script 4**: `(8 + 7 + 1) × 12`. The catalogue’s **204 files** additionally includes 12 selected-fit copies written by script 5. Those copies are not another 12 fitting operations.

**Packet answer 2 — AGREE with REML here; DISAGREE with claiming demonstrated superiority or a guaranteed smoothing direction.**

REML is a documented, reasonable choice and preserves the original estimation method and consistency with the prediction fit. ML predictions are nevertheless legitimate. The documentation’s concern about ML oversmoothing does not establish that every spatial fit here becomes smoother, or quantify its prediction change. [mgcv `gam`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.html)

I retrieved Wood’s abstract: it reports benefits of **both** REML and ML relative to GCV/AIC smoothing selection. It does not establish universal REML superiority over ML. Wood (2011), *Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models*, DOI: [10.1111/j.1467-9868.2010.00749.x](https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-9868.2010.00749.x).

**Settling check for the numerical claim:** compare paired ML/REML fits with identical formulas and retained observations, inspecting smoothing parameters, effective degrees of freedom, fitted means and precision.

**Packet answer 4 — AGREE with correcting the explanation, with two qualifications.**

First, say **“data likelihood evaluated at the penalized coefficient estimates”**, rather than “penalized log-likelihood”: the latter could suggest that the smoothing penalty itself enters the likelihood reported by `logLik.gam`. I checked its versioned implementation. [mgcv 1.9-4 source](https://raw.githubusercontent.com/cran/mgcv/1.9-4/R/mgcv.r)

Second, **DISAGREE** with naming `predicted_samples()` as an alternative that adds model uncertainty. The author’s documentation explicitly says it calls `simulate()` and excludes uncertainty in estimated model parameters. `fitted_samples()` addresses coefficient uncertainty in the mean; `posterior_samples()` combines coefficient draws with response variation. These are different targets. [gratia documentation](https://gavinsimpson.github.io/gratia/reference/predicted_samples.html), [posterior-simulation vignette](https://gavinsimpson.github.io/gratia/articles/posterior-simulation.html)

Reading the installed implementations would settle the version-specific behavior. I verified the installed package version and registration, but did not extract its installed function bodies.

**Packet answer 5 — AGREE with the narrow restoration and comment corrections; DISAGREE with making the rule universal.**

Dropping `method = comparison_method` restores the current helper default of REML. My earlier preference for explicit `method = "REML"` makes the intention visible, but the two are scientifically equivalent with the present helper.

The comments should describe this pipeline’s chosen methods and actual consumers. “REML for every fit that is predicted or simulated from” is too broad as a general methodological rule. Agreement on the fitting method also does not, by itself, validate every analysis-of-deviance comparison. Keep deduplication and any change to uncertainty sampling separate from this restoration.

**Fable’s proposed verification:**

1. **AGREE with inspecting actual saved methods, with corrected accounting.** Check script 4’s 192 outputs and subsequently script 5’s 12 copies. Under the proposed restoration: current ladder ML, seven experiments REML, prediction fits and selected copies REML; legacy all REML. Record observed methods, not merely intended configuration values. The [existing spec](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-spatial/docs/cc/verification/2026-10-15_spatial-ml.R:8) does not execute the fitting script.

2. **AGREE with the reference prediction invariant, subject to finding 2.** Require matching formulas and inputs, condition the assertion on `model_id = "mod8"`, and include precision if claiming equivalent simulations.

3. **DISAGREE that a parse-and-grep test prevents recurrence.** A name-based search can miss aliases and does not validate explanatory comments. It is useful audit evidence, but actual method routing and consumer tracing provide stronger checks. A simple counterexample is assigning an experiment fit to a variable without “spatial” in its name.

4. **DISAGREE with literal call-byte identity against `legacy`.** Legacy contains seven expanded `bam()` calls using `get(month)`; the migrated code uses a registry and helper. Compare formula meaning, family, method, controls and retained inputs, then numerical predictions under the existing tolerance. A source diff alone cannot establish numerical equivalence.

These executable checks belong in `tools/verify_change.R`. My earlier recommendation to exercise one month’s seven actual fits, check convergence and finite predictions, and confirm preservation of the selected prediction route remains.

**Differences from my report that remain open:**

- **UNRESOLVED — ladder chi-square interpretation.** I retain my concern about `scripts/4_calibration_model.R:70` and `scripts/5_calibration_eval.R:351`: the registry replaces smooth structures and bases; nesting has not been demonstrated. Matching methods is insufficient. Check each intended hypothesis, model-space nesting and identical retained observations before interpreting the approximate tests. [mgcv `anova.gam`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/anova.gam.html)
- **AGREE — simulation figures need accurate interpretation.** These are diagnostics on the calibration data; they do not establish performance on unseen data or transferable land-cover effects.
- **Withdrawn:** my claims that B4 lacked the decisions and lacked a correction to monotonic ordering. The residual AIC explanation identified above still needs correction.

**Questions for Chris or Andria — UNRESOLVED:**

- **B4:** With the corrected conditional-AIC explanation, should the ladder retain ML or use REML? Which table should govern the paper’s selection, and should both January results be shown? Meanwhile the configured prediction model remains `mod8`; a January winner alone does not decide the all-month choice.
- **General question 7:** Should `SP_ELEV_LC` always represent its fixed model-8 formula, or follow whichever prediction model is selected? That answer determines whether the duplicate can safely disappear. The separate retain-or-retire question for the optional spatial-evaluation script also remains.
- **A2:** Should the intervals show response spread, uncertainty in the fitted mean, or both? Meanwhile they show response spread. `predicted_samples()` would not change that uncertainty target.
- **B3:** Are the seven intended as descriptive fit diagnostics or evidence about transferable cover effects? The present calibration plots establish only the former.

**Not checked:** no fits, simulations or verification suites were run; saved objects’ methods, convergence, precision and `edf2` remain unchecked. Book passages, Gurka’s conclusion, and Wood (2011)’s full-text bias argument remain unverified and are not decisive evidence here. No files were changed, including the living question files, under the read-only instruction.

Files re-read: packet, Astra report, diff/help, relevant configs/helpers/scripts and legacy counterparts, both trunk question files, changelog, verification spec, tests and anchor CSV. Tools used: read-only shell/git inspections and web lookups. No `writing/`, PDF, DOCX or PPTX content was opened.