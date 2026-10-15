# Argument structure and methods-from-code tools: survey and design by Astra

Codex CLI, model gpt-6-astra, reasoning effort max, web search on, read-only, 2026-10-15.
Chris's request: draft argument structures for full papers, extract them from partial or
complete drafts at every level (paper, section, subsection), and write detailed, correct
bullet-point methods from a codebase, experiment logs and configs; include existing popular
tools; reusable across projects. He had asked for this with the scientific-writing tools on
2026-10-14; it was missed in the decisions that followed and picked up again here.
The request is in the scratchpad prompt; the report below is Astra's text, unedited.
Nothing here is a decision.

---

**I recommend two small, reusable Claude Code tools: a paper-argument skill and a methods-from-code skill.** Keep their records in Markdown, use one new read-only argument reader, and reuse the existing evidence, scientific-review and verification roles.

For argument structure, combine reverse outlining with explicit claims, evidence, reasoning, limitations and objections. Argdown offers useful notation and optional diagrams; it need not become the authoritative format. Of the writing skills inspected, `academic-writing-skills` is the closest existing match and deserves a controlled comparison in the pilot.

For methods, extract checked facts before generating sentences. The strongest existing pattern comes from fMRIPrep: descriptions assembled from the configured workflow. Extend that pattern with explicit links to completed runs, numerical checks and human approval.

The repository demonstrates why these distinctions matter. The current configuration compares calibration models using ML but still fixes model 8 for prediction and fits it using REML. It does **not** automatically select the model with the lowest AIC. I also re-derived 192 scheduled calibration fits from the current code; that is not evidence that 192 fits completed.

Pilot argument extraction on McWilliams et al. (2019), including planted faults. Pilot methods generation on calibration, keeping current and legacy settings separate. Chris and Claude should then compare the generated methods with the manuscript.

Nothing was installed or changed. I did not open `writing/` or any Word, PowerPoint or PDF file, or send repository content in web requests.

---

**The survey distinguishes documented capability from demonstrated reliability.** Sources below are the tools’ own repositories, documentation, release histories or institutional publications, checked on 15 October 2026. Approximate GitHub stars indicate attention, not users or scientific accuracy. Where recent maintenance, licensing or suitability could not be established, I say so. I inspected documentation and relevant instructions; I did not install or benchmark these tools.

### 

For argument formats and outline tools, the useful choices are:

| Tool or practice | What the primary sources establish | Recommendation for Chris |
|---|---|---|
| **Markdown with Pandoc** — established | Plain-text headings, lists, links and citations; Pandoc converts between Markdown and Word. Repository: about 46,600 stars, GPL-2.0; the release page lists 3.12.1. This supplies document structure and conversion, not argument analysis. [Repository](https://github.com/jgm/pandoc), [releases](https://github.com/jgm/pandoc/releases). | **Adopt as the foundation**, using the already available Pandoc. Add a small argument schema. |
| **Org mode** — established | Plain-text hierarchical outlines, properties, links and executable document features, usable in terminal Emacs. Active GNU Emacs project; current source identifies version 9.8.10 and GPL-3.0-or-later. [Project](https://orgmode.org/), [source](https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/org/org.el). | **Borrow outlining ideas.** Use directly only if Chris already prefers Emacs. Markdown asks less of collaborators. |
| **Kialo Edu** — established hosted service | Collaborative thesis/pro/con trees, moderation and text export. It operates under service terms, rather than an open-source software licence; public educational content has separate CC BY-NC-SA terms. No official public source repository was verified. Support and release pages exist; the latest release date and active-user count were not verified. [Features](https://www.kialo-edu.com/features), [export](https://support.kialo-edu.com/en/hc/exporting-a-discussion/), [terms](https://www.kialo-edu.com/terms), [release history](https://releases.kialo-edu.com/). | **Borrow concise claims and explicit objections; skip the service.** Its browser workflow, hosting and export conditions fit this project poorly. |
| **Argdown** — specialist, actively maintained | A plain-text argument language with claims, arguments, support and attack relations. Its command-line tools export maps and structured data. About 1,100 stars; MIT code licence, with a separate optional font licence; release notes show 2026 maintenance. [Repository](https://github.com/argdown/argdown), [CLI](https://argdown.org/guide/installing-the-commandline-tool.html), [changes](https://argdown.org/changes/). | **Borrow the relations; consider an optional exporter.** Its argument notation is useful, but manuscript locations, evidence reviews and approvals would still need additional fields. |
| **IBIS and CompendiumNG** — specialist, ageing implementation | IBIS represents issues, possible positions and arguments for or against them. CompendiumNG implements visual mapping in a Java desktop application. Its repository declares LGPL; I did not verify the precise version. The site advertises 2.1.4 and older Java requirements; recent maintenance was not established. [Repository](https://github.com/CompendiumNG/CompendiumNG), [history](https://compendiumng.org/history/), [downloads](https://compendiumng.org/download-windows/). | **Borrow the question–alternatives–reasons pattern** for unresolved scientific choices. Skip the desktop application. |
| **Toulmin and claim–evidence–reasoning schemes** — established practices | Toulmin distinguishes a claim, grounds, the reasoning connecting them, backing, qualifications and rebuttals. CER/CERR offers a simpler educational formulation. These are reasoning practices, not software packages; software repository and licence are inapplicable. The linked teaching materials remain copyrighted. [Purdue](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html), [NSTA](https://www.nsta.org/resources/teachers-toolkit-four-elements-claim-evidence-reasoning-and-rebuttal-cerr-framework). | **Borrow the concepts**, expressed in ordinary words. In particular, record *why* an observation supports a claim. Do not copy worksheets or force every paragraph into the same rhetorical pattern. |
| **Reverse outlining** — established writing practice | Read an existing draft and record each paragraph’s subject and function; use the resulting outline to assess progression, omissions and repetition. It works with partial drafts. This is a practice rather than licensed software. [University of Wisconsin–Madison guidance](https://writing.wisc.edu/handbook/processandstructure/reverseoutlines/). | **Adopt as the extraction procedure**, then extend it with evidence and argument relations. |

For LLM writing aids, the closest fit is a relatively small project rather than the most popular system:

| Tool | Verified capability, use and maintenance evidence, licence | Recommendation |
|---|---|---|
| **Stanford STORM** — popular research prototype | Researches a topic, develops an outline and generates a Wikipedia-style article, using multiple perspectives. About 31,600 stars; MIT. Its repository documents research publications and a public demonstration. I found 2025 activity, but did not establish a recent maintenance date. [Repository](https://github.com/stanford-oval/storm), [commit history](https://github.com/stanford-oval/storm/commits/main/). | **Borrow staged research and outline development.** Skip the full system: it is not documented as an auditor of a scientific paper’s existing argument or experimental provenance. |
| **K-Dense `claude-scientific-writer`** — substantial community project | CLI/API/plugin for research and scientific manuscript generation, including IMRaD writing. About 2,500 stars; MIT; repository activity includes August 2026 updates. Its documented workflow uses additional search/extraction services. Hierarchical extraction and comparison of an existing draft were not established as tested capabilities. [Repository](https://github.com/K-Dense-AI/claude-scientific-writer), [history](https://github.com/K-Dense-AI/claude-scientific-writer/commits/main/). | **Skip the full installation initially.** It overlaps existing citation and evidence arrangements and introduces more services and workflow than this task needs. |
| **WenyuChiou `academic-writing-skills`** — niche, actively maintained | Explicitly supports extended outlines, paragraph contracts, top-down and bottom-up alignment, evidence boundaries and change-impact review. About 106 stars; MIT; changelog lists version 1.3.12 on 12 October 2026. These capabilities are present in the actual skill instructions. Its tests do not establish editorial accuracy. [Repository](https://github.com/WenyuChiou/academic-writing-skills), [skill](https://raw.githubusercontent.com/WenyuChiou/academic-writing-skills/main/skills/academic-writing-skills/SKILL.md), [changelog](https://raw.githubusercontent.com/WenyuChiou/academic-writing-skills/main/CHANGELOG.md). | **Pilot as the closest existing comparator.** Borrow selected procedures if they outperform a shorter local skill. Do not import its entire project-state system alongside the existing one. |
| **SFETNI `manuscript-qc`** — very small project | Manuscript plans, claim and numerical registers, integrity checks and human hand-offs. Eight stars and six commits when inspected; changelog begins with 0.1.0 on 16 May 2026. Scripts are MIT; skills/templates/documentation CC BY 4.0; examples CC0. [Repository](https://github.com/SFETNI/Scientific-Writing-Skills-Claude-Code-Codex), [changelog](https://raw.githubusercontent.com/SFETNI/Scientific-Writing-Skills-Claude-Code-Codex/main/CHANGELOG.md). | **Borrow acceptance checks and explicit numerical records.** Skip its parallel claim register: the project already has an evidence table. |
| **DFKI SAM at SciArg** — academic prototype | Argument-unit recognition and relation extraction in scholarly text, developed for the WIESP 2022 shared task. The inspected repository had one star and three commits. I did not establish a reusable software licence or current maintenance. [Repository](https://github.com/DFKI-NLP/sam). | **Borrow the evaluation distinction between finding claims and finding relations.** Skip installation: this is not a maintained paper-planning system. |
| **`outline-mcp`** — very small MCP project | Stores an editable tree of sections and content in local JSON and exposes navigation/editing operations to an LLM. Dual MIT/Apache-2.0 licence; zero stars when inspected. No dated maintenance release or broader use was verified. [Repository](https://github.com/ynishi/outline-mcp). | **Skip initially.** It provides storage and access, but the argument schema and checking procedures would still have to be built. Ordinary files already meet the need. |

I did not verify a ready-made MCP server that combines hierarchical scientific argument extraction, comparison against an approved plan, evidence-table integration and controlled regeneration. That is a finding about this survey, not a claim that none exists.

For methods generation and provenance, established workflow tools provide useful components, but they do different jobs:

| Tool or standard | Verified capability, maintenance/use evidence and licence | Recommendation |
|---|---|---|
| **Quarto** — established publishing system | Combines narrative with executable analysis and values, producing documents including Word. Maintained by Posit and contributors; repository is MIT and the release page lists 1.10.19. It does not infer a complete method from arbitrary existing code. [Repository](https://github.com/quarto-dev/quarto-cli), [releases](https://github.com/quarto-dev/quarto-cli/releases), [project description](https://quarto.org/about.html). | **Borrow executable values and parameterised text.** Optional for a future report; not required for the first bullet generator. |
| **`targets`** — established R workflow tool | Tracks dependencies and rebuilds outdated results. rOpenSci project, about 1,100 stars, MIT; 1.12.0 released on CRAN in February 2026. It supplies dependency management, not methods prose. [Repository](https://github.com/ropensci/targets), [news](https://docs.ropensci.org/targets/news/index.html). | **Borrow dependency-based invalidation.** Consider for new projects; do not migrate paleo-RF solely to write its methods. |
| **Snakemake and Nextflow** — established workflow engines | Execute workflows and generate reports or traces describing jobs, dependencies, resources and software environments. Snakemake is MIT; Nextflow Apache-2.0. Their current documentation/release histories show continuing development; Nextflow’s releases include 26.10.0. [Snakemake repository](https://github.com/snakemake/snakemake), [reporting](https://snakemake.readthedocs.io/en/stable/snakefiles/reporting.html), [Nextflow repository](https://github.com/nextflow-io/nextflow), [releases](https://github.com/nextflow-io/nextflow/releases), [reports](https://docs.seqera.io/nextflow/reports). | **Borrow execution records and dependency graphs.** Skip a pipeline migration for this purpose. Execution reports alone do not explain scientific choices. |
| **`workflowr`** — established R research tool | Combines R Markdown, Git and reproducibility checks in versioned research websites. MIT; published software and a documented release history through 1.7.2. A recent release date was not verified. [Project](https://workflowr.github.io/workflowr/), [repository](https://github.com/workflowr/workflowr), [news](https://workflowr.github.io/workflowr/news/index.html). | **Borrow visible version/check stamps.** Website publication is unnecessary for a Word-led paper. |
| **`renv` and `sessioninfo`** — established R infrastructure | `renv` records/restores R package environments; MIT, with 1.3.1 dated 6 October 2026. `sessioninfo` reports the running R/package/platform environment; GPL-2, with 1.2.4 dated 4 June 2026. Neither establishes which analysis branches executed. [renv](https://rstudio.github.io/renv/), [renv news](https://rstudio.github.io/renv/news/index.html), [sessioninfo](https://sessioninfo.r-lib.org/), [sessioninfo news](https://sessioninfo.r-lib.org/news/index.html). | **Retain environment reporting.** The existing manifests already capture much of it. Consider `renv` for projects needing it; do not silently replace this project’s environment management. |
| **easystats `report`** — established model-reporting package | Produces prose and tables from supported model/data objects. MIT; its changelog lists 0.6.3 on 8 January 2026. Exact support for this project’s `bam` fits was not tested. It cannot recover upstream filtering or establish run lineage from a model object alone. [Repository](https://github.com/easystats/report), [news](https://easystats.github.io/report/news/index.html). | **Borrow model-object extraction and templates.** Optional pilot component, not the provenance authority. |
| **fMRIPrep / NiPreps** — established specialist software | Generates methods/citation boilerplate describing configured preprocessing workflows. fMRIPrep code is Apache-2.0; generated boilerplate is CC0. It has maintained releases and an LTS policy. A boilerplate-only option means text can be generated without completing the analysis. [Project](https://fmriprep.org/en/latest/), [changes](https://fmriprep.org/en/latest/changes.html), [usage](https://fmriprep.org/en/stable/usage.html), [licensing](https://www.nipreps.org/community/licensing/). | **Borrow the central design:** small descriptions connected to actual parameters and workflow components. Add a separate completed-run check. Do not install a neuroimaging pipeline. |
| **R `boilerplate`** — niche template package | Hierarchical text templates with variable substitution for methods/results and JSON export. MIT; two stars. Release notes document 1.3.0 in June 2025 plus undated development fixes; wider adoption and recent maintenance are unverified. It consumes supplied facts rather than extracting them from code. [Repository](https://github.com/go-bayes/boilerplate), [news](https://github.com/go-bayes/boilerplate/blob/main/NEWS.md). | **Borrow simple parameterised templates.** A few local templates are likely sufficient initially. |
| **`data-to-paper`** — research framework | Generates analyses and papers from data, with backwards traceability of numerical statements. About 829 stars; MIT; repository links a published evaluation. Recent maintenance was not established. Its documented starting point is data, not auditing an existing R pipeline. [Repository](https://github.com/technion-kishony-lab/data-to-paper). | **Borrow numerical traceability.** Skip the full autonomous research workflow. |
| **`rdtLite`** — niche R provenance tool | Instruments R execution to record operations and intermediate data in provenance records. GPL-3; the inspected repository had two stars and 31 commits. Current maintenance was not established. [Repository](https://github.com/End-to-end-provenance/rdtLite). | **Defer.** Existing manifests should be evaluated before adding execution instrumentation. |
| **RO-Crate and Workflow Run RO-Crate** — community standards | Describe research artefacts and workflow executions using structured metadata. RO-Crate 1.2 is a June 2025 recommendation; the specification uses Apache-2.0, with context/examples under CC0. Workflow Run profiles distinguish process, workflow and more detailed execution records. [RO-Crate specification](https://www.researchobject.org/ro-crate/specification/1.2/), [Workflow Run profiles](https://www.researchobject.org/workflow-run-crate/profiles/), [profile repository](https://github.com/ResearchObject/workflow-run-crate). | **Borrow the vocabulary now; defer export.** A release-time crate could later package code, inputs, configuration, outputs and approvals. |
| **W3C PROV** — mature standard | Distinguishes entities, activities and agents, with relationships such as use, generation and derivation. PROV-O is a W3C Recommendation, governed by W3C document notices rather than a package licence. [Specification](https://www.w3.org/TR/prov-o/). | **Borrow the distinctions.** Do not require scientists to author RDF or an ontology. |

**My recommendation is to build a small integration of these ideas, while keeping the adopted reference workflow intact.** RefChecker/refaudit, Zotero/Better BibTeX, and PaperTrace with the independent Astra challenger already have defined responsibilities. A new outline tool should neither replace them nor introduce another authoritative claim ledger.

---

**The argument tool should represent both placement and reasoning in one readable Markdown format.** Heading nesting records where material belongs. Explicit links record which claims support, challenge or answer other claims. A subsection appearing under a section does not, by itself, establish logical support.

Use the same record at paper, section, subsection and paragraph levels. Allow a node to express a question, definition or procedural purpose when it makes no substantive claim. Otherwise the tool will invent claims for transitions and methods paragraphs.

A complete node would look like this; the angle-bracketed text is a template, not an assertion about the unpublished paper:

```markdown
### <Descriptive title>

Level: subsection
Purpose: <What this part must establish for the reader>
Main claim or question: <One explicit, bounded statement>

Supporting claims:
- [<Descriptive claim>](#<descriptive-anchor>)
  Relation: supports
  Combination: needed together with <other claim>, or independently sufficient

Evidence or result:
- <Named result, table, figure or source>
  Evidence record: <link to the existing evidence table or checked result record>
  Citation: <author/year and existing citation key, when applicable>

Why the evidence supports the claim:
<The reasoning, including any necessary intermediate step>

Assumptions:
<Explicit assumptions, or "not stated">

Scope and limitations:
<Population, place, time, measurement and inference boundaries>

Objection:
<The strongest relevant objection, or "none identified in the supplied material">

Answer:
<Answer and its evidence, or "unanswered">

Draft location:
<Source version; heading path; paragraph opening words; local source locator>

Origin: explicit in draft | inferred by reader | proposed for the plan
Review: proposed | approved | stale
Approval: <person, date, exact record version and scope of approval>
```

All four levels use those fields. A paper contains section headings, sections contain subsections, and subsections contain paragraph records. A direct paper-to-paragraph cross-link is also allowed where needed.

Three refinements prevent the representation becoming misleading:

- **Separate containment from inference.** Record whether two supporting claims are jointly necessary or independently supportive. An objection may challenge a conclusion, an assumption or the connection between evidence and conclusion.
- **Link shared claims rather than copying them.** Repeated appearances can have different rhetorical purposes, particularly in the abstract and discussion. Repetition is not automatically a fault.
- **Use descriptive names.** “Winter calibration lacks northern observations” is readable; “C17” is not. Existing technical identifiers may remain behind descriptive links.

Keep an **intended structure** and an **observed structure**, using this same format. The intended version is the author’s plan. The observed version is an extraction from a particular draft. Never silently replace the intended structure with whatever the latest draft happens to say.

For this repository, dated records could be:

```text
docs/cc/<date>_argument_plan.md
docs/cc/<date>_argument_observed.md
docs/cc/<date>_argument_review.md
```

Their front matter should identify the brief or draft, its content hash, the extraction date, tool/prompt versions and what was excluded. Generated machine indexes may assist comparison, but Markdown remains the human-reviewable record.

The five requested operations would work as follows:

| Operation | Procedure | Human review |
|---|---|---|
| **Draft top-down** | Start with the question, audience, intended contribution, known results and limits. Propose the paper-level claim and section contributions. Mark desired but unavailable evidence as missing. | Chris approves the paper’s purpose and proposed structure; Andria approves scientific claims and decisions within her remit. |
| **Extract bottom-up** | Read each supplied paragraph completely. Record what it says and what it does, then assemble subsection, section and paper-level arguments. Preserve qualifiers. Mark implicit reasoning as inferred. | Review whether the extraction faithfully represents the draft, including ambiguous passages and disagreements. |
| **Expand one level** | Expand approved parent nodes into their immediate children. Do not generate all intervening prose merely because a high-level plan exists. | Review siblings together: paper → sections → subsections → paragraphs. Approval may cover a batch; unresolved branches remain proposed. |
| **Compare** | Align intended and observed nodes, allowing one-to-many and many-to-one matches. Report omissions, additions, missing reasoning, misplaced claims, scope changes, repetition and unresolved evidence requests. | Each finding gets a location, consequence, evidence and proposed remedy. The author decides whether the draft or plan should change. |
| **Regenerate** | Extract the edited material again, update affected relations and show a semantic change report alongside ordinary text differences. | Preserve approval only where the wording, evidence and relevant dependencies remain unchanged. Mark affected ancestors or conclusions stale when their support changes. |

Bottom-up extraction should be **blind to the intended plan on its first pass**. Otherwise the reader is encouraged to find the argument the author hoped to write. A fresh comparison pass can then receive both records.

For partial drafts, record the supplied scope explicitly. An absent discussion section is not an author error when only an introduction was supplied.

**The existing evidence table remains the authority on claim support.** Argument nodes point to its claim/source records and checked result records. The argument tool may request a new check, but it does not independently assign or revise:

`SUPPORTED`, `PARTIAL`, `CONTRADICTED`, `NOT ESTABLISHED`, `UNVERIFIABLE`.

An approved outline is therefore not a declaration that all its claims are supported. Likewise, a supported claim can still be irrelevant or badly placed. These are separate review questions.

For the Word connection, use local extraction from an explicitly authorised copy in a future session:

1. Record the source file’s hash and choose how tracked changes are interpreted: accepted, rejected or retained for inspection.
2. Extract headings, paragraphs, lists, tables, captions and notes into a plain-text snapshot. Maintain a locator sidecar using heading path, paragraph order, opening words and available Word paragraph identifiers.
3. Report anything not successfully extracted, including text boxes, equations or revision structures. Do not silently treat the extract as complete.
4. Return a review table or a newly generated review document. Keep the PI’s Word document authoritative for prose; incorporate changes there deliberately, then extract a new snapshot.

Pandoc supports explicit tracked-change handling. A naïve `python-docx` paragraph loop is insufficient: its documented paragraph list excludes paragraphs inside certain revision elements. [Pandoc manual](https://pandoc.org/MANUAL.html), [python-docx document API](https://python-docx.readthedocs.io/en/latest/api/document.html).

Do not promise lossless round-tripping of live Zotero fields, comments or tracked changes. Keep only the authorised plain-text records in Git; conversion copies need not become repository artefacts. No Word conversion was attempted in this session.

---

**The methods generator should assemble statements from a checked fact record.** An LLM can help identify relevant operations and explain them, but it should not supply numerical values, execution history or undocumented reasons for scientific choices.

Each bullet needs four separate kinds of evidence:

| Evidence | What it establishes |
|---|---|
| **Code and configuration inspected** | What the selected code and effective settings specify. |
| **Run observed** | What a particular execution recorded, with its status and input/output hashes. |
| **Quantity recomputed** | How a number was independently calculated from the appropriate artefact. |
| **Human approved** | Who accepted the wording and scientific description, for which version. |

These are not interchangeable. A regression anchor establishes agreement with a reference output within the check’s scope. It does not establish that the reference analysis is scientifically appropriate.

The collection procedure should be explicit:

1. **Freeze the scope.** Choose the commit, configuration, stages, months or other overrides, and exact run records. “Current” must mean a named snapshot, not whatever happens to be checked out tomorrow.
2. **Read the effective configuration and code paths.** Use the project’s trusted loader and schema where appropriate, and inspect overrides and conditional branches. R configuration files are executable code; loading one is not a security sandbox.
3. **Follow the data flow.** Record the actual inputs, transformations, fitted objects and outputs. Distinguish active options from irrelevant settings. For example, this repository’s configured CACK year is not used by the CACK climatological-mean branch.
4. **Match execution evidence.** Check configuration values, source revision, input/output hashes, completion status and run scope. Identify copied or seeded artefacts separately from artefacts regenerated in that run.
5. **Recompute reported quantities.** Derive row counts, exclusions, dates, sample sizes, draw counts and numerical summaries from artefacts. Record the calculation, units, missing-value policy, weights and rounding. Do not copy numbers from comments or a changelog.
6. **Render bounded bullets.** Templates provide stable wording; an LLM may propose explanatory phrasing using only the collected facts. Unknown reasons become questions.
7. **Review and approve.** Scientific checks precede human approval of the exact wording. Missing execution evidence permits a provisional description of the code, but not a claim that an analysis completed.

A bullet’s source record should contain:

```text
Statement: <the methods bullet>
Configuration: <named profile and effective key/value pairs>
Code: <commit, file, function and line locations>
Inputs and outputs: <named artefacts and hashes>
Execution: <manifest, status, actual overrides and relevant recorded details>
Numerical check: <calculation, units, source artefact, check result>
Verification: <specification/report, checked commit and coverage>
Limitations: <missing or conflicting evidence>
Approval: <person, date and exact bullet/source-record version>
```

Keep the readable record beside the bullet. A generated JSON representation can support validation, but should not become a second manually maintained database.

For paleo-RF, reuse `R/config.R`, the model registry, path definitions and manifest reader. Add a small, reviewed calibration adapter rather than attempting a universal R-program analyser. Inspect fitted objects as well as declarations: formula, family/link, fitting method, smooth specification, observations used and convergence information.

Avoid calling helpers that create output directories merely to discover paths. Run executable collection or numerical checks through the project’s authorised verification arrangement in its own worktree.

There is a concrete reason to check manifest status independently. In [R/run_manifest.R](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/R/run_manifest.R:161), `check_producer()` checks the latest producer’s output hash and analysis settings, but does not explicitly require a completed status. Consequently, its `ok` alone cannot establish completed execution. The proposed methods validator should reject a “completed analysis” statement in a fixture where a failed manifest has a matching output hash. That is a proposed validator requirement, not a change made here.

**Current and legacy methods should be separate products.** For the calibration-related settings inspected:

| Setting | Current configuration | Legacy configuration |
|---|---|---|
| Configured prediction model | `mod8` | `mod8` |
| Ladder comparison method | `ML` | `REML` |
| Calibration ice mask | `centre_in_1ka_polygon` | `none` |
| Evaluation and prediction draws | 100 each | 100 each |
| Base-seed setting | `"random"` | `1` |

Sources: [current configuration](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/config.R:51) and [legacy configuration](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/configs/legacy_config.R:42).

A random-seed setting is not the realised seed. Methods describing a stochastic run should link to the actual recorded seed and relevant random-number environment.

After changes, regenerate a review showing:

| Previous statement | Proposed statement | Cause | Evidence affected | Approval needed |
|---|---|---|---|---|
| Previous wording | New wording | Code, setting, input, run or evidence change | Exact dependencies and checks | Scientific wording, execution claim or placement |

An unchanged input packet should produce unchanged factual output. A changed line number alone may require updating a locator; a changed default or branch condition requires substantive review. Unresolved dependencies should conservatively mark affected statements stale.

**Here are six example bullets derived from the current calibration code.** They describe configured behaviour, not a verified completed current run.

All six share this source envelope:

- Commit: `3bcf725bc8deb9dea780ea6e444fb97d1df2869f`.
- Configuration: `config.R`, as inspected in the clean checkout.
- Current matching execution manifest: **not available in this checkout**.
- Checks performed here: source tracing and a read-only Python calculation of model/month counts.
- Pipeline execution, fitted-object inspection and human approval: **not performed**.

1. **Calibration excludes modern cells whose centres intersect the polygon set identified in the code as the 1 ka ice mask.** The script takes the first polygon object and removes matching rows from its calibration input. The helper uses the spatial relation `intersects`; it assigns EPSG:4326 when the polygons lack a coordinate reference system. The number excluded is not established by this inspection.  
   **Sources:** `analysis$calibration$ice_mask = "centre_in_1ka_polygon"`; [script 2, mask branch](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/2_calibration_lct_bluesky.R:46); [polygon helper](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/R/preparation.R:56).  
   **Check:** followed the configuration branch, geometric test and row filter. The polygon object’s age label and coordinate validity still require data-level confirmation.

2. **Monthly calibration responses are sampled from the native blue-sky albedo raster at modern land-cover cell centres.** A separately averaged raster and calibration table are also produced, but script 4 reads the native-resolution table.  
   **Sources:** [script 2, raster extraction and output](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/2_calibration_lct_bluesky.R:141), lines 141–158; [script 4, calibration input](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/4_calibration_model.R:23). This is a code-defined choice, not a configurable switch.  
   **Check:** traced the native and coarse objects separately through their output names to the fitting input.

3. **Exactly zero sampled albedo values are replaced by 0.0001 before fitting; incomplete observations are omitted by the fitting call.** This does not establish that every month uses the same number of observations.  
   **Sources:** [script 2, zero replacement](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/2_calibration_lct_bluesky.R:149); [model registry, `na.action = na.omit`](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/R/model_registry.R:72). Both are code constants.  
   **Check:** confirmed the literal replacement and omission rule. Counts of replacements and observations retained require the calibration data and fitted objects.

4. **The configured prediction model is a beta generalised additive model with a logit link.** Model 8 combines a Gaussian-process smooth of location (`x`, `y`, basis dimension 500), an elevation smooth (basis dimension 50), and a joint thin-plate smooth of `OL`, `ET` and `ST` (basis dimension 200). These are basis dimensions, not fitted degrees of freedom.  
   **Sources:** `analysis$calibration$model_id = "mod8"`; [model 8 formula](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/R/model_registry.R:33), and fitting function at lines 72–78. Smooth terminology checked against [mgcv’s documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html).  
   **Check:** resolved the configured identifier to its formula and family/link. No claim about fitted complexity or convergence is made.

5. **The eight-model comparison ladder uses ML and is compared by AIC; the model used for prediction remains the explicitly configured model 8 and is fitted separately using REML.** The script does not select the AIC minimum automatically.  
   **Sources:** `analysis$calibration$comparison_method = "ML"` and `model_id = "mod8"`; [script 4, ladder fitting](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/scripts/4_calibration_model.R:36), AIC calculation at line 64, and separate prediction fit at lines 93–101.  
   **Check:** traced comparison and prediction paths independently. The exported AIC table is rounded, so precise AIC differences should be recomputed from fitted objects.

6. **With all twelve configured months, script 4 schedules 192 fitting calls if every loop completes: 96 ladder fits using ML, 84 spatial-alternative fits using REML, and 12 prediction fits using REML.** These are fitting calls, including repeated model structures, not 192 distinct models or confirmed successful fits.  
   **Sources:** [configured months](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/config.R:27); [eight ladder and seven spatial identifiers](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/R/model_registry.R:52); the three loops in script 4.  
   **Check:** independently counted the declarations and calculated `12 × (8 + 7 + 1) = 192`; `12 × 8 = 96` ML and `12 × (7 + 1) = 96` REML calls.

The available [merged legacy verification report](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/verification/2026-10-15_i16b-merged-legacy-seed1_report.md:1) records PASS for scripts 5 and 6 with legacy settings and seeded model fits at commit `4d2c1b1e9069c69fd3a6ebc4ad5659eca2d4b637`. It does not establish a new full calibration under the current settings.

The [migration changelog](/tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-feature/docs/cc/changelog_single_config_migration.md:835) reports exclusion counts and a January comparison favouring model 7. Those are reported results here, not independently reproduced results: the corresponding run artefacts were unavailable. The prediction-model choice remains an author decision already tracked in the scientific questions.

---

**The Claude Code implementation can stay small: two skills, one new role and a few deterministic scripts.**

| Component | Responsibility |
|---|---|
| `paper-argument` skill | Main-session procedure for planning, extraction, expansion, comparison and refresh; owns proposed argument records and review gates. |
| `methods-from-code` skill | Main-session procedure for collecting facts, rendering bullets, requesting checks and preparing approval. |
| `argument-reader` role | Read-only extraction or structural assessment from an explicit packet. First extraction can be blind to the plan. |
| Existing `claim-checker` and `evidence-extractor` | Literature support and evidence-table work within their established responsibilities. |
| Existing `science-reviewer` plus Astra | Independent scientific review of methods and consequential argument inferences. |
| Existing `verifier` | Executes the approved numerical and regression checks. |
| Existing `change-reviewer` | Reviews the new scripts, configuration plumbing and tests. |
| Existing `changelog-writer` | Records implemented changes after approval and verification. |
| Proposed `argument_io.py` | Local document import, source locators, format validation and structural/text differences. It does not judge scientific arguments. |
| Proposed `methods_io.py` | Validates source records, renders stable bullets and reports changed dependencies. |
| Project-specific `tools/collect_methods.R` | Extracts effective R settings and stage facts; reads approved artefacts and performs project-specific calculations through the verification workflow. |

The reusable skills would live at `~/.claude/skills/<name>/SKILL.md`, and the role at `~/.claude/agents/argument-reader.md`, on the machine where Claude Code runs. Keep their source in a small version-controlled tooling repository and record the installed version. Claude Code documents both personal skill and subagent locations. [Skills documentation](https://code.claude.com/docs/en/skills), [subagents documentation](https://code.claude.com/docs/en/sub-agents).

Project-specific settings belong in a proposed `.paper-tools.yml`: permitted inputs, prohibited paths/extensions, output naming, configuration profiles, stage adapters, evidence-table location, citation export, question files, verification command and review roles. Resolve the actual evidence-table path during implementation; do not assume a path named in a role description already exists.

No new MCP server or automatic hook is necessary for the pilot. A later local freshness check could flag stale records after a change. It should not silently rewrite or approve scientific text.

Astra should receive the same frozen source-and-requirements packet as the scientific reviewer **before seeing that reviewer’s conclusions**. Reconcile once, preserving unresolved differences for Chris or Andria. For the methods pilot, that packet can contain code, configurations, manifests, checks and generated bullets; it need not contain the unpublished manuscript. This follows the existing review arrangement without adding an Astra-wrapper role.

These are complete proposed instruction files, not installed files.

The first skill would be `~/.claude/skills/paper-argument/SKILL.md`:

```markdown
---
name: paper-argument
description: Draft, extract, expand, compare or refresh a scientific paper's argument structure at paper, section, subsection and paragraph levels. Use for explicit argument planning or structural review, not ordinary proofreading or citation verification.
---

# Paper argument

Help the authors make the paper's reasoning explicit and reviewable.
Work in the main session. Only the main session writes proposed records.

## Establish the task

Read the applicable project instructions and paper-tools profile.
Identify the operation: plan, extract, expand, compare or refresh.
Identify the supplied brief or draft, its version, permitted scope,
intended audience, known results and author-approved decisions.

Respect the current session's confidentiality and file restrictions.
Do not upload source material or put it into web searches.
Do not install tools or alter project configuration without authorisation.

If the project profile is absent, use explicitly supplied paths and return
a proposed profile. Do not search excluded directories to fill gaps.

## Representation

Use Markdown headings for containment and descriptive links for reasoning.
At every level record:

- level and reader purpose;
- main claim or question;
- supporting claims and their relations;
- whether supporting claims are needed together or independently;
- evidence or result links;
- why that evidence supports the claim;
- assumptions, scope and limitations;
- objections and answers, including unanswered objections;
- existing evidence-table links and citation keys;
- draft version and source locators;
- origin: explicit, inferred or proposed;
- review state: proposed, approved or stale;
- human approval, date, version and scope, when supplied.

Allow question, definition, procedure and transition nodes.
Do not invent a substantive claim for every paragraph.
Use "not stated", "not supplied" or "unanswered" where appropriate.

The intended plan and observed draft use the same format but remain
separate records. Heading containment does not imply logical support.

## Procedure

1. For planning, propose the paper-level question, bounded contribution
   and immediate section contributions from the authorised brief.
   Distinguish known evidence from evidence still needed.

2. For extraction, give argument-reader the supplied draft and scope
   without the intended plan. Require full-paragraph reading, coverage
   of all supplied text units and locators for extracted claims.
   Assemble higher-level arguments from the returned records.

3. For expansion, develop only the next level below the approved scope.
   Review siblings together. Independent approved branches may proceed;
   unresolved branches remain proposed.

4. For comparison, use a fresh reader with the frozen intended and
   observed structures. Allow one-to-many and many-to-one alignments.
   Inspect omissions, additions, reasoning gaps, lost qualifications,
   placement, repetition and unresolved objections.

5. Link requests for literature support to the existing claim-checking
   workflow. Do not create a parallel evidence table or assign its
   support verdicts yourself.

6. After edits, refresh affected observations and relations. Show the
   old and new claims and the reason for each substantive change.
   Preserve approvals only when their content and relevant dependencies
   remain unchanged. Mark affected conclusions stale.

## Review and approval

Ask the author to approve each proposed level in a readable batch.
Record exactly what was approved; model agreement is not approval.
Scientific decisions belong to the designated scientific authors.
Do not overwrite an approved plan merely to make it match the draft.

For Word, use only the authorised local conversion procedure and an
explicit tracked-change policy. Report extraction omissions. Return a
review copy or change list; do not promise lossless citation-field or
revision round-tripping.

## Output

Return the proposed or observed argument record, a coverage statement,
and located findings. Each finding states the consequence, evidence
and proposed check or revision.

List unresolved questions and route them to the project's existing
question records when writing is authorised. Otherwise return proposed
question entries. End with sources read and checks not performed.
```

The second skill would be `~/.claude/skills/methods-from-code/SKILL.md`:

```markdown
---
name: methods-from-code
description: Produce or refresh detailed scientific methods bullets from a specified code revision, effective configuration, execution records and verification evidence. Use for traceable methods descriptions, not generic code documentation or autonomous analysis changes.
---

# Methods from code

Describe what the selected analysis specifies and what execution evidence
establishes. Never turn a planned operation into a completed-run claim.

Work in the main session. Reuse the project's verifier and reviewers.
Only the main session writes proposed methods records.

## Establish the source packet

Read project instructions and the paper-tools profile.
Identify the commit, configuration profile, stages, overrides, permitted
inputs, selected manifests, verification reports and output destination.
Keep different configurations and execution scopes separate.

Respect session confidentiality and file restrictions.
Do not upload repository material or include it in web searches.
Do not install dependencies, change scientific options or start an
unapproved pipeline run.

If execution records are missing, produce a clearly labelled code-derived
draft and identify the missing evidence.

## Collect facts

Use the approved project adapter and verification workflow.
Do not write or run an ad hoc pipeline driver.

Read effective settings, including overrides and active branch conditions.
Trace inputs, transformations, model definitions and outputs.
Inspect relevant fitted or result objects where authorised.
Treat comments and historical notes as statements to check, not as proof.

For each proposed bullet collect:

- exact wording and configuration profile;
- effective setting names and values;
- code commit, file, function and line locations;
- input and output artefact identities and hashes;
- execution manifest, completion status and scope;
- numerical calculation, units and source artefact;
- verification specification, report, checked revision and coverage;
- missing or conflicting evidence;
- human approval of the exact version, when supplied.

Distinguish configured behaviour, observed execution, recomputed quantities
and human approval. None substitutes for another.

## Check facts

Cross-check configuration, executed code, manifests and artefacts.
Identify copied or seeded outputs separately from freshly computed ones.
Do not select a run merely because its filename or modification time is
latest. Require appropriate lineage and matching hashes.

Recompute reported quantities with the approved executable checks.
Record missing-value rules, weighting, units and rounding.
Do not obtain sample sizes or results by copying comments, prose logs
or rounded presentation tables.

For stochastic operations, distinguish the configured seed policy from
the realised seed and record the relevant random-number environment.
Mark unresolved dynamic behaviour as unresolved.

## Write and review

Render concise, detailed bullets from checked facts.
Use stable templates for recurring operations.
Explain scientific rationale only when an identified decision or source
supports it; otherwise return a question.

Attach a readable source record to every bullet.
Use language that matches the evidence: configured operations are not
automatically operations that were completed.

Send the same frozen source packet and requirements independently to the
designated scientific reviewer and Astra before either sees the other's
view. Request numerical checks from the verifier. Reconcile once and
preserve unresolved disagreements for the authors.

Human approval applies to exact wording, configuration and source-record
versions. Never assign approval on the author's behalf.

## Refresh

Compare new inputs with the previous source packet.
Regenerate affected bullets and show previous wording, proposed wording,
cause, changed sources, check results and approval status.
Mark dependent statements stale when their evidence changes.
Keep a no-change regeneration factually and textually stable.

## Output

Return methods bullets with sources, a configuration/execution statement,
checks passed and not performed, a change report when applicable, and
unresolved questions. Route questions through the project's established
records when authorised; otherwise return proposed entries.
```

The new role would be `~/.claude/agents/argument-reader.md`:

```markdown
---
name: argument-reader
description: Reads an authorised scientific draft or compares a frozen intended and observed argument structure. Returns located claims, reasoning relations, coverage gaps and structural findings. Read-only; does not repair the draft or decide citation support.
tools: Read, Grep, Glob
model: inherit
effort: high
color: cyan
---

You determine what supplied text actually argues and whether its structure
meets an explicitly supplied plan. You do not inherit the main session's
conversation. Your task packet must contain the scope and restrictions.

## What you are given

For extraction: the authorised plain-text draft snapshot, source version,
locator map, supplied scope and required argument format. The intended
plan is deliberately withheld.

For comparison: the frozen intended structure, observed structure,
relevant draft passages, author-approved constraints and scope.

If required material is missing or outside the permitted scope, report
the limitation. Do not search excluded locations.

## Extraction procedure

Read every supplied paragraph in full, including qualifications.
For each text unit record what it says and what it does.
A paragraph may contain several claims or no substantive claim.

Record claims, evidence references, reasoning, assumptions, limitations,
objections and answers. Give each extracted claim a source locator.
Distinguish explicit statements from your inferred interpretation.
Do not complete missing reasoning or invent a citation.

Build subsection, section and paper-level descriptions from the text.
Distinguish heading containment from support, challenge and answer links.
Record whether premises appear jointly required or independently useful.
If the relation is uncertain, say so.

Account for every supplied unit as mapped, non-argument material,
unreadable or outside the declared task scope.
Do not treat unsupplied sections of a partial draft as writing failures.

## Comparison procedure

Align intended and observed claims without requiring identical wording.
Preserve changes in scope, uncertainty, causality and generalisation.
Allow one-to-many and many-to-one matches.

Check for missing or additional claims, missing reasoning, misplaced
material, unexplained transitions, unresolved objections and repetition.
Do not flag justified abstract/body repetition merely because it repeats.

Use the plan's stated purposes, not a universal section-order rule.
Locate each finding in the source and explain its consequence.

## What you must never do

- Edit files or silently repair the argument.
- Treat an inferred interpretation as the author's explicit statement.
- Assign or revise the evidence table's claim-support verdicts.
- Treat a citation's presence as proof of support.
- Approve scientific choices or manuscript changes.
- Use unexplained private identifiers.

## Output

For extraction, return the argument records and a coverage table.
For comparison, return the alignment and located findings.

Each finding gives severity, source location, consequence, evidence and
a proposed check or author decision. Separate errors from uncertainties.
List material not checked and any alternative plausible interpretation.

End with the files read and tool calls made.
```

These instructions deliberately leave scientific verification with the existing roles. The tracked role definitions already provide the appropriate separation between reading, checking, executing and writing; the new role follows that style.

---

**The argument pilot should test extraction and fault detection separately.** McWilliams et al. (2019) is a suitable first paper because Chris can judge the intended reasoning. The publisher confirms the paper’s identity, publication date and CC BY 4.0 licence. Use its HTML or XML, not a PDF. [BMJ Open article](https://bmjopen.bmj.com/content/9/3/e025925), [publication and licence information](https://bmjopen.bmj.com/content/9/3/e025925.info).

A proposed pilot procedure is:

1. **Create an author reference map first.** Chris marks the paper-level contribution, section purposes, important claims, necessary reasoning links, limitations and genuine ambiguities. Permit more than one defensible mapping.
2. **Freeze three baselines.** Compare an ordinary reverse-outline prompt, a pinned version of `academic-writing-skills`, and the proposed tool. Use the same model and supplied material; record prompts and cost.
3. **Test partial and complete inputs.** Include the introduction alone, a draft missing sections, and the full paper. Measure whether missing input is distinguished from defective writing.
4. **Create separate altered copies.** Plant one fault per copy before testing combinations. Keep the fault key hidden from the extracting and reviewing agents.
5. **Include harmless controls.** Preserve legitimate abstract/body repetition, move a paragraph with a correct cross-reference, and make stylistic changes that leave reasoning intact.
6. **Use a held-out test.** Develop the instructions on one subset of faults, then freeze them before evaluating the rest. Eventually repeat on another open-access paper.

Useful planted faults include:

| Fault | What detection requires |
|---|---|
| Remove the reasoning between a result and a conclusion | Checking the inference, not merely finding both statements |
| Remove a scope qualification | Comparing claim strength and population |
| Attach a result or table reference to the wrong claim | Checking evidence alignment |
| Place a claim in a subsection serving another purpose | Comparing function and placement |
| Duplicate a claim in different words without adding anything | Recognising semantic repetition |
| Introduce an objection with no answer | Following challenge/answer relations |
| Make the conclusion broader than the reported evaluation | Tracking scope across levels |
| Leave a research question unanswered | Connecting introduction and results/discussion |
| Separate premises that are only persuasive together | Identifying joint support |
| Make the abstract inconsistent with the body | Comparing distant representations of the same claim |

Proposed acceptance criteria should be frozen before the test:

- Every supplied text unit is accounted for.
- No fabricated quotation, result or evidence link.
- At least 90% of Chris’s important claims are recovered, reporting results separately by level.
- All five predesignated critical faults are detected; at least 80% of the remaining planted faults are detected.
- At most one false alarm among ten harmless controls.
- Chris can review the output faster than constructing the equivalent map manually.

Report counts and disagreements, not a headline “accuracy” that hides differences between claim extraction, relation extraction and fault detection. Familiarity with a published paper can benefit the model as well as Chris; altered passages and a later held-out paper help test whether the tool actually reads its input.

**The methods pilot should begin with calibration under both configurations.** First build the source records for approximately twenty selected statements, including the six examples above. Then obtain the necessary artefacts and run targeted checks through `tools/verify_change.R`.

Use a January verification subset where a refit is needed, explicitly labelled as a subset. It must not be described as evidence that all twelve months completed. Preserve the distinction between frozen/seeded legacy fits and new fits.

Plant controlled faults in copies of the evidence packet:

- An ML configuration paired with a REML description.
- An unmasked calibration table paired with the current mask setting.
- A manifest from the wrong commit or configuration.
- A failed run with a matching output hash.
- A copied model described as newly fitted.
- A January run described as an all-month run.
- An incorrect exclusion count or fitted-observation count.
- A rounded AIC difference presented as an exact value.
- A random-seed policy without the realised seed.
- A source change that should make an approved bullet stale.

Proposed success measures are:

- Every bullet has resolvable code/configuration sources and an explicit execution-evidence status.
- Every numerical statement is either recomputed or clearly marked unverified.
- No current/legacy mixing and no false claim of completed verification.
- Every planted critical provenance fault is detected.
- A no-change regeneration produces no substantive difference.
- Relevant code/configuration changes invalidate the affected statements.
- Chris and Claude identify no material discrepancy after comparing the approved bullets with the manuscript’s methods.

That final manuscript comparison belongs to **Chris and Claude, not this Codex review**. They should classify discrepancies as stale manuscript text, incorrect extraction, missing execution evidence or an unresolved scientific decision. The manuscript should not automatically be treated as correct when it conflicts with the code.

---

**The decisions I recommend, in order, are:**

| Order | Decision | Cost and principal risk |
|---|---|---|
| 1 | **Adopt Markdown argument records and reverse outlining**, retaining the existing evidence table and citation workflow. | Low additional infrastructure. The risk is excessive record-keeping; pilot the smallest useful fields. |
| 2 | **Approve the public-paper pilot**, comparing a simple prompt, `academic-writing-skills` and the proposed procedure. | Model usage and Chris’s review time. A small successful pilot must not be presented as general validation. |
| 3 | **Build the two skills and one argument-reader role**, with project settings kept separately. | Moderate instruction and integration work. Role boundaries must prevent duplicate evidence assessments. |
| 4 | **Build one calibration facts adapter and stable methods renderer.** | Moderate engineering and scientific review; numerical checks and any refits are likely to dominate cost. |
| 5 | **Borrow fMRIPrep’s parameter-bound descriptions and workflow provenance concepts.** | Template maintenance is required when code changes. Unsupported rationale remains a human question. |
| 6 | **Defer Argdown export, RO-Crate export, MCP storage and pipeline migration.** | They add maintenance and conversion work before their benefit has been measured. |

No new paid service is necessary for this design, although Claude/Astra usage, review time and computation still have costs. Record actual token usage, check runtime and author review time during the pilots rather than estimating savings in advance.

The unresolved scientific choices—including the prediction-model choice—remain questions for the authors. I left both living question files unchanged under the read-only instruction; the design above specifies how future authorised sessions should route and record new questions.