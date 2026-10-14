# Tools for literature review and academic writing with Claude Code

**What this is.** Chris asked (2026-10-14), to update this paper's literature review: "look
for tools and solutions to get Claude Code to function well for literature review and
academic writing in general." A web-searched survey (MCP servers, skills and plugins,
GitHub projects, complementary services) with a ranked shortlist and a workflow for this
paper.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `high`,
with live web search, 2026-10-14. Claims marked V were checked by the model on the web
(with URLs), not installed or run here; everything else is its assessment.

---

For this paper, I would build around **Zotero, a versioned bibliography, direct scholarly searches, and a claim-to-source evidence table**. Claude Code should search, extract, compare, and propose revisions; Chris should verify the evidence and give Andria a short set of decisions.

**Checked 14 October 2026.** **V** below means verified in primary documentation, repository files, or release records—not installed or benchmarked here. **Assessment** means my judgement about suitability. GitHub stars are approximate snapshots; dated activity is evidence of maintenance, not necessarily the latest commit.

My ranked shortlist follows. Start with **1–2, one search route from 3, and selected checks from 4**. The remaining tools are optional.

| Rank | Tool and verified state | Why it fits; setup and caveats |
|---|---|---|
| **1** | **[Zotero + Better BibTeX](https://retorque.re/zotero-better-bibtex/)**. **V:** BBT is MIT, ≈7.2k stars; its [release page](https://github.com/retorquere/zotero-better-bibtex/releases) shows continuing releases. Supports stable citation keys and automatic bibliography export. | **Best foundation.** Install Zotero, browser Connector, and a BBT release compatible with the installed Zotero version. Create one paper collection; export it to `references.bib` with “Keep updated.” Zotero owns metadata; git records bibliography changes. BBT does not verify scientific claims. |
| **2** | **[54yyyu/zotero-mcp](https://github.com/54yyyu/zotero-mcp)**. **V:** ≈5.4k stars; server/CLI MIT, optional Zotero UI plugin AGPL. [Version 0.14.1 published 8 October 2026](https://pypi.org/project/zotero-mcp-server/0.14.1/). | **Best Claude connection to an existing library.** Search metadata, retrieve full text and annotations, export citations. Prefer its `zotero-cli` plus skill for terminal work; MCP also works. Start without write authorization or semantic indexing. Local retrieval still sends retrieved text to the chosen cloud model. |
| **3** | **[openags/paper-search-mcp](https://github.com/openags/paper-search-mcp)**. **V:** ≈2.8k stars, MIT; MCP, CLI and skill; Crossref, OpenAlex, Semantic Scholar, arXiv, PubMed and OA retrieval. [PyPI 0.1.4 dates to 2 July 2026](https://pypi.org/project/paper-search-mcp/). | **Useful search convenience layer, subject to a pilot.** Select sources explicitly. Current source has fixes absent from the July release, including OpenAlex authentication changes. Install a reviewed checkout and record its commit. For reproducible comprehensive retrieval, direct APIs remain preferable. |
| **4** | **[K-Dense Scientific Agent Skills](https://github.com/K-Dense-AI/scientific-agent-skills)**, formerly `claude-scientific-skills`. **V:** ≈49.1k stars, MIT; `citation-management` and `literature-review` marked reviewed **30 September 2026**. | **Best established collection to select from.** Copy those two complete skill folders, including scripts/references, into `.claude/skills/`. They provide useful procedures and executable bibliography checks. **The citation validator is incomplete:** see the source-level finding below. Avoid installing all 177 skills. |
| **5** | **[WenyuChiou/academic-writing-skills](https://github.com/WenyuChiou/academic-writing-skills)**. **V:** ≈104 stars, MIT; [v1.3.12 dated 12 October 2026](https://github.com/WenyuChiou/academic-writing-skills/blob/main/CHANGELOG.md). | **Promising lightweight writing companion.** Emphasizes evidence, claim scope, manuscript consistency and journal overrides. Install through its documented marketplace commands below. Small adoption; procedural instructions and synthetic evaluations do not establish citation accuracy. |
| **6** | **[Scite MCP](https://scite.ai/blog/introducing-scite-mcp)**. **V:** launched **26 February 2026**; [September additions](https://scite.ai/blog/september-2026-release-notes), published 6 October, include citation evidence, eligible full text and citation reporting. | **Best optional citation-context check.** Inspect how important methods/results have been discussed or challenged. Connect the official MCP, inspect underlying citation statements, save DOI/context/locator. Labels such as “supporting” are not verdicts on an entire paper. Current documentation requires paid access or an active trial. |
| **7** | **[Undermind](https://www.undermind.ai/)** with its **[official MCP](https://www.undermind.ai/mcp)**. **V:** current documentation explicitly supports Claude Code; follows literature and citation trails. | **Best candidate for a narrowly targeted discovery trial.** Give it a precise research question and known seed papers. Capture returned identifiers and search output, then verify/import through Zotero. Provider benchmarks are not independent evidence of performance in palaeoecology. |
| **8** | **[Elicit](https://support.elicit.com/en/articles/14757543-getting-started-with-elicit-which-workflow-should-i-use)**. **V:** Research Agent consolidated older workflows on **30 September 2026**; [official MCP documentation](https://support.elicit.com/en/articles/14757404-use-elicit-via-mcp-server), dated **26 August**, specifies Pro/Scale/Enterprise access. | **Best optional extraction-table interface.** Define the extraction columns first; export references as RIS/BibTeX and tables/reports where supported. Check every consequential extracted value against the paper. Helpful if Chris wants a UI; unnecessary if a CSV and Claude already suffice. |

Two findings materially affect these recommendations:

- **V—citation verification is frequently overstated.** K-Dense’s [`validate_citations.py`](https://github.com/K-Dense-AI/scientific-agent-skills/blob/main/skills/citation-management/scripts/validate_citations.py) retrieves Crossref title/year/authors, but its DOI-validation loop does **not compare that metadata with the submitted bibliography entry**. A real DOI attached to the wrong title can pass. Use it for registration, formatting, duplicates and missing keys; add an identity check.
- **V—installation instructions can outrun releases.** Paper Search’s [July-release OpenAlex connector](https://github.com/openags/paper-search-mcp/blob/c8b642183bb725f0a7faec89e58b558df09079d1/paper_search_mcp/academic_platforms/openalex.py) lacks the API-key handling present in [current source](https://github.com/openags/paper-search-mcp/blob/main/paper_search_mcp/academic_platforms/openalex.py). Pinning an old release is reproducible, but may reproduce obsolete behaviour.

The longer annotated list is grouped below. Capabilities are **V**; suitability judgements are assessments.

**Search APIs and MCP connectors**

| Option | What it contributes; assessment |
|---|---|
| **[Crossref REST API](https://www.crossref.org/documentation/retrieve-metadata/rest-api)** | Publisher-deposited metadata, DOI lookup, references where supplied, updates and Retraction Watch information. **Essential identity-check source.** No MCP required: Claude can invoke a small R/Python script. Metadata existence does not establish claim support. |
| **[OpenAlex API](https://help.openalex.org/api/)** | Broad discovery, references/citing works, OA locations. Its [2026 pricing model](https://help.openalex.org/access/pricing/) provides $1/day of free usage with an account; this is no longer the old unlimited-looking keyless workflow. **Strong primary search backbone.** |
| **[Semantic Scholar API](https://www.semanticscholar.org/product/api)** | Paper lookup, citations, references and recommendations. Official documentation gives an introductory authenticated rate of **1 request/second**. **Useful complementary search and citation traversal**, with caching/backoff. |
| **[ckgerteis/semantic-scholar-mcp](https://github.com/ckgerteis/semantic-scholar-mcp)** | MIT, ≈2 stars; documented recorded-response tests from **4 September 2026** and query receipts. **Interesting traceability design**, but too little adoption to displace direct API access without testing. |
| **[DeyangLiu123/openalex-mcp](https://github.com/DeyangLiu123/openalex-mcp)** | MIT, ≈4 stars; quota visibility, citation traversal and guarded downloads. **Focused alternative**, though another small dependency to maintain. |
| **[blazickjp/arxiv-mcp-server](https://github.com/blazickjp/arxiv-mcp-server)** | Apache-2.0; current package manifests show v0.7.2, bounded full-text/original-LaTeX retrieval and citation tools. **Useful for methods/preprints**, secondary coverage for this paper. |
| **[CellCS/pubmed-mcp](https://github.com/CellCS/pubmed-mcp)** | MIT; NCBI E-utilities wrapper. **Low priority here**: PubMed complements ecological searches but should not define the search universe. Paper Search already includes it. |
| **[Unpaywall API](https://unpaywall.org/api)** | DOI-based OA discovery; requires an email. **Important change:** general search retired **18 September 2026**; DOI lookup remains. Use it to locate accessible copies, not as the main literature index. |
| **Google Scholar alternatives** | Prefer OpenAlex/Semantic Scholar for scripted retrieval; use Scholar manually for missed titles and older literature. [Google’s guidance](https://scholar.google.com/intl/en/scholar/help.html) disallows bulk access and asks automated clients to respect robots rules. Scraping wrappers introduce blocking and maintenance problems. |
| **R-native access** | [`openalexR`](https://docs.ropensci.org/openalexR/reference/oa_request.html) supports API keys and raw responses; [`RefManageR`](https://docs.ropensci.org/RefManageR/reference/index.html) reads BibTeX and retrieves DOI/Crossref metadata. **Especially good fit with the existing R repository.** These supply components, not a complete citation audit. |

**Research packages, skills and agent packs**

| Project | Popularity, maintenance and licence | Citation safeguards and fit |
|---|---|---|
| **[Imbad0202/academic-research-skills](https://github.com/Imbad0202/academic-research-skills)** | ≈52k stars; [September 2026 activity](https://github.com/Imbad0202/academic-research-skills/commits/main/); **CC BY-NC 4.0**. | Source locators, provenance and an **opt-in** claim audit. Substantial engineering, but a large research-to-publication workflow. **Defer for this finishing task.** Noncommercial terms need clarification for paid contracting; academic subject matter alone does not settle that question. |
| **[Galaxy-Dawn/claude-scholar](https://github.com/Galaxy-Dawn/claude-scholar)** | ≈5.8k stars, MIT; [27 August 2026 activity](https://github.com/Galaxy-Dawn/claude-scholar/commits/main/). | Zotero integration, literature-review agents, reproducibility rules; separate client branches. **Useful reference architecture**, but CS/AI orientation and configuration breadth exceed this paper’s needs. |
| **[K-Dense/claude-scientific-writer](https://github.com/K-Dense-AI/claude-scientific-writer)** | ≈2.5k stars, MIT; [18 August 2026 activity](https://github.com/K-Dense-AI/claude-scientific-writer/commits/main/). | Research-before-writing workflow with literature lookup; API/CLI/plugin. Requires additional services and document tooling. **Reasonable for a new writing environment; excessive for revising an established Word manuscript.** |
| **[FutureHouse PaperQA](https://github.com/Future-House/paper-qa)** | ≈9.4k stars, Apache-2.0; [release 12 August 2026](https://github.com/Future-House/paper-qa/releases). | Retrieval and cited answers over supplied documents. **Strong optional tool once a substantial PDF corpus exists.** Adds indexing/model configuration and cost; it does not establish search completeness. |
| **[GPT Researcher](https://github.com/assafelovic/gpt-researcher)** | ≈30.1k stars, Apache-2.0; [2026 commits](https://github.com/assafelovic/gpt-researcher/commits/master/). | Web/local research, references, MCP and agent workflows. **Good general research framework**, weaker fit than DOI-first tools for controlled scholarly bibliography updates. |
| **[VoltAgent subagent pack](https://github.com/VoltAgent/awesome-claude-code-subagents)** | ≈25.7k stars, MIT; [September 2026 activity](https://github.com/VoltAgent/awesome-claude-code-subagents/commits/main/). | Its [scientific-literature agent](https://github.com/VoltAgent/awesome-claude-code-subagents/blob/main/categories/10-research-analysis/scientific-literature-researcher.md) depends on BGPT. **An agent role is not a citation-verification system.** Prefer a short project-specific reviewer instruction. |
| **[Research Hound](https://github.com/AlveeeRahman/research-hound)** | ≈1 star, MIT, 20 commits observed; exact latest activity date unverified. | Runnable checks and source-ledger discipline; DOI resolution is narrower than bibliographic identity verification. **Worth inspecting, not adopting solely on README assurances.** |
| **[SFETNI manuscript-qc](https://github.com/SFETNI/Scientific-Writing-Skills-Claude-Code-Codex)** | Six commits observed; scripts MIT, skills/docs CC BY 4.0; exact latest date unverified. | Claim calibration, numerical registries and human acceptance gates. **Conceptually well matched to finishing this paper**, but immature and documentation contains repository-name inconsistencies. |
| **[Orchestra AI Research Skills](https://github.com/Orchestra-Research/AI-research-SKILLs)** | ≈13.6k stars, MIT; June 2026 update documented. | Mainly ML training, architecture and engineering workflows. **Low relevance:** “AI research” here largely means researching AI. |

My distinction between useful tools and hype: **retrieval code, exported records, failure reporting and inspectable evidence matter more than agent counts, stars, or claims of “publication-ready” prose.** Multiple agents repeating a citation do not independently verify it.

**Writing, Word, Google Docs and reproducible documents**

| Option | Assessment |
|---|---|
| **[Anthropic document skills](https://github.com/anthropics/skills)** | Official DOCX/PDF handling, including tracked-change workflows. Useful for a **copy** of the manuscript. Document skills have a [restrictive proprietary licence](https://github.com/anthropics/skills/blob/main/skills/docx/LICENSE.txt), unlike many other skills in the repository; do not assume they can be copied into Codex or redistributed. |
| **[Pandoc](https://pandoc.org/MANUAL.html) / [Quarto](https://quarto.org/docs/authoring/footnotes-and-citations)** | Excellent for versioned notes, evidence tables, R outputs and review memos. BibTeX/CSL produce formatted citations. Ordinary DOCX output is **not a reliable round trip preserving live Zotero citation fields**. Keep the current manuscript authoritative. |
| **[Google Workspace MCP](https://github.com/taylorwilsdon/google_workspace_mcp)** | MIT; Docs/Drive operations, CLI and read-only mode. Requires Google OAuth setup. **Optional retrieval convenience**, not a Zotero-aware citation editor. Downloading a dated manuscript copy is simpler for this task. |
| **[Office-Word-MCP-Server](https://github.com/GongRzhe/Office-Word-MCP-Server)** | **Archived 3 March 2026.** Do not choose it as the new long-term dependency for this manuscript. |
| **Plain Markdown/CSV plus git** | **My preferred interchange format** between Claude Code, Codex and the R pipeline. A shared evidence contract is easier to maintain than synchronizing several research platforms. |

**Other research services and reliable handoffs**

| Tool | Best use here; reliable route into Claude Code |
|---|---|
| **[Consensus](https://docs.consensus.app/docs/mcp)** | Quick supplementary discovery; official MCP with current Claude Code instructions. [Export CSV/RIS/BibTeX](https://help.consensus.app/en/articles/10008300-how-to-use-pro-messages), import references into Zotero, retain original output. Avoid interpreting a summary or “consensus” indicator as evidence strength for heterogeneous palaeoclimate methods. |
| **[Connected Papers](https://www.connectedpapers.com/pricing)** | Fast visual exploration from several seed papers; free tier currently allows five graphs/month. Its graphs represent **similarity**, not simply citation trees. Save seed IDs, graph/date and candidate DOIs. I did not verify an official MCP; manual capture is sufficient. |
| **[ResearchRabbit](https://learn.researchrabbit.ai/en/articles/13192798-how-to-share-papers-and-collections)** | Good collection-based discovery; exports BibTeX/RIS/CSV. Its [current Zotero documentation](https://learn.researchrabbit.ai/en/articles/12796541-using-the-zotero-importer-in-researchrabbit) describes **one-way import**, with two-way sync still forthcoming. Export additions back to Zotero and deduplicate. |
| **[Litmaps](https://docs.litmaps.com/en/articles/9092828-export-articles-from-litmaps)** | Citation exploration, monitoring, Zotero integration and BibTeX/RIS/CSV export. An [official MCP is now in beta](https://docs.litmaps.com/en/articles/17508256-use-litmaps-with-your-ai-assistant-mcp), including write operations. **Choose it or ResearchRabbit**, rather than maintaining both. |
| **[SciSpace](https://scispace.com/resources/scispace-literature-review-workspace/)** | PDF explanation and comparison tables; documented CSV export. Treat extracted summaries as provisional. I did not verify a supported official MCP; the browser-automation wrappers found add fragility. |
| **Elicit / Undermind / Scite** | Export or retain raw results, identifiers and source passages before synthesis. Elicit has [documented RIS/BibTeX exports](https://support.elicit.com/en/articles/14758189-export-your-data-from-elicit). For Undermind, I verified MCP access but not a current BibTeX export specification; DOI capture is the dependable fallback. |
| **[ASReview](https://asreview.nl/blog/why-asreview-does-not-make-recommendations/)** | Consider only if screening becomes large enough to justify a dedicated tool. Its February 2026 explanation emphasizes human screening decisions. Probably unnecessary for a focused introduction/discussion update. |

For this paper, I recommend the following workflow. These steps are **my proposed design**, informed by the verified capabilities above.

**1. Establish the manuscript and bibliography baseline.**

Download a dated DOCX copy from Drive; record the Drive document/version identifier and file hash. Inspect whether references are live Zotero fields, another manager’s fields, or plain text.

A Word file stored in Drive is not automatically a native Google Doc. Zotero warns that directly moving documents between Word, LibreOffice and Google Docs can break citations; use its [document-transfer procedure](https://www.zotero.org/support/kb/existing_citations_not_detected). On Linux, Google Docs plus Zotero Connector is an option if the team chooses that format. Otherwise keep Word authoritative and have the person using Word/Zotero insert approved citations.

Create a Zotero collection such as `NA-Holocene-RF`, with tags for `candidate`, `included`, `excluded`, `needs-fulltext`, and `verified`. Auto-export its bibliography; correct metadata in Zotero rather than editing the generated `.bib`.

**2. Install the small local stack.**

With Python and `uv` available, the verified Zotero release can be installed as:

```bash
uv tool install "zotero-mcp-server==0.14.1"
export ZOTERO_LOCAL=true

zotero-mcp install-skill --list-targets
zotero-mcp install-skill
zotero-cli --json search "REVEALS" --limit 5 --detail keys_only
```

Enable Zotero’s local API in Settings → Advanced. Inspect the installer’s detected destinations; it can modify agent instruction files. The [CLI documentation](https://github.com/54yyyu/zotero-mcp/blob/main/docs/cli.md) explains these behaviours.

For Paper Search, review a checkout, record its commit, then install that checkout and its matching skill:

```bash
git clone https://github.com/openags/paper-search-mcp.git tools/paper-search-mcp
mkdir -p literature .claude/skills/paper-search
git -C tools/paper-search-mcp rev-parse HEAD > literature/paper-search-commit.txt
uv tool install ./tools/paper-search-mcp
cp tools/paper-search-mcp/claude-code/SKILL.md \
   .claude/skills/paper-search/SKILL.md
```

Keep credentials outside git in `~/.config/paper-search-mcp/.env`:

```dotenv
PAPER_SEARCH_MCP_OPENALEX_API_KEY=your-key
PAPER_SEARCH_MCP_SEMANTIC_SCHOLAR_API_KEY=your-key
PAPER_SEARCH_MCP_UNPAYWALL_EMAIL=your-email
```

If MCP is preferred over the CLI skill, these installed binaries can be registered:

```bash
claude mcp add --env ZOTERO_LOCAL=true --transport stdio zotero -- zotero-mcp
claude mcp add --transport stdio paper-search -- paper-search-mcp
claude mcp list
```

This combines the projects’ documented launch commands with [Claude Code’s official MCP syntax](https://code.claude.com/docs/en/mcp). Use one access route per tool initially.

Optional writing plugin:

```text
/plugin marketplace add WenyuChiou/ai-research-skills
/plugin install academic-writing-skills@ai-research-skills
```

Optional hosted connectors—**choose only those needed**, then authenticate through `/mcp`:

```bash
claude mcp add --transport http scite https://api.scite.ai/mcp
claude mcp add --transport http undermind https://mcp.undermind.ai/mcp
claude mcp add --transport http elicit https://elicit.com/api/mcp
```

Endpoints are documented by [Scite](https://scite.ai/blog/claude-code-dissertation-writing), [Undermind](https://www.undermind.ai/mcp) and [Elicit](https://support.elicit.com/en/articles/14757404-use-elicit-via-mcp-server). These commands were documentation-checked, not executed here.

**3. Search by scientific question, retaining older methodological foundations.**

First recover the draft’s previous search cutoff. Update from that date through **14 October 2026**, with an overlapping interval; retain unrestricted searches for foundational methods.

| Search strand | Starting terms; adapt syntax to each database |
|---|---|
| Reconstruction | `REVEALS`, `pollen`, `Holocene`, `North America`, `LandCover6k`, land-cover uncertainty |
| Albedo calibration | `MODIS`, `blue-sky albedo`, diffuse fraction, snow masking, vegetation, beta regression, GAM |
| Forcing methods | `surface albedo radiative kernel`, `HadGEM3`, `CAM5`, `CACK`, TOA shortwave |
| Ice and attribution | Laurentide/Cordilleran retreat, palaeovegetation, ice-free area, land cover versus land use |
| Interpretation | Holocene biogeophysical effects, radiative versus effective radiative forcing, AR6 comparison |

Use OpenAlex and Semantic Scholar for discovery, Crossref for metadata, publisher/repository searches for full text, and backward/forward citation searches around seeds. If available, add institutional Web of Science/Scopus coverage.

A CLI pilot might be:

```bash
paper-search search "Holocene North America vegetation albedo" \
  -s openalex,semantic,crossref -n 50 \
  > literature/search-pilot.json
```

A capped ranked search is **not exhaustive**. For broader retrieval, use provider-specific filters and pagination; record errors and truncation rather than treating empty output as proof of no literature.

Useful verified seed/test records:

- **Dawson et al., 6 November 2025**, *Holocene land cover change in North America…*, DOI [`10.5194/cp-21-2031-2025`](https://cp.copernicus.org/articles/21/2031/2025/).
- **Hopcroft et al., 18 February 2026**, *Biogeophysical Impact of Land-Use Scenarios on Holocene Surface Temperatures*, DOI [`10.1029/2025GL118518`](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2025GL118518). **Andria is a coauthor**; use it to test retrieval and map the current manuscript’s distinct contribution.
- **Bright and O’Halloran, 9 September 2019**, [CACK v1.0](https://gmd.copernicus.org/articles/12/3975/2019/), DOI `10.5194/gmd-12-3975-2019`.

**4. Screen and extract into a small, explicit evidence table.**

Deduplicate by DOI, then normalized title/authors/year; link preprints to published versions. Claude proposes inclusion/exclusion with reasons. Chris checks exclusions that could remove an important method or contrary result.

Record:

```text
citekey, DOI, decision, reason, study_region, time_span,
land_cover_method, albedo_definition, snow_ice_treatment,
kernel, forcing_definition, reference_period, spatial_normalization,
estimate_units, uncertainty, supporting_passage, page_table_figure,
fulltext_status, reviewer
```

For this paper, the high-value checks are:

- Blue-sky versus black-/white-sky albedo and seasonal weighting.
- Modern calibration transferred to past vegetation/climate conditions.
- Separation of ice-sheet and vegetation contributions.
- Kernel units, sign, cloud conditions and baseline climatology.
- Regional mean versus globally normalized forcing.
- Pollen, spatial-model, calibration and kernel uncertainty.

Treat this as a **documented targeted update**, unless the team actually intends a systematic review.

**5. Verify each citation at three separate levels.**

| Level | Required check |
|---|---|
| **Identity** | DOI record matches title, authors, venue and publication year. Use Crossref; use DataCite/publisher records where appropriate. A missing Crossref record does not prove fabrication. |
| **Document** | Downloaded PDF is the intended work/version; record access URL, date and hash. Mark abstract-only evidence explicitly. |
| **Claim support** | Read the cited passage/table/figure. Confirm that it supports the exact claim, including qualifiers, units and geographical/time scope. |

A small R audit should save raw metadata responses and produce `verified`, `mismatch`, `unverified-network`, or `manual-no-DOI` statuses. Never automatically accept the first fuzzy title match. Check corrections/retractions against publisher records. Do not count agreement between aggregators as fully independent verification.

**Particular scientific concern:** AR6’s comparison is **1750–2019 effective radiative forcing**. A regional Holocene kernel estimate requires explicit treatment of reference period, area normalization and forcing definition before comparison. AR6’s [Chapter 7](https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-7/) establishes those definitions; whether the draft has harmonized them remains unverified.

**6. Draft from accepted evidence, then make a small PI handoff.**

Put a short project rule in `CLAUDE.md`, and provide the same evidence requirements to Codex:

```text
Cite only records in the verified bibliography.
Every substantive literature claim needs an evidence-table row and locator.
Distinguish full-text evidence from abstract-only evidence.
Never invent metadata, numerical results, quotations, or missing passages.
Mark unresolved evidence [SOURCE NEEDED].
Preserve units, uncertainty, reference periods, and scientific qualifiers.
Return proposed paragraph edits and citation changes for review.
```

Keep this alongside the R pipeline:

```text
literature/
  protocol.md
  searches.csv
  raw/
  screening.csv
  evidence.csv
  citation-audit.csv
  references.bib
  proposed-edits.md
  tool-versions.txt
```

Log query, provider, timestamp, filters, retrieved count, pagination/cap, failures and output filename. Record model/tool versions and accepted changes. Keep copyrighted PDFs, credentials and confidential drafts out of any public repository.

Give Andria **one page of findings, a few essential papers, and proposed paragraph changes**. Insert accepted citations using the manuscript’s reference manager. Use Quarto/Pandoc for the review memo or supplement; avoid regenerating the live Word manuscript merely to update its literature.

If GRL remains the target, its current rules require **≤12 publication units**, an abstract **under 150 words**, and a plain-language summary; references are excluded from the word count. Aim for a focused introduction/discussion update. [AGU submission checklist](https://www.agu.org/publications/authors/journals/submission-checklists), [length rules](https://www.agu.org/publications/authors/journals/text-graphics-requirements).

Finally, keep four risks explicit:

- **Fabrication:** retrieval reduces invented references; identity and passage checks remain necessary.
- **Access and licences:** use OA repositories, institutional access or author copies; disable optional Sci-Hub paths. Software licences do not grant rights to publisher PDFs.
- **Unpublished material:** local MCP execution is not local model inference. Send hosted search tools topic queries rather than the whole draft. Claude Code’s training treatment depends on account type and settings. [Anthropic data-use documentation](https://code.claude.com/docs/en/data-usage).
- **Publication transparency:** retain the assistance log so the paper can accurately disclose AI use under [AGU’s policy](https://www.agu.org/publications/authors/policies).