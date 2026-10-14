# Research tooling decisions: citation checking, claim checking, instruction tidying

**What this is.** The record of Chris's decisions of 2026-10-14 on tools for literature work
and for keeping Claude Code's instructions and memory tidy, with how each tool is installed
and run. Chris wants these reusable across projects if they work well, so everything is
installed at user level (not inside this repository) and this file says how to set it up
again elsewhere.

Evidence behind the decisions:
`2026-10-14_literature_review_tools_codex_gpt6astra.md` (the first literature-tools survey),
`2026-10-14_citation_checkers_and_memory_tools_codex_gpt6astra.md` (the follow-up on citation
and claim checkers and memory tidying), `2026-10-14_standing_roles_design_fable.md` (roles).

## Decisions

| # | Decision | Status |
|---|---|---|
| 1 | **Reference identity** (is each reference the paper we think it is?): adopt **RefChecker** (`academic-refchecker`, MIT) and **refaudit** (MIT), run side by side on the Better BibTeX export. Disagreements between them are reviewed by a person. | Decided |
| 2 | **Claim support** (does the cited paper say what we claim?): pilot a combination: **PaperTrace** (MIT, beta) finds the supporting passage; an **independent challenger** from a different model family (Astra, via Codex CLI) argues against every verdict in a fresh context; **Chris adjudicates** disagreements. | Decided: pilot first |
| 3 | **Pilot material** for decision 2: Chris's own paper, McWilliams et al. (2019), BMJ Open, machine learning for intensive-care discharge (open access). He knows its sources, so he can judge the verdicts; planted errors are added to measure what the checkers miss. | Decided |
| 4 | **Instruction and memory tidying**: adopt all three: Claude Code's built-in `/doctor prompt-audit` (run read-only: one user reported it editing without approval in auto mode); Anthropic's official `claude-md-management` plugin; our own approve-first skill `tidy-instructions-and-memory` (proposes, never deletes or edits without approval; also covers the auto-memory directory, which the built-in audit does not). | Decided |
| 5 | **Standing roles** (Fable's seven): install **locally, not tracked in git** (this repository has no `.claude/` directory today). Chris will be asked again shortly before anything is installed. | Leaning; ask again |
| 6 | Zotero MCP and paper-search MCP: the scope of `claude mcp add` (user or project) is still to be asked. Reuse across projects argues for user scope. | Open |

Rules that go with decisions 1 and 2 (from Astra's survey):
- "Not found", "API unavailable", "skipped" and "DOI resolves" are never treated as verified.
  RefChecker can downgrade a DOI that points to a different paper to a *warning* when the
  DOI resolves: warnings are reviewed, not just errors.
- Verdicts: SUPPORTED, PARTIAL, CONTRADICTED, NOT ESTABLISHED, UNVERIFIABLE; abstract-only
  access and retrieval failures are recorded as such. "No passage retrieved" is not evidence
  that a claim is false.
- Every claim-to-source pair is challenged, including those first judged SUPPORTED. The
  challenger sees the source and the claim before it sees the first verdict.
- The evidence table records, per atomic claim and source: claim id, manuscript version,
  verbatim claim; source key, identifier, identity verdict; access status, exact quote, PDF
  and printed page, section/figure/table; assessor verdict, challenger verdict, final
  verdict, proposed wording, human approval; model ids, prompt version, dates.
- Pilot success is measured as missed citations and wrong SUPPORTED verdicts on the planted
  errors, not as "a report was produced".

## Installation (user level, reusable)

| Tool | Where | How |
|---|---|---|
| RefChecker, refaudit | `~/.local/share/research-tools/citecheck-venv` | `uv venv` then `uv pip install 'academic-refchecker' refaudit`; versions pinned in `~/.local/share/research-tools/README.md` |
| PaperTrace | `~/.local/share/research-tools/PaperTrace` | `git clone https://github.com/defraction0/PaperTrace` at a recorded commit; `pip install -e '.[full]'` in its own venv |
| claude-md-management | Claude Code plugin, user scope | `claude plugin install claude-md-management@claude-plugins-official` |
| `/doctor prompt-audit` | built into Claude Code (2.1.283 or later) | Chris runs it interactively; accept no edits until reviewed |
| `tidy-instructions-and-memory` | `~/.claude/skills/tidy-instructions-and-memory/` | written here; user-level so every project has it |

Paths and versions actually installed are recorded below as they happen.

## Installed
