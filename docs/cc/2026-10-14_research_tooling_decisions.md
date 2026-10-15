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

- **RefChecker** (`academic-refchecker` 3.1.6, with its `webui` extra for the retraction
  check) and **refaudit** 0.4.9: in `~/.local/share/research-tools/citecheck-venv`.
  refaudit sends a contact email to Crossref and OpenAlex: Chris chose
  Chris.mcwilliams@bristol.ac.uk.
- **`combine_reference_checks.py`** in `~/.local/share/research-tools/`: one table from both
  tools and the retraction check (see the pilot below).
- **PaperTrace** (commit 4f1c3ac, 2026-09-27) in `~/.local/share/research-tools/PaperTrace/.venv`,
  and the **`claude-md-management`** plugin 1.0.0 (user scope): first refused by Claude
  Code's auto-mode safety check, then installed with Chris's explicit permission.
- **`tidy-instructions-and-memory`** skill: `~/.claude/skills/tidy-instructions-and-memory/SKILL.md`.
- **Identity test set**, reusable for any reference checker:
  `~/.local/share/research-tools/testsets/bmjopen-2019-identity/` (`pilot.bib`,
  `answer_key.tsv`, the Crossref record).

The install commands (for another machine):
```bash
cd ~/.local/share/research-tools && git clone https://github.com/defraction0/PaperTrace
cd PaperTrace && uv venv .venv --python 3.12 && VIRTUAL_ENV=$PWD/.venv uv pip install -e '.[full]'
claude plugin install claude-md-management@claude-plugins-official
```

## Pilot 1: reference identity, on McWilliams et al. (2019), 2026-10-14

**Test set.** The 49 references of the BMJ Open paper (doi:10.1136/bmjopen-2018-025925),
44 with DOIs, as BibTeX from each DOI's publisher record, the other 5 typed from the
reference list; plus 3 added entries. 11 errors planted, with an answer key:
two wrong DOIs (real DOIs of other papers), a wrong year (3 years), a year off by one, wrong
authors, an altered title, a wrong journal, two fabricated references (one with a DOI that
does not exist), a retracted paper; and one correct entry with its DOI removed.

**RefChecker result** (`academic-refchecker --paper pilot.bib`, then
`refchecker-webui check --paper pilot.bib --no-llm --check-retractions`):

| Planted | Caught? |
|---|---|
| Wrong DOI (MIMIC-III given the eICU paper's DOI) | Yes: error (authors and year differ); it does not say "wrong DOI" |
| Wrong DOI (research article given an editorial's DOI) | Yes: error (no matching authors) |
| Wrong authors | Yes: error |
| Wrong year (3 years) | Yes, but it matched the wrong paper (a textbook) and reported wrong authors |
| Two fabricated references | Yes: "could not verify"; the bad DOI also an error |
| Wrong journal | Warning only |
| Year off by one | Warning only |
| **Altered title** ("scoping review of discharge" to "systematic review of readmission") | **Missed**: verified, because it matched on the DOI and did not compare titles |
| Retracted paper | Missed by the main check; **caught** by the retraction check |
| Correct entry without DOI | Correctly passed (found the paper) |

False alarms: warnings on 5 of the 41 clean entries (online-first versus print year, journal
name forms). Acceptable as warnings.

**A real error found in passing.** Reference 30 of the published paper, as deposited with
Crossref by the publisher, carries the DOI of a dental-research paper ("Indian Hedgehog: a
mechanotransduction mediator in condylar cartilage", J Dent Res 2004). Either the paper's
reference or the publisher's DOI matching is wrong; Chris: not worth pursuing.

**refaudit result** (`refaudit pilot.bib --email ...`): caught both wrong DOIs (title
mismatch: it names the paper the DOI really belongs to), the wrong authors, the wrong year,
both fabricated references (one "unverified", one "DOI not registered"). It passed the
altered title, the wrong journal, the year off by one, and the retracted paper. It does
report a title similarity for the altered title (0.77) but does not act on it when the DOI
resolves, even with `--title-match 0.9`.

**Combined** (`combine_reference_checks.py`: PASS only if both tools pass, the DOI record's
title is at least 0.95 similar to ours, and the paper is not retracted): **all 10 planted
errors flagged**; the correct entry without a DOI passes; 5 of the 41 clean entries flagged
for a look (online-first versus print year, journal name forms). Every clean entry's title
similarity was 0.95 or more.

**What this means.** Neither tool alone is enough; both together, with the retraction check
and a strict title rule, caught everything planted. The 0.95 rule is set on one test set of
45 DOI entries: check it again on our own bibliography.

## Pilot 2: claim support, on McWilliams et al. (2019), 2026-10-14

**Test set** (Chris: open-access sources only). A two-page test manuscript of 28 claims
with bracketed citations to 10 sources: 20 sentences of the BMJ Open paper citing a source
whose full text is open (lightly shortened to stand alone, which changed the scope of two),
and 8 planted errors: four reversed conclusions, a wrong number and setting (MIMIC-III as
400,000 admissions in 200 hospitals), a wrong setting (a single UK hospital as 40 US
hospitals), causation for an association, and a claim pinned on the wrong source. The full
texts: 5 publisher or repository PDFs, 5 rendered from the Europe PMC open-access XML
(publishers refused automated PDF downloads). Claim ids were blinded (C01-C28) for the
challenger. Kept as a reusable test set in
`~/.local/share/research-tools/testsets/bmjopen-2019-claims/` (answer key, sources,
both tools' outputs, PaperTrace's evidence viewer).

**Assessor:** PaperTrace (commit 4f1c3ac), `--model claude-opus-5-5`, sources provided,
contact email Chris's Bristol address (Unpaywall). **Challenger:** Astra (Codex CLI,
`gpt-6-astra`, reasoning `max`), blind to PaperTrace's verdicts and to which claims were
planted, given the claims and the source texts with the adversarial prompt above.

**Result.**
- **Planted errors: both caught all 8, each with the expected verdict** (6 contradicted,
  1 partial for the causation claim, 1 not established for the wrong source).
- **Real claims:** PaperTrace 8 supported, 10 partial, 2 not addressed; Astra 6 supported,
  13 partial, 1 unverifiable. They agree on 11 of 20.
- **The 9 disagreements** are judgement calls, not misses: whether "recently" or "our
  previous work" needs support from the source; whether a methods reference supports a
  sentence about the citing paper's own analysis; one-instance versus trend. Astra marked
  one "unverifiable" because the source supplied was the arXiv preprint, not the cited
  journal version: a real identity point. Listed for Chris to adjudicate
  (`disagreements.md` in the test set).
- PaperTrace's evidence crops (page images with the matched text boxed) make checking a
  verdict fast; Astra's table gives a suggested correction for each.

**What this means.** On this small set, the combination works as designed: neither checker
passed a planted error, and the disagreements point at the claims a person should look at.
Limits: one paper, 28 claims, sources partly rendered from XML, and the "real" claims were
shortened by me. PaperTrace reads only bracketed numeric citations; a Word manuscript with
author-year citations needs a numbered PDF export first.

**Chris's rules for the judgement calls (2026-10-15), to go into the claim-checking
instructions:**
- Framing the source cannot vouch for ("recently", "in our previous work") is **flagged as
  partly supported**, with the suggested wording; Chris decides whether to change it.
- A **methods citation** for the citing paper's own analysis ("performance was
  characterised by ROC and precision-recall curves [Davis & Goadrich]") is **supported**
  when the source describes the method.
- **One example cited for a general statement** ("datasets are increasingly available
  [one database paper]") is **partly supported**: suggest "e.g." or a review citation.
Against these rules, of the 9 disagreements: Astra matches Chris on the 4 framing claims
(C03, C04, C05, C25); PaperTrace on the one-example claim (C22); neither on the methods
citation (C15: PaperTrace "not addressed", Astra "partly"; Chris "supported"); the other 3
(C13, C26, C27) are not covered by the rules and are left for the full run.
