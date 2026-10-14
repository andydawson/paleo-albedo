# Review of how we work on this project

**What this is.** Chris asked (2026-10-14) whether we are missing tricks: could we be more
agentic, use roles, be more token-efficient. A review of our process against current
Claude Code documentation (web-searched), with ten improvements for approval. Its doubt
about the exit-code bug was checked afterwards: `bash -c 'false; echo "$(date) exit=$?"'`
prints exit=0, while capturing the status first prints exit=1, so the diagnosis stands.

**Who wrote it.** OpenAI Codex CLI 0.155.1, model `gpt-6-astra`, reasoning effort `high`,
with live web search, 2026-10-14, from a description of our process written by the Claude
session, AGENTS.md, CONTRIBUTING.md, the memory files, the issue list and the change log.

---

**Yes—there are worthwhile improvements. You already work agentically; the biggest gains are more reliable verification, less repeated context, and fewer decisions reaching Chris. Adding more agents indiscriminately would increase the overhead.**

This assessment uses your supplied records and current official documentation. Recommendations and expected benefits are my judgment, not measured savings. I could not inspect the workstation: local command execution failed at sandbox startup. Confirm installed CLI versions before adopting newer features.

**What works**

Keep the regression contract, real pipeline runs, frozen inputs, explicit scientific questions, granular commits, and independent review. These provide stronger evidence than model agreement. Giving the agent executable acceptance criteria is also Anthropic’s central recommendation for effective autonomous work. [Claude Code best practices](https://code.claude.com/docs/en/best-practices)

The decision to abandon unnecessary extraction was sensible. Shared arithmetic deserves a function; moving readable script flow into layers merely to satisfy an architectural pattern does not advance the paper.

**What does not**

- The review process sometimes spends substantial effort establishing agreement before establishing whether a change is worth doing.
- Verification infrastructure has itself become a source of uncertainty and reruns.
- Instructions are duplicated and contradictory. The migration memory says both “wait for approval” and “carry straight on”; older memories prescribe obsolete branching, execution and script-copying procedures.
- The changelog’s opening issue order contradicts its later entries and the supplied issue list. More documentation has not produced one reliable account of current state.
- Large Astra prompts and repeated reconciliation are plausible major token costs, but prompt size alone cannot establish billed waste: caching and reasoning/output tokens matter.
- Byte equality establishes preservation of the baseline, **not scientific correctness**. I16’s new methods need separate validation.

**Ten approvals, in priority order**

Each item can be accepted independently. Costs below describe setup and maintenance burden, not duration.

1. **Make the verification driver a tested, checked-in tool.**  
   **Benefit:** high—prevents invalid evidence and avoidable expensive reruns. **Cost:** moderate.

   Replace scratchpad drivers with one plain-R driver under `tools/`, invoked through a `verify-change` skill. It should record the code snapshot, config, input hashes, seed/RNG, environment and thread settings; execute non-interactively; and require **process success, completed manifests, expected newly produced outputs, anchor comparisons and unchanged frozen inputs**.

   Test the driver against deliberately failing, missing-output and incomplete-manifest fixtures before trusting it with script 7. Test any interactive compatibility separately with an explicit termination path.

   Capture process status immediately. However, reproduce the reported `echo "$(date) exit=$?"` failure using the exact shell and surrounding driver before preserving “date overwrites the status” as its cause; the supplied excerpt alone does not establish that diagnosis.

2. **Run expensive verification from an explicit, immutable code snapshot.**  
   **Benefit:** high—eliminates the demonstrated branch-switch hazard. **Cost:** moderate disk and run-management overhead.

   Create a dedicated worktree at the **approved candidate commit**, give it its own output directory and fixed config, and record its path and commit in the run manifest. Permit no edits, checkout or cleanup there until completion. Continue implementation in a different worktree.

   Do not assume Claude’s `isolation: worktree` selects the correct base: current documentation says it defaults to the default branch, rather than the parent session’s `HEAD`. Verify the base explicitly. [Subagent worktree documentation](https://code.claude.com/docs/en/sub-agents)

   Apply the 24-core ceiling across **all** concurrent jobs, including nested BLAS workers. Budget RAM too. Run dependent stages sequentially unless each has independent, frozen inputs.

3. **Replace consensus-until-agreement with review proportional to risk.**  
   **Benefit:** high—less review cost and earlier delivery. **Cost:** small policy change.

   | Change | Proposed review |
   |---|---|
   | Documentation or local naming | Implementer checks; inexpensive independent check where useful |
   | Paths, manifests, config plumbing | One independent reviewer plus targeted executable checks |
   | Arithmetic, stochastic behaviour, scientific options | Independent Fable and Astra reviews, followed by evidence-based adjudication |
   | Final integration | Preserve the already-required final Astra review and integration gate |

   For dual review, give each reviewer the same code and requirements **before showing the other’s conclusions**. Require findings with file/line, consequence, evidence and proposed verification. Allow one reconciliation pass; unresolved methodological questions go to Chris/Andria as questions, not manufactured consensus.

   Before detailed design, require one sentence explaining the benefit to the paper or Andria’s use of the code. Reject speculative generality early.

4. **Batch questions into one bounded approval per issue.**  
   **Benefit:** high—reduces Chris’s coordination burden. **Cost:** small instruction cleanup.

   Use one `AskUserQuestion` round containing the outcome, recommendation, unresolved choices, acceptance checks and stop conditions. Once approved, execute implementation, verification and close-out under the standing protocol without asking again about routine details.

   Reopen approval for changed scientific behaviour, added scope or materially different verification—not ordinary implementation choices.

   Resolve the contradictory memory explicitly: **“Continue autonomously within approved scope; wait for answers on dependent work; progress independent authorized work meanwhile.”** “Never stop just to report” does not authorize implementing an unapproved issue. Future batching of multiple issues requires Chris’s explicit agreement.

5. **Restore Astra’s file access, then reduce review packets.**  
   **Benefit:** potentially high—less repeated input and better investigative review. **Cost:** moderate troubleshooting; savings need measurement.

   Diagnose the installed Codex version, exact sandbox failure, `bwrap` availability, user namespaces and applicable AppArmor/container restrictions. Current OpenAI documentation identifies these Linux prerequisites; use a supported host or narrowly scoped configuration repair rather than routinely bypassing isolation. [Codex sandbox documentation](https://developers.openai.com/codex/concepts/sandboxing)

   With file access, supply: task, base/head commits, changed-file list, regression contract and specific review questions. Let Astra retrieve dependencies.

   Without access, send a reproducible packet containing the diff, necessary surrounding code/interfaces, tests and relevant decisions; allow requests for missing context. A standing context **filename alone is useless to a reader without file access**.

   Resume a specific Codex session for follow-up review; this avoids manual retransmission but does not make context free. Stable prefixes can benefit caching; confirm actual cached-token usage. [Codex resume](https://developers.openai.com/codex/noninteractive), [OpenAI prompt caching](https://developers.openai.com/api/docs/guides/prompt-caching)

6. **Consolidate instructions and use smaller, issue-focused sessions.**  
   **Benefit:** high—less contradiction and context recovery. **Cost:** moderate initial pruning.

   Use this division:

   | Location | Responsibility |
   |---|---|
   | `~/.claude/CLAUDE.md` | Chris’s genuinely cross-project preferences |
   | `AGENTS.md` | Short project rules, critical hazards and pointers |
   | `CONTRIBUTING.md` | Authoritative version-control protocol, including migration exception |
   | One living file under `docs/cc/` | Current issue, approved scope, active runs, next action and blockers |
   | Existing decision records | Explicit decisions with source/date; proposed and undecided states distinguished |
   | Memory | Durable lessons and pointers, not copied project status |

   Current Claude Code can load `AGENTS.md`, but behaviour depends on version/settings. A minimal `CLAUDE.md` containing `@AGENTS.md` is a documented compatibility option. Imports still consume context. The memory index is loaded initially; detailed memory files are retrieved separately, so directory size is not automatically per-turn token cost. [Memory documentation](https://code.claude.com/docs/en/memory)

   Correct the obsolete `CODE_MAP.md`, direct-Rscript environment advice, branch identity claims and retired script-copying instructions. Replace blanket `.rundirs` deletion reminders with an explicit reviewed inventory.

   Start fresh at issue boundaries after saving a compact handoff; retain session IDs and ownership for background jobs. Avoid clearing context midway through tightly related debugging. [Session practices](https://code.claude.com/docs/en/best-practices)

7. **Define roles, but retain one implementation owner.**  
   **Benefit:** medium—repeatable delegation and cleaner context. **Cost:** small definitions plus ongoing agent usage.

   Suggested `.claude/agents/*.md` contracts:

   | Role | Responsibility and limits |
   |---|---|
   | Implementer | Main session remains owner; changes approved scope and integrates results |
   | Verifier | Runs the approved driver in the designated worktree; reports evidence; never repairs production code |
   | Reviewer | Reads code independently; reports defects and scientific/readability risks; no edits |
   | Writer | Drafts changelog and issue text from verified evidence; cannot invent decisions or publish |

   Define each with `name`, `description`, explicit `tools` and an available `model`. Give a static reviewer `Read, Grep, Glob`; unrestricted `Bash` is not read-only merely because its prompt says so. Restrict verifier commands separately.

   Pass essential constraints explicitly rather than assuming the subagent inherits the parent’s conversation. These mechanisms are documented; the role boundaries above are my recommendation. [Custom subagents](https://code.claude.com/docs/en/sub-agents)

8. **Create three small skills and reuse one evidence record.**  
   **Benefit:** medium—fewer procedural omissions and duplicate narratives. **Cost:** small to moderate.

   Add:

   - `.claude/skills/verify-change/SKILL.md`: selects approved checks and invokes the tested driver.
   - `.claude/skills/close-issue/SKILL.md`: checks branch, evidence, merge/push and issue status against the existing protocol.
   - `.claude/skills/changelog-entry/SKILL.md`: explains behaviour, rationale, scientific effect and verification for Andria.

   Keep algorithms in R tools and procedural instructions in skills. Skills load their full instructions when invoked; descriptions generally remain discoverable. `disable-model-invocation: true` makes a skill manually invoked, so use it only where that extra intervention is wanted. [Skills documentation](https://code.claude.com/docs/en/skills)

   Preserve Chris’s comprehensive commits and required migration changelog. Derive them from the same verified issue summary; make issue comments link to evidence instead of repeating the entire history. Keep unanswered PI questions in the existing two files.

9. **Repair hooks and introduce cheap, deterministic gates.**  
   **Benefit:** medium to high—fewer interruptions and preventable mistakes. **Cost:** moderate; hooks need meaningful tests.

   Scope the destructive-action `PreToolUse` hook to executable tool calls and inspect structured `tool_name`/`tool_input`. Distinguish quoted text, heredocs and actual execution; do not simply exempt every command containing quotes. Preserve Chris’s confirmation requirement and test both false positives and genuine destructive cases. [Hooks reference](https://code.claude.com/docs/en/hooks)

   Use a Git pre-commit check for staged R syntax and relevant cheap tests, with Claude hooks providing early feedback. Check the staged contents, not merely a different working-tree version. Never launch the long pipeline from every commit or `Stop` event.

   Before merging, require verification evidence matching the candidate’s relevant code/config/input state. Missing or stale evidence should prevent a success claim.

10. **Measure efficiency and apply a verification budget.**  
    **Benefit:** medium initially; establishes which optimizations actually work. **Cost:** small recurring measurement.

    For the next few issues, record input/cached/output tokens, model, review rounds, Chris interruptions, invalidated runs and defects caught. Current Claude docs use `/usage` for token/cache reporting; use `/context` to inspect context. Subscription usage and API dollar estimates are different measures. [Cost documentation](https://code.claude.com/docs/en/costs)

    Trial a cheaper available model for inventory, bounded searches and evidence-based drafting; retain stronger models for scientific reasoning and difficult changes. Subagents protect the main context but can increase total tokens. Maximum reasoning should answer a difficult question, not be a universal default.

    Run cheap checks first, then affected-stage verification. Reuse expensive evidence only when relevant inputs/code/config/environment are unchanged and the regression contract permits it. Preserve the final calibration/integration gate. Validate I16 alternatives with hand-checkable cases and scientific invariants; legacy anchors apply to legacy behaviour.

**Parallelism and workflows**

Use a verifier and a read-only reviewer alongside the implementer where their work is independent. Keep dependent migration issues sequential. Background completion notifications are appropriate; repeated model polling is wasteful.

A scripted workflow could help the final audit by assigning bounded checks—paths, manifests, arithmetic and readability—and consolidating findings. It is excessive for ordinary issue close-out. Claude’s dynamic workflows explicitly target repeatable orchestration across many agents and require opt-in; agent teams remain experimental. [Workflows](https://code.claude.com/docs/en/workflows), [Agent teams](https://code.claude.com/docs/en/agent-teams)

**Stop immediately:** treating model consensus as proof; promoting doubts into decisions; writing new ad hoc verification drivers; editing any checkout used by a live run; and treating historical prose as current operational state.

Changes to required approvals, review coverage or regression checks remain proposals for Chris to approve. The most valuable first approvals are **1–4**: trustworthy verification, immutable runs, proportionate review and bounded autonomy.