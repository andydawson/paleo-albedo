# Session handoff, 2026-10-15

**Superseded:** Chris decided on 2026-10-15 to keep long sessions, so this handoff was not used.

**What this was.** The state of the work when the long session of 2026-10-08 to 2026-10-15 was to be
closed, so a fresh session can continue (Chris agreed to try shorter sessions; this is the
first switch). Read this, AGENTS.md, CONTRIBUTING.md and the memory index first.

## Where things stand
- **Merged into feature/single-config:** I16a (kernels, endpoint ice method, legacy config),
  I16b (ML comparison, REML prediction fit, ice caps out of calibration, fraction check),
  the verification tool and merge check (tools/verify_change.R, tools/check_merge_evidence.R,
  installed as .git/hooks/pre-merge-commit), I15a (#21, closed), I15b (#22, closed), and
  I15c (#18) scripts 8, 9, 3, 1, 2, 5, 6. The changelog has a section for each.
- **Not yet merged:** I15c scripts 7 and 7a, branch i15c-scripts7 (worktree
  /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/wt-i15c7), verification running (below).
- **Next:** I16c land support (the MOD44W mask has arrived: data/mod44w/2015/, provenance in
  docs/cc/supplementary_data_provenance.md); plan in docs/cc/2026-10-14_i16_plan_consensus.md.
  Then the two I15c renames that wait for I16c (inventory rows 2 and 4, scripts 1 and 2).
  Then I13b (#16), the final gate.

## Jobs still running when the session closed, and what to do with each
Their outputs are files; nothing depends on the old session.
1. **Scripts 7 and 7a verification**, old code and new code, in two lanes:
   logs /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/i15/verify7_before.log and verify7_after.log; reports
   docs/cc/verification/2026-10-15_i15c-scripts7-{before,after}_report.md (written into the
   main checkout, untracked). Specs: /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/spec_i15c7_{before,after}.R. When both PASS: the
   tail anchor must MATCH (check log in each run folder under
   ../paleo-RF-verify/<sha>/.rundirs/i15c-scripts7-<v>/), and every figure and Rplots.pdf of
   the two runs must be identical (PDFs apart from CreationDate/ModDate; the earlier scripts'
   loops show how). Then move both reports and specs into the i15c-scripts7 branch, add the
   changelog section "Scripts 7 and 7a" (changes are in its two commits, 51e638d and
   7dc79a1), merge into feature from a worktree (the hook checks it), and comment on #18:
   I15c done except rows 2 and 4, waiting for I16c.
2. **Astra, maximum effort, on snow and on the water masking** (Chris's question: snow may be
   the paper's weakest point, since past snow is assumed modern; middle-ground options;
   literature since about 2019; and the water masking in the light of Oleson et al. 2003's
   methods). Prompt /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/astra_snow/prompt.md; result /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/astra_snow/result.md. Save it as
   docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md with a header; check every
   reference with the identity checkers (~/.local/share/research-tools/, README there)
   before relaying; give Chris the findings in plain words and the decisions through the
   question interface; add questions for Andria to the question files and to the Word
   write-up's lists (writing/Draft manuscript (spatially complete) - CM methods
   comments.docx; python-docx in /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/venv).
3. **Fable, maximum effort, on how to define reviews consistently for Fable or Astra**
   (Chris: a role that calls Astra through Codex, or a skill that launches Astra reliably,
   so change reviews and claim checks behave the same with either model). Report
   /tmp/claude-1000/-home-expert-Documents-contracting-Andria-paleo-RF/912627b7-1f54-4406-9a56-dc38e8ab3d9e/scratchpad/fable_roles2/report.md. Present the options to Chris; change nothing until he
   decides (see the memory note "no preference is not yes").

## Waiting on Chris
docs/cc/waiting_on_chris.md (keep it current; send it to him with SendUserFile when it
changes).

## Housekeeping
- The tool's main-checkout copy may lag: run it as
  `tools/rscript.sh <feature worktree>/tools/verify_change.R <spec>` from the main
  checkout, or bring the main checkout onto feature/single-config first.
- Verification worktrees in ../paleo-RF-verify/ can be removed with `git worktree remove`
  once their reports are committed; the scratch worktrees of the old session likewise.
