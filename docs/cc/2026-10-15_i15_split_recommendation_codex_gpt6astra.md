# I15 readability cleanup: one issue or several? (Astra)

**What this is.** Chris asked whether the ~55-item cleanup should stay one issue; he asked Astra (2026-10-15). Astra was given the refreshed inventory (`2026-10-15_i15_cleanup_inventory_refresh.md`; row numbers below refer to it), the three options offered, run costs and ordering constraints. Not yet approved by Chris.

**Who wrote it.** OpenAI Codex CLI, `gpt-6-astra`, reasoning effort `high`, 2026-10-15.

---

**Choose option 1, with script-sized branches inside the third issue.** Keep #18 for script cleanup; open two linked issues for comments and helpers. That gives three work issues without another tracking issue.

An issue groups work; a branch sets the review and merge size. Option 2 leaves completion and exceptions buried in one large issue. Option 3 leaves Andria with one large merge despite the atomic commits.

**Use this split.** Numbers below are inventory rows, not proposed commit numbers.

| Issue | Included work |
|---|---|
| **A — Comments and indentation** | #8, #34/#72, #45, remaining #53/#69, comment part of #56, #58, #70, #54. Include section labels from #59, but no write changes. Preserve open questions, `# CM:` notes and the explanation at 3:217–220. |
| **B — Helper names** | #35, #37, #38 including `R/preparation.R`, #39, #40–42, and #44 **in its existing file**. |
| **C — Script cleanup, existing #18** | The script batches below. Each gets a separate branch and merge, with atomic commits inside it. |

| C batch | Inventory rows |
|---|---|
| Script 1 | #2–3; hold #2 for I16c |
| Script 2 | #4–5 and code part of #56 at 2:62; hold #4 for I16c |
| Script 3 | #6–7 |
| Script 4 | #9 and its part of #71, subject to the limited-check decision below |
| Script 5 | #12–16, seed split from #61, its part of #71 |
| Script 6 | #17–18, seed split from #61, its part of #71 |
| Script 7 | #19–22 and explicit join keys from #56 |
| Script 7a | #23–25, #27, #66, code part of #56, its part of #71 |
| Script 8 | #28 and its part of #71 |
| Script 9 | #29–33, #57 and its part of #71 |

Fold duplicates into those entries: #55’s list part → #12; #63 → #18/#56/#61; #64 → #57; #65 → #9/#12/#16, with its save-once part excluded. #74 is the verification method, not another task.

Close completed/obsolete rows #1, #10–11, #26, #36 and #43. Leave retained rows #46–51 alone. Do not add the optional `selected_fit` rename or the remaining tiny `run_manifest.R` names.

**Trim these items from the cleanup:**

- **#67, moving `make_grid()`: drop.** Moving it makes the function harder to find for an uncertain gain. Rename its locals in place under #44.
- **#60, rearranging calculation, plots and writes: drop.** It adds ordering risk and changes when files appear if a later step fails.
- **#59, saving once: separate follow-up only if Chris wants it.** It changes manifest behaviour and exceeds “no output changes.”
- **#62, tool reporting: separate tooling follow-up.** Better messages are useful, but deliberately change printed output.
- **#73, source changes: leave out.** Removing explicit sources makes dependencies less visible and can alter execution. Consider an explicit registry source separately if it fixes a demonstrated dependency problem.
- **#55’s `reformulate()` part: close as obsolete.**
- **#68: remove from this inventory.** There is no join at the claimed site. Chris can identify an intended join for a separate assertion change.

**Order the work this way:**

1. Start from `chris-dev` after I16b merges. Recheck the inventory against that merged tree.
2. Merge A, then B. Reconcile Andria’s edits before indentation; show both ordinary and `git diff -w` views.
3. Merge cheap C batches first: 8, 9, 3, then available work in 1 and 2. Follow with 5, 6 and the agreed script-4 batch.
4. Build all approved changes for 7 and 7a before their expensive checks. Run each once per final batch, then run 8 and 9 on the resulting outputs.
5. Finish with I13b’s full integration runs.

If I16c is ready, merge it before the script-1 and script-2 batches. Otherwise, complete their unrelated work and leave only #2 and #4 pending. After I16c merges, make those renames against its final code and rerun the affected checks. **Merge trunk changes into feature branches; never rebase Andria’s edits.**

Each issue gets its own changelog entry. Extend #18’s entry as its script batches land.

**Checks must match the claim.**

| Work | Required evidence |
|---|---|
| A | Identical parsed/deparsed expressions for every changed R file. Read the comments for factual accuracy too; parsing cannot establish units or scientific reasons. |
| B | The listed affected unit tests. For #39, compare old/new `prepare_boundaries()` results directly. For #44, compare old/new `make_grid()` results directly and script-3 figures. Unit tests alone do not establish pipeline-wide equivalence. |
| C | Each script’s listed anchors **and unanchored outputs**, compared with the old code. Include figures, PDFs, selected fits and the coarse table where applicable. Use only the existing PDF date treatment. |

Use `configs/legacy_config.R`, seed 1, frozen inputs and separate run directories. For script 1, apply the same diagnostics override to both versions. Run 7 and 7a concurrently only with independent frozen inputs and sufficient memory.

For **every merge**, use `tools/verify_change.R` in a separate worktree and commit its spec and PASS report under `docs/cc/verification/`. The report must cover the code actually being merged after reconciliation. If reconciliation changes executable code, rerun the affected checks. Keep atomic commits, but verify the completed script batch once.

Two coverage limits need explicit treatment:

- **Script 1’s cache-miss block:** an ordinary run does not test #2. Exercise it with a suitable local fixture if possible; otherwise leave that rename out rather than imply the anchor run covers it.
- **Script 4:** recommend the one-month legacy run plus `check_calibration_month.R`. Chris must accept that this is partial coverage pending I13b. Parse plus searching for unused names is not proof of unchanged execution. If that limited coverage is unacceptable, omit these low-value deletions.

Preserve name-derived outputs: model IDs, kernel keys, saved facet values and the saved `month` factor. For #23, preserve the old histogram title **and axis label** explicitly, or retain that binding. For #57, pass the filtered table and the limits current at each call.

**Keep decisions out of the critical path.** Leave #52 untouched until Andria answers; track it separately so her decision does not hold up cleanup.

Ask Chris once, before the expensive batches, about the newly found items. Recommend accepting genuinely dead local definitions, the trailing commented vector and the outdated REML question correction. Keep unused input reads: removing them changes manifests and stale-input checks. Treat the unprinted plot and its supporting objects as candidates requiring inspection and output comparison, not automatically dead code. Unanswered additions stay out of scope.