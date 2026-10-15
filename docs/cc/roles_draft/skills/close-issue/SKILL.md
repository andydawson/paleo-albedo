---
name: close-issue
description: Close out a finished paleo-RF migration issue - changelog, merge into the trunk, push, issue comment, question files, memory. Use when a branch is verified (PASS report committed) and Chris has approved the work.
---

# Close out an issue (paleo-RF)

Follow CONTRIBUTING.md. In order:

1. The branch has a committed PASS report covering its code (verify-change skill), and all
   tests pass (`tools/rscript.sh tests/test_*.R`, each exit status checked, never through a pipe).
2. Changelog entry (changelog-entry skill).
3. Merge into the issue's trunk (`feature/single-config` for migration issues) with
   `git merge --no-ff`, from a checkout no run is reading; a why-focused merge message.
   The merge check must pass; `--no-verify` only as a recorded exception Chris approved.
4. Update the changelog status row with the merge sha; commit.
5. Push the trunk and the branch to `origin`.
6. Comment on the GitHub issue (`gh issue comment`): what changed, effect on outputs, how
   verified, link to the report; close it if finished (`gh issue close`), or say what is
   left.
7. Question files: add any question for Andria that arose; remove answered ones, recording
   the answer.
8. Memory: update the workflow note (order, what is next); no status that belongs in files.
9. Repeat to Chris the key outputs and what is next; ask any decision via the question
   interface, then continue with the next issue.
