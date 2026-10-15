---
name: changelog-entry
description: Write the changelog entry for one finished paleo-RF migration issue (docs/cc/changelog_single_config_migration.md), for Andria, from verified evidence only. Use when an issue's work is verified and about to merge.
---

# Changelog entry (paleo-RF single-config migration)

Readers: Andria, a scientist who did not watch the work, and coauthors later. Plain words;
no internal question ids without saying what they are; no time estimates (measured
runtimes are fine).

From the evidence only (diff, commit messages, verification report, review files): never
record Chris's doubts or tentative answers as decisions; only explicit choices.

1. **Status table** at the top: one row `| <id> <short title> | #<issue> | merged <date> | <merge sha> |`
   (or "verified <date>; merge pending" until merged).
2. **Section** `## <id>: <what it does> (#<issue>), merged <date>` with:
   - **Purpose.** Why, in two or three sentences, with who decided what and when.
   - **Changed.** One bullet per logical change: file, what, why.
   - **Outputs changed.** "None", or what changed and by how much (a small table where it
     helps), separating each cause.
   - **Verification.** What was run, from which configuration, against which anchor; the
     report file; byte-identical / within tolerance / measured; runtimes.
   - Anything unexpected found, and where it was recorded (question files, reviews).
   - Any slip in the process, honestly, with its consequence.
3. Commit with a why-focused message.
