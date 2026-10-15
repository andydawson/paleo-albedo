---
name: astra-review
description: Run Astra (OpenAI Codex, gpt-6-astra) for a design or adversarial review, a literature search, or an independent claim challenge in paleo-RF. Use whenever AGENTS.md's review table or a role calls for an Astra review, or Chris asks for Astra.
---

# Astra review (paleo-RF)

Astra is OpenAI Codex CLI with the model `gpt-6-astra`. Always:

- **Model and effort:** `-m gpt-6-astra -c 'model_reasoning_effort="max"'` for reviews,
  scientific questions and claim challenges. Use `"high"` only for routine surveys when Chris
  asks for high effort; never lower. Record the model and effort in the saved report.
- **Sandbox:** `--sandbox read-only`, run from the repository root, so Astra reads the files it
  needs itself (the AppArmor profile for Codex's bubblewrap makes this work since 2026-10-15).
  Name the files to read in the prompt; do not paste whole files unless the sandbox fails.
- **Web search:** add `--search` (before `exec`) for literature, data products and anything
  that needs current information.
- **Never send:** the unpublished manuscript draft, credentials, or copyrighted full texts.
  Describe the draft's methods instead; quote papers briefly.
- **Command:**
  ```bash
  codex [--search] exec -m gpt-6-astra -c 'model_reasoning_effort="max"' --sandbox read-only \
    --skip-git-repo-check -o <scratchpad>/<task>/result.md - < <scratchpad>/<task>/prompt.md
  ```
  Run it in the background; wait for completion; never poll in a loop.
- **Prompt:** the question in Chris's words; context; the files to read; what to produce
  (ranked answer, verified versus inferred, file:line or URL for every claim, decisions to put
  to Chris and Andria). For dual review (AGENTS.md), give Astra the same code and requirements
  as Fable before either sees the other's view.
- **Afterwards:** save the result under `docs/cc/<date>_<topic>_codex_gpt6astra.md` with a
  header saying what it is, who asked, model and effort; check every reference Astra gives
  with the identity checkers (RefChecker + refaudit + combine_reference_checks.py) and any
  quoted passage in the source itself before relaying it; repeat the key findings to Chris in
  plain words; questions for Chris through the question interface.
