# Review of the partial-ice headline switch: Opus, standing in for Fable

_2026-10-19. Independent review of branch `headline-partial-ice` at 66072d2 (base feature/single-config 19c7ff5), in the change-reviewer role. One reviewer, as Chris chose for this change (no arithmetic changed; only which total is reported and a settings check). Opus stood in for Fable, whose usage limit was reached. Outcome: Chris approved merging with the records added and the comment nits fixed (commit 34a2365), and chose a figure subtitle, not a table column, to name the ice method (commit 10d27c7); verified again (`2026-10-19_headline-partial-ice-2`, `-legacy-2`, both PASS). The questions file was updated on chris-dev after the merge. The tracked `output/forcing/forcing_by_period.csv` question is left open._

**Verdict.** Ready to merge once the records are added. I found nothing wrong in the code, the tests or the verification. What is missing is the records: there is no changelog entry, and that entry needs to warn that script 9 will now stop in existing run folders. The questions file will also say the wrong thing about the configured headline once this merges.

**What was checked.** Branch `headline-partial-ice`, base 19c7ff5, head 66072d2. It makes partial ice (`veg_ice_parts`, the area-weighted mix of ice and vegetation albedo) the configured headline forcing, and only the four vegetation-plus-ice totals are now accepted for that setting.

**Findings** (worst first)

| severity | file:line | what is wrong | consequence | evidence | check |
|---|---|---|---|---|---|
| should-fix | `docs/cc/changelog_single_config_migration.md` (no entry for this change) | The migration changelog has no entry for the headline switch. Its last line (~1203) only says the totals-only rule was "done with the switch to partial ice". | The coauthor change log does not record that the headline numbers changed. Measured: HadGEM3 8-10 ka study-area mean 4.547 W/m², global equivalent 0.1713, versus 4.90 for the all-or-nothing total in the same entry. | `git diff --stat 19c7ff5 66072d2` touches no changelog file. | Add an entry like the variant-components one: outputs changed (`forcing_by_period.csv` and the slide-18, kernel-spread and domain-mean figures, plus script 10's breakdown table and figure); legacy anchors unchanged; reviews; verification report names. |
| should-fix | same entry (missing note); behaviour in `R/run_manifest.R:157-182` | `analysis$forcing$variant` sits in the `analysis` section. `check_producer()` compares the whole `analysis` section with the one script 8's latest run recorded. | In any existing run folder (Andria's included), script 9 now stops with "used different analysis settings: forcing.variant ..." until script 8 is rerun or `run$allow_stale` is set. The verification could not see this: `tools/verify_change.R:111` sets `allow_stale = TRUE` whenever files are seeded or copied. | `run_manifest.R:174` (`analysis_differences(latest$config$analysis, ...)`); `verify_change.R:111`. | Say so in the changelog, as the variant-components entry did ("Note for existing run folders"). Behaviour itself is as designed. |
| should-fix | `docs/cc/questions_for_andria_scientific.md`: C1 "Answered 2026-10-19" note (chris-dev, commit 724bfae) and C5 item (a) at line ~669 | The C1 note on chris-dev says "The configured headline ... is still `veg_ice_thresh`: switching it is a code change, to be asked first." C5(a) still describes `veg_ice_thresh` as what "the script currently assumes" and asks Andria to check it. Both are wrong once this merges. | Andria and Chris would read a record that contradicts config.R. AGENTS.md requires recording answers and removing answered questions. | `git show 724bfae`; line 669 of the questions file in the worktree. | After the merge into chris-dev, update the C1 note (switched on 19 October, commit a099008), mark C5(a) answered by C1, and say the slide-18 comparison table under C5 (line ~679) is for `veg_ice_thresh`. |
| nit | `R/config_schema.R:31-34` | The comment gives two reasons a part cannot be the headline. The second, "script 9's headline sums would drop cells whose steps are of both kinds", holds only for the all-or-nothing parts (`veg_thresh`, `ice_thresh`, `icesc_thresh`), which are NA where they do not apply. The partial-ice parts (`veg_part`, `ice_part`, `icesc_part`) have a value for every known pair. Likewise "completed" applies only to the all-or-nothing parts. | A reader could think the partial-ice parts also lose cells. | `R/differences.R:72-75` (partial-ice parts computed without NA); script 9 header A2 (lines 61-63: "every known pair has both parts"). | Reword, e.g. "a part is not the forcing of the whole surface (and the all-or-nothing parts would also drop cells with both kinds of step)". |
| nit | `config.R:133-137`; `R/config_schema.R:156` | The comment says "Legal: one of the four totals" without naming them. The schema error only lists the legal values, with no reason. | Andria has to open the schema to see which four values are allowed, or why `veg_thresh` is refused. | Error text comes from `check_choice` ("has 'veg_thresh'; legal values are ..."). | List the four names in the config.R comment, in both config.R and legacy_config.R. Optionally, add a reason to the error. |
| nit | `config.R:134-136`; `scripts/9_forcing_barplot.R:44-45` | "times the glacier albedo": `veg_ice_parts` uses the fixed glacier albedo (`ice_albedo_fixed`, `R/differences.R:74`). The `_icesc_` total uses the month-by-month one. | Minor ambiguity between the two partial-ice totals. | `R/differences.R:74-78`. | Say "the fixed glacier albedo". |
| nit (already there before this change) | `R/run_manifest.R:194` | The example line `'forcing.variant: "veg_thresh" -> "veg_ice_thresh"'` shows a value that is now illegal. | Cosmetic only. | Line 194. | Change the example to `veg_ice_thresh -> veg_ice_parts`. |

**Confirmed correct**
- Only scripts 9 and 10 read `analysis$forcing$variant` (9:149, 10:60 and 10:66). No tool, test or other config does, apart from test_config. Neither script assumes an all-or-nothing headline:
  - Script 9's headline aggregation does not use the completed components.
  - Its A2 note (line 79, "The headline (A1) does not use these components") is still true.
  - The sensitivity subtitle ("the headline is one of the totals") is still true.
  - The figure titles do not say "threshold".
  - Script 10's header (line 40, "combines vegetation change and ice-sheet retreat") is still true, and its outputs and figure name the variant.
- The schema change is right:
  - `FORCING_TOTALS` holds exactly the four totals.
  - `sensitivity_variants` still accepts all ten.
  - The legacy config keeps `veg_ice_thresh`, which is legal, so the legacy run validates.
- Scripts 1-8 do not depend on this setting. Between the 18 October reference run (4ba4d2a) and the base, only `R/forcing_components.R`, `config.R` (sensitivity list) and script 9 changed. So the per-cell forcing table copied into the current-config verification is valid input.
- Tests (`tests/test_config.R`):
  - The text-matching edits are safe. `load_variant` requires the `from` text to match exactly one line, and `variant = "veg_ice_parts"` matches only config.R:138. The sensitivity line contains `"veg_ice_parts",` but not `variant = "`.
  - The new assertions cover: the new default, the legacy value, acceptance of another total (`veg_icesc_thresh`), and rejection of an all-or-nothing part (`veg_thresh`) and a partial-ice part (`ice_part`).
  - The JSON assertion is updated.
- Verification is adequate:
  - Current configuration: the headline is recomputed from the per-cell table for all three kernels. The HadGEM3 headline equals the `veg_ice_parts` rows of the sensitivity table. The headline changed. The EGU-recipe, IPCC and sensitivity tables are byte-identical to the earlier runs. Script 10's own sum check passed.
  - Legacy: all four script 9 tables match the legacy forcing anchors (`tests/anchors/interp-forcing-2026-10-08`) byte for byte.
  - The recomputation uses the same aggregation as script 9, so it is not an independent test of the aggregation; it only confirms the column chosen.
- Commit messages match the diffs, and the values they quote (4.55 and 0.171) match the check's thresholds. "Scripts 9 and 10 ... follow without change" is accurate.
- The comments describe the partial-ice albedo correctly (`scripts/7a_alb_diff_full.R:115-120`). The A1 note reads well for Andria.

**Questions for Chris**
- `forcing_by_period.csv` and the slide-18 and kernel-spread figures do not name the variant they show; only script 10's outputs do. Now that the current and legacy configurations report different variants under the same file names, do you want a `variant` column or a subtitle?
  - A `variant` column in `forcing_by_period.csv` would break the legacy anchors and needs your decision.
  - A figure subtitle would not touch the anchors.
  - Meanwhile the run manifest records the setting.
- The `output/forcing/forcing_by_period.csv` tracked in git was last committed in e0ad29b in September. It is not today's headline, and this change makes the gap larger. Should it be refreshed or stop being tracked? This was already the case before this change.

**Not checked**
- I did not run anything, so I relied on the PASS reports for the test and run results.
- The arithmetic of `veg_ice_parts` itself, which this change does not touch.
- That the C1 answer correctly reflects what Andria said; I read only the questions file record.

Files read: 15. Tool calls: 18.
