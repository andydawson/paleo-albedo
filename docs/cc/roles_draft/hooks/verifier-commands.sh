#!/usr/bin/env bash
# PreToolUse hook for the `verifier` subagent (.claude/agents/verifier.md).
#
# Why: a subagent's `tools:` line cannot restrict Bash to particular commands (a
# `Bash(git push *)` entry removes the whole tool), so the allowlist lives here. The verifier
# may run the project's checked-in verification tools, read-only git queries, and make a
# detached worktree; nothing that edits, switches, deletes or kills. Exit 2 blocks the call
# and the message on stderr is shown to the agent.
#
# Tested with test_hooks.sh (allowed and forbidden samples); re-run it after any change.
set -euo pipefail
input=$(cat)
command=$(jq -r '.tool_input.command // empty' <<<"$input")
scratch=$(jq -r '.scratchpad_dir // empty' <<<"$input")
[[ -z "$command" ]] && exit 0

deny() { echo "verifier hook: refused: $1" >&2; exit 2; }

# No command substitution or process substitution: they hide commands from the checks.
grep -Eq '\$\(|`|<\(' <<<"$command" && deny "command substitution"

# File redirections may only target the scratchpad; `2>&1` descriptor merges are fine.
stripped=$(sed -E 's/[0-9]*>&[0-9]+//g' <<<"$command")
for target in $(grep -Eo '>>?[[:space:]]*[^[:space:]]+' <<<"$stripped" | sed -E 's/^>>?[[:space:]]*//'); do
  [[ -n "$scratch" && "$target" == "$scratch"* ]] || deny "writes only under the scratchpad: $target"
done

# Each simple command (split on pipes, &&, ||, ;) is judged by its first word.
forbidden='^(rm|rmdir|mv|cp|chmod|chown|kill|pkill|killall|sudo|tee|dd|truncate|shred|bash|sh|eval|exec|xargs|source|\.|python3?|perl)$'
allowed='^(Rscript[[:space:]]+tools/[^[:space:]]+|tools/rscript\.sh[[:space:]]+tools/[^[:space:]]+|git[[:space:]]+(-C[[:space:]]+[^[:space:]]+[[:space:]]+)?(worktree[[:space:]]+(add|list)|rev-parse|status|log|diff|show|lfs[[:space:]]+(ls-files|pull)|ls-files)|ls|cat|head|tail|wc|grep|md5sum|nproc|ps|pgrep|uptime|free|df|date|echo|true|test|\[|realpath|basename|dirname|find|sort|uniq|cut|tr|awk|sed[[:space:]]+-n|jq|sleep|env|printf)([[:space:]]|$)'
IFS=$'\n'
for seg in $(sed -E 's/\|\|/\n/g; s/&&/\n/g; s/;/\n/g; s/\|/\n/g' <<<"$command"); do
  seg=$(sed -E 's/^[[:space:]]+//; s/^([A-Z_]+=[^[:space:]]+[[:space:]]+)*//' <<<"$seg")
  [[ -z "$seg" ]] && continue
  first=${seg%%[[:space:]]*}
  grep -Eq "$forbidden" <<<"$first" && deny "'$first' is a writing, deleting, killing or wrapping command"
  if [[ "$first" == git ]]; then
    grep -Eq '^git[[:space:]]+(-C[[:space:]]+[^[:space:]]+[[:space:]]+)?(checkout|switch|commit|merge|rebase|reset|stash|clean|push|pull|fetch|restore|rm|add|tag|branch[[:space:]]+-|worktree[[:space:]]+(remove|prune|move|lock|unlock))' <<<"$seg" \
      && deny "a git command that changes a checkout or history"
  fi
  if [[ "$first" == Rscript ]]; then
    grep -Eq '^Rscript[[:space:]]+(-e|--vanilla[[:space:]]+-e|scripts/)' <<<"$seg" \
      && deny "ad hoc R or a pipeline script run outside the verification tool"
  fi
  grep -Eq "$allowed" <<<"$seg" || deny "'$seg' is not on the verifier's allowlist"
done
exit 0
