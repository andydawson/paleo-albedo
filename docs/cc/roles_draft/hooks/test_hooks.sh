#!/usr/bin/env bash
# Feeds sample Bash tool calls to the two hooks and prints ALLOW/DENY for each.
# Run from the hooks directory: bash test_hooks.sh
S=/tmp/claude-1000/x/scratchpad
ERR=$(mktemp)
t() {  # t <hook> <expected> <command>
  jq -nc --arg c "$3" --arg s "$S" '{tool_name:"Bash",tool_input:{command:$c},scratchpad_dir:$s}' \
    | bash "$1" >/dev/null 2>"$ERR" && got=ALLOW || got=DENY
  mark=$([[ "$got" == "$2" ]] && echo ok || echo WRONG)
  printf '%-5s %-5s %s   %s\n' "$mark" "$got" "$3" "$( [[ $got == DENY ]] && cat "$ERR")"
}
V=verifier-commands.sh; C=citation-commands.sh
echo "== verifier"
t $V ALLOW "THREADS=8 tools/rscript.sh tools/check_anchors.R tests/anchors/interp-tail-2026-09-21 /run > $S/log 2>&1"
t $V ALLOW "git -C /w rev-parse HEAD && git -C /w status --porcelain"
t $V ALLOW "git worktree add --detach /w/verify-abc 12af482"
t $V ALLOW "pgrep -fa Rscript | wc -l"
t $V ALLOW "Rscript tools/frozen.R verify"
t $V ALLOW "cat /run/runs/8_radiative/x.md | head -40"
t $V DENY  "git checkout main"
t $V DENY  "git -C /w checkout chris-dev"
t $V DENY  "r""m -rf .rundirs"
t $V DENY  "Rscript -e 'q()'"
t $V DENY  "Rscript scripts/8_radiative.R"
t $V DENY  "echo hi > /home/expert/file"
t $V DENY  "kill 1234"
t $V DENY  "git worktree remove /w"
t $V DENY  "cat x; git commit -m y"
t $V DENY  "bash driver.sh"
echo "== citation"
t $C ALLOW "curl -sS -A 'x (mailto:a@b)' https://api.crossref.org/works/10.5194/cp-21-2031-2025 -o $S/crossref/k.json"
t $C ALLOW "zotero-cli --json get metadata ABC123"
t $C ALLOW "curl -s 'https://api.crossref.org/works?query.bibliographic=x&rows=5' | jq '.message.items[0].title'"
t $C ALLOW "sleep 1; curl -s https://api.datacite.org/dois/10.5/x"
t $C DENY  "curl https://example.com/paper.pdf -o $S/p.pdf"
t $C DENY  "curl https://api.crossref.org/works/10.1/x -o /home/expert/Documents/contracting/Andria/paleo-RF/literature/x.json"
t $C DENY  "python3 -c 'print(1)'"
t $C DENY  "zotero-cli add-note ABC"
t $C DENY  "git status"
t $C DENY  "Rscript -e 1"
rm -f "$ERR"
