#!/usr/bin/env bash
# PreToolUse hook for the `citation-identity-checker` subagent.
#
# Why: the checker needs the network (Crossref, DataCite, OpenAlex, Semantic Scholar,
# doi.org) and read-only Zotero lookups, and nothing else. It may save registry responses
# only under its scratchpad. Exit 2 blocks the call; stderr is shown to the agent.
#
# Tested with test_hooks.sh; re-run it after any change.
set -euo pipefail
input=$(cat)
command=$(jq -r '.tool_input.command // empty' <<<"$input")
scratch=$(jq -r '.scratchpad_dir // empty' <<<"$input")
[[ -z "$command" ]] && exit 0

deny() { echo "citation hook: refused: $1" >&2; exit 2; }

grep -Eq '\$\(|`|<\(' <<<"$command" && deny "command substitution"

# Every URL must be a registry, over https.
for url in $(grep -Eo 'https?://[^[:space:]"'"'"']+' <<<"$command"); do
  grep -Eq '^https://(api\.crossref\.org|api\.datacite\.org|api\.openalex\.org|doi\.org|api\.semanticscholar\.org)/' <<<"$url" \
    || deny "URL not on the registry allowlist: $url"
done

# Any file written (-o, --output, >, >>) must be under the scratchpad.
stripped=$(sed -E 's/[0-9]*>&[0-9]+//g' <<<"$command")
for target in $(grep -Eo '(-o|--output|>>|>)[[:space:]]*[^[:space:]]+' <<<"$stripped" | sed -E 's/^(-o|--output|>>|>)[[:space:]]*//'); do
  [[ -n "$scratch" && "$target" == "$scratch"* ]] || deny "writes only under the scratchpad: $target"
done

allowed='^(curl|wget|zotero-cli[[:space:]]+(--json[[:space:]]+)?(search|s|get|g|read|outline|path|ann|notes|children)|Rscript[[:space:]]+tools/check_references\.R|ls|cat|head|tail|wc|grep|jq|sort|uniq|cut|tr|sed[[:space:]]+-n|awk|sleep|date|echo|printf|md5sum|find|basename|dirname)([[:space:]]|$)'
IFS=$'\n'
for seg in $(sed -E 's/\|\|/\n/g; s/&&/\n/g; s/;/\n/g; s/\|/\n/g' <<<"$command"); do
  seg=$(sed -E 's/^[[:space:]]+//; s/^([A-Z_]+=[^[:space:]]+[[:space:]]+)*//' <<<"$seg")
  [[ -z "$seg" ]] && continue
  grep -Eq "$allowed" <<<"$seg" || deny "'$seg' is not on the checker's allowlist"
done
exit 0
