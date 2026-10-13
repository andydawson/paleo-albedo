#!/usr/bin/env bash
# Build an isolated run directory for a verification run.
#
# Why: the scripts write to fixed paths, so re-running one in the repo replaces the files
# later checks compare against. The scripts use relative paths, so running them from a
# directory whose data/ and output/ are trees of links to the repo's files makes them read
# the real inputs and write every output locally. Files matching an exclude pattern (the
# outputs of the scripts you will run there) are not linked, so the run writes fresh ones.
# As a backstop, run `tools/frozen.sh lock` first: writing through a link to a locked file
# fails loudly instead of overwriting it.
#
# Usage: tools/rundir.sh <name> [exclude-glob ...]
#   Globs are relative to the repo root, e.g. 'data/lct_*_reveals_interp.RDS' 'output/prediction/*'.
#   Creates .rundirs/<name>/ (gitignored). scripts/, R/ and config.R are COPIED from the working
#   tree (tests/, the 265 MB of anchors, is linked: no script reads it), so the run is pinned to the code as it was at creation: a branch switch
#   or edit in the repo during the run cannot reach it (git checkout rewrites files in
#   place, and R reads a script progressively while running it; learned 2026-10-12).
#   Prints the directory path. Fails if it already exists.
set -euo pipefail
ROOT=$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)
NAME=${1:?usage: tools/rundir.sh <name> [exclude-glob ...]}; shift
DIR="$ROOT/.rundirs/$NAME"
[[ -e "$DIR" ]] && { echo "rundir: $DIR already exists" >&2; exit 1; }
mkdir -p "$DIR/figures" "$DIR/runs"
for d in scripts R; do cp -r "$ROOT/$d" "$DIR/$d"; done
[[ -e "$ROOT/config.R" ]] && cp "$ROOT/config.R" "$DIR/config.R"   # the settings, read by R/config.R
ln -s "$ROOT/tests" "$DIR/tests"
git -C "$ROOT" rev-parse --short HEAD > "$DIR/CODE_COMMIT"   # which commit the copies came from
[[ -n "$(git -C "$ROOT" status --porcelain -- scripts R config.R)" ]] && echo "dirty" >> "$DIR/CODE_COMMIT"
linked=0; skipped=0
while IFS= read -r -d '' f; do
  rel=${f#"$ROOT/"}
  skip=0
  for g in "$@"; do
    # shellcheck disable=SC2053
    [[ "$rel" == $g ]] && { skip=1; break; }
  done
  mkdir -p "$DIR/$(dirname "$rel")"
  if (( skip )); then skipped=$((skipped+1)); else ln -s "$f" "$DIR/$rel"; linked=$((linked+1)); fi
done < <(find "$ROOT/data" "$ROOT/output" -type f -print0)
echo "rundir: $linked files linked, $skipped excluded" >&2
echo "$DIR"
