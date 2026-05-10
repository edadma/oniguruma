#!/usr/bin/env bash
# Regenerate jvm/src/test/resources/textmate-corpus.txt from a directory of
# .tmLanguage.json grammars. The corpus drives the parser/compiler validation
# tests in stages 2 and 5.
#
# Usage:
#   tools/regen-corpus.sh [GRAMMARS_DIR]
#
# GRAMMARS_DIR defaults to ~/dev/juicer/docs/grammars. Each .tmLanguage.json
# file is mined for `.match`, `.begin`, `.end`, and `.while` fields (the four
# places where TextMate grammars store regex patterns); the union is
# deduplicated and written one pattern per line.
#
# Requires `jq`.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
GRAMMARS_DIR="${1:-$HOME/dev/juicer/docs/grammars}"
OUT="$REPO_ROOT/jvm/src/test/resources/textmate-corpus.txt"

if [[ ! -d "$GRAMMARS_DIR" ]]; then
  echo "error: grammars dir not found: $GRAMMARS_DIR" >&2
  exit 1
fi

if ! command -v jq >/dev/null; then
  echo "error: jq is required" >&2
  exit 1
fi

count=$(find "$GRAMMARS_DIR" -maxdepth 1 -name '*.tmLanguage.json' | wc -l)
echo "scanning $count grammars in $GRAMMARS_DIR ..."

tmp=$(mktemp)
# Each pattern is base64-encoded onto its own line. Plain-text encoding
# would have to escape any embedded newlines, which would change the
# semantics of `(?x)`-mode `# line comments` (those terminate at a real
# newline; the `\n` regex escape is two source characters and does NOT
# end an extended-mode comment). Base64 round-trips every byte verbatim
# so multi-line `match` / `begin` / `end` / `while` fields keep their
# original line structure and the parser sees what the grammar author
# actually wrote.
for f in "$GRAMMARS_DIR"/*.tmLanguage.json; do
  jq -r '.. | objects
              | (.match // empty, .begin // empty, .end // empty, .while // empty)
              | select(. != "")
              | @base64' "$f" 2>/dev/null
done > "$tmp"

# Dedupe (the base64 strings are unique iff the source patterns were).
awk '!seen[$0]++' "$tmp" > "$OUT"
rm -f "$tmp"

echo "wrote $(wc -l < "$OUT") unique patterns to $OUT"
