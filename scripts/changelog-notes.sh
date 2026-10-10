#!/usr/bin/env bash
# Print the CHANGELOG.md body for one version (no heading).
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

version="${1:-}"
version="${version#v}"
if [[ -z "$version" ]]; then
  echo "usage: $0 1.0.0" >&2
  exit 1
fi

awk -v ver="$version" '
  $0 ~ "^## " ver "( |$)" { p = 1; next }
  p && /^## / { exit }
  p && NF { started = 1 }
  started { print }
' CHANGELOG.md
