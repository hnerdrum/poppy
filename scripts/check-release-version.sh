#!/usr/bin/env bash
# Verify both packages and changelogs name the same version.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

version="${1:-}"
version="${version#v}"
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.][0-9]+)?$ ]]; then
  echo "usage: $0 1.0.0" >&2
  exit 1
fi

pkg_version() {
  awk '/^version:/{print $2; exit}' "$1"
}

check_file_version() {
  local file="$1"
  local actual
  actual="$(pkg_version "$file")"
  if [[ "$actual" != "$version" ]]; then
    echo "$file version is $actual, expected $version" >&2
    exit 1
  fi
}

check_changelog() {
  local file="$1"
  if ! grep -Eq "^## ${version}( |$)" "$file"; then
    echo "$file has no heading for $version" >&2
    exit 1
  fi
}

check_file_version poppy/package.yaml
check_file_version poppy/poppy.cabal
check_file_version poppy-codegen/package.yaml
check_file_version poppy-codegen/poppy-codegen.cabal
check_changelog CHANGELOG.md
check_changelog poppy/CHANGELOG.md
check_changelog poppy-codegen/CHANGELOG.md

echo "release version $version is consistent"
