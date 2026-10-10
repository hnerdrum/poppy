#!/usr/bin/env bash
# Upload poppy then poppy-codegen (and Haddock tarballs) to Hackage.
#
# Requires HACKAGE_TOKEN (Hackage account → Enable 2FA / API tokens).
# Default is --publish. Pass --candidate to upload unpublished candidates.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

publish=(--publish)
version=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --candidate)
      publish=()
      shift
      ;;
    --publish)
      publish=(--publish)
      shift
      ;;
    -*)
      echo "unknown option: $1" >&2
      exit 1
      ;;
    *)
      version="${1#v}"
      shift
      ;;
  esac
done

if [[ -z "$version" ]]; then
  echo "usage: $0 [--candidate] 1.0.0" >&2
  exit 1
fi

if [[ -z "${HACKAGE_TOKEN:-}" ]]; then
  echo "HACKAGE_TOKEN is not set" >&2
  exit 1
fi

"$root/scripts/check-release-version.sh" "$version"

sdist_dir="dist-newstyle/sdist"
poppy_tar="${sdist_dir}/poppy-${version}.tar.gz"
codegen_tar="${sdist_dir}/poppy-codegen-${version}.tar.gz"

if [[ ! -f "$poppy_tar" ]]; then
  echo "missing $poppy_tar (run: cabal sdist all)" >&2
  exit 1
fi
if [[ ! -f "$codegen_tar" ]]; then
  echo "missing $codegen_tar (run: cabal sdist all)" >&2
  exit 1
fi

auth=(--token "$HACKAGE_TOKEN")

upload_src() {
  local tar="$1"
  echo "uploading $tar"
  cabal upload "${publish[@]}" "${auth[@]}" "$tar"
}

find_docs() {
  local name="$1"
  local f
  f="$(find dist-newstyle -name "${name}-${version}-docs.tar.gz" | head -n 1 || true)"
  if [[ -n "$f" ]]; then
    printf '%s' "$f"
  fi
}

upload_docs() {
  local name="$1"
  local tar
  tar="$(find_docs "$name")"
  if [[ -z "$tar" ]]; then
    echo "no ${name}-${version}-docs.tar.gz (run: cabal haddock --haddock-for-hackage ${name})" >&2
    exit 1
  fi
  echo "uploading docs $tar"
  cabal upload --documentation "${publish[@]}" "${auth[@]}" "$tar"
}

wait_for_hackage() {
  local name="$1"
  local url="https://hackage.haskell.org/package/${name}-${version}"
  local i
  echo "waiting for $url"
  for i in $(seq 1 36); do
    if curl -fsS -o /dev/null "$url"; then
      echo "found $url"
      return 0
    fi
    sleep 10
  done
  echo "timed out waiting for $url" >&2
  exit 1
}

upload_src "$poppy_tar"
if [[ ${#publish[@]} -gt 0 ]]; then
  wait_for_hackage poppy
fi
upload_src "$codegen_tar"

upload_docs poppy
upload_docs poppy-codegen
