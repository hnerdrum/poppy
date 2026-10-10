#!/usr/bin/env bash
# Tag origin/main as vVERSION and push.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"

version="${1:-}"
version="${version#v}"
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.][0-9]+)?$ ]]; then
  echo "usage: $0 1.0.0" >&2
  exit 1
fi

"$root/scripts/check-release-version.sh" "$version"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "working tree is not clean" >&2
  exit 1
fi

branch="$(git branch --show-current)"
if [[ "$branch" != "main" ]]; then
  echo "must be on main (on ${branch:-detached})" >&2
  exit 1
fi

git fetch origin
if [[ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]]; then
  echo "HEAD is not origin/main" >&2
  exit 1
fi

tag="v${version}"
if git rev-parse "$tag" >/dev/null 2>&1; then
  echo "tag $tag already exists" >&2
  exit 1
fi

git tag -a "$tag" -m "$version"
git push origin "$tag"
echo "pushed $tag; Release workflow will upload Hackage"
