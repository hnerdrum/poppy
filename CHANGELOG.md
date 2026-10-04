# Changelog

## Unreleased

- User-facing guides under `docs/` and Haddock on Client-facing `Poppy` / `Poppy.Codegen.Schema` exports.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Generated model modules no longer include `HasMany` / `BelongsTo` join values. Ad-hoc joins are raw SQL. Include loading is unchanged. The depth-5 and join-alias schema checks are gone.

## 0.1.0.0 — 2026-09-17

First release of `poppy` (runtime) and `poppy-codegen` (Schema builder, Client emission, drift-check).
