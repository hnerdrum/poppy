# Changelog

## Unreleased

- User-facing guides under `docs/` and Haddock on Client-facing `Poppy` / `Poppy.Codegen.Schema` exports.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Generated model modules no longer include `HasMany` / `BelongsTo` join values. Ad-hoc joins are raw SQL. Include loading is unchanged. The depth-5 and join-alias schema checks are gone.
- An include or nested-write field is the relation name unchanged. A relation name used twice on one model, or equal to a scalar field on that model, fails validation.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and `loadWith`.

## 0.1.0.0 — 2026-09-17

First release of `poppy` (runtime) and `poppy-codegen` (Schema builder, Client emission, drift-check).
