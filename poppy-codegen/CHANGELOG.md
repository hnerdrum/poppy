# Changelog

## Unreleased

- Haddock on @Poppy.Codegen.Schema@ and @Poppy.Codegen.CLI@. Guides: repository @docs/@.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Model modules no longer emit `HasMany` / `BelongsTo` values. Schema checks no longer reject include trees deeper than 5 or for running out of join aliases.
- An include or nested-write field is the same as the relation name. Validation rejects a duplicated relation name and a relation name that matches a scalar field.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and `loadWith`.

## 0.1.0.0 — 2026-09-17

First release. Schema builder, `generate` Client emission, `--check` freshness, and `--check-schema` drift against live Postgres.
