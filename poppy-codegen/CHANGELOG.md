# Changelog

## Unreleased

- Haddock on @Poppy.Codegen.Schema@ and @Poppy.Codegen.CLI@. Guides: repository @docs/@.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Model modules no longer emit `HasMany` / `BelongsTo` values. Schema checks no longer reject include trees deeper than 5 or for running out of join aliases.
- An include or nested-write field is the same as the relation name. Validation rejects a duplicated relation name and a relation name that matches a scalar field.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and `loadWith`.
- Include loaders apply `where_`, `orderBy_`, and per-parent `take_` from the edge.
- Clients emit `<Model>Unique` (`ById`) and `<Model>UniqueKey` (`OnId`). `findUnique` takes `uniqueQuery`. `update` and `delete` take the unique. `upsert` takes the conflict target.

## 0.1.0.0 — 2026-09-17

First release. Schema builder, `generate` Client emission, `--check` freshness, and `--check-schema` drift against live Postgres.
