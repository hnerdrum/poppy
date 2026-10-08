# Changelog

## Unreleased

- Haddock on `Poppy.Codegen.Schema` and `Poppy.Codegen.CLI`. Guides: repository `docs/`.
- Only `Poppy.Codegen.Schema` and `Poppy.Codegen.CLI` are exposed. `Schema`, `Model`, `FieldSpec`, and `RelationSpec` are abstract. Generated modules import `Poppy.Internal.Generated`. `mainWith` / `simpleTarget` remain on CLI for multi-schema apps.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Model modules no longer emit `HasMany` / `BelongsTo` values. Schema checks no longer reject include trees deeper than 5 or for running out of join aliases.
- An include or nested-write field is the relation name. Validation rejects a duplicated relation name and a relation name that matches a scalar field.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and `loadWith`. Result types follow the include shape.
- Include loaders apply `where_`, `orderBy_`, and per-parent `take_` from the edge.
- Clients emit `<Model>Unique` (`ById`) and `<Model>UniqueKey` (`OnId`). `findUnique` takes `uniqueQuery`. `update` and `delete` take the unique. `upsert` takes the conflict target.
- Nested writes live on `create` / `update` relation fields. `createNested` / `updateNested` and `*WriteCreate` / `*WriteUpdate` are gone. `replaceWith` replaces children; `disconnect` nulls a nullable FK; nested `upsert` never re-parents another parent's row. `createMany` / top-level `upsert` take scalar-only payloads when the model has relations.

## 0.1.0.0 — 2026-09-17

First release. Schema builder, `generate` Client emission, `--check` freshness, and `--check-schema` drift against live Postgres.
