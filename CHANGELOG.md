# Changelog

## Unreleased

Breaking changes from pre-1.0:

- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it. Repo tests use `TEST_DATABASE_URL` with no fallback.
- `Poppy.JoinChain`, `Poppy.Relation`, and generated `HasMany` / `BelongsTo` values are gone. Ad-hoc joins are raw SQL. The depth-5 and join-alias schema checks are gone.
- Include and nested-write fields use the relation name as declared. Duplicate relation names on a model, or a name that matches a scalar field, fail validation.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and `loadWith`. Result types follow the include shape: a skipped field is a type error, not `[]` / `Nothing`. A required `belongsTo` is the parent row type.
- `load` / `loadWith` take `where_`, `orderBy_`, and `take_` (per-parent). Includes load with one batched query per edge.
- `findUnique` takes `uniqueQuery` with a generated unique key (`ById`, `ByTitle`, …). `update` and `delete` take that unique. `upsert` takes `OnId` / `OnTitle`. `findUniqueWhere`, `requireUniqueWhere`, and `InvalidUniqueInput` are gone.
- Nested writes are fields on `create` / `update` (not `createNested` / `updateNested`). `replaceWith` replaces children; `disconnect` nulls a nullable FK; nested `upsert` refuses to re-parent. `createMany` / top-level `upsert` take scalar-only payloads when the model has relations.
- Application code imports `Poppy` (and `Schema.*`). Generated modules import `Poppy.Internal.Generated`. Low-level builders and `Db (..)` / `DbPool (..)` are no longer re-exported from `Poppy`. Codegen exposes `Poppy.Codegen.Schema` and `Poppy.Codegen.CLI` only; `Schema` / `Model` / `FieldSpec` / `RelationSpec` are abstract.

## 0.1.0.0 — 2026-09-17

First release of `poppy` (runtime) and `poppy-codegen` (Schema builder, Client emission, drift-check).
