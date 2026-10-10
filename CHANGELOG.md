# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

`poppy` and `poppy-codegen` are released in **lockstep**: the same major.minor.patch
on both packages. Use matching versions; regenerate Clients when you bump codegen.

## 1.0.0 — 2026-10-09

First stable release. Postgres CRUD ORM with a Schema DSL, generated Client,
relation includes, nested writes, hand-written SQL migrations, drift check, and
an apply-runner.

### API contract

- Breaking changes to the documented Client surface or Schema DSL are a **major** version.
- Generated `Schema.*` / `Schema.Client.*` / `Schema.Include.*` files are part of the
  contract for a given codegen version: commit them and run `generate --check` in CI.
- Application code imports `Poppy` and generated `Schema.*`. Generated modules import
  `Poppy.Internal.Generated`. Public codegen is `Poppy.Codegen.Schema` and
  `Poppy.Codegen.CLI`.

### Changed since 0.1.0.0

- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
  Repo tests use `TEST_DATABASE_URL` with no fallback.
- `Poppy.JoinChain`, `Poppy.Relation`, and generated `HasMany` / `BelongsTo` values are
  gone. Ad-hoc joins are raw SQL.
- Include and nested-write fields use the relation name as declared. Duplicate relation
  names on a model, or a name that matches a scalar field, fail validation.
- Models with relations emit `Schema.Include.<Model>`. Edges are `skip`, `load`, and
  `loadWith`. Result types follow the include shape.
- `load` / `loadWith` take `where_`, `orderBy_`, and `take_` (per-parent). Includes load
  with one batched query per edge.
- `findUnique` takes `uniqueQuery` with a generated unique key (`ById`, `ByTitle`, …).
  `update` and `delete` take that unique. `upsert` takes `OnId` / `OnTitle`.
- Nested writes are fields on `create` / `update` (not `createNested` / `updateNested`).
  `create` / `update` return the root `*Row` (no include on the write return).
- Nested-write Clients import root scalar types (`UTCTime`, `NullableValue`, …) needed by
  re-emitted create/update payloads.
- Low-level builders and `Db (..)` / `DbPool (..)` are no longer re-exported from `Poppy`.
  Schema IR types from codegen are abstract.

## 0.1.0.0 — 2026-09-17

First release of `poppy` (runtime) and `poppy-codegen` (Schema builder, Client emission, drift-check).
