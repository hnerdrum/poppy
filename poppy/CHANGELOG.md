# Changelog

## Unreleased

- Haddock on Client-facing exports. Guides: repository `docs/`.
- `--check-schema` / pool helpers: shipped code reads `DATABASE_URL` only; tests use `TEST_DATABASE_URL`.
- Remove `Poppy.JoinChain` and `Poppy.Relation`. Ad-hoc joins are raw SQL.
- `load` and `loadWith` take `where_`, `orderBy_`, and `take_`. `take_` keeps that many included rows per parent.
- Include result types follow the include shape (`skip` / `load` / `loadWith`). A required `belongsTo` is not `Maybe`.
- `updateWhere` updates by a `Where`. `findUniqueWhere`, `requireUniqueWhere`, and `InvalidUniqueInput` are gone.
- Nested writes are fields on `create` / `update`. `createNested` / `updateNested` are gone.
- Public API is `Poppy` only. Generated Clients import `Poppy.Internal.Generated`. Low-level `findMany` / builders / `Db (..)` are no longer re-exported from `Poppy`.

## 0.1.0.0 — 2026-09-17

First release. Runtime for Schema-generated Clients: queries, writes, `Db`, and Postgres errors.
