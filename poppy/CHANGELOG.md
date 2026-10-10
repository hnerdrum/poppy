# Changelog

All notable changes to `poppy` are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Released in **lockstep** with `poppy-codegen`. See the [repository changelog](../CHANGELOG.md)
for the full 1.0 contract, 1.1 plans, and non-goals.

## 1.0.0 — 2026-10-09

Stable Postgres runtime for Schema-generated Clients.

- Public API is `Poppy` only. Generated Clients import `Poppy.Internal.Generated`.
  Low-level `findMany` / builders / `Db (..)` are no longer re-exported from `Poppy`.
- Haddock on Client-facing exports. Guides: repository `docs/`.
- `--check-schema` / pool helpers: shipped code reads `DATABASE_URL` only; tests use
  `TEST_DATABASE_URL`.
- Remove `Poppy.JoinChain` and `Poppy.Relation`. Ad-hoc joins are raw SQL.
- `load` and `loadWith` take `where_`, `orderBy_`, and `take_`. Include result types
  follow the include shape (`skip` / `load` / `loadWith`).
- Nested writes are fields on `create` / `update`. `createNested` / `updateNested` are gone.
  Write success values remain root `*Row` values.
- `updateWhere` updates by a `Where`. `findUniqueWhere`, `requireUniqueWhere`, and
  `InvalidUniqueInput` are gone.

## 0.1.0.0 — 2026-09-17

First release. Runtime for Schema-generated Clients: queries, writes, `Db`, and Postgres errors.
