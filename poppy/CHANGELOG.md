# Changelog

## Unreleased

- Haddock on Client-facing exports. Guides: repository @docs/@.
- Remove `Poppy.JoinChain` and `Poppy.Relation`. Ad-hoc joins are raw SQL.
- `load` and `loadWith` take `where_`, `orderBy_`, and `take_`. `take_` keeps that many included rows per parent.

## 0.1.0.0 — 2026-09-17

First release. Runtime for Schema-generated Clients: queries, writes, `Db`, and Postgres errors.
