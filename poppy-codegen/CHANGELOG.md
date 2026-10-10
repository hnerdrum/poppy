# Changelog

All notable changes to `poppy-codegen` are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Released in **lockstep** with `poppy` (`poppy >= 1.0 && < 1.1` for this release).
See the [repository changelog](../CHANGELOG.md) for the full 1.0 contract, 1.1 plans,
and non-goals.

## 1.0.0 — 2026-10-09

Stable Schema builder, Client emission, `--check`, and `--check-schema`.

- Only `Poppy.Codegen.Schema` and `Poppy.Codegen.CLI` are exposed. `Schema`, `Model`,
  `FieldSpec`, and `RelationSpec` are abstract. Generated modules import
  `Poppy.Internal.Generated`.
- Haddock on the public modules. Guides: repository `docs/`.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Model modules no longer emit `HasMany` / `BelongsTo` values.
- Include and nested-write fields use the relation name. Models with relations emit
  `Schema.Include.<Model>` (`skip` / `load` / `loadWith`, with edge `where_` /
  `orderBy_` / `take_`).
- Clients emit `<Model>Unique` / `<Model>UniqueKey`. Nested writes live on `create` /
  `update` relation fields.
- Nested-write Clients import root scalar types required by re-emitted create/update
  payloads (`UTCTime`, `NullableValue`, …).

## 0.1.0.0 — 2026-09-17

First release. Schema builder, `generate` Client emission, `--check` freshness, and `--check-schema` drift against live Postgres.
