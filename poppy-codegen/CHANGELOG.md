# Changelog

## Unreleased

- Haddock on @Poppy.Codegen.Schema@ and @Poppy.Codegen.CLI@. Guides: repository @docs/@.
- `--check-schema` reads `DATABASE_URL` only. Pass `--database-url URL` to override it.
- Model modules no longer emit `HasMany` / `BelongsTo` values. Schema checks no longer reject include trees deeper than 5 or for running out of join aliases.

## 0.1.0.0 — 2026-09-17

First release. Schema builder, `generate` Client emission, `--check` freshness, and `--check-schema` drift against live Postgres.
