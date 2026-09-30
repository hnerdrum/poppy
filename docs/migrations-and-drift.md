# Migrations and drift

Poppy does not generate SQL from the Schema. You write migrations; Codegen checks that the Schema matches live Postgres.

## Apply runner

```haskell
applied <- applyMigrations pool "migrations"
```

`applyMigrations` looks at `*.sql` files in that directory (not hidden names), sorted by filename. It creates `_poppy_migrations (name TEXT PRIMARY KEY, applied_at TIMESTAMPTZ)` if needed, skips names already recorded, and runs each pending file in its own transaction. On success the filename is inserted. A failed file is **not** recorded; fix it and re-run. The return is `Either MigrateError [Text]` (names applied this call).

`MigrateDirectoryError` is a missing directory or unreadable file. `MigrateFailed name err` wraps the `ORMError` from Postgres.

This is an apply runner, not a migration generator and not a down-migration tool.

## Drift-check

```bash
cabal run task-codegen -- --check-schema
```

Needs `TEST_DATABASE_URL` or `DATABASE_URL`. Codegen introspects tables, columns, nullability, defaults, primary keys, uniques, enums, and foreign keys, then compares them to the Schema. Haskell IR is the source of truth: mismatch means the database or the Schema is wrong.

Typical reports: missing/extra table or column, type or nullability mismatch, PK/unique/enum/FK drift, missing or mismatched `DEFAULT` for `withDefault`.

`--check` is different: it only diffs generated Haskell files against a fresh emit. Use both in CI (this repo's examples do).

## Workflow

1. Write or edit a `.sql` file.
2. Run the app (or `applyMigrations`) against a database.
3. Align the Schema with that SQL.
4. `cabal run …-codegen` to regenerate Clients.
5. `…-codegen -- --check-schema` to confirm IR and Postgres agree.
