# Migrations and drift

Poppy does not generate SQL from the Schema. You write migrations; Codegen checks that the Schema matches live Postgres.

## Applying files

```haskell
applied <- applyMigrations pool "migrations"
```

`applyMigrations` picks up `*.sql` files in that directory (hidden names are ignored), in filename order. It creates `_poppy_migrations` if needed, skips names already in that table, and runs each remaining file in its own transaction. After a successful file, the name is recorded. A failed file is left unrecorded.

## Checking drift

```bash
cabal run task-codegen -- --check-schema
```

`--check-schema` reads `DATABASE_URL`. Pass `--database-url URL` to override it (the two flags can appear in either order). The check compares tables, columns, keys, and related catalog data to the Schema.

## Workflow

1. Add or edit a `.sql` file.
2. Start the app so `applyMigrations` runs.
3. Update the Schema if the SQL changed the shape of a table.
4. Run codegen again.
5. Run `--check-schema` when you want to be sure Postgres and the Schema still agree.
