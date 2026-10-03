# Poppy docs

Poppy is a Postgres ORM for Haskell. You write a Schema, generate table types and a Client, then query and write through that Client.

Start here:

1. [Getting started](getting-started.md) — Schema, generate, migrate, first query
2. [Schema](schema.md) — fields, enums, relations, uniques
3. [Client](client.md) — `emptyQuery`, reads, `select_`
4. [Relations](relations.md) — `hasMany` / `belongsTo`, include records
5. [Writes](writes.md) — create, update, upsert, nested `Set` / `Ops`
6. [Migrations and drift](migrations-and-drift.md) — hand-written SQL, `applyMigrations`, `--check-schema`
7. [Errors](errors.md) — `ORMError`
8. [Raw SQL](raw-sql.md) — `queryRaw`, `JoinChain`
9. [Why Poppy](why-poppy.md) — shape of the 1.0 bar

Worked examples: [`examples/task/`](../examples/task/) (one table) and [`examples/blog/`](../examples/blog/) (relations). Haddock on `Poppy` and `Poppy.Codegen.Schema` is the API lookup; these pages are the tutorial.
