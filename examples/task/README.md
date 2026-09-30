# Task example

One Schema, a codegen executable, a generated Client, and two queries. For relations, nested writes, and includes see [`examples/blog/`](../blog/). Full walkthrough: [Getting started](../../docs/getting-started.md).

From this directory:

```bash
cabal run task-codegen
```

That writes `src/Schema/Task.hs` and `src/Schema/Client/Task.hs` (`generate "src/Schema"`). Re-run after Schema changes. `--check` fails if those files are stale; `--check-schema` compares the Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`).

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal run task
```

`task` applies `migrations/*.sql` with `applyMigrations`, inserts a row, lists open tasks, and loads one by id.

## Layout

| Path                    | Role                                 |
| ----------------------- | ------------------------------------ |
| `codegen/TaskSchema.hs` | Schema value                         |
| `codegen/Main.hs`       | `generate "src/Schema" taskSchema`   |
| `src/Schema/`           | Generated table types and Client     |
| `app/Main.hs`           | Queries                              |
| `migrations/`           | Hand-written SQL, applied at startup |

This package is not on Hackage. It path-depends on `../../poppy` and `../../poppy-codegen` via `cabal.project`. In your own app, depend on those packages with a git extra-dep (or Hackage after the first release).
