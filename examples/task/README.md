# Task example

A minimal Poppy app: one Schema, a thin codegen executable, a generated Client, and two queries.

From this directory:

```bash
cabal run task-codegen
```

That writes `src/Schema/Task.hs` and `src/Schema/Client/Task.hs`. Re-run after Schema changes. `--check` fails if those files are stale; `--check-schema` compares the Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`).

```bash
psql "$DATABASE_URL" -f migrations/001-task.sql
cabal run task
```

`task` inserts a row, lists open tasks, and loads one by id.

## Layout

| Path                      | Role                                        |
| ------------------------- | ------------------------------------------- |
| `codegen/TaskSchema.hs`   | Schema value                                |
| `codegen/TaskTarget.hs`   | Output layout + Client                      |
| `codegen/Main.hs`         | `main = mainWith [taskTarget] [taskTarget]` |
| `src/Schema/`             | Generated table types and Client            |
| `app/Main.hs`             | Queries                                     |
| `migrations/001-task.sql` | Hand-written SQL (Poppy does not migrate)   |

This package is not published. Path-depend on `../../poppy` and `../../poppy-codegen` via `cabal.project`. In your own app, depend on those packages from Hackage (or a git extra-dep) instead.
