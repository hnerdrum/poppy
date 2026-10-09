# Task example

A single `task` table, the codegen executable that generates its client, and an app that runs two queries. For relations, nested writes, and includes, see [`examples/blog/`](../blog/). [Getting started](../../docs/getting-started.md) walks through this code in order.

Run the generator from this directory:

```bash
cabal run task-codegen
```

It writes `src/Schema/Task.hs` and `src/Schema/Client/Task.hs`. Re-run it whenever you change `codegen/TaskSchema.hs`. Pass `--check` to fail when those files are out of date, or `--check-schema` to compare the schema against Postgres. The schema check uses `DATABASE_URL`, or the URL given to `--database-url`.

Start Postgres with `docker compose up -d` from the repo root, then run the app:

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal run task
```

The app applies `migrations/*.sql` with `applyMigrations`, inserts one task, counts the open tasks, and loads the new task by id.

## Layout

| Path                    | Contents                             |
| ----------------------- | ------------------------------------ |
| `codegen/TaskSchema.hs` | The schema                           |
| `codegen/Main.hs`       | `generate "src/Schema" taskSchema`   |
| `src/Schema/`           | Generated table types and client     |
| `app/Main.hs`           | The app                              |
| `migrations/`           | Hand-written SQL, applied at startup |

This package isn't published to Hackage. Its `cabal.project` points at `../../poppy` and `../../poppy-codegen` by path. In your own app, pull both packages from git with a `source-repository-package` stanza until they're on Hackage.
