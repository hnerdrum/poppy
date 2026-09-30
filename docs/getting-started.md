# Getting started

Depend on `poppy` and `poppy-codegen` (path deps in this repo; Hackage after the first release). GHC 9.4.8–9.10.3.

The snippets below match [`examples/task/`](../examples/task/). For authors, posts, nested create, and includes see [`examples/blog/`](../examples/blog/) and [Relations](relations.md).

## 1. Write SQL

Poppy does not generate migrations. Put `.sql` files in a directory and apply them at startup with `applyMigrations`. Names are sorted; only `.sql` files that are not already in `_poppy_migrations` run.

```sql
CREATE TABLE IF NOT EXISTS task (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  title TEXT NOT NULL,
  done BOOLEAN NOT NULL
);
```

## 2. Write a Schema

```haskell
{-# LANGUAGE OverloadedStrings #-}

module TaskSchema
  ( taskSchema,
  )
where

import Poppy.Codegen.Schema

taskSchema :: Schema
taskSchema =
  schema [] [taskModel] []

taskModel :: Model
taskModel =
  model
    "Task"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      timestamptz "createdAt" & withDefault DefaultNow,
      timestamptz "updatedAt" & withDefault DefaultNow & updatedAt,
      text "title",
      bool "done"
    ]
    []
```

Table and column names default from Haskell names (`Task` → `task`, `createdAt` → `created_at`). Override with `& table "…"` or `& column "…"` when needed. Field combinators are in [Schema](schema.md).

## 3. Generate

A small executable:

```haskell
module Main (main) where

import Poppy.Codegen.CLI (generate)
import TaskSchema (taskSchema)

main :: IO ()
main = generate "src/Schema" taskSchema
```

```bash
cabal run task-codegen
```

That writes `Schema.Task` and `Schema.Client.Task` under `src/Schema/`. The module prefix is the output path with source-dir segments dropped (`src/Schema` → `Schema`). Re-run after Schema changes.

| Flag             | Purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files                              |
| `--check`        | Fail if generated files on disk differ from Codegen                     |
| `--check-schema` | Compare Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`) |
| `--list`         | Print output paths without writing                                      |

`--check-schema` needs a database URL. Haskell IR is the source of truth: if Postgres disagrees, fix the SQL or the Schema. See [Migrations and drift](migrations-and-drift.md).

## 4. Query with the Client

```haskell
import Poppy (Db, ORMError, applyMigrations, closePool, connect, runDb)
import Poppy.Where (eq)
import qualified Schema.Client.Task as Task
import Schema.Task (TaskCreate (..), TaskRow (..), taskDone, taskId)

listOpenTasks :: Db [TaskRow]
listOpenTasks =
  Task.findMany
    Task.emptyQuery {Task.where_ = Just (eq taskDone False)}

getTask :: UUID -> Db (Either ORMError TaskRow)
getTask taskKey =
  Task.findUniqueOrFail
    Task.emptyQuery {Task.where_ = Just (eq taskId taskKey)}
```

`findUnique` / `findUniqueOrFail` require a `where_` that is exactly a primary key or a unique declared on the Schema. Other reads and writes are in [Client](client.md) and [Writes](writes.md).

## 5. Connect and migrate

```haskell
main :: IO ()
main = do
  url <- getEnv "DATABASE_URL"
  pool <- connect url
  applied <- applyMigrations pool "migrations"
  case applied of
    Left err -> print err
    Right _ -> do
      open <- runDb pool listOpenTasks
      print (length open)
  closePool pool
```

`connect` uses `defaultPool`. Tune stripes, idle time, and SQL logging with `connectWith`. Bound parameters are not logged.

From this repo, after `docker compose up -d`:

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
(cd examples/task && cabal run task)
```
