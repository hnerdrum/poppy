# Getting started

The snippets below are from [`examples/task/`](../examples/task/). [`examples/blog/`](../examples/blog/) adds table relations; see [Relations](relations.md).

## 1. Write a Schema

```haskell
{-# LANGUAGE OverloadedStrings #-}

module TaskSchema
  ( taskModel,
    taskSchema,
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

Table and column names default from the model and field names (`Task` → `task`, `createdAt` → `created_at`). You can override the default names with `& table "…"` or `& column "…"` when needed. The full list of column types is in [Schema](schema.md).

## 2. Generate

Call `generate` from an executable to generate the client from the above schema:

```haskell
module Main (main) where

import Poppy.Codegen.CLI (generate)
import TaskSchema (taskSchema)

main :: IO ()
main = generate "src/Schema" taskSchema
```

`generate` takes an output directory and the schema value. In the Task example that executable is `task-codegen`:

```bash
cabal run task-codegen
```

The executable accepts the following flags:

| Flag             | Purpose                                             |
| ---------------- | --------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files          |
| `--check`        | Fail if generated files on disk differ from Codegen |
| `--check-schema` | Compare Schema to live Postgres database            |
| `--list`         | Print output paths without writing                  |

`--check-schema` reads `DATABASE_URL`. Pass `--database-url URL` to override it. The check fails if tables, columns, or keys do not match the schema. See [Migrations and drift](migrations-and-drift.md).

## 3. Query with the Client

```haskell
import Data.UUID (UUID)
import Poppy (Db, ORMError, eq)
import qualified Schema.Client.Task as Task
import Schema.Task (TaskRow (..), taskDone)

listOpenTasks :: Db [TaskRow]
listOpenTasks =
  Task.findMany
    Task.emptyQuery {Task.where_ = Just (eq taskDone False)}

getTask :: UUID -> Db (Either ORMError TaskRow)
getTask taskKey =
  Task.findUniqueOrFail (Task.uniqueQuery (Task.ById taskKey))
```

More of the read and write API is in [Client](client.md) and [Writes](writes.md), respectively.

## 4. Migrations

Poppy does not generate SQL from the Schema, so you write the `.sql` files for migrations yourself. To apply migrations, you call the `applyMigrations` runner before the app starts:

Add a `001-task.sql` file to the `migrations` folder:

```sql
CREATE TABLE IF NOT EXISTS task (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  title TEXT NOT NULL,
  done BOOLEAN NOT NULL
);
```

The Task app connects, applies the file, then runs the queries:

```haskell
import Poppy (applyMigrations, closePool, connect, runDb)
import System.Environment (getEnv)

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

[Migrations and drift](migrations-and-drift.md) explains this process in more detail.

From this repo, after `docker compose up -d`:

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
(cd examples/task && cabal run task)
```
