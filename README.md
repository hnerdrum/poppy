# Poppy

[![CI](https://github.com/hnerdrum/poppy/actions/workflows/ci.yml/badge.svg)](https://github.com/hnerdrum/poppy/actions/workflows/ci.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Poppy is a Postgres ORM for Haskell. You describe tables in a schema, generate a client, then query and write through that client.

The generator and the runtime are separate Cabal packages. [`poppy-codegen`](poppy-codegen/) is for generating the client, while [`poppy`](poppy/) is the runtime library.

All the example code below is from the example folder in [`examples/task/`](examples/task/). There are also a number of guides in [docs/](docs/).

## Getting started

### 1. Write a Schema

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

Table and column names default from the model and field names (`Task` → `task`, `createdAt` → `created_at`). You can override the default names with `& table "…"` or `& column "…"` when needed.

### 2. Generate

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

`--check-schema` connects to Postgres via `DATABASE_URL`, or via `--database-url URL`, and fails if tables, columns, or keys do not match the schema. See [Migrations and drift](docs/migrations-and-drift.md) for more details.

### 3. Query with the Client

```haskell
import Data.UUID (UUID)
import Poppy (Db, ORMError)
import Poppy.Where (eq)
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

## Migrations

Poppy does not generate SQL from the Schema, so you write the .sql files for migrations yourself. To apply migrations, you call the `applyMigrations` runner before the app starts:

```haskell
applied <- applyMigrations pool "migrations"
```

[Migrations and drift](docs/migrations-and-drift.md) explains this process in more detail.

## Build and test

These packages build with Cabal. Supported compilers are GHC 9.4.8 through 9.10.3.

```bash
cabal build all
```

Tests need a running Postgres instance. The compose file in this repo starts one on port 5435:

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

## Examples

- [`examples/task/`](examples/task/) is a minimal, single-table example.
- [`examples/blog/`](examples/blog/) is a more involved example, including table relations.

From the repo root, after `docker compose up -d`:

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
(cd examples/task && cabal run task)
(cd examples/blog && cabal run blog)
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
