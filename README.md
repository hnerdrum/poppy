# Poppy

[![CI](https://github.com/hnerdrum/poppy/actions/workflows/ci.yml/badge.svg)](https://github.com/hnerdrum/poppy/actions/workflows/ci.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Poppy is a Postgres ORM for Haskell. You write a Schema, generate table types and a Client, then query and write through that Client.

Two packages:

- [`poppy`](poppy/) (runtime, `Poppy.*`)
- [`poppy-codegen`](poppy-codegen/) (Schema builder, file generation, drift-check)

Snippets below are from [`examples/task/`](examples/task/).

## Getting started

Depend on `poppy` and `poppy-codegen` (path deps in this repo; Hackage after the first release). GHC 9.4.8.

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

Table and column names default from the model and field names (`Task` → `task`, `createdAt` → `created_at`). Override with `& table "…"` or `& column "…"` when needed.

### 2. Generate

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

That writes `Schema.Task` and `Schema.Client.Task` under `src/Schema/`. The module prefix is the output path with source-dir segments dropped (`src/Schema` → `Schema`).

| Flag             | Purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files                              |
| `--check`        | Fail if generated files on disk differ from Codegen                     |
| `--check-schema` | Compare Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`) |
| `--list`         | Print output paths without writing                                      |

### 3. Query with the Client

```haskell
listOpenTasks :: Db [TaskRow]
listOpenTasks =
  Task.findMany
    Task.emptyQuery {Task.where_ = Just (eq taskDone False)}

getTask :: UUID -> Db (Either ORMError TaskRow)
getTask taskKey =
  Task.findUniqueOrFail
    Task.emptyQuery {Task.where_ = Just (eq taskId taskKey)}
```

`findUnique` / `findUniqueOrFail` require a `where_` that matches exactly one row: a primary key or a unique constraint on the Schema.

Apply the example migration, then:

```bash
psql "$DATABASE_URL" -f migrations/001-task.sql
cabal run task
```

## Drift-check and migrations

Codegen does not run migrations.

1. Hand-written SQL migrations change the database.
2. `--check-schema` introspects live Postgres and reports drift.
3. Fix the Schema or the SQL, whichever side is wrong, then re-run.

## Limits

- Postgres only
- `where_` applies to the root model, not nested relations
- Include combinators select whole relations, not subsets
- Queries are assembled as text with bound parameters
- Scalars: uuid, text, int, bool, timestamptz, and Schema-defined enums

## Build and test

Build with Cabal. `stack.yaml` is pinned to LTS 21.22 (GHC 9.4.8) for Stack.

```bash
cabal build all
```

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is used if `TEST_DATABASE_URL` is unset.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
