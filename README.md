# Poppy

[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Postgres-first Haskell ORM: write a **Schema**, run **Codegen** to produce table types and a **Client**, then query and write through that Client at runtime.

This repository contains two packages:

- [`poppy`](poppy/) — runtime (`Poppy.*` modules)
- [`poppy-codegen`](poppy-codegen/) — Schema builder, file generation, drift-check

A complete getting-started app lives in [`examples/task/`](examples/task/). Snippets below are taken from that example.

## Why Poppy

You describe tables as ordinary Haskell values, generate a per-model Client, and call `findMany` / `create` with a query record (`where_`, `orderBy_`, `limit_`, Includes). Nested reads are combinators on that record. The Schema is the source of truth for drift-check against Postgres.

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

### 2. Thin codegen executable

Each app owns a `CodegenTarget` and calls `mainWith`:

```haskell
module Main (main) where

import Poppy.Codegen.CLI (mainWith)
import TaskTarget (taskTarget)

main :: IO ()
main = mainWith [taskTarget] [taskTarget]
```

```haskell
taskTarget :: CodegenTarget
taskTarget =
  CodegenTarget
    { ctSchemas = [TargetSchema taskSchema False EmitAll],
      ctLayout = SchemaLayout "Schema." "src/Schema/",
      ctClients =
        DeriveClients
          ClientLayout
            { clModulePrefix = "Schema.Client.",
              clOutputDir = "src/Schema/Client/",
              clGolden = False
            }
    }
```

From [`examples/task/`](examples/task/):

```bash
cabal run task-codegen
```

| Flag             | Purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files                              |
| `--check`        | Fail if generated files on disk differ from Codegen                     |
| `--check-schema` | Compare Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`) |
| `--list`         | Print output paths without writing                                      |

`--check-migrations` is an alias for `--check-schema`.

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

`findUnique` / `findUniqueOrFail` require a `where_` that matches exactly one row — primary key or a unique constraint declared on the Schema.

Apply the example migration, then:

```bash
psql "$DATABASE_URL" -f migrations/001-task.sql
cabal run task
```

## Drift-check and migrations

**The Schema is the source of truth** for what Postgres should look like. Codegen does not run migrations. See [ADR 0001](docs/adr/0001-no-migration-generation.md).

1. Hand-written SQL migrations change the database.
2. `--check-schema` introspects live Postgres and reports drift.
3. Fix the Schema or the SQL — whichever side is wrong — then re-run.

## Limits

- **No built-in migration handling**
- **Postgres only**
- **No relation filters** — `where_` applies to the root model, not nested relations
- **No filtered Includes** — Include combinators select whole relations, not subsets
- **String-backed SQL** — queries are assembled as text with bound parameters
- **Limited scalars** — uuid, text, int, bool, timestamptz, and Schema-defined enums

## Compared to persistent, beam, and rel8

|                   | Poppy                           | persistent                             | beam                | rel8                 |
| ----------------- | ------------------------------- | -------------------------------------- | ------------------- | -------------------- |
| Model description | Haskell Schema values + codegen | Template Haskell / QuasiQuotes         | Haskell table types | Haskell table types  |
| Query style       | Generated Client + query record | Esqueleto / persistent queries         | Beam SQL DSL        | Rel8/Opaleye selects |
| Nested reads      | Include combinators             | Joins / esqueleto                      | Explicit joins      | Explicit selects     |
| Migrations        | Hand-written SQL + drift-check  | Auto-migrate or persistent-mysql style | Not in-tree         | Not in-tree          |
| Database          | Postgres only                   | Several backends                       | Several backends    | Postgres             |

Use persistent if you want auto-migrate and multi-backend. Use beam or rel8 if you want a typed SQL DSL without a generated Client. Use Poppy if you want a Schema → Client workflow on Postgres and are willing to write SQL migrations yourself.

## Build and test

Cabal is the source of truth. A `stack.yaml` pinned to LTS 21.22 (GHC 9.4.8) is included for Stack consumers.

```bash
cabal build all
```

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is accepted as a fallback if `TEST_DATABASE_URL` is unset.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
