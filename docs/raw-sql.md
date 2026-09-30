# Raw SQL

Use this when the Client does not express the query. Stay in `Db` so you share the pool connection and optional SQL log.

## `queryRaw` / `executeRaw`

```haskell
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy (Db, executeRaw, param, queryRaw)

titles :: Db [Only Text]
titles =
  queryRaw "SELECT title FROM task WHERE done = ?" [param False]

n :: Db Int
n =
  executeRaw "UPDATE task SET done = ? WHERE id = ?" [param True, param taskKey]
```

`param` wraps any `ToField`. Placeholders are `?` (`postgresql-simple`). `queryRaw` needs a `FromRow` result. `executeRaw` returns the affected row count.

These log the SQL text (not bound parameters) through `PoolConfig.poolSqlLog`. They do **not** catch driver errors; use `catchDb` for `Either ORMError`.

## `JoinChain`

`Poppy.JoinChain` builds a SELECT with explicit joins from `HasMany` / `BelongsTo` values (the same relation types Codegen emits on table modules). Start a chain, add joins, `selectColumns` / `selectFields`, `whereJoin`, `limitJoin`, then `runJoinChain`.

This is for ad-hoc SQL, not a replacement for include records. Includes stay N+1 + in-memory nest; `JoinChain` is one query you assemble yourself.

Identifier helpers `quoteIdent` and `quoteQualified` live in `Poppy.Sql` if you interpolate names (values still go through `param`).
