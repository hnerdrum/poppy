# Raw SQL

Poppy offers the `queryRaw` and `executeRaw` functions when you want to write pure SQL queries instead of using the generated client. These functions run on the same `Db` connection as the Client.

```haskell
import Database.PostgreSQL.Simple (Only (..))
import Poppy (Db, executeRaw, param, queryRaw)

titles :: Db [Only Text]
titles =
  queryRaw "SELECT title FROM task WHERE done = ?" [param False]

n :: Db Int
n =
  executeRaw "UPDATE task SET done = ? WHERE id = ?" [param True, param taskKey]
```

Each `?` is filled by the matching `param`. `queryRaw` decodes each row as the result type (`Only Text` above). `executeRaw` returns how many rows changed.

If Postgres rejects the statement, these throw. Wrap with `catchDb` to get `Either ORMError` instead.

If you set `poolSqlLog` when you create the pool, the SQL string is logged. The `param` values are not.
