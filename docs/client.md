# Client

Generated Clients live under `Schema.Client.<Model>`. Table types (`TaskRow`, field witnesses like `taskId`) come from `Schema.<Model>`.

```haskell
import qualified Schema.Client.Task as Task
import Schema.Task (TaskRow (..), taskDone, taskId)
```

Reads take a query record. Start from `emptyQuery` and override the bits you need:

```haskell
Task.findMany
  Task.emptyQuery
    { Task.where_ = Just (eq taskDone False),
      Task.orderBy_ = [desc taskId],
      Task.limit_ = Just 20,
      Task.offset_ = Just 0
    }
```

## Reads

| Function           | Result                        |
| ------------------ | ----------------------------- |
| `findMany`         | `[row]`                       |
| `findFirst`        | `Maybe row`                   |
| `findFirstOrFail`  | `Either ORMError row`         |
| `findUnique`       | `Either ORMError (Maybe row)` |
| `findUniqueOrFail` | `Either ORMError row`         |
| `count`            | `Int`                         |

`findUnique` and `findUniqueOrFail` take `uniqueQuery` with a generated unique key (`Task.ById`, `Post.ByTitle`, …). Compound uniques are one constructor with one argument per field. A non-unique filter does not type-check.

```haskell
Task.findUniqueOrFail (Task.uniqueQuery (Task.ById taskKey))
```

| Error                  | When                                                           |
| ---------------------- | -------------------------------------------------------------- |
| `RecordNotFound`       | `findUniqueOrFail` / `findFirstOrFail` found nothing           |
| `MultipleRecordsFound` | More than one row matched (DB out of sync with Schema uniques) |

`where_` on the query record filters the root table. To filter a loaded relation, record-update `where_`, `orderBy_`, and `take_` on `load` or `loadWith`. Combinators (`eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in_`, `contains`, `isNull`, `and_`, `or_`, `not_`) come from `Poppy`.

`orderBy_` is a list of `asc field` / `desc field`. `limit_` and `offset_` are `Maybe Int`.

## Select

Leave `select_` alone if you want every column. To pick columns, pass a `TaskSelect` and set the ones you want to `True`. The result is `TaskPicked`: each field is either `Picked value` or `Skipped`. If you also set `include_`, you get a combined type with the picked root plus the loaded relations.
