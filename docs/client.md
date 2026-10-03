# Client

Generated Clients live under `Schema.Client.<Model>`. Table types (`TaskRow`, field witnesses like `taskId`) can be imported from `Schema.<Model>`.

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

`findUnique` / `findUniqueOrFail` require `where_` to be equalities on **exactly one** unique key: the primary key, or a `unique_` from the Schema. Incomplete keys, non-equality predicates, and filters that are not unique fail with `InvalidUniqueInput`. Multiple matching rows fail with `MultipleRecordsFound`.

`where_` is only the root table. Nested includes are not filtered. Combinators (`eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in_`, `contains`, `isNull`, `and_`, `or_`, `not_`) come from `Poppy` / `Poppy.Where`.

`orderBy_` is a list of `asc field` / `desc field`. `limit_` and `offset_` are `Maybe Int`.

## Select

Leave `select_` alone if you want every column. To pick columns, pass a `TaskSelect` and set the ones you want to `True`. The result is `TaskPicked`: each field is either `Picked value` or `Skipped`. If you also set `include_`, you get a combined type with the picked root plus the loaded relations.
