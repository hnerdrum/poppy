# Client

Generated Clients live under `Schema.Client.<Model>`. Import the Client qualified and the table types (`TaskRow`, field witnesses like `taskId`) from `Schema.<Model>`.

```haskell
import qualified Schema.Client.Task as Task
import Schema.Task (TaskRow (..), taskDone, taskId)
```

Every read takes a query record. Start from `emptyQuery` and override fields.

```haskell
Task.findMany
  Task.emptyQuery
    { Task.where_ = Just (eq taskDone False),
      Task.orderBy_ = [desc taskId],
      Task.limit_ = Just 20,
      Task.offset_ = Just 0
    }
```

`emptyQuery` has no filter, no order, no limit/offset, `select_ = OmitSelect` (full row), and `include_ = noInclude` when the model has relations.

## Reads

| Function           | Result                                              |
| ------------------ | --------------------------------------------------- |
| `findMany`         | `[row]` (empty list is success)                     |
| `findFirst`        | `Maybe row`                                         |
| `findFirstOrFail`  | `Either ORMError row` (`RecordNotFound` if missing) |
| `findUnique`       | `Either ORMError (Maybe row)`                       |
| `findUniqueOrFail` | `Either ORMError row`                               |
| `count`            | `Int`                                               |

`findUnique` / `findUniqueOrFail` require `where_` to be equalities on **exactly one** unique key: the primary key, or a `unique_` from the Schema. Incomplete keys, non-equality predicates, and filters that are not unique fail with `InvalidUniqueInput`. Multiple matching rows fail with `MultipleRecordsFound`.

`where_` is only the root table. Nested includes are not filtered. Combinators (`eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `in_`, `contains`, `isNull`, `and_`, `or_`, `not_`) come from `Poppy` / `Poppy.Where`.

`orderBy_` is a list of `asc field` / `desc field`. `limit_` and `offset_` are `Maybe Int`.

## Select

Leave `select_ = OmitSelect` for a full `TaskRow`. To pick columns, pass the generated `TaskSelect` record (`True` keeps the column). The result type becomes `TaskPicked` (`Picked a` or `Skipped` per field). Mixing include and select uses a combined picked-with-relations type.

Writes (`create`, `update`, …) are in [Writes](writes.md). Includes are in [Relations](relations.md).
