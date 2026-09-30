# Writes

Client write functions return `Db (Either ORMError …)`. Constraint failures become `UniqueViolation`, `ForeignKeyViolation`, or `NotNullViolation`. See [Errors](errors.md).

## Flat writes

| Function     | Arguments                  | Result                |
| ------------ | -------------------------- | --------------------- |
| `create`     | `TaskCreate`               | `TaskRow`             |
| `createMany` | `[TaskCreate]`             | `Int` (rows inserted) |
| `update`     | primary key, `TaskUpdate`  | `TaskRow`             |
| `updateMany` | `Where`, `TaskUpdate`      | `Int`                 |
| `upsert`     | `TaskCreate`, `TaskUpdate` | `TaskRow`             |
| `delete`     | primary key                | `TaskRow`             |
| `deleteMany` | `Where`                    | `Int`                 |

`createMany` inserts **one row at a time** inside a single transaction, not a multi-row `INSERT`. An empty list succeeds with `0`.

Create fields that have Schema defaults are `Maybe` (`Nothing` omits the column so Postgres fills `DEFAULT`). `updatedAt` columns are set to now on update.

`upsert` runs `INSERT … ON CONFLICT (cols) DO UPDATE`. Conflict columns are the **first** `unique_` on that model, or the primary key if there is none. The update side uses the `TaskUpdate` payload, not `EXCLUDED`. If the update sets no columns, Poppy still assigns the conflict columns to themselves so `RETURNING` yields a row.

`updateMany` / `deleteMany` with no `Where` fail with `EmptyWhere`.

## Nested writes

Generated on models that `hasMany` children. `createNested` / `updateNested` take an include record (what to return) and a write payload.

```haskell
Author.createNested
  Author.AuthorInclude {posts = True}
  Author.AuthorWriteCreate
    { root = Author.AuthorCreate {id = Nothing, name = "Ada"},
      posts =
        Author.Set
          [ Author.PostNestedCreate {id = Nothing, title = "Notes", status = Draft},
            Author.PostNestedCreate {id = Nothing, title = "Essay", status = Published}
          ]
    }
```

For each `hasMany` edge:

- `Set xs` — delete every child with this parent's foreign key, then insert `xs`
- `Ops o` — `create`, `createMany`, `connect`, `disconnect`, `delete`, `update`, `upsert` on that relation

`emptyPostNestedOps` is all empty lists. Nested `createMany` is sequential inserts (and `ON CONFLICT DO NOTHING` when the child has a unique). Nested `upsert` conflicts on the child's first unique, or inserts with no conflict clause when there is none.

There is no `connectOrCreate`. `connect` sets the child's foreign key to this parent. `disconnect` and `delete` both delete those child rows.

The whole nested write runs in one transaction (`transactionEither`: a `Left` rolls back).
