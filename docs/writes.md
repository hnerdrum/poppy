# Writes

Every write returns `Db (Either ORMError …)`. Postgres unique, foreign-key, and not-null failures map to `UniqueViolation`, `ForeignKeyViolation`, and `NotNullViolation`. See [Errors](errors.md).

| Function     | You pass                                        | Success value         |
| ------------ | ----------------------------------------------- | --------------------- |
| `create`     | `TaskCreate`                                    | `TaskRow`             |
| `createMany` | `[TaskCreateScalars]` (or `TaskCreate`)         | `Int` (rows inserted) |
| `update`     | `TaskUnique`, `TaskUpdate`                      | `TaskRow`             |
| `updateMany` | `Where`, `TaskUpdateScalars`                    | `Int`                 |
| `upsert`     | `TaskUniqueKey`, create scalars, update scalars | `TaskRow`             |
| `delete`     | `TaskUnique`                                    | `Int` (rows deleted)  |
| `deleteMany` | `Where`                                         | `Int`                 |

`update` and `delete` take a unique key (`Task.ById id`), not a raw primary-key value. `upsert` takes a conflict target (`Task.OnId`, `Post.OnTitle`, …).

Without `hasMany` relations, create and update are scalar-only and `createMany` / `upsert` / `updateMany` use those same types. With relations, `createMany` and `upsert` take `*CreateScalars` / `*UpdateScalars` instead.

## Nested writes

Each `hasMany` relation is a field on `create` and `update`:

- On `create`: `[CreateChild {…} | ConnectChild ChildUnique]` (default `[]`)
- On `update`: `Maybe RelUpdate` (default `Nothing`; leave it to skip that relation)

```haskell
Author.create
  Author.AuthorCreate
    { id = Nothing,
      name = "Ada",
      posts =
        [ Author.CreatePost {id = Nothing, title = "Notes", status = Draft},
          Author.CreatePost {id = Nothing, title = "Essay", status = Published}
        ]
    }
```

`create` and `update` open a transaction only when a relation field is set.

A `RelUpdate` (such as `PostsUpdate`) has:

- `replaceWith` — delete this parent's children, then insert the list
- `create` / `createMany` — insert children (no silent `ON CONFLICT DO NOTHING`)
- `connect` — attach existing children by unique key
- `delete` — delete children of this parent by unique key
- `update` — `(ChildUnique, ChildUpdate)` partial updates, scoped to this parent
- `upsert` — `{ where_, create, update }`; conflict stays on this parent (won't steal another parent's child)
- `disconnect` — only when the child FK is nullable; sets it to `NULL`, does not delete the row

Use `emptyPostsUpdate` (or the matching empty helper) and record-update the ops you want.
