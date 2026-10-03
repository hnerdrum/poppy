# Writes

These return `Db (Either ORMError …)`. Unique, foreign-key, and not-null failures from Postgres show up as `UniqueViolation`, `ForeignKeyViolation`, and `NotNullViolation`. See [Errors](errors.md).

| Function     | You pass                   | You get               |
| ------------ | -------------------------- | --------------------- |
| `create`     | `TaskCreate`               | `TaskRow`             |
| `createMany` | `[TaskCreate]`             | `Int` (rows inserted) |
| `update`     | primary key, `TaskUpdate`  | `TaskRow`             |
| `updateMany` | `Where`, `TaskUpdate`      | `Int`                 |
| `upsert`     | `TaskCreate`, `TaskUpdate` | `TaskRow`             |
| `delete`     | primary key                | `TaskRow`             |
| `deleteMany` | `Where`                    | `Int`                 |

## Nested writes

If a model `hasMany` children, the Client also has `createNested` and `updateNested`. These functions take two arguments:

1. An include record specifying what to load back after the operation
2. The write operation itself

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

Each `hasMany` field on the payload is either:

- `Set xs` — delete every child that points at this parent, then insert `xs`
- `Ops o` — run the lists on `o`: `create`, `createMany`, `connect`, `disconnect`, `delete`, `update`, `upsert`
