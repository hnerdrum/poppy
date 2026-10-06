# Schema

A Schema is Haskell code with three separate parts: enums, models, and unique constraints. Codegen then turns it into table types (`Schema.Task`) and a Client (`Schema.Client.Task`).

```haskell
schema
  [enum_ "ArticleStatus" [variant "Draft", variant "Published"]]
  [authorModel, postModel]
  [unique_ "Post" ["title"]]
```

## Models and fields

```haskell
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

The first argument is the model name. By default the table is that name in snake_case (`Task` → `task`), and each field becomes a snake_case column (`createdAt` → `created_at`). Use `& table "tasks"` or `& column "heading"` when the database already uses different names.

| Helper                               | Postgres                      | Haskell type       |
| ------------------------------------ | ----------------------------- | ------------------ |
| `uuid`                               | `uuid`                        | `UUID`             |
| `text`                               | `text`                        | `Text`             |
| `int`                                | `integer`                     | `Int`              |
| `numeric`                            | `numeric`                     | `Scientific`       |
| `jsonb`                              | `jsonb`                       | `aeson` `Value`    |
| `bool`                               | `boolean`                     | `Bool`             |
| `timestamptz`                        | `timestamp with time zone`    | `UTCTime`          |
| `enumField "status" "ArticleStatus"` | enum type named in the Schema | generated sum type |

You can chain extra options onto a field:

- `pk` marks the primary key. A model needs exactly one.
- `nullable` makes the row field a `Maybe`. On create and update you pass `Omit`, `Value x`, or `Null`.
- `updatedAt` is filled with the current time whenever the Client writes the row.
- `withDefault DefaultUuidV4` or `withDefault DefaultNow` makes the create field a `Maybe` (`Nothing` leaves the column out so Postgres can apply its `DEFAULT`). `--check-schema` expects that default to be `uuid_generate_v4()` / `gen_random_uuid()`, or `now()` / `CURRENT_TIMESTAMP`.

## Enums

```haskell
enum_ "ArticleStatus" [variant "Draft", variant "Published"]
```

Each variant becomes a Haskell constructor. Postgres gets the same spelling unless you use `variantMap "Draft" "draft"`. If you already have a sum type, `enumImportFrom "My.Module"` imports that instead of generating one.

You still have to create the type in SQL (`CREATE TYPE articlestatus AS ENUM (...)`).

## Relations

```haskell
hasMany "posts" "Post" "authorId"
belongsTo "author" "Author" "authorId"
```

`hasMany` means the other table has a foreign key (`authorId` on `Post`) that points at this model's primary key. `belongsTo` is the other direction: this model has the foreign key.

Add `hasMany` on `Author` when you want to create posts with the author, or load an author with their posts. Add `belongsTo` on `Post` when you want to load a post's author. [Relations](relations.md) covers how that loading works.

The include and nested-write field is the relation name. `hasMany "posts" …` on `Author` becomes `AuthorInclude { posts = True }`. Two relations from one model to the same target are fine when the names differ (`writtenPosts` and `editedPosts`). A name used twice on one model, or a name equal to a scalar field on that model, fails validation.

## Uniques

```haskell
unique_ "Post" ["title"]
```

Pass the Schema field names (`title`), not the Postgres column. Each unique, including the primary key, becomes a constructor on `PostUnique` (`ById`, `ByTitle`) and `PostUniqueKey` (`OnId`, `OnTitle`). `findUnique` uses `uniqueQuery (Post.ByTitle "Notes")`. `update` and `delete` take `PostUnique`. `upsert` takes `PostUniqueKey` and conflicts on that key.

## Validation

`generate` and `--check` run these checks:

- Model, enum, and include names are unique.
- Every model has exactly one `pk`.
- Every enum has at least one variant, and variant names are unique.
- `enumField` names an enum that exists on the Schema.
- Each relation's `from` / `to` models exist, and the foreign-key field exists on the right model.
- Each `unique_` names a real model and at least one real field.
- The generated include tree has no duplicate edge on the same model and no unknown relation.
- Relation names are unique on a model, and none of them match a scalar field on that model.
