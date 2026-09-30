# Schema

A Schema is Haskell: enums, models, and unique constraints. Codegen turns it into table types (`Schema.Task`) and a Client (`Schema.Client.Task`). Import combinators from `Poppy.Codegen.Schema`.

```haskell
schema
  [enum_ "ArticleStatus" [variant "Draft", variant "Published"]]
  [authorModel, postModel]
  [unique_ "Post" ["title"]]
```

The third argument is uniques that are not the primary key. Models that declare relations also get a full-graph Include (see [Relations](relations.md)).

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

`model "Task" fields relations` sets the table name to snake_case of the model name. Override with `& table "tasks"`. Field names become snake_case columns unless you `& column "…"`.

| Combinator                           | Postgres                      | Haskell            |
| ------------------------------------ | ----------------------------- | ------------------ |
| `uuid`                               | `uuid`                        | `UUID`             |
| `text`                               | `text`                        | `Text`             |
| `int`                                | `integer`                     | `Int`              |
| `numeric`                            | `numeric`                     | `Scientific`       |
| `jsonb`                              | `jsonb`                       | `aeson` `Value`    |
| `bool`                               | `boolean`                     | `Bool`             |
| `timestamptz`                        | `timestamp with time zone`    | `UTCTime`          |
| `enumField "status" "ArticleStatus"` | enum type named in the Schema | generated sum type |

Modifiers:

- `pk` — exactly one per model
- `nullable` — `Maybe` on the row; create/update use `NullableValue` (`Omit` / `Value` / `Null`)
- `updatedAt` — Client writes set this column to now
- `withDefault DefaultUuidV4` / `withDefault DefaultNow` — create input is `Maybe`; `--check-schema` requires a matching column `DEFAULT` (`uuid_generate_v4()` or `gen_random_uuid()`; `now()` or `CURRENT_TIMESTAMP`)

## Enums

```haskell
enum_ "ArticleStatus" [variant "Draft", variant "Published"]
```

Variant names are the Haskell constructors. The Postgres labels default to the same spelling. Use `variantMap "Draft" "draft"` when they differ. `enumImportFrom "My.Module"` skips generating the sum type and imports yours instead.

Create the Postgres type in a migration (`CREATE TYPE articlestatus AS ENUM (...)`). Enum type names are lowercased for drift-check.

## Relations

```haskell
hasMany "authorPosts" "Post" "authorId"
belongsTo "author" "Author" "authorId"
```

`hasMany name toModel foreignField` — the other table has `foreignField` pointing at this model's primary key.

`belongsTo name toModel foreignField` — this model has `foreignField` pointing at `toModel`'s primary key.

The relation `name` is not the include record field. Codegen strips the owner prefix when present (`authorPosts` on `Author` → include field `posts`); otherwise it uses the target model name. Declare the edges you will query: nested writes and parent→children includes need `hasMany` on the parent; child→parent includes need `belongsTo` on the child. See [Relations](relations.md).

## Uniques

```haskell
unique_ "Post" ["title"]
```

Field names, not columns. They become `uniqueKeys` on the generated `Entity` instance. `findUnique` `where_` must be equalities on the primary key or one of these. `upsert` conflicts on the **first** unique, or the primary key if there are none.

## Validation

`generate` and `--check` run `validateSchema` first (duplicate names, missing PK, unknown relation fields, empty uniques, include depth, and so on). Fix those before drift-check.
