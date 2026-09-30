# Relations

Declare relations on the Schema, generate, then pass an include **record** on the query. There are no `withPosts`-style helpers: the record is the API.

```haskell
model
  "Author"
  [ uuid "id" & pk & withDefault DefaultUuidV4,
    text "name"
  ]
  [hasMany "authorPosts" "Post" "authorId"]
```

`hasMany "authorPosts" "Post" "authorId"` means `Post.authorId` points at `Author.id`. The include field is `posts` (owner prefix stripped). Name the relation so that strip is the field you want.

`belongsTo "author" "Author" "authorId"` on `Post` is the inverse, for loading the parent from a child. Nested writes and parent→children includes need `hasMany` on the parent. Declare the direction you will query.

Worked example: [`examples/blog/`](../examples/blog/).

## Include records

Leaf edges are `Bool`. Nested edges are `Maybe ChildInclude`.

```haskell
Author.findMany
  Author.emptyQuery {Author.include_ = Author.AuthorInclude {posts = True}}
```

`emptyQuery` uses `noInclude` (`NoInclude` wrapping the include type) so the result is a plain `AuthorRow`. Setting `include_ = AuthorInclude {posts = True}` changes the result to `AuthorWithPosts { author :: AuthorRow, posts :: [PostRow] }`.

Deeper graphs (shelf → books → chapters) look like:

```haskell
ShelfInclude
  { books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}),
    tags = True
  }
```

`True` / `Just …` loads that edge for **every** parent in the result. There is no per-relation `where_`, `orderBy_`, or `take` in 1.0.

## How includes load

Each included edge is a follow-up query: `WHERE foreign_key IN (…parent ids…)`, then assembly in memory. That is N+1 (one root query plus one per included edge), not a SQL `JOIN`. Root `where_`, `orderBy_`, `limit_`, and `offset_` do not apply to children.

`where_` on the query record is still only the root model.

## Nested writes

`createNested` / `updateNested` take the same include type so the returned graph matches what you asked to load. Child payloads use `Set` (replace all children) or `Ops` (create, connect, disconnect, delete, update, upsert). See [Writes](writes.md).
