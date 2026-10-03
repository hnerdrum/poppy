# Relations

Tables can point at each other. An author has many posts; each post stores `authorId`. You declare that on the Schema with `hasMany` or `belongsTo`. After codegen, a Client query can load the related rows in the same call, instead of you fetching posts yourself and grouping them.

Set `include_` on the query when you want that. [`examples/blog/`](../examples/blog/) is a small app that loads authors with their posts.

```haskell
model
  "Author"
  [ uuid "id" & pk & withDefault DefaultUuidV4,
    text "name"
  ]
  [hasMany "authorPosts" "Post" "authorId"]
```

`Post.authorId` points at `Author.id`. The include field is `posts` because Codegen drops the `author` prefix from `authorPosts`.

## Include records

```haskell
Author.findMany
  Author.emptyQuery {Author.include_ = Author.AuthorInclude {posts = True}}
```

`emptyQuery` uses `noInclude`, so you get `[AuthorRow]`. Set `include_` to `AuthorInclude {posts = True}` and the result type becomes `AuthorWithPosts`: an `author` row plus a `posts` list.

`posts = True` is enough when you only want that list. To go further (books, then chapters), wrap the next include in `Just`:

```haskell
ShelfInclude
  { books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}),
    tags = True
  }
```

`True` or `Just …` loads that relation for every parent in the result. You cannot attach a `where_`, `orderBy_`, or `take` to a child.
