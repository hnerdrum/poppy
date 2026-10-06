# Relations

Tables can point at each other. An author has many posts; each post stores `authorId`. You declare that on the Schema with `hasMany` or `belongsTo`. After codegen, a Client query can load the related rows in the same call, instead of you fetching posts yourself and grouping them.

Set `include_` on the query when you want that. [`examples/blog/`](../examples/blog/) is a small app that loads authors with their posts.

```haskell
model
  "Author"
  [ uuid "id" & pk & withDefault DefaultUuidV4,
    text "name"
  ]
  [hasMany "posts" "Post" "authorId"]
```

`Post.authorId` points at `Author.id`. The include field is `posts`, the name you gave the relation.

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

`load` and `loadWith` fetch the related rows. Record-update `where_`, `orderBy_`, and `take_` on that value to filter, sort, or keep a per-parent number of children:

```haskell
AuthorInclude
  { posts =
      load
        { where_ = Just (eq postStatus Published),
          orderBy_ = [desc postTitle],
          take_ = Just 2
        }
  }
```

`take_` uses a per-parent window, so each author keeps two posts. `Nothing` keeps every match.
