# Relations

Tables can point at each other. An author has many posts; each post stores `authorId`. You declare that on the Schema with `hasMany` or `belongsTo`. After codegen, a Client query can load the related rows in the same call.

Set `include_` on the query when you want that. [`examples/blog/`](../examples/blog/) loads authors with their posts.

```haskell
model
  "Author"
  [ uuid "id" & pk & withDefault DefaultUuidV4,
    text "name"
  ]
  [hasMany "posts" "Post" "authorId"]
```

`Post.authorId` points at `Author.id`. The include field is `posts`, the name you gave the relation.

## Include shape

Each model with relations gets `Schema.Include.<Model>`. That module is shared: a shelf's books and `Book.findMany` with an include use the same `BookInclude` / `BookWith` types.

Edges can be either `skip`, `load`, or `loadWith`:

```haskell
import Poppy (load, loadWith, skip)
import Schema.Include.Author (AuthorInclude (..))
import Schema.Include.Shelf (ShelfInclude (..))
import Schema.Include.Book (BookInclude (..))

Author.findMany
  Author.emptyQuery
    { Author.include_ = AuthorInclude {posts = load}
    }

ShelfInclude
  { books = loadWith BookInclude {chapters = load},
    tags = skip
  }
```

`emptyQuery` leaves `include_ = ()`, so you get plain rows (`[AuthorRow]`). With an include record, the result type follows what you loaded: `AuthorWith posts`, and so on.

- `skip` omits that relation. Reading it is a type error (`Skipped "posts" …`), not an empty list or `Nothing`.
- `load` fetches the related rows and stops there.
- `loadWith` nests another include on the child.

A required `belongsTo` is the parent row type (`AuthorRow`), not `Maybe`. A nullable `belongsTo` is `Maybe`.

Include types are recursive: `AuthorInclude.posts` leads to `PostInclude.author` and back. You cut the cycle with `skip` or by stopping at `load`.

Prefer constructor syntax for include records. Record update works when that field name is unique in scope; if another in-scope record shares the name, the update is ambiguous.

## Filters on an edge

Record-update `where_`, `orderBy_`, and `take_` on `load` or `loadWith`:

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

`take_` is per parent (windowed in SQL), so each author keeps two posts. `Nothing` keeps every match.
