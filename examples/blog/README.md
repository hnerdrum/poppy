# Blog example

Two related tables (an `Author` has many `Post`s) and an `ArticleStatus` enum. The app creates an author and two posts in one nested write, then loads every author with their posts. For a single-table version, see [`examples/task/`](../task/). The [Relations](../../docs/relations.md) and [Writes](../../docs/writes.md) guides explain the API used here.

Run the generator from this directory:

```bash
cabal run blog-codegen
```

It writes the table types, the enum, `Schema.Include.Author`, and a client per table under `src/Schema/`. Re-run it whenever you change `codegen/BlogSchema.hs`. Pass `--check` to fail when those files are out of date, or `--check-schema` to compare the schema against Postgres. The schema check uses `DATABASE_URL`, or the URL given to `--database-url`.

Start Postgres with `docker compose up -d` from the repo root, then run the app:

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal run blog
```

The app applies `migrations/*.sql` with `applyMigrations` before it writes anything.

## Layout

| Path                    | Contents                                    |
| ----------------------- | ------------------------------------------- |
| `codegen/BlogSchema.hs` | The schema                                  |
| `codegen/Main.hs`       | `generate "src/Schema" blogSchema`          |
| `src/Schema/`           | Generated table types, includes, and client |
| `app/Main.hs`           | Nested create and include query             |
| `migrations/`           | Hand-written SQL, applied at startup        |

This package isn't published to Hackage. Its `cabal.project` points at `../../poppy` and `../../poppy-codegen` by path.
