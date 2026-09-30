# Blog example

Author, Post, an enum, nested create, and includes. The single-table hello-world is [`examples/task/`](../task/).

From this directory:

```bash
cabal run blog-codegen
```

That writes table types and Clients under `src/Schema/` (`generate "src/Schema"`). Re-run after Schema changes. `--check` fails if those files are stale; `--check-schema` compares the Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`).

```bash
export DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal run blog
```

`blog` applies `migrations/*.sql` with `applyMigrations`, creates an author with two posts in one nested write, then lists authors including their posts.

## Layout

| Path                    | Role                                     |
| ----------------------- | ---------------------------------------- |
| `codegen/BlogSchema.hs` | Schema value                             |
| `codegen/Main.hs`       | `generate "src/Schema" blogSchema`       |
| `src/Schema/`           | Generated table types, includes, Clients |
| `app/Main.hs`           | Nested create + include query            |
| `migrations/`           | Hand-written SQL, applied at startup     |

This package is not on Hackage. It path-depends on `../../poppy` and `../../poppy-codegen` via `cabal.project`.
