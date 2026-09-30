# Versioning

`poppy` (runtime) and `poppy-codegen` stay at the **same version** and are intended to be bumped together. Mixing versions is not supported.

Current version: `0.1.0.0`. Tested GHC: **9.4.8–9.10.3**.

## What 1.0 is

- Schema + generated table types and Client
- Nested reads (include records) and nested writes (`Set` / `Ops`)
- Hand-written SQL, `applyMigrations`, `--check-schema`
- Scalars: `uuid`, `text`, `int`, `numeric`, `jsonb`, `bool`, `timestamptz`, Schema enums
- Docs in this directory plus Haddock on `Poppy` and `Poppy.Codegen.Schema`

## What 1.0 is not

- Generated migrations or down migrations
- Filtered includes (per-relation `where_` / `orderBy` / `take`)
- `connectOrCreate`
- Multi-row `INSERT` for `createMany` (it is sequential inserts in one transaction)
- Databases other than Postgres
- A `withPosts`-style include combinator API (use include records)

Breaking Schema or Client shape will bump the major version once 1.0 is tagged. Until then, treat `0.1.x` as the preview of that bar.
