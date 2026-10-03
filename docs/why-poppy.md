# Why Poppy

Poppy aims at Prisma 2.0-shaped DX in Haskell: one Schema, a generated Client, nested reads and writes, and a short path from empty repo to a relation include. It is not Prisma-complete and not a general SQL compiler.

What that means in practice:

- **Schema is Haskell**, not a separate DSL file. Combinators live in `Poppy.Codegen.Schema`. GHC type-checks the Schema module; Codegen validates names, keys, and relations.
- **The Client is generated.** Application code calls `Task.findMany`, `Author.createNested`, and so on. You do not hand-write `Entity` instances.
- **Includes are records** (`AuthorInclude {posts = True}`), loaded with extra `IN` queries and nested in memory. That is explicit and limited: no filtered includes in 1.0.
- **SQL stays yours.** Migrations are files you write. `--check-schema` tells you when the Schema and Postgres disagree. `applyMigrations` records what ran.
- **Queries are text plus bound parameters**, not a typed query AST you extend. Escape hatch: `queryRaw` / `JoinChain`.

Compared with Persistent (models and migrations in Template Haskell), Diesel (Rust, query builder as the center), or Drizzle (SQL-first schema in TypeScript), Poppy puts the generated Client in front and treats SQL schema as a drift-checked artifact. If you want a full query builder or generated migrations, this is the wrong tool.
