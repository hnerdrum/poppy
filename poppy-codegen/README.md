# poppy-codegen

The code generator for [Poppy](https://github.com/hnerdrum/poppy), a Postgres ORM for Haskell. You describe tables with `Poppy.Codegen.Schema`, then call `generate` from an executable to write table types and a client for the [`poppy`](https://github.com/hnerdrum/poppy/tree/main/poppy) runtime.

```haskell
module Main (main) where

import Poppy.Codegen.CLI (generate)
import TaskSchema (taskSchema)

main :: IO ()
main = generate "src/Schema" taskSchema
```

`generate` takes an output directory and the schema value. The directory sets the module prefix, so `src/Schema` produces modules under `Schema`, with the clients in `src/Schema/Client`.

The executable accepts the following flags:

| Flag             | Purpose                                             |
| ---------------- | --------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files          |
| `--check`        | Fail if generated files on disk differ from Codegen |
| `--check-schema` | Compare Schema to live Postgres database            |
| `--list`         | Print output paths without writing                  |

`--check-schema` reads `DATABASE_URL`. Pass `--database-url URL` to override it. The check fails if tables, columns, or keys do not match the schema.

The [repository README](https://github.com/hnerdrum/poppy#readme) shows the full setup. For working code, see [`examples/task/`](https://github.com/hnerdrum/poppy/tree/main/examples/task) and [`examples/blog/`](https://github.com/hnerdrum/poppy/tree/main/examples/blog). The [guides](https://github.com/hnerdrum/poppy/tree/main/docs) cover the schema builder and migrations in more depth.
