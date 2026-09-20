# poppy-codegen

Schema builder and Client codegen for [Poppy](https://github.com/hnerdrum/poppy).

Each app has a codegen executable:

```haskell
import Poppy.Codegen.CLI (generate)
import TaskSchema (taskSchema)

main :: IO ()
main = generate "src/Schema" taskSchema
```

The module prefix follows the output path (`src/Schema` → `Schema`). Clients are written to `<dir>/Client`.

| Flag             | Purpose                                                                 |
| ---------------- | ----------------------------------------------------------------------- |
| _(none)_         | Validate Schemas and write generated files                              |
| `--check`        | Fail if files on disk differ from Codegen output                        |
| `--check-schema` | Compare Schema to live Postgres (`TEST_DATABASE_URL` or `DATABASE_URL`) |
| `--list`         | Print output paths without writing                                      |

Setup is in the [repository README](https://github.com/hnerdrum/poppy#readme) and [`examples/task/`](https://github.com/hnerdrum/poppy/tree/main/examples/task).
