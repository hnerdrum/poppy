# Poppy

Postgres-first Haskell ORM: write a **Schema**, run **Codegen** to produce table types and a **Client**, then query and write through that Client at runtime.

This repository contains two packages:

- [`poppy`](poppy/) — runtime (`ORM.*` modules)
- [`poppy-codegen`](poppy-codegen/) — Schema builder, file generation, drift-check

The library modules are still named `ORM.*`. They will be renamed to `Poppy.*` in a later pass. Documentation, tests, and a generic getting-started example will follow.

## Build

Cabal is the source of truth:

```bash
cabal build all
```

A `stack.yaml` pinned to LTS 21.22 (GHC 9.4.8) is included for Stack consumers.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
