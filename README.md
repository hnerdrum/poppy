# Poppy

Postgres-first Haskell ORM: write a **Schema**, run **Codegen** to produce table types and a **Client**, then query and write through that Client at runtime.

This repository contains two packages:

- [`poppy`](poppy/) — runtime (`ORM.*` modules)
- [`poppy-codegen`](poppy-codegen/) — Schema builder, file generation, drift-check

The library modules are still named `ORM.*`. They will be renamed to `Poppy.*` in a later pass. A generic getting-started example will follow.

## Build

Cabal is the source of truth:

```bash
cabal build all
```

A `stack.yaml` pinned to LTS 21.22 (GHC 9.4.8) is included for Stack consumers.

## Test

Postgres is required. From the repo root:

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is accepted as a fallback if `TEST_DATABASE_URL` is unset.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
