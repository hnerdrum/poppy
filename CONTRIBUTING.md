# Contributing

Poppy is two Cabal packages in one repo: `poppy` (runtime) and `poppy-codegen` (Schema builder and file generation). They stay lockstep at the same version.

## Build

GHC 9.4.8 (LTS 21.22). Cabal is the source of truth; `stack.yaml` is provided for Stack consumers.

```bash
cabal build all
```

`package.yaml` is edited by hand; regenerate `.cabal` files with `hpack` in each package directory after changing it. Commit both. CI fails if they drift.

CI on `main` and pull requests runs `cabal test all` (Postgres), Haddock, `cabal check`, and `cabal sdist all`.

## Test

Postgres is required.

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is accepted if `TEST_DATABASE_URL` is unset.

The getting-started example lives in [`examples/task/`](examples/task/). After changing emitters, regenerate it from that directory:

```bash
cd examples/task
cabal run task-codegen
cabal run task-codegen -- --check
```

## Docs

Haddock on the public surface (`Poppy`, `Poppy.Db`, `Poppy.Where`, `Poppy.Errors`, `Poppy.Codegen.Schema`, `CLI`, `Target`, `Drift`) should stay accurate. `cabal check` in `poppy/` and `poppy-codegen/` should stay clean.

## Scope

Poppy does not generate or apply SQL migrations. See [ADR 0001](docs/adr/0001-no-migration-generation.md).
