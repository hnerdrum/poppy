# Contributing

Poppy is two Cabal packages in one repo: `poppy` (runtime) and `poppy-codegen` (Schema builder and file generation). They stay lockstep at the same version.

## Build

GHC 9.4.8 (LTS 21.22). Build with Cabal; `stack.yaml` is for Stack.

```bash
cabal build all
```

Edit `package.yaml` by hand, then run `hpack` in each package directory and commit both files. CI fails if they drift.

CI on `main` and pull requests runs `cabal test all` (Postgres), Haddock, `cabal check`, and `cabal sdist all`.

## Test

Postgres is required.

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is used if `TEST_DATABASE_URL` is unset.

The example app is [`examples/task/`](examples/task/). After changing emitters, regenerate it from that directory:

```bash
cd examples/task
cabal run task-codegen
cabal run task-codegen -- --check
```
