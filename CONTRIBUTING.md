# Contributing

Poppy is two Cabal packages in one repo: `poppy` (runtime) and `poppy-codegen` (Schema builder and file generation). They stay lockstep at the same version.

## Build

GHC 9.4.8–9.10.3. Build with Cabal; `stack.yaml` is LTS 21.22 (GHC 9.4.8) for Stack.

```bash
cabal build all
```

Edit `package.yaml` by hand, then run `hpack` 0.38.3 in each package directory and commit both files. CI fails if they drift.

CI on `main` and pull requests runs `cabal test all` (Postgres), Haddock, `cabal check`, `cabal sdist all`, and builds both example apps (`codegen --check` plus a run against Postgres).

## Test

Postgres is required.

```bash
docker compose up -d
export TEST_DATABASE_URL=postgres://poppy:poppy@127.0.0.1:5435/poppy_test
cabal test all
```

`DATABASE_URL` is used if `TEST_DATABASE_URL` is unset.

Example apps are [`examples/task/`](examples/task/) (single table) and [`examples/blog/`](examples/blog/) (relations). After changing emitters, regenerate from each example directory:

```bash
cd examples/task
cabal run task-codegen
cabal run task-codegen -- --check

cd ../blog
cabal run blog-codegen
cabal run blog-codegen -- --check
```
