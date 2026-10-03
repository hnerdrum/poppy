# poppy

The runtime library for [Poppy](https://github.com/hnerdrum/poppy), a Postgres ORM for Haskell. Your app imports `poppy` to query and write through the client that [`poppy-codegen`](https://github.com/hnerdrum/poppy/tree/main/poppy-codegen) generates from your schema. It also provides `applyMigrations`, which runs your hand-written `.sql` migrations before the app starts.

The [repository README](https://github.com/hnerdrum/poppy#readme) shows the full setup, and the [guides](https://github.com/hnerdrum/poppy/tree/main/docs) cover the client API, writes, relations, raw SQL, and errors.
