# Errors

Client writes and `findUnique*` return `Either ORMError`. `findMany` / `findFirst` / `count` throw only if the driver does (those are wrapped when they go through `catchSql` on writes). `ORMError` is also an `Exception`.

```haskell
data ORMError
  = RecordNotFound Text
  | MultipleRecordsFound Text
  | UniqueViolation Text
  | ForeignKeyViolation Text
  | NotNullViolation Text
  | EmptyWhere Text
  | InvalidUniqueInput Text
  | UnsupportedIncludeModifier Text
  | DatabaseError DatabaseErrorInfo
  | DriverError DriverErrorKind Text
```

| Constructor                  | When                                                              |
| ---------------------------- | ----------------------------------------------------------------- |
| `RecordNotFound`             | `findUniqueOrFail` / `findFirstOrFail` / update-by-id missed      |
| `MultipleRecordsFound`       | `findUnique` matched more than one row                            |
| `UniqueViolation`            | Postgres unique_violation (`23505`)                               |
| `ForeignKeyViolation`        | Postgres foreign_key_violation (`23503`)                          |
| `NotNullViolation`           | Postgres not_null_violation (`23502`)                             |
| `EmptyWhere`                 | `updateMany` / `deleteMany` (and similar) with no `WHERE`         |
| `InvalidUniqueInput`         | `findUnique` `where_` is missing, not equalities, or not a unique |
| `UnsupportedIncludeModifier` | Include API used a modifier that is not generated                 |
| `DatabaseError`              | Other `SqlError` (includes `sqlState`, `message`, `detail`)       |
| `DriverError`                | `postgresql-simple` format / query / decode failures              |

`DriverErrorKind` is `FormatMismatch`, `ClientQuery`, or `ResultDecode`.

Pattern-match the constructor you care about; use `DatabaseErrorInfo.sqlState` for anything else. `queryRaw` / `executeRaw` do not catch: wrap with `catchDb` if you want `Either ORMError`.
