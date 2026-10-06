# Errors

Some Client calls return `Either ORMError`. The constructors are:

```haskell
data ORMError
  = RecordNotFound Text
  | MultipleRecordsFound Text
  | UniqueViolation Text
  | ForeignKeyViolation Text
  | NotNullViolation Text
  | EmptyWhere Text
  | UnsupportedIncludeModifier Text
  | DatabaseError DatabaseErrorInfo
  | DriverError DriverErrorKind Text
```

| Constructor                  | When                                                         |
| ---------------------------- | ------------------------------------------------------------ |
| `RecordNotFound`             | `findUniqueOrFail` / `findFirstOrFail` / update-by-id missed |
| `MultipleRecordsFound`       | `findUnique` matched more than one row                       |
| `UniqueViolation`            | Postgres unique_violation (`23505`)                          |
| `ForeignKeyViolation`        | Postgres foreign_key_violation (`23503`)                     |
| `NotNullViolation`           | Postgres not_null_violation (`23502`)                        |
| `EmptyWhere`                 | `updateMany` / `deleteMany` with no `Where`                  |
| `UnsupportedIncludeModifier` | Include API used a modifier that is not generated            |
| `DatabaseError`              | Other `SqlError` (`sqlState`, `message`, `detail`)           |
| `DriverError`                | `postgresql-simple` format, query, or decode failure         |
