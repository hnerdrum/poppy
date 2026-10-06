-- | Runtime used by generated Poppy Clients.
--
-- Application code typically imports this module plus @Schema.Client.*@.
-- Guides: the repository @docs/@ directory. This module is the Haddock
-- entry point for @Db@, @ORMError@, query combinators, and @applyMigrations@.
module Poppy
  ( findMany,
    findUnique,
    findUniqueOrFail,
    findUniqueWhere,
    requireUniqueWhere,
    findFirst,
    findFirstOrFail,
    count,
    delete,
    deleteWhere,
    deleteMany,
    offset,
    ORMError (..),
    DatabaseErrorInfo (..),
    DriverErrorKind (..),
    selectAll,
    select,
    selectColumns,
    OmitSelect (..),
    Picked (..),
    picked,
    Where,
    matching,
    eq,
    neq,
    gt,
    gte,
    lt,
    lte,
    in_,
    contains,
    isNull,
    and_,
    or_,
    not_,
    orderBy,
    asc,
    desc,
    limit,
    applyQueryModifiers,
    OrderBy (..),
    OrderDirection (..),
    QueryBuilder,
    insert,
    insertMany,
    insertBuilder,
    insertReturning,
    Insertable (..),
    set,
    setNull,
    setMaybe,
    setNullable,
    setValue,
    upsert,
    InsertBuilder,
    update,
    updateBuilder,
    updateReturning,
    Updatable (..),
    setField,
    setFieldNull,
    setFieldMaybe,
    setFieldNullable,
    whereUpdate,
    UpdateBuilder,
    emptyUpdate,
    updateMany,
    deleteReturning,
    whereDelete,
    DeleteBuilder,
    emptyDelete,
    Entity (..),
    Field (..),
    Column (..),
    PrimaryKeyType,
    NullableValue (..),
    toField,
    requireFound,
    runDb,
    withTransaction,
    transaction,
    transactionEither,
    Db (..),
    DbPool (..),
    PoolConfig (..),
    SqlLogger,
    defaultPool,
    connect,
    connectWith,
    closePool,
    queryRaw,
    executeRaw,
    param,
    catchDb,
    applyMigrations,
    MigrateError (..),
    load,
    loadWith,
    skip,
  )
where

import Database.PostgreSQL.Simple.ToField (toField)
import Poppy.Column
import Poppy.Core
import Poppy.Db (Db (..), DbPool (..), PoolConfig (..), SqlLogger, closePool, connect, connectWith, defaultPool, runDb, transaction, transactionEither, withTransaction)
import Poppy.Delete
import Poppy.Errors
import Poppy.Include (load, loadWith, skip)
import Poppy.Insert
import Poppy.Migrate (MigrateError (..), applyMigrations)
import Poppy.Operations
import Poppy.Query
import Poppy.Select
import Poppy.Sql (catchDb, executeRaw, param, queryRaw)
import Poppy.Update
import Poppy.Where
