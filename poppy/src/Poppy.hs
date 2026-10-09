-- | Runtime used by generated Poppy Clients.
--
-- Application code typically imports this module plus @Schema.Client.*@.
-- Guides: the repository @docs/@ directory. This module is the Haddock
-- entry point for @Db@, @ORMError@, query combinators, and @applyMigrations@.
--
-- Generated @Schema.*@ modules import 'Poppy.Internal.Generated' instead.
module Poppy
  ( -- * Connection
    Db,
    DbPool,
    PoolConfig (..),
    SqlLogger,
    connect,
    connectWith,
    defaultPool,
    closePool,
    runDb,
    transaction,
    transactionEither,
    withTransaction,

    -- * Errors
    ORMError (..),
    DatabaseErrorInfo (..),
    DriverErrorKind (..),

    -- * Where
    Where,
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

    -- * Order
    OrderBy,
    asc,
    desc,

    -- * Select / values
    OmitSelect (..),
    Picked (..),
    NullableValue (..),

    -- * Includes
    Load (..),
    load,
    loadWith,
    skip,

    -- * Raw SQL
    queryRaw,
    executeRaw,
    param,
    catchDb,

    -- * Migrations
    applyMigrations,
    MigrateError (..),
  )
where

import Poppy.Internal.Column ()
import Poppy.Internal.Core (NullableValue (..))
import Poppy.Internal.Db
  ( Db,
    DbPool,
    PoolConfig (..),
    SqlLogger,
    closePool,
    connect,
    connectWith,
    defaultPool,
    runDb,
    transaction,
    transactionEither,
    withTransaction,
  )
import Poppy.Internal.Errors (DatabaseErrorInfo (..), DriverErrorKind (..), ORMError (..))
import Poppy.Internal.Include (Load (..), load, loadWith, skip)
import Poppy.Internal.Migrate (MigrateError (..), applyMigrations)
import Poppy.Internal.Query (OrderBy, asc, desc)
import Poppy.Internal.Select (OmitSelect (..), Picked (..))
import Poppy.Internal.Sql (catchDb, executeRaw, param, queryRaw)
import Poppy.Internal.Where
  ( Where,
    and_,
    contains,
    eq,
    gt,
    gte,
    in_,
    isNull,
    lt,
    lte,
    neq,
    not_,
    or_,
  )
