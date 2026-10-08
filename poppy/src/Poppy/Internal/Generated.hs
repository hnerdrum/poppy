-- | Support for Clients emitted by @poppy-codegen@.
--
-- Generated @Schema.*@ modules import this module and nothing else from
-- @poppy@. Application code should import 'Poppy' and @Schema.Client.*@
-- instead. The surface is stable within a major version so a Client built
-- against codegen 1.x keeps working with runtime 1.x.
module Poppy.Internal.Generated
  ( -- * Postgres bindings
    FromRow (..),
    RowParser,
    field,
    FromField (..),
    ResultError (..),
    returnError,
    ToField (..),

    -- * Table metadata
    Entity (..),
    Column (..),
    SqlType (..),
    Field (..),
    NullableValue (..),
    PrimaryKeyType,

    -- * Connection
    Db,
    DbPool,
    transactionEither,

    -- * Errors
    ORMError (..),
    DatabaseErrorInfo (..),
    DriverErrorKind (..),
    requireFound,
    fromUniqueRows,
    uniqueOrFail,
    parseSingleton,

    -- * Select / pick
    OmitSelect (..),
    Picked (..),
    picked,

    -- * Insert
    InsertBuilder,
    Insertable (..),
    insert,
    insertMany,
    insertBuilder,
    insertReturning,
    executeInsert,
    tryExecuteInsert,
    emptyInsert,
    onConflictDoNothing,
    onConflictDoUpdate,
    onConflictDoUpdateSet,
    upsert,
    set,
    setNull,
    setMaybe,
    setNullable,
    setValue,

    -- * Update
    UpdateBuilder,
    Updatable (..),
    update,
    updateWhere,
    updateBuilder,
    updateReturning,
    setField,
    setFieldNull,
    setFieldMaybe,
    setFieldNullable,
    whereUpdate,
    emptyUpdate,
    updateMany,
    updateSets,
    touchUpdatedAt,

    -- * Delete
    DeleteBuilder,
    deleteWhere,
    deleteMany,
    deleteReturning,
    whereDelete,
    emptyDelete,

    -- * Low-level reads
    findMany,
    findManyWith,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstWith,
    findFirstOrFail,
    count,
    delete,

    -- * Query builders
    QueryBuilder,
    matching,
    selectColumns,
    applyQueryModifiers,
    OrderBy (..),
    OrderDirection (..),
    asc,
    desc,
    limit,
    offset,
    orderBy,
    selectAll,
    select,

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
    compileWhere,

    -- * Includes
    Skip (..),
    Load (..),
    load,
    loadWith,
    skip,
    skipped,
    Skipped,
    ModelTable,
    IncludeFor,
    ValidEdge,
    requireRelated,
    GroupIndex,
    ByPk,
    findByIn,
    emptyGroups,
    indexHasMany,
    indexHasManyMaybe,
    lookupGroups,
    emptyByPk,
    indexByPk,
    lookupByPk,
    prepareIncludeRootQuery,
  )
where

import Poppy.Internal.Column ()
import Poppy.Internal.Core
import Poppy.Internal.Db (Db, DbPool, transactionEither)
import Poppy.Internal.Delete
import Poppy.Internal.Errors
import Poppy.Internal.Include
import Poppy.Internal.Insert
import Poppy.Internal.Operations hiding (ORMError (..))
import Poppy.Internal.PG
import Poppy.Internal.Query
import Poppy.Internal.Select
import Poppy.Internal.SelectIn
import Poppy.Internal.Update
import Poppy.Internal.Where
