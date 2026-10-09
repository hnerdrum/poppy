{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Low-level reads used by generated Clients. Application code calls @Schema.Client.*@.
module Poppy.Internal.Operations
  ( findMany,
    findManyWith,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstWith,
    findFirstOrFail,
    count,
    delete,
    ORMError (..),
  )
where

import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser)
import Database.PostgreSQL.Simple.ToField (ToField)
import Poppy.Internal.Core (Entity (..), PrimaryKeyType)
import Poppy.Internal.Db (Db (..))
import qualified Poppy.Internal.Delete as Delete
import Poppy.Internal.Errors (ORMError (..), requireFound)
import Poppy.Internal.Query
  ( QueryBuilder,
    limit,
    matching,
    runCountQuery,
    runQuery,
    runQueryOne,
    runQueryWith,
    selectAll,
  )
import Poppy.Internal.Where (compileWhere, eq)

findMany ::
  forall table result.
  (Entity table, FromRow result) =>
  (QueryBuilder table -> QueryBuilder table) ->
  Db [result]
findMany modifier = runQuery (modifier (selectAll @table))

findManyWith ::
  forall table result.
  (Entity table) =>
  RowParser result ->
  (QueryBuilder table -> QueryBuilder table) ->
  Db [result]
findManyWith parser modifier = runQueryWith parser (modifier (selectAll @table))

findUnique ::
  forall table result.
  (Entity table, FromRow result, ToField (PrimaryKeyType table)) =>
  PrimaryKeyType table ->
  Db (Maybe result)
findUnique pkValue =
  runQueryOne $
    matching (eq (primaryKey @table) pkValue) (selectAll @table)

findUniqueOrFail ::
  forall table result.
  (Entity table, FromRow result, ToField (PrimaryKeyType table), Show (PrimaryKeyType table)) =>
  PrimaryKeyType table ->
  Db (Either ORMError result)
findUniqueOrFail pkValue = do
  result <- findUnique @table @result pkValue
  pure $
    requireFound result $
      RecordNotFound ("Record not found with primary key: " <> Text.pack (show pkValue))

findFirst ::
  forall table result.
  (Entity table, FromRow result) =>
  (QueryBuilder table -> QueryBuilder table) ->
  Db (Maybe result)
findFirst modifier = runQueryOne (modifier (selectAll @table))

findFirstWith ::
  forall table result.
  (Entity table) =>
  RowParser result ->
  (QueryBuilder table -> QueryBuilder table) ->
  Db (Maybe result)
findFirstWith parser modifier = do
  results <- runQueryWith parser (limit 1 (modifier (selectAll @table)))
  pure $ case results of
    [] -> Nothing
    (row : _) -> Just row

findFirstOrFail ::
  forall table result.
  (Entity table, FromRow result) =>
  (QueryBuilder table -> QueryBuilder table) ->
  Db (Either ORMError result)
findFirstOrFail modifier = do
  result <- findFirst @table modifier
  pure $
    requireFound result $
      RecordNotFound "No record found matching criteria"

count ::
  forall table.
  (Entity table) =>
  (QueryBuilder table -> QueryBuilder table) ->
  Db Int
count modifier = runCountQuery (modifier (selectAll @table))

delete ::
  forall table.
  (Entity table, ToField (PrimaryKeyType table)) =>
  PrimaryKeyType table ->
  Db (Either ORMError Int)
delete pkValue =
  let (sql, params) = compileWhere (eq (primaryKey @table) pkValue)
      builder = Delete.whereDelete sql params (Delete.emptyDelete @table)
   in Delete.deleteWhere builder
