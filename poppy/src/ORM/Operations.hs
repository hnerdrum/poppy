{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORM.Operations
  ( findMany,
    findManyWith,
    findUnique,
    findUniqueOrFail,
    findUniqueWhere,
    requireUniqueWhere,
    findFirst,
    findFirstOrFail,
    count,
    delete,
    ORMError (..),
  )
where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser)
import Database.PostgreSQL.Simple.ToField (ToField)
import ORM.Core (Entity (..), PrimaryKeyType)
import ORM.Db (Db (..))
import qualified ORM.Delete as Delete
import ORM.Errors (ORMError (..), requireFound)
import ORM.Query
  ( QueryBuilder,
    matching,
    runCountQuery,
    runQuery,
    runQueryOne,
    runQueryWith,
    selectAll,
  )
import ORM.Where (Where, compileWhere, eq, equalityColumns)

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

findUniqueWhere ::
  forall table result.
  (Entity table, FromRow result) =>
  Maybe (Where table) ->
  Db (Either ORMError (Maybe result))
findUniqueWhere predicate = do
  case requireUniqueWhere @table predicate of
    Left err -> pure (Left err)
    Right where_ -> do
      rows <- runQuery (matching where_ (selectAll @table))
      pure $ case rows of
        [] -> Right Nothing
        [row] -> Right (Just row)
        _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")

requireUniqueWhere ::
  forall table.
  (Entity table) =>
  Maybe (Where table) ->
  Either ORMError (Where table)
requireUniqueWhere = \case
  Nothing ->
    Left (InvalidUniqueInput "findUnique requires a unique where_")
  Just where_ ->
    case equalityColumns where_ of
      Nothing ->
        Left (InvalidUniqueInput "findUnique where_ must be equalities on a unique key")
      Just cols ->
        let given = Set.fromList cols
            keys = map Set.fromList (uniqueKeys @table)
            exact = filter (== given) keys
            incomplete = any (Set.isProperSubsetOf given) keys
         in case exact of
              [_] -> Right where_
              _ : _ ->
                Left (InvalidUniqueInput "findUnique where_ matches multiple unique keys")
              []
                | incomplete ->
                    Left (InvalidUniqueInput "findUnique where_ is an incomplete unique key")
              [] ->
                Left (InvalidUniqueInput "findUnique where_ is not a unique key")

findFirst ::
  forall table result.
  (Entity table, FromRow result) =>
  (QueryBuilder table -> QueryBuilder table) ->
  Db (Maybe result)
findFirst modifier = runQueryOne (modifier (selectAll @table))

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
  Db Int
delete pkValue =
  let (sql, params) = compileWhere (eq (primaryKey @table) pkValue)
      builder = Delete.whereDelete sql params (Delete.emptyDelete @table)
   in Delete.deleteWhere builder
