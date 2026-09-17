{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module ORM.Include
  ( IncludesJoin (..),
    NestInclude (..),
    ExecuteInclude (..),
    findMany,
    findUnique,
    findUniqueOrFail,
  )
where

import Data.Kind (Type)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import ORM.Core (Entity (..), PrimaryKeyType)
import ORM.Db (Db (..))
import ORM.Errors (ORMError (..), requireFound)
import qualified ORM.Operations as Ops
import ORM.Query (QueryBuilder, matching)
import ORM.SelectIn (prepareIncludeRootQuery)
import ORM.Where (eq)

class IncludesJoin include where
  includesJoin :: include -> Bool

class (IncludesJoin include) => NestInclude include result | include -> result where
  type RootRow include :: Type
  wrapRoot :: include -> RootRow include -> result

class ExecuteInclude root include result | include -> result where
  executeInclude ::
    include ->
    (QueryBuilder root -> QueryBuilder root) ->
    Db [result]

instance
  {-# OVERLAPPABLE #-}
  ( Entity root,
    NestInclude include result,
    FromRow (RootRow include)
  ) =>
  ExecuteInclude root include result
  where
  executeInclude include modifier = do
    roots <-
      Ops.findMany @root @(RootRow include) (prepareIncludeRootQuery @root modifier)
    pure $ map (wrapRoot include) roots

findMany ::
  (ExecuteInclude root include result) =>
  include ->
  (QueryBuilder root -> QueryBuilder root) ->
  Db [result]
findMany = executeInclude

findUnique ::
  forall root include result.
  ( Entity root,
    ToField (PrimaryKeyType root),
    ExecuteInclude root include result
  ) =>
  include ->
  PrimaryKeyType root ->
  Db (Maybe result)
findUnique include pk = do
  results <- findMany include (matching (eq (primaryKey @root) pk))
  pure $ case results of
    [] -> Nothing
    (row : _) -> Just row

findUniqueOrFail ::
  forall root include result.
  ( Entity root,
    ToField (PrimaryKeyType root),
    Show (PrimaryKeyType root),
    ExecuteInclude root include result
  ) =>
  include ->
  PrimaryKeyType root ->
  Db (Either ORMError result)
findUniqueOrFail include pk = do
  result <- findUnique @root include pk
  pure $
    requireFound result $
      RecordNotFound $
        "Record not found with primary key: " <> Text.pack (show pk)
