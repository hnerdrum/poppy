{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.SelectIn
  ( GroupIndex,
    ByPk,
    findByIn,
    emptyGroups,
    indexHasMany,
    lookupGroups,
    emptyByPk,
    indexByPk,
    lookupByPk,
    prepareIncludeRootQuery,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Poppy.Core (Entity (..), Field (..))
import Poppy.Db (Db)
import Poppy.Group (groupByKey)
import qualified Poppy.Operations as Ops
import Poppy.Query
  ( OrderDirection (Asc),
    QueryBuilder,
    matching,
    orderBy,
    queryOrderBy,
  )
import Poppy.Where (in_)

newtype GroupIndex k a = GroupIndex (Map k [a])

newtype ByPk k a = ByPk (Map k a)

findByIn ::
  forall table result key.
  (Entity table, FromRow result, ToField key) =>
  Field table key ->
  [key] ->
  Db [result]
findByIn field keys
  | null keys = pure []
  | otherwise =
      Ops.findMany @table @result $
        orderBy (primaryKey @table) Asc . matching (in_ field keys)

emptyGroups :: GroupIndex k a
emptyGroups = GroupIndex Map.empty

indexHasMany :: (Ord k) => (a -> k) -> [a] -> GroupIndex k a
indexHasMany keyFn rows =
  GroupIndex . Map.fromList $
    [(keyFn (head group), group) | group <- groupByKey keyFn rows]

lookupGroups :: (Ord k) => k -> GroupIndex k a -> [a]
lookupGroups key (GroupIndex groups) =
  Map.findWithDefault [] key groups

emptyByPk :: ByPk k a
emptyByPk = ByPk Map.empty

indexByPk :: (Ord k) => (a -> k) -> [a] -> ByPk k a
indexByPk keyFn rows =
  ByPk (Map.fromList [(keyFn row, row) | row <- rows])

lookupByPk :: (Ord k) => k -> ByPk k a -> Maybe a
lookupByPk key (ByPk byPk) =
  Map.lookup key byPk

prepareIncludeRootQuery ::
  forall table.
  (Entity table) =>
  (QueryBuilder table -> QueryBuilder table) ->
  QueryBuilder table ->
  QueryBuilder table
prepareIncludeRootQuery modifier =
  defaultPkOrder @table . modifier

defaultPkOrder ::
  forall table.
  (Entity table) =>
  QueryBuilder table ->
  QueryBuilder table
defaultPkOrder qb =
  case queryOrderBy qb of
    Just _ -> qb
    Nothing -> orderBy (primaryKey @table) Asc qb
