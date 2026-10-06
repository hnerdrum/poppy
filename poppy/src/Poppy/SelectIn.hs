{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.SelectIn
  ( GroupIndex,
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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (In (..), Query (..))
import Poppy.Core (Entity (..), Field (..))
import Poppy.Db (Db, dbIO, logSql)
import Poppy.Group (groupByKey)
import qualified Poppy.Operations as Ops
import Poppy.Query
  ( OrderBy (..),
    OrderDirection (..),
    QueryBuilder,
    matching,
    orderBy,
    queryOrderBy,
    setOrderBy,
  )
import Poppy.Sql (quoteIdent)
import Poppy.Where (Where, compileWhere, in_)

newtype GroupIndex k a = GroupIndex (Map k [a])

newtype ByPk k a = ByPk (Map k a)

findByIn ::
  forall table result key.
  (Entity table, FromRow result, ToField key) =>
  Field table key ->
  [key] ->
  Maybe (Where table) ->
  [OrderBy table] ->
  Maybe Int ->
  Db [result]
findByIn field keys mWhere orders mTake
  | null keys = pure []
  | Just n <- mTake, n <= 0 = pure []
  | Just n <- mTake = findTaken @table @result field keys mWhere orders n
  | otherwise =
      Ops.findMany @table @result $
        setOrderBy (childOrder @table orders)
          . maybe id matching mWhere
          . matching (in_ field keys)

findTaken ::
  forall table result key.
  (Entity table, FromRow result, ToField key) =>
  Field table key ->
  [key] ->
  Maybe (Where table) ->
  [OrderBy table] ->
  Int ->
  Db [result]
findTaken field keys mWhere orders n = do
  let (sql, params) = takenSql @table field keys mWhere orders n
  logSql sql
  dbIO $ \conn -> PGSimple.queryWith fromRow conn (Query (TE.encodeUtf8 sql)) params

takenSql ::
  forall table key.
  (Entity table, ToField key) =>
  Field table key ->
  [key] ->
  Maybe (Where table) ->
  [OrderBy table] ->
  Int ->
  (Text, [Action])
takenSql field keys mWhere orders n =
  let cols = T.intercalate ", " (map quoteIdent (tableColumns @table))
      part = quoteIdent (fieldColumn field)
      rn = quoteIdent "poppy_rn"
      (extraSql, extraParams) = case mWhere of
        Nothing -> ("", [])
        Just pred_ ->
          let (clause, clauseParams) = compileWhere pred_
           in (" AND (" <> clause <> ")", clauseParams)
      sql =
        "SELECT "
          <> cols
          <> " FROM (SELECT "
          <> cols
          <> ", ROW_NUMBER() OVER (PARTITION BY "
          <> part
          <> " ORDER BY "
          <> orderSql (childOrder @table orders)
          <> ") AS "
          <> rn
          <> " FROM "
          <> quoteIdent (tableName @table)
          <> " WHERE "
          <> part
          <> " IN ?"
          <> extraSql
          <> ") AS "
          <> quoteIdent "poppy_window"
          <> " WHERE "
          <> rn
          <> " <= ? ORDER BY "
          <> part
          <> " ASC, "
          <> rn
          <> " ASC"
   in (sql, toField (In keys) : extraParams ++ [toField n])

childOrder :: forall table. (Entity table) => [OrderBy table] -> [OrderBy table]
childOrder orders
  | null orders = [OrderBy pk Asc]
  | any isPk orders = orders
  | otherwise = orders ++ [OrderBy pk Asc]
  where
    pk = fieldColumn (primaryKey @table)
    isPk (OrderBy col _) = col == pk

orderSql :: [OrderBy table] -> Text
orderSql orders = T.intercalate ", " (map term orders)
  where
    term (OrderBy col dir) =
      quoteIdent col <> case dir of
        Asc -> " ASC"
        Desc -> " DESC"

emptyGroups :: GroupIndex k a
emptyGroups = GroupIndex Map.empty

indexHasMany :: (Ord k) => (a -> k) -> [a] -> GroupIndex k a
indexHasMany keyFn rows =
  GroupIndex . Map.fromList $
    [(keyFn (head group), group) | group <- groupByKey keyFn rows]

-- | Like 'indexHasMany', dropping rows whose key is 'Nothing'.
indexHasManyMaybe :: (Ord k) => (a -> Maybe k) -> [a] -> GroupIndex k a
indexHasManyMaybe keyFn rows =
  GroupIndex $
    Map.fromListWith (flip (<>)) [(key, [row]) | row <- rows, Just key <- [keyFn row]]

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
    [] -> orderBy (primaryKey @table) Asc qb
    _ -> qb
