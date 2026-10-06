{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Query builders used by generated Clients (@matching@, @asc@ / @desc@, @limit@).
module Poppy.Query
  ( Query,
    QueryBuilder,
    queryWhereClauses,
    queryLimit,
    queryOffset,
    queryOrderBy,
    select,
    selectAll,
    selectColumns,
    matching,
    orderBy,
    setOrderBy,
    asc,
    desc,
    limit,
    offset,
    applyQueryModifiers,
    OrderBy (..),
    OrderDirection (..),
    buildWhereClause,
    runQuery,
    runQueryWith,
    runQueryOne,
    runCountQuery,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.Simple (Only (..))
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Core (Entity (..), Field (..))
import Poppy.Db (Db, dbIO, logSql)
import Poppy.Sql (quoteIdent)
import Poppy.Where (Where, compileWhere)

data QueryBuilder table = QueryBuilder
  { qbTable :: Text,
    qbColumns :: [Text],
    qbWhere :: [Where table],
    qbOrderBy :: [OrderBy table],
    qbLimit :: Maybe Int,
    qbOffset :: Maybe Int
  }

data OrderDirection = Asc | Desc
  deriving (Show, Eq)

data OrderBy table = OrderBy Text OrderDirection
  deriving (Show, Eq)

-- | @ASC@
asc :: Field table a -> OrderBy table
asc field = OrderBy (fieldColumn field) Asc

-- | @DESC@
desc :: Field table a -> OrderBy table
desc field = OrderBy (fieldColumn field) Desc

-- | All columns, no filter.
selectAll :: forall table. (Entity table) => QueryBuilder table
selectAll =
  QueryBuilder
    { qbTable = tableName @table,
      qbColumns = tableColumns @table,
      qbWhere = [],
      qbOrderBy = [],
      qbLimit = Nothing,
      qbOffset = Nothing
    }

select :: forall table. (Entity table) => QueryBuilder table
select = selectAll @table

selectColumns :: [Text] -> QueryBuilder table -> QueryBuilder table
selectColumns cols qb = qb {qbColumns = cols}

-- | Append a @WHERE@ clause (root table).
matching :: Where table -> QueryBuilder table -> QueryBuilder table
matching clause qb =
  qb {qbWhere = qbWhere qb ++ [clause]}

-- | Append an @ORDER BY@ column.
orderBy :: Field table a -> OrderDirection -> QueryBuilder table -> QueryBuilder table
orderBy field dir qb =
  qb {qbOrderBy = qbOrderBy qb ++ [OrderBy (fieldColumn field) dir]}

-- | @LIMIT@
limit :: Int -> QueryBuilder table -> QueryBuilder table
limit n qb = qb {qbLimit = Just n}

-- | @OFFSET@
offset :: Int -> QueryBuilder table -> QueryBuilder table
offset n qb = qb {qbOffset = Just n}

queryWhereClauses :: QueryBuilder table -> [(Text, [Action])]
queryWhereClauses qb =
  case qbWhere qb of
    [] -> []
    preds ->
      let compiled = map compileWhere preds
          sql = T.intercalate " AND " ["(" <> s <> ")" | (s, _) <- compiled]
          params = concatMap snd compiled
       in [(sql, params)]

queryLimit :: QueryBuilder table -> Maybe Int
queryLimit = qbLimit

queryOffset :: QueryBuilder table -> Maybe Int
queryOffset = qbOffset

queryOrderBy :: QueryBuilder table -> [OrderBy table]
queryOrderBy = qbOrderBy

applyQueryModifiers ::
  Maybe (Where table) ->
  [OrderBy table] ->
  Maybe Int ->
  Maybe Int ->
  QueryBuilder table ->
  QueryBuilder table
applyQueryModifiers mWhere orders mLimit mOffset =
  maybe id matching mWhere
    . setOrderBy orders
    . maybe id limit mLimit
    . maybe id offset mOffset

-- | Replace the @ORDER BY@ list.
setOrderBy :: [OrderBy table] -> QueryBuilder table -> QueryBuilder table
setOrderBy orders qb = qb {qbOrderBy = orders}

buildQuery :: QueryBuilder table -> (Query, [Action])
buildQuery qb =
  let baseQuery = "SELECT " <> buildSelectList (qbColumns qb) <> " FROM " <> quoteIdent (qbTable qb)
      clauses = queryWhereClauses qb
      whereClause = buildWhereClause (map fst clauses)
      orderClause = buildOrderClause (qbOrderBy qb)
      limitClause = buildLimitClause (qbLimit qb)
      offsetClause = buildOffsetClause (qbOffset qb)
      finalQuery = baseQuery <> whereClause <> orderClause <> limitClause <> offsetClause
   in (Query (TE.encodeUtf8 finalQuery), concatMap snd clauses)

buildSelectList :: [Text] -> Text
buildSelectList cols = T.intercalate ", " (map quoteIdent cols)

buildCountQuery :: QueryBuilder table -> (Query, [Action])
buildCountQuery qb =
  let clauses = queryWhereClauses qb
      finalQuery = "SELECT COUNT(*) FROM " <> quoteIdent (qbTable qb) <> buildWhereClause (map fst clauses)
   in (Query (TE.encodeUtf8 finalQuery), concatMap snd clauses)

buildWhereClause :: [Text] -> Text
buildWhereClause [] = ""
buildWhereClause conditions = " WHERE " <> T.intercalate " AND " conditions

buildOrderClause :: [OrderBy table] -> Text
buildOrderClause [] = ""
buildOrderClause terms =
  " ORDER BY " <> T.intercalate ", " (map termSql terms)
  where
    termSql (OrderBy col dir) =
      quoteIdent col <> case dir of
        Asc -> " ASC"
        Desc -> " DESC"

buildLimitClause :: Maybe Int -> Text
buildLimitClause Nothing = ""
buildLimitClause (Just n) = " LIMIT " <> T.pack (show n)

buildOffsetClause :: Maybe Int -> Text
buildOffsetClause Nothing = ""
buildOffsetClause (Just n) = " OFFSET " <> T.pack (show n)

runQuery :: (FromRow result) => QueryBuilder table -> Db [result]
runQuery = runQueryWith fromRow

runQueryWith :: RowParser result -> QueryBuilder table -> Db [result]
runQueryWith parser qb = do
  let (query, params) = buildQuery qb
  logSql (queryText query)
  dbIO $ \conn -> PGSimple.queryWith parser conn query params

runQueryOne :: (FromRow result) => QueryBuilder table -> Db (Maybe result)
runQueryOne qb = do
  results <- runQuery (limit 1 qb)
  pure $ case results of
    [] -> Nothing
    (x : _) -> Just x

runCountQuery :: QueryBuilder table -> Db Int
runCountQuery qb = do
  let (query, params) = buildCountQuery qb
  logSql (queryText query)
  dbIO $ \conn -> do
    [Only count] <- PGSimple.query conn query params
    pure (fromIntegral (count :: Int64))

queryText :: Query -> Text
queryText (Query bytes) = TE.decodeUtf8With lenientDecode bytes
