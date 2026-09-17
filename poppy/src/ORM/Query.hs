{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ORM.Query
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
    limit,
    offset,
    applyQueryModifiers,
    OrderDirection (..),
    buildWhereClause,
    runQuery,
    runQueryWith,
    runQueryOne,
    runCountQuery,
  )
where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Only (..))
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types (Query (..))
import ORM.Core (Entity (..), Field (..))
import ORM.Db (Db (..))
import ORM.Sql (quoteIdent)
import ORM.Where (Where, compileWhere)

data QueryBuilder table = QueryBuilder
  { qbTable :: Text,
    qbColumns :: [Text],
    qbWhere :: [Where table],
    qbOrderBy :: Maybe (Text, OrderDirection),
    qbLimit :: Maybe Int,
    qbOffset :: Maybe Int
  }

data OrderDirection = Asc | Desc

selectAll :: forall table. (Entity table) => QueryBuilder table
selectAll =
  QueryBuilder
    { qbTable = tableName @table,
      qbColumns = tableColumns @table,
      qbWhere = [],
      qbOrderBy = Nothing,
      qbLimit = Nothing,
      qbOffset = Nothing
    }

select :: forall table. (Entity table) => QueryBuilder table
select = selectAll @table

selectColumns :: [Text] -> QueryBuilder table -> QueryBuilder table
selectColumns cols qb = qb {qbColumns = cols}

matching :: Where table -> QueryBuilder table -> QueryBuilder table
matching clause qb =
  qb {qbWhere = qbWhere qb ++ [clause]}

orderBy :: Field table a -> OrderDirection -> QueryBuilder table -> QueryBuilder table
orderBy field dir qb =
  qb {qbOrderBy = Just (fieldColumn field, dir)}

limit :: Int -> QueryBuilder table -> QueryBuilder table
limit n qb = qb {qbLimit = Just n}

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

queryOrderBy :: QueryBuilder table -> Maybe (Text, OrderDirection)
queryOrderBy = qbOrderBy

applyQueryModifiers ::
  Maybe (Where table) ->
  Maybe (QueryBuilder table -> QueryBuilder table) ->
  Maybe Int ->
  Maybe Int ->
  QueryBuilder table ->
  QueryBuilder table
applyQueryModifiers mWhere mOrder mLimit mOffset =
  maybe id matching mWhere
    . fromMaybe id mOrder
    . maybe id limit mLimit
    . maybe id offset mOffset

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

buildOrderClause :: Maybe (Text, OrderDirection) -> Text
buildOrderClause Nothing = ""
buildOrderClause (Just (col, dir)) =
  " ORDER BY " <> quoteIdent col <> case dir of
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
runQueryWith parser qb =
  Db $ \conn -> do
    let (query, params) = buildQuery qb
    PGSimple.queryWith parser conn query params

runQueryOne :: (FromRow result) => QueryBuilder table -> Db (Maybe result)
runQueryOne qb = do
  results <- runQuery (limit 1 qb)
  pure $ case results of
    [] -> Nothing
    (x : _) -> Just x

runCountQuery :: QueryBuilder table -> Db Int
runCountQuery qb =
  Db $ \conn -> do
    let (query, params) = buildCountQuery qb
    [Only count] <- PGSimple.query conn query params
    pure (fromIntegral (count :: Int64))
