{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Poppy.Delete
  ( DeleteBuilder,
    deleteWhere,
    deleteMany,
    deleteReturning,
    whereDelete,
    emptyDelete,
  )
where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Core (Entity (..))
import Poppy.Db (Db (..))
import Poppy.Errors (ORMError (..))
import Poppy.Query (buildWhereClause)
import Poppy.Sql (catchSql, quoteIdent)
import Poppy.Where (Where, compileWhere)

data DeleteBuilder table = DeleteBuilder
  { dbTable :: Text,
    dbWhere :: [Text],
    dbWhereParams :: [Action]
  }

emptyDelete :: forall table. (Entity table) => DeleteBuilder table
emptyDelete =
  DeleteBuilder
    { dbTable = tableName @table,
      dbWhere = [],
      dbWhereParams = []
    }

whereDelete :: Text -> [Action] -> DeleteBuilder table -> DeleteBuilder table
whereDelete condition params builder =
  builder
    { dbWhere = dbWhere builder ++ [condition],
      dbWhereParams = dbWhereParams builder ++ params
    }

deleteWhere ::
  forall table.
  (Entity table) =>
  DeleteBuilder table ->
  Db (Either ORMError Int)
deleteWhere builder
  | null (dbWhere builder) =
      pure (Left (EmptyWhere "DELETE requires a WHERE clause"))
  | otherwise = Db $ \conn -> catchSql (runDeleteWhere conn builder)

deleteMany ::
  forall table.
  (Entity table) =>
  Where table ->
  Db (Either ORMError Int)
deleteMany clause =
  let (sql, params) = compileWhere clause
      builder = whereDelete sql params (emptyDelete @table)
   in deleteWhere builder

deleteReturning ::
  forall table result.
  (Entity table, FromRow result) =>
  DeleteBuilder table ->
  Db (Either ORMError [result])
deleteReturning builder
  | null (dbWhere builder) =
      pure (Left (EmptyWhere "DELETE requires a WHERE clause"))
  | otherwise = Db $ \conn -> catchSql (runDeleteReturning conn builder)

runDeleteWhere ::
  Connection ->
  DeleteBuilder table ->
  IO Int
runDeleteWhere conn builder = do
  let queryText =
        "DELETE FROM "
          <> quoteIdent (dbTable builder)
          <> buildWhereClause (dbWhere builder)
      query = Query (TE.encodeUtf8 queryText)
  fromIntegral <$> PGSimple.execute conn query (dbWhereParams builder)

runDeleteReturning ::
  forall table result.
  (FromRow result) =>
  Connection ->
  DeleteBuilder table ->
  IO [result]
runDeleteReturning conn builder = do
  let queryText =
        "DELETE FROM "
          <> quoteIdent (dbTable builder)
          <> buildWhereClause (dbWhere builder)
          <> " RETURNING *"
      query = Query (TE.encodeUtf8 queryText)
  PGSimple.queryWith fromRow conn query (dbWhereParams builder)
