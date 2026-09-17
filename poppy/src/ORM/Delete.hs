{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ORM.Delete
  ( DeleteBuilder,
    deleteWhere,
    deleteMany,
    deleteReturning,
    whereDelete,
    emptyDelete,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types (Query (..))
import ORM.Core (Entity (..))
import ORM.Db (Db (..), liftIO)
import ORM.Errors (ORMError (..))
import ORM.Query (buildWhereClause)
import ORM.Sql (quoteIdent)
import ORM.Where (Where, compileWhere)

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
  Db Int
deleteWhere builder
  | null (dbWhere builder) =
      liftIO $ throwIO (EmptyWhere "DELETE requires a WHERE clause")
  | otherwise = Db (`runDeleteWhere` builder)

deleteMany ::
  forall table.
  (Entity table) =>
  Where table ->
  Db (Either ORMError Int)
deleteMany clause =
  let (sql, params) = compileWhere clause
      builder = whereDelete sql params (emptyDelete @table)
   in Right <$> deleteWhere builder

deleteReturning ::
  forall table result.
  (Entity table, FromRow result) =>
  DeleteBuilder table ->
  Db [result]
deleteReturning builder
  | null (dbWhere builder) =
      liftIO $ throwIO (EmptyWhere "DELETE requires a WHERE clause")
  | otherwise = Db (`runDeleteReturning` builder)

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
