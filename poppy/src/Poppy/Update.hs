{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Poppy.Update
  ( UpdateBuilder,
    Updatable (..),
    update,
    updateBuilder,
    updateReturning,
    setField,
    setFieldNull,
    setFieldMaybe,
    setFieldNullable,
    whereUpdate,
    emptyUpdate,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Core (Entity (..), Field (..), NullableValue (..), PrimaryKeyType)
import Poppy.Db (Db (..), liftIO)
import Poppy.Errors (ORMError (..), parseSingleton)
import qualified Poppy.Operations as Ops
import Poppy.Query (buildWhereClause)
import Poppy.Sql (catchSql, quoteIdent)

class (Entity table) => Updatable table where
  type UpdateInput table
  toUpdateBuilder :: UpdateInput table -> UpdateBuilder table
  updatedAtField :: Maybe (Field table UTCTime)
  default updatedAtField :: Maybe (Field table UTCTime)
  updatedAtField = Nothing

data UpdateBuilder table = UpdateBuilder
  { ubTable :: Text,
    ubSets :: [(Text, Action)],
    ubWhere :: [Text],
    ubWhereParams :: [Action]
  }

emptyUpdate :: forall table. (Entity table) => UpdateBuilder table
emptyUpdate =
  UpdateBuilder
    { ubTable = tableName @table,
      ubSets = [],
      ubWhere = [],
      ubWhereParams = []
    }

setField :: forall table a. (ToField a) => Field table a -> a -> UpdateBuilder table -> UpdateBuilder table
setField field value builder =
  builder
    { ubSets = ubSets builder ++ [(fieldColumn field, toField value)]
    }

setFieldNull ::
  forall table a.
  (ToField (Maybe a)) =>
  Field table a ->
  UpdateBuilder table ->
  UpdateBuilder table
setFieldNull field builder =
  builder
    { ubSets = ubSets builder ++ [(fieldColumn field, toField (Nothing :: Maybe a))]
    }

setFieldMaybe ::
  forall table a.
  (ToField a) =>
  Field table a ->
  Maybe a ->
  UpdateBuilder table ->
  UpdateBuilder table
setFieldMaybe _ Nothing builder = builder
setFieldMaybe field (Just value) builder = setField field value builder

setFieldNullable ::
  forall table a.
  (ToField a, ToField (Maybe a)) =>
  Field table a ->
  NullableValue a ->
  UpdateBuilder table ->
  UpdateBuilder table
setFieldNullable _ Omit builder = builder
setFieldNullable field (Value value) builder = setField field value builder
setFieldNullable field Null builder = setFieldNull field builder

whereUpdate :: Text -> [Action] -> UpdateBuilder table -> UpdateBuilder table
whereUpdate condition params builder =
  builder
    { ubWhere = ubWhere builder ++ [condition],
      ubWhereParams = ubWhereParams builder ++ params
    }

update ::
  forall table result.
  (Updatable table, FromRow result, ToField (PrimaryKeyType table), Show (PrimaryKeyType table)) =>
  PrimaryKeyType table ->
  UpdateInput table ->
  Db (Either ORMError result)
update pkValue input = do
  let builder = toUpdateBuilder @table input
  if null (ubSets builder)
    then Ops.findUniqueOrFail @table @result pkValue
    else do
      now <- liftCurrentTime
      let pkField = primaryKey @table
          builderWithTouch = touchUpdatedAt @table now builder
          builderWithWhere =
            whereUpdate
              (quoteIdent (fieldColumn pkField) <> " = ?")
              [toField pkValue]
              builderWithTouch
      updateBuilder builderWithWhere

updateBuilder ::
  forall table result.
  (Entity table, FromRow result) =>
  UpdateBuilder table ->
  Db (Either ORMError result)
updateBuilder builder
  | null (ubWhere builder) =
      pure (Left (EmptyWhere "UPDATE requires a WHERE clause"))
  | otherwise = Db $ \conn -> do
      result <- catchSql (runUpdateReturning conn builder)
      pure $
        case result of
          Left err -> Left err
          Right rows ->
            parseSingleton
              rows
              (RecordNotFound "No record found to update")
              (MultipleRecordsFound "Update affected multiple rows")

updateReturning ::
  forall table result.
  (Entity table, FromRow result) =>
  UpdateBuilder table ->
  Db [result]
updateReturning builder
  | null (ubWhere builder) =
      liftIO $ throwIO (EmptyWhere "UPDATE requires a WHERE clause")
  | otherwise = Db (`runUpdateReturning` builder)

liftCurrentTime :: Db UTCTime
liftCurrentTime = Db (const getCurrentTime)

buildSetClause :: [(Text, Action)] -> Text
buildSetClause sets = " SET " <> Text.intercalate ", " (map (\(col, _) -> quoteIdent col <> " = ?") sets)

runUpdateReturning ::
  forall table result.
  (FromRow result) =>
  Connection ->
  UpdateBuilder table ->
  IO [result]
runUpdateReturning conn builder = do
  let setClause = buildSetClause (ubSets builder)
      setParams = map snd (ubSets builder)
      whereClause = buildWhereClause (ubWhere builder)
      allParams = setParams ++ ubWhereParams builder
      queryText =
        "UPDATE "
          <> quoteIdent (ubTable builder)
          <> setClause
          <> whereClause
          <> " RETURNING *"
      query = Query (TE.encodeUtf8 queryText)
  PGSimple.queryWith fromRow conn query allParams

touchUpdatedAt ::
  forall table.
  (Updatable table) =>
  UTCTime ->
  UpdateBuilder table ->
  UpdateBuilder table
touchUpdatedAt now builder =
  case updatedAtField @table of
    Nothing -> builder
    Just field ->
      if fieldColumn field `elem` map fst (ubSets builder)
        then builder
        else setField field now builder
