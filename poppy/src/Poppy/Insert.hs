{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Poppy.Insert
  ( InsertBuilder,
    Insertable (..),
    insert,
    insertMany,
    insertBuilder,
    insertReturning,
    executeInsert,
    tryExecuteInsert,
    emptyInsert,
    onConflictDoNothing,
    onConflictDoUpdate,
    onConflictDoUpdateSet,
    upsert,
    set,
    setNull,
    setMaybe,
    setNullable,
    setValue,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (..))
import Poppy.Core (Entity (..), Field (..), NullableValue (..))
import Poppy.Db (Db, dbIO, transactionEither)
import Poppy.Errors (ORMError (..), parseSingleton)
import Poppy.Sql (catchSql, quoteIdent)
import qualified Poppy.Update as Update

class (Entity table) => Insertable table where
  type CreateInput table
  toInsertBuilder :: CreateInput table -> InsertBuilder table

data InsertBuilder table = InsertBuilder
  { ibTable :: Text,
    ibColumns :: [Text],
    ibValues :: [Action],
    ibConflict :: OnConflict
  }

data OnConflict
  = NoConflict
  | DoNothing [Text]
  | DoUpdate [Text] [Text]
  | DoUpdateSet [Text] [(Text, Action)]

insert ::
  forall table result.
  (Insertable table, FromRow result) =>
  CreateInput table ->
  Db (Either ORMError result)
insert input = insertBuilder (toInsertBuilder @table input)

insertMany ::
  forall table.
  (Insertable table) =>
  [CreateInput table] ->
  Db (Either ORMError Int)
insertMany [] = pure (Right 0)
insertMany inputs = transactionEither (go 0 inputs)
  where
    go n [] = pure (Right n)
    go n (input : rest) = do
      result <- tryExecuteInsert (toInsertBuilder @table input)
      case result of
        Left err -> pure (Left err)
        Right () -> go (n + 1) rest

upsert ::
  forall table result.
  (Insertable table, Update.Updatable table, FromRow result) =>
  [Text] ->
  CreateInput table ->
  Update.UpdateInput table ->
  Db (Either ORMError result)
upsert conflictCols createInput updateInput = do
  now <- liftIO getCurrentTime
  let insertB = toInsertBuilder @table createInput
      updateB = Update.touchUpdatedAt @table now (Update.toUpdateBuilder @table updateInput)
      sets = Update.updateSets updateB
      builder =
        if null sets
          then onConflictDoUpdate conflictCols conflictCols insertB
          else onConflictDoUpdateSet conflictCols sets insertB
  insertBuilder builder

insertBuilder ::
  forall table result.
  (FromRow result) =>
  InsertBuilder table ->
  Db (Either ORMError result)
insertBuilder builder = dbIO $ \conn -> do
  result <- catchSql (runInsertReturning conn builder)
  pure $
    case result of
      Left err -> Left err
      Right rows ->
        parseSingleton
          rows
          (RecordNotFound $ "INSERT into " <> ibTable builder <> " returned no rows")
          (MultipleRecordsFound "Insert returned multiple rows")

insertReturning ::
  forall table result.
  (FromRow result) =>
  InsertBuilder table ->
  Db [result]
insertReturning builder = dbIO (`runInsertReturning` builder)

executeInsert :: InsertBuilder table -> Db ()
executeInsert builder = dbIO (`runExecuteInsert` builder)

tryExecuteInsert :: InsertBuilder table -> Db (Either ORMError ())
tryExecuteInsert builder = dbIO $ \conn -> catchSql (runExecuteInsert conn builder)

emptyInsert :: forall table. (Entity table) => InsertBuilder table
emptyInsert =
  InsertBuilder
    { ibTable = tableName @table,
      ibColumns = [],
      ibValues = [],
      ibConflict = NoConflict
    }

onConflictDoNothing :: [Text] -> InsertBuilder table -> InsertBuilder table
onConflictDoNothing cols builder = builder {ibConflict = DoNothing cols}

onConflictDoUpdate :: [Text] -> [Text] -> InsertBuilder table -> InsertBuilder table
onConflictDoUpdate cols setCols builder = builder {ibConflict = DoUpdate cols setCols}

onConflictDoUpdateSet :: [Text] -> [(Text, Action)] -> InsertBuilder table -> InsertBuilder table
onConflictDoUpdateSet cols sets builder = builder {ibConflict = DoUpdateSet cols sets}

set :: forall table a. (ToField a) => Field table a -> a -> InsertBuilder table -> InsertBuilder table
set field value builder =
  builder
    { ibColumns = ibColumns builder ++ [fieldColumn field],
      ibValues = ibValues builder ++ [toField value]
    }

setValue :: forall table a. (Entity table, ToField a) => Field table a -> a -> InsertBuilder table
setValue field value = set field value (emptyInsert @table)

setNull ::
  forall table a.
  (ToField (Maybe a)) =>
  Field table a ->
  InsertBuilder table ->
  InsertBuilder table
setNull field builder =
  builder
    { ibColumns = ibColumns builder ++ [fieldColumn field],
      ibValues = ibValues builder ++ [toField (Nothing :: Maybe a)]
    }

setMaybe ::
  forall table a.
  (ToField a) =>
  Field table a ->
  Maybe a ->
  InsertBuilder table ->
  InsertBuilder table
setMaybe _ Nothing builder = builder
setMaybe field (Just value) builder = set field value builder

setNullable ::
  forall table a.
  (ToField a, ToField (Maybe a)) =>
  Field table a ->
  NullableValue a ->
  InsertBuilder table ->
  InsertBuilder table
setNullable _ Omit builder = builder
setNullable field (Value value) builder = set field value builder
setNullable field Null builder = setNull field builder

runInsertReturning ::
  forall table result.
  (FromRow result) =>
  Connection ->
  InsertBuilder table ->
  IO [result]
runInsertReturning conn builder =
  let (queryText, allValues) = insertQueryParts builder " RETURNING *"
      query = Query (TE.encodeUtf8 queryText)
   in PGSimple.queryWith fromRow conn query allValues

runExecuteInsert ::
  Connection ->
  InsertBuilder table ->
  IO ()
runExecuteInsert conn builder = do
  let (queryText, allValues) = insertQueryParts builder ""
      query = Query (TE.encodeUtf8 queryText)
  _ <- PGSimple.execute conn query allValues
  pure ()

insertQueryParts :: InsertBuilder table -> Text -> (Text, [Action])
insertQueryParts builder suffix =
  let allColumns = ibColumns builder
      allValues = ibValues builder
      (conflictSql, conflictParams) = conflictParts (ibConflict builder)
      placeholders = Text.intercalate ", " $ replicate (length allValues) "?"
      columnsText = Text.intercalate ", " (map quoteIdent allColumns)
      queryText =
        "INSERT INTO "
          <> quoteIdent (ibTable builder)
          <> " ("
          <> columnsText
          <> ") VALUES ("
          <> placeholders
          <> ")"
          <> conflictSql
          <> suffix
   in (queryText, allValues ++ conflictParams)

conflictParts :: OnConflict -> (Text, [Action])
conflictParts NoConflict = ("", [])
conflictParts (DoNothing cols) =
  (" ON CONFLICT (" <> Text.intercalate ", " (map quoteIdent cols) <> ") DO NOTHING", [])
conflictParts (DoUpdate cols setCols) =
  ( " ON CONFLICT ("
      <> Text.intercalate ", " (map quoteIdent cols)
      <> ") DO UPDATE SET "
      <> Text.intercalate ", " [quoteIdent col <> " = EXCLUDED." <> quoteIdent col | col <- setCols],
    []
  )
conflictParts (DoUpdateSet cols sets) =
  ( " ON CONFLICT ("
      <> Text.intercalate ", " (map quoteIdent cols)
      <> ") DO UPDATE SET "
      <> Text.intercalate ", " [quoteIdent col <> " = ?" | (col, _) <- sets],
    map snd sets
  )
