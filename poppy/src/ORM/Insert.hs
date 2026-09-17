{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ORM.Insert
  ( InsertBuilder,
    Insertable (..),
    insert,
    insertBuilder,
    insertReturning,
    executeInsert,
    tryExecuteInsert,
    emptyInsert,
    onConflictDoNothing,
    onConflictDoUpdate,
    set,
    setNull,
    setMaybe,
    setNullable,
    setValue,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PGSimple
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (..))
import ORM.Core (Entity (..), Field (..), NullableValue (..))
import ORM.Db (Db (..))
import ORM.Errors (ORMError (..), parseSingleton)
import ORM.Sql (catchSql, quoteIdent)

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

insert ::
  forall table result.
  (Insertable table, FromRow result) =>
  CreateInput table ->
  Db (Either ORMError result)
insert input = insertBuilder (toInsertBuilder @table input)

insertBuilder ::
  forall table result.
  (FromRow result) =>
  InsertBuilder table ->
  Db (Either ORMError result)
insertBuilder builder = Db $ \conn -> do
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
insertReturning builder = Db (`runInsertReturning` builder)

executeInsert :: InsertBuilder table -> Db ()
executeInsert builder = Db (`runExecuteInsert` builder)

tryExecuteInsert :: InsertBuilder table -> Db (Either ORMError ())
tryExecuteInsert builder = Db $ \conn -> catchSql (runExecuteInsert conn builder)

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
          <> conflictClause (ibConflict builder)
          <> suffix
   in (queryText, allValues)

conflictClause :: OnConflict -> Text
conflictClause NoConflict = ""
conflictClause (DoNothing cols) =
  " ON CONFLICT (" <> Text.intercalate ", " (map quoteIdent cols) <> ") DO NOTHING"
conflictClause (DoUpdate cols setCols) =
  " ON CONFLICT ("
    <> Text.intercalate ", " (map quoteIdent cols)
    <> ") DO UPDATE SET "
    <> Text.intercalate ", " [quoteIdent col <> " = EXCLUDED." <> quoteIdent col | col <- setCols]
