{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compare a Schema to live Postgres. Poppy does not generate migrations.
module Poppy.Codegen.Drift
  ( DbCatalog (..),
    DbTable (..),
    DbColumn (..),
    DriftError (..),
    emptyCatalog,
    checkSchema,
    formatDriftError,
  )
where

import Data.List (find, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR hiding (field, variant)

data DbCatalog = DbCatalog
  { dbTables :: Map Text DbTable,
    dbEnums :: Map Text [Text]
  }
  deriving (Show, Eq)

data DbTable = DbTable
  { dbTableName :: Text,
    dbColumns :: Map Text DbColumn,
    dbPrimaryKey :: [Text],
    dbUniques :: [Set Text]
  }
  deriving (Show, Eq)

data DbColumn = DbColumn
  { dbColName :: Text,
    dbColType :: Text,
    dbColNullable :: Bool
  }
  deriving (Show, Eq)

data DriftError
  = DriftMissingTable Text Text
  | DriftMissingColumn Text Text
  | DriftExtraColumn Text Text
  | DriftNullability Text Text Bool Bool
  | DriftType Text Text Text Text
  | DriftPrimaryKey Text [Text] [Text]
  | DriftMissingUnique Text [Text]
  | DriftUnexpectedUnique Text [Text]
  | DriftMissingEnum Text Text
  | DriftEnumLabels Text [Text] [Text]
  deriving (Show, Eq)

emptyCatalog :: DbCatalog
emptyCatalog = DbCatalog {dbTables = Map.empty, dbEnums = Map.empty}

checkSchema :: Schema -> DbCatalog -> [DriftError]
checkSchema schema catalog =
  concatMap (checkModel catalog) (schemaModels schema)
    ++ concatMap (checkEnum catalog) (schemaEnums schema)
    ++ concatMap (checkMissingUnique catalog schema) (schemaUniques schema)
    ++ concatMap (checkUnexpectedUniques catalog schema) (schemaModels schema)

formatDriftError :: DriftError -> Text
formatDriftError = \case
  DriftMissingTable model table ->
    "IR model " <> model <> " table " <> table <> " is missing from the database"
  DriftMissingColumn table col ->
    "IR column " <> table <> "." <> col <> " is missing from the database"
  DriftExtraColumn table col ->
    "database column " <> table <> "." <> col <> " is not in the IR"
  DriftNullability table col irNull dbNull ->
    "nullability drift on "
      <> table
      <> "."
      <> col
      <> ": IR nullable="
      <> showBool irNull
      <> " database nullable="
      <> showBool dbNull
  DriftType table col expected actual ->
    "type drift on " <> table <> "." <> col <> ": IR " <> expected <> " database " <> actual
  DriftPrimaryKey table expected actual ->
    "primary key drift on " <> table <> ": IR " <> csv expected <> " database " <> csv actual
  DriftMissingUnique table cols ->
    "IR unique (" <> csv cols <> ") is missing from " <> table
  DriftUnexpectedUnique table cols ->
    "database unique (" <> csv cols <> ") on " <> table <> " is not in the IR"
  DriftMissingEnum name dbName ->
    "IR enum " <> name <> " (database type " <> dbName <> ") is missing"
  DriftEnumLabels name expected actual ->
    "enum " <> name <> " labels: IR " <> csv expected <> " database " <> csv actual

showBool :: Bool -> Text
showBool True = "true"
showBool False = "false"

csv :: [Text] -> Text
csv = T.intercalate ", "

checkModel :: DbCatalog -> Model -> [DriftError]
checkModel catalog model =
  case Map.lookup (modelTable model) (dbTables catalog) of
    Nothing ->
      [DriftMissingTable (modelName model) (modelTable model)]
    Just table ->
      concatMap (checkColumn table) (modelFields model)
        ++ extraColumns table (modelFields model)
        ++ checkPrimaryKey table model

checkColumn :: DbTable -> FieldSpec -> [DriftError]
checkColumn table spec =
  case Map.lookup (fieldColumn spec) (dbColumns table) of
    Nothing ->
      [DriftMissingColumn (dbTableName table) (fieldColumn spec)]
    Just col ->
      [ DriftNullability (dbTableName table) (fieldColumn spec) (fieldNullable spec) (dbColNullable col)
        | fieldNullable spec /= dbColNullable col
      ]
        ++ [ DriftType (dbTableName table) (fieldColumn spec) expected (dbColType col)
             | dbColType col /= expected
           ]
  where
    expected = irType spec

extraColumns :: DbTable -> [FieldSpec] -> [DriftError]
extraColumns table fields =
  [ DriftExtraColumn (dbTableName table) col
    | col <- Map.keys (dbColumns table),
      col `notElem` map fieldColumn fields
  ]

checkPrimaryKey :: DbTable -> Model -> [DriftError]
checkPrimaryKey table model =
  [ DriftPrimaryKey (modelTable model) expected actual
    | expected /= actual
  ]
  where
    expected = map fieldColumn (filter fieldIsPrimaryKey (modelFields model))
    actual = dbPrimaryKey table

checkMissingUnique :: DbCatalog -> Schema -> UniqueConstraint -> [DriftError]
checkMissingUnique catalog schema UniqueConstraint {uniqueModel, uniqueFields} =
  case find ((== uniqueModel) . modelName) (schemaModels schema) of
    Nothing -> []
    Just model ->
      case Map.lookup (modelTable model) (dbTables catalog) of
        Nothing -> []
        Just table ->
          let irCols = map (fieldColumnFor model) uniqueFields
              irSet = Set.fromList irCols
           in [ DriftMissingUnique (modelTable model) (sort irCols)
                | irSet `notElem` dbUniques table
              ]

checkUnexpectedUniques :: DbCatalog -> Schema -> Model -> [DriftError]
checkUnexpectedUniques catalog schema model =
  case Map.lookup (modelTable model) (dbTables catalog) of
    Nothing -> []
    Just table ->
      [ DriftUnexpectedUnique (dbTableName table) (sort (Set.toList dbSet))
        | dbSet <- dbUniques table,
          dbSet `notElem` irSets
      ]
  where
    irSets =
      [ Set.fromList (map (fieldColumnFor model) uniqueFields)
        | UniqueConstraint {uniqueModel, uniqueFields} <- schemaUniques schema,
          uniqueModel == modelName model
      ]

fieldColumnFor :: Model -> Text -> Text
fieldColumnFor model wanted =
  maybe
    wanted
    fieldColumn
    (find ((== wanted) . fieldName) (modelFields model))

checkEnum :: DbCatalog -> EnumSpec -> [DriftError]
checkEnum catalog enumSpec =
  case Map.lookup dbName (dbEnums catalog) of
    Nothing ->
      [DriftMissingEnum (enumName enumSpec) dbName]
    Just labels ->
      [ DriftEnumLabels (enumName enumSpec) expected (sort labels)
        | sort labels /= expected
      ]
  where
    dbName = T.toLower (enumName enumSpec)
    expected = sort (map variantSqlValue (enumVariants enumSpec))

variantSqlValue :: EnumVariant -> Text
variantSqlValue spec =
  case variantDbValue spec of
    Just value -> value
    Nothing -> variantName spec

irType :: FieldSpec -> Text
irType spec =
  case fieldType spec of
    TyText -> "text"
    TyUuid -> "uuid"
    TyInt -> "int"
    TyTimestamptz -> "timestamptz"
    TyBool -> "boolean"
    TyEnum name -> "enum:" <> T.toLower name
