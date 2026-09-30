{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compare a Schema to live Postgres. Poppy does not generate migrations.
module Poppy.Codegen.Drift
  ( DbCatalog (..),
    DbTable (..),
    DbColumn (..),
    DbForeignKey (..),
    DriftError (..),
    emptyCatalog,
    checkSchema,
    formatDriftError,
  )
where

import Data.List (find, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR hiding (field, variant)
import Poppy.Codegen.TextUtil (lowerFirst)

data DbCatalog = DbCatalog
  { dbTables :: Map Text DbTable,
    dbEnums :: Map Text [Text],
    dbForeignKeys :: [DbForeignKey]
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
    dbColNullable :: Bool,
    dbColDefault :: Maybe Text
  }
  deriving (Show, Eq)

data DbForeignKey = DbForeignKey
  { dbFkFromTable :: Text,
    dbFkFromColumn :: Text,
    dbFkToTable :: Text,
    dbFkToColumn :: Text
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
  | DriftMissingDefault Text Text FieldDefault
  | DriftDefaultMismatch Text Text FieldDefault Text
  | DriftMissingForeignKey Text Text Text Text
  deriving (Show, Eq)

emptyCatalog :: DbCatalog
emptyCatalog =
  DbCatalog
    { dbTables = Map.empty,
      dbEnums = Map.empty,
      dbForeignKeys = []
    }

checkSchema :: Schema -> DbCatalog -> [DriftError]
checkSchema schema catalog =
  concatMap (checkModel catalog) (schemaModels schema)
    ++ concatMap (checkEnum catalog) (schemaEnums schema)
    ++ concatMap (checkMissingUnique catalog schema) (schemaUniques schema)
    ++ concatMap (checkUnexpectedUniques catalog schema) (schemaModels schema)
    ++ checkForeignKeys catalog schema

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
  DriftMissingDefault table col expected ->
    "IR column "
      <> table
      <> "."
      <> col
      <> " declares DEFAULT "
      <> defaultLabel expected
      <> " but the database has none"
  DriftDefaultMismatch table col expected actual ->
    "default drift on "
      <> table
      <> "."
      <> col
      <> ": IR "
      <> defaultLabel expected
      <> " database "
      <> actual
  DriftMissingForeignKey fromTable fromCol toTable toCol ->
    "IR foreign key "
      <> fromTable
      <> "."
      <> fromCol
      <> " → "
      <> toTable
      <> "."
      <> toCol
      <> " is missing from the database"

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
        ++ checkDefault (dbTableName table) (fieldColumn spec) (fieldDefault spec) (dbColDefault col)
  where
    expected = irType spec

checkDefault :: Text -> Text -> Maybe FieldDefault -> Maybe Text -> [DriftError]
checkDefault _ _ Nothing _ = []
checkDefault table col (Just expected) Nothing =
  [DriftMissingDefault table col expected]
checkDefault table col (Just expected) (Just actual) =
  [ DriftDefaultMismatch table col expected actual
    | canonicalizeDefault actual /= Just expected
  ]

canonicalizeDefault :: Text -> Maybe FieldDefault
canonicalizeDefault raw
  | mentions ["uuid_generate_v4", "gen_random_uuid"] = Just DefaultUuidV4
  | mentions ["now()", "current_timestamp"] = Just DefaultNow
  | otherwise = Nothing
  where
    normalized = T.toLower (T.strip raw)
    mentions = any (`T.isInfixOf` normalized)

defaultLabel :: FieldDefault -> Text
defaultLabel DefaultUuidV4 = "uuid_generate_v4()"
defaultLabel DefaultNow = "now()"

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
    Nothing -> lowerFirst (variantName spec)

irType :: FieldSpec -> Text
irType spec =
  case fieldType spec of
    TyText -> "text"
    TyUuid -> "uuid"
    TyInt -> "int"
    TyNumeric -> "numeric"
    TyJsonb -> "jsonb"
    TyTimestamptz -> "timestamptz"
    TyBool -> "boolean"
    TyEnum name -> "enum:" <> T.toLower name

checkForeignKeys :: DbCatalog -> Schema -> [DriftError]
checkForeignKeys catalog schema =
  [ DriftMissingForeignKey fromTable fromCol toTable toCol
    | DbForeignKey fromTable fromCol toTable toCol <- impliedForeignKeys schema,
      Map.member fromTable (dbTables catalog),
      DbForeignKey fromTable fromCol toTable toCol `notElem` dbForeignKeys catalog
  ]

impliedForeignKeys :: Schema -> [DbForeignKey]
impliedForeignKeys schema =
  nub
    [ fk
      | model <- schemaModels schema,
        rel <- modelRelations model,
        Just fk <- [relationForeignKey schema rel]
    ]

relationForeignKey :: Schema -> RelationSpec -> Maybe DbForeignKey
relationForeignKey schema rel =
  case (findModel (relFromModel rel), findModel (relToModel rel)) of
    (Just fromModel, Just toModel) ->
      Just $
        case relKind rel of
          RelHasMany ->
            DbForeignKey
              { dbFkFromTable = modelTable toModel,
                dbFkFromColumn = fieldColumn (lookupNamedField toModel (relForeignField rel)),
                dbFkToTable = modelTable fromModel,
                dbFkToColumn = fieldColumn (lookupNamedField fromModel (relLocalField rel))
              }
          RelBelongsTo ->
            DbForeignKey
              { dbFkFromTable = modelTable fromModel,
                dbFkFromColumn = fieldColumn (lookupNamedField fromModel (relForeignField rel)),
                dbFkToTable = modelTable toModel,
                dbFkToColumn = fieldColumn (lookupNamedField toModel (relLocalField rel))
              }
    _ -> Nothing
  where
    findModel name = find ((== name) . modelName) (schemaModels schema)

lookupNamedField :: Model -> Text -> FieldSpec
lookupNamedField model name =
  case find ((== name) . fieldName) (modelFields model) of
    Just spec -> spec
    Nothing ->
      error $
        "Poppy.Codegen.Drift: unknown field "
          <> T.unpack name
          <> " on "
          <> T.unpack (modelName model)
