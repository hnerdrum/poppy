{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.EmitCommon
  ( fieldBinder,
    hsType,
    primaryKeyField,
    includeFieldName,
    createTypeName,
    updateTypeName,
    tableTypeName,
    rowTypeName,
    selectTypeName,
    pickedTypeName,
    selectDefaultName,
    selectColumnsFnName,
    parsePickedName,
    toPickedName,
    uniqueTypeName,
    uniqueWhereName,
    uniqueKeyTypeName,
    CreateKind (..),
    createKind,
    createHsType,
    updateHsType,
    updateFields,
    pkHsType,
    clientSchemaPrefix,
    schemaModuleFor,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

fieldBinder :: Model -> FieldSpec -> Text
fieldBinder model f =
  lowerFirst (modelName model) <> upperFirst (fieldName f)

hsType :: FieldType -> Text
hsType TyText = "Text"
hsType TyUuid = "UUID"
hsType TyInt = "Int"
hsType TyNumeric = "Scientific"
hsType TyJsonb = "Value"
hsType TyTimestamptz = "UTCTime"
hsType TyBool = "Bool"
hsType (TyEnum name) = name

primaryKeyField :: Model -> FieldSpec
primaryKeyField model =
  case filter fieldIsPrimaryKey (modelFields model) of
    [f] -> f
    _ ->
      error $
        "Poppy.Codegen.EmitCommon: model "
          <> T.unpack (modelName model)
          <> " must have exactly one primary key"

includeFieldName :: Model -> RelationSpec -> Text
includeFieldName _parent = relName

createTypeName :: Model -> Text
createTypeName model = modelName model <> "Create"

updateTypeName :: Model -> Text
updateTypeName model = modelName model <> "Update"

tableTypeName :: Model -> Text
tableTypeName model = modelName model <> "Table"

rowTypeName :: Model -> Text
rowTypeName model = modelName model <> "Row"

selectTypeName :: Model -> Text
selectTypeName model = modelName model <> "Select"

pickedTypeName :: Model -> Text
pickedTypeName model = modelName model <> "Picked"

selectDefaultName :: Model -> Text
selectDefaultName model = lowerFirst (modelName model) <> "Select"

selectColumnsFnName :: Model -> Text
selectColumnsFnName model = lowerFirst (modelName model) <> "SelectColumns"

parsePickedName :: Model -> Text
parsePickedName model = "parse" <> modelName model <> "Picked"

toPickedName :: Model -> Text
toPickedName model = "to" <> modelName model <> "Picked"

uniqueTypeName :: Model -> Text
uniqueTypeName model = modelName model <> "Unique"

uniqueWhereName :: Model -> Text
uniqueWhereName model = lowerFirst (modelName model) <> "UniqueWhere"

uniqueKeyTypeName :: Model -> Text
uniqueKeyTypeName model = modelName model <> "UniqueKey"

data CreateKind = CreateMaybe | CreateNullable | CreateRequired
  deriving (Eq)

createKind :: FieldSpec -> CreateKind
createKind f
  | fieldIsPrimaryKey f && isJust (fieldDefault f) = CreateMaybe
  | fieldIsPrimaryKey f = CreateRequired
  | isJust (fieldDefault f) = CreateMaybe
  | fieldNullable f = CreateNullable
  | otherwise = CreateRequired

createHsType :: FieldSpec -> Text
createHsType f = case createKind f of
  CreateMaybe -> "Maybe " <> hsType (fieldType f)
  CreateNullable -> "NullableValue " <> hsType (fieldType f)
  CreateRequired -> hsType (fieldType f)

updateFields :: Model -> [FieldSpec]
updateFields = filter (not . fieldIsPrimaryKey) . modelFields

updateHsType :: FieldSpec -> Text
updateHsType f
  | fieldNullable f = "NullableValue " <> hsType (fieldType f)
  | otherwise = "Maybe " <> hsType (fieldType f)

pkHsType :: Model -> Text
pkHsType model = hsType (fieldType (primaryKeyField model))

-- | Schema types live next to the Client unless the Client is nested under Schema.
--
-- @Foo.Client.Bar@ → @Foo.Schema.@ (sibling Client/Schema packages)
-- @Schema.Client.Bar@ → @Schema.@ (Client nested under Schema)
clientSchemaPrefix :: Text -> Text
clientSchemaPrefix clientModule =
  case T.breakOnEnd ".Client." clientModule of
    (before, after)
      | not (T.null after) && ".Client." `T.isSuffixOf` before ->
          let parent = T.take (T.length before - T.length ".Client.") before
           in if parent == "Schema"
                then "Schema."
                else parent <> ".Schema."
    _ -> "Schema."

schemaModuleFor :: Text -> Model -> Text
schemaModuleFor clientModule model =
  clientSchemaPrefix clientModule <> modelName model
