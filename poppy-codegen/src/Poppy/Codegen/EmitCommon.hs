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
    resultTypeName,
    singlePrefixEdge,
    leafResultTypeName,
    leafTagTypeName,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupModel, lookupRelation)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

fieldBinder :: Model -> FieldSpec -> Text
fieldBinder model f =
  lowerFirst (modelName model) <> upperFirst (fieldName f)

hsType :: FieldType -> Text
hsType TyText = "Text"
hsType TyUuid = "UUID"
hsType TyInt = "Int"
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
includeFieldName parent rel =
  case T.stripPrefix (lowerFirst (modelName parent)) (relName rel) of
    Just rest | not (T.null rest) -> lowerFirst rest
    _ -> lowerFirst (relToModel rel)

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

resultTypeName :: Schema -> Model -> [IncludeTree] -> Text
resultTypeName _schema model edges =
  modelName model
    <> "With"
    <> T.concat (map (upperFirst . fieldOf) edges)
  where
    fieldOf edge =
      includeFieldName model (lookupRelation model (includeRelation edge))

-- Single relation whose children are themselves leaves (Recipe: ingredients → ingredient).
singlePrefixEdge :: ModelInclude -> Maybe IncludeTree
singlePrefixEdge incl =
  case includeTree incl of
    [edge]
      | not (null (includeChildren edge)),
        all (null . includeChildren) (includeChildren edge) ->
          Just edge
    _ -> Nothing

leafResultTypeName :: Schema -> Model -> IncludeTree -> Text
leafResultTypeName schema parent edge =
  modelName parent <> "With" <> modelName child
  where
    rel = lookupRelation parent (includeRelation edge)
    child = lookupModel schema (relToModel rel)

leafTagTypeName :: Model -> IncludeTree -> Text
leafTagTypeName parent edge =
  upperFirst (includeFieldName parent (lookupRelation parent (includeRelation edge)))
