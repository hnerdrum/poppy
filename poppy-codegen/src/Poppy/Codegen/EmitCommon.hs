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
    includeRecordValue,
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

-- | Include record literal with flags set for @selected@ edges of @parent@.
includeRecordValue :: Schema -> Model -> [IncludeTree] -> Text
includeRecordValue schema parent selected =
  includeRecordLiteral schema parent (includeTree (fullInclude schema parent)) selected

fullInclude :: Schema -> Model -> ModelInclude
fullInclude schema model =
  case [incl | incl <- schemaIncludes schema, includeRootModel incl == modelName model] of
    (incl : _) -> incl
    [] ->
      ModelInclude
        { includeName = modelName model <> "Include",
          includeRootModel = modelName model,
          includeTree = []
        }

includeRecordLiteral :: Schema -> Model -> [IncludeTree] -> [IncludeTree] -> Text
includeRecordLiteral schema parent fullEdges selected =
  includeNameFor parent
    <> " {"
    <> T.intercalate ", " (map (fieldAssign schema parent selected) fullEdges)
    <> "}"

includeNameFor :: Model -> Text
includeNameFor model = modelName model <> "Include"

fieldAssign :: Schema -> Model -> [IncludeTree] -> IncludeTree -> Text
fieldAssign schema parent selected fullEdge =
  fld <> " = " <> value
  where
    fld = includeFieldName parent (lookupRelation parent (includeRelation fullEdge))
    child = lookupModel schema (relToModel (lookupRelation parent (includeRelation fullEdge)))
    match = findEdge (includeRelation fullEdge) selected
    value = case match of
      Nothing
        | null (includeChildren fullEdge) -> "False"
        | otherwise -> "Nothing"
      Just sel
        | null (includeChildren fullEdge) -> "True"
        | null (includeChildren sel) ->
            "Just ("
              <> includeRecordLiteral schema child (includeChildren fullEdge) []
              <> ")"
        | otherwise ->
            "Just ("
              <> includeRecordLiteral schema child (includeChildren fullEdge) (includeChildren sel)
              <> ")"

findEdge :: Text -> [IncludeTree] -> Maybe IncludeTree
findEdge name = foldr go Nothing
  where
    go edge acc
      | includeRelation edge == name = Just edge
      | otherwise = acc
