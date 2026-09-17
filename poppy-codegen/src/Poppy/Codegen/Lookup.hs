module Poppy.Codegen.Lookup
  ( lookupModel,
    lookupField,
    lookupRelation,
    lookupUniques,
  )
where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR

lookupModel :: Schema -> Text -> Model
lookupModel schema name =
  case find ((== name) . modelName) (schemaModels schema) of
    Just foundModel -> foundModel
    Nothing ->
      error $
        "Poppy.Codegen.Lookup: unknown model "
          <> T.unpack name

lookupField :: Model -> Text -> FieldSpec
lookupField model name =
  case find ((== name) . fieldName) (modelFields model) of
    Just spec -> spec
    Nothing ->
      error $
        "Poppy.Codegen.Lookup: unknown field "
          <> T.unpack name
          <> " on "
          <> T.unpack (modelName model)

lookupRelation :: Model -> Text -> RelationSpec
lookupRelation model name =
  case find ((== name) . relName) (modelRelations model) of
    Just relation -> relation
    Nothing ->
      error $
        "Poppy.Codegen.Lookup: unknown relation "
          <> T.unpack name
          <> " on "
          <> T.unpack (modelName model)

lookupUniques :: Schema -> Text -> [UniqueConstraint]
lookupUniques schema name =
  filter ((== name) . uniqueModel) (schemaUniques schema)
