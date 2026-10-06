{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Validate
  ( ValidationError (..),
    validateSchema,
  )
where

import Data.List (find, nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Poppy.Codegen.IR

data ValidationError
  = DuplicateModelName Text
  | DuplicateEnumName Text
  | EnumHasNoVariants Text
  | DuplicateEnumVariant Text Text
  | UnknownEnumType Text Text Text
  | ModelMissingPrimaryKey Text
  | ModelMultiplePrimaryKeys Text
  | UnknownRelationModel Text Text Text
  | UnknownRelationField Text Text Text
  | DuplicateRelationName Text Text
  | RelationNameClashesWithField Text Text
  | UnknownUniqueModel Text
  | UnknownUniqueField Text Text
  | EmptyUniqueConstraint Text
  deriving (Show, Eq)

validateSchema :: Schema -> [ValidationError]
validateSchema schema =
  concat
    [ duplicateModelNames schema,
      duplicateEnumNames schema,
      concatMap validateEnum (schemaEnums schema),
      concatMap (validateModel schema) (schemaModels schema),
      concatMap (validateRelation schema) (concatMap modelRelations (schemaModels schema)),
      concatMap (validateUnique schema) (schemaUniques schema)
    ]

duplicateModelNames :: Schema -> [ValidationError]
duplicateModelNames schema =
  map DuplicateModelName (duplicates (map modelName (schemaModels schema)))

duplicateEnumNames :: Schema -> [ValidationError]
duplicateEnumNames schema =
  map DuplicateEnumName (duplicates (map enumName (schemaEnums schema)))

validateEnum :: EnumSpec -> [ValidationError]
validateEnum enumSpec =
  let emptyErr =
        [EnumHasNoVariants (enumName enumSpec) | null (enumVariants enumSpec)]
      dupVars =
        map
          (DuplicateEnumVariant (enumName enumSpec))
          (duplicates (map variantName (enumVariants enumSpec)))
   in emptyErr ++ dupVars

validateModel :: Schema -> Model -> [ValidationError]
validateModel schema model =
  let pks = filter fieldIsPrimaryKey (modelFields model)
      pkErrs = case pks of
        [] -> [ModelMissingPrimaryKey (modelName model)]
        [_] -> []
        _ -> [ModelMultiplePrimaryKeys (modelName model)]
      enumErrs = concatMap (validateFieldEnum schema (modelName model)) (modelFields model)
   in pkErrs ++ enumErrs ++ validateRelationNames model

validateRelationNames :: Model -> [ValidationError]
validateRelationNames model =
  map (DuplicateRelationName (modelName model)) (duplicates (map relName (modelRelations model)))
    ++ [ RelationNameClashesWithField (modelName model) name
         | rel <- modelRelations model,
           let name = relName rel,
           any ((== name) . fieldName) (modelFields model)
       ]

validateFieldEnum :: Schema -> Text -> FieldSpec -> [ValidationError]
validateFieldEnum schema modelName fieldSpec =
  case fieldType fieldSpec of
    TyEnum wanted ->
      ([UnknownEnumType modelName (fieldName fieldSpec) wanted | not (any ((== wanted) . enumName) (schemaEnums schema))])
    _ -> []

validateRelation :: Schema -> RelationSpec -> [ValidationError]
validateRelation schema rel =
  let fromOk = findModel schema (relFromModel rel)
      toOk = findModel schema (relToModel rel)
      modelErrs =
        catMaybes
          [ if isNothing fromOk
              then Just (UnknownRelationModel (relName rel) "from" (relFromModel rel))
              else Nothing,
            if isNothing toOk
              then Just (UnknownRelationModel (relName rel) "to" (relToModel rel))
              else Nothing
          ]
      fieldErrs = case (fromOk, toOk, relKind rel) of
        (Just fromModel, Just toModel, RelHasMany) ->
          requireField (relName rel) (modelName fromModel) (relLocalField rel) fromModel
            ++ requireField (relName rel) (modelName toModel) (relForeignField rel) toModel
        (Just fromModel, Just toModel, RelBelongsTo) ->
          requireField (relName rel) (modelName fromModel) (relForeignField rel) fromModel
            ++ requireField (relName rel) (modelName toModel) (relLocalField rel) toModel
        _ -> []
   in modelErrs ++ fieldErrs

findModel :: Schema -> Text -> Maybe Model
findModel schema name =
  find ((== name) . modelName) (schemaModels schema)

validateUnique :: Schema -> UniqueConstraint -> [ValidationError]
validateUnique schema UniqueConstraint {uniqueModel, uniqueFields} =
  case findModel schema uniqueModel of
    Nothing ->
      [UnknownUniqueModel uniqueModel]
    Just model
      | null uniqueFields ->
          [EmptyUniqueConstraint uniqueModel]
      | otherwise ->
          [ UnknownUniqueField uniqueModel wanted
            | wanted <- uniqueFields,
              not (any ((== wanted) . fieldName) (modelFields model))
          ]

requireField :: Text -> Text -> Text -> Model -> [ValidationError]
requireField relName modelName wantedField model =
  [UnknownRelationField relName modelName wantedField | not (any ((== wantedField) . fieldName) (modelFields model))]

duplicates :: (Eq a) => [a] -> [a]
duplicates xs = nub [x | x <- xs, length (filter (== x) xs) > 1]
