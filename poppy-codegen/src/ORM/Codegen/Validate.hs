{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Validate
  ( ValidationError (..),
    validateSchema,
  )
where

import Data.List (find, nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import     ORM.Codegen.IR
import ORM.Codegen.IncludePath (defaultMaxIncludeDepth, includeMaxDepth)
import ORM.Codegen.JoinAlias (allocateJoinAlias, includeModelsInOrder)
import ORM.Codegen.Schema (fullGraphIncludeName, isFullGraphInclude)

data ValidationError
  = DuplicateModelName Text
  | DuplicateIncludeName Text
  | DuplicateEnumName Text
  | EnumHasNoVariants Text
  | DuplicateEnumVariant Text Text
  | UnknownEnumType Text Text Text
  | ModelMissingPrimaryKey Text
  | ModelMultiplePrimaryKeys Text
  | UnknownRelationModel Text Text Text
  | UnknownRelationField Text Text Text
  | UnknownIncludeRoot Text Text
  | UnknownIncludeRelation Text Text Text
  | JoinAliasExhausted Text Text
  | MaxIncludeDepthExceeded Text Int Int
  | DuplicateIncludeRelation Text Text Text
  | UnknownUniqueModel Text
  | UnknownUniqueField Text Text
  | EmptyUniqueConstraint Text
  | LeftoverIncludeDeclaration Text
  deriving (Show, Eq)

validateSchema :: Schema -> [ValidationError]
validateSchema schema =
  concat
    [ duplicateModelNames schema,
      duplicateIncludeNames schema,
      duplicateEnumNames schema,
      concatMap validateEnum (schemaEnums schema),
      concatMap (validateModel schema) (schemaModels schema),
      concatMap (validateRelation schema) (concatMap modelRelations (schemaModels schema)),
      concatMap (validateInclude schema) (schemaIncludes schema),
      concatMap (validateUnique schema) (schemaUniques schema)
    ]

duplicateModelNames :: Schema -> [ValidationError]
duplicateModelNames schema =
  map DuplicateModelName (duplicates (map modelName (schemaModels schema)))

duplicateIncludeNames :: Schema -> [ValidationError]
duplicateIncludeNames schema =
  map DuplicateIncludeName (duplicates (map includeName (schemaIncludes schema)))

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
   in pkErrs ++ enumErrs

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

validateInclude :: Schema -> ModelInclude -> [ValidationError]
validateInclude schema incl =
  case findModel schema (includeRootModel incl) of
    Nothing ->
      [UnknownIncludeRoot (includeName incl) (includeRootModel incl)]
    Just root ->
      let treeErrs = validateIncludeTree schema (includeName incl) root (includeTree incl)
       in if null treeErrs
            then
              leftoverIncludeDeclaration schema incl
                ++ validateIncludeJoinAliases schema incl
                ++ validateIncludeDepth schema incl
            else treeErrs

leftoverIncludeDeclaration :: Schema -> ModelInclude -> [ValidationError]
leftoverIncludeDeclaration schema incl =
  [ LeftoverIncludeDeclaration (includeName incl)
    | not (isFullGraphInclude (schemaModels schema) incl)
        || includeName incl /= fullGraphIncludeName (includeRootModel incl)
  ]

validateIncludeDepth :: Schema -> ModelInclude -> [ValidationError]
validateIncludeDepth schema incl =
  let depth = includeMaxDepth schema incl
   in [ MaxIncludeDepthExceeded (includeName incl) depth defaultMaxIncludeDepth
        | depth > defaultMaxIncludeDepth
      ]

validateIncludeTree :: Schema -> Text -> Model -> [IncludeTree] -> [ValidationError]
validateIncludeTree schema includeName current edges =
  map (DuplicateIncludeRelation includeName (modelName current)) (duplicates (map includeRelation edges))
    ++ concatMap go edges
  where
    go tree =
      case findRelation current (includeRelation tree) of
        Nothing ->
          [UnknownIncludeRelation includeName (modelName current) (includeRelation tree)]
        Just rel ->
          case findModel schema (relToModel rel) of
            Nothing ->
              [UnknownRelationModel (relName rel) "to" (relToModel rel)]
            Just childModel ->
              validateIncludeTree schema includeName childModel (includeChildren tree)

findModel :: Schema -> Text -> Maybe Model
findModel schema name =
  find ((== name) . modelName) (schemaModels schema)

findRelation :: Model -> Text -> Maybe RelationSpec
findRelation model name =
  find ((== name) . relName) (modelRelations model)

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

validateIncludeJoinAliases :: Schema -> ModelInclude -> [ValidationError]
validateIncludeJoinAliases schema incl =
  if null (includeTree incl)
    then []
    else go (includeModelsInOrder schema incl) []
  where
    go [] _ = []
    go (model : rest) taken =
      case allocateJoinAlias model taken of
        Nothing ->
          [JoinAliasExhausted (includeName incl) (modelName model)]
        Just alias -> go rest (alias : taken)

duplicates :: (Eq a) => [a] -> [a]
duplicates xs = nub [x | x <- xs, length (filter (== x) xs) > 1]
