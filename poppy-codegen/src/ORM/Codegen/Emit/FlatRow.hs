{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Emit.FlatRow
  ( emitFlatJoinRow,
    emitColumnList,
    emitJoinBuilder,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.EmitCommon (fieldBinder, hsType)
import ORM.Codegen.IR
import ORM.Codegen.IncludePath
  ( JoinPath (..),
    PathStep (..),
    columnListName,
    joinBuilderName,
    joinPathModels,
    joinRowName,
    lookupModelAlias,
  )
import qualified ORM.Codegen.JoinAlias as JoinAlias

emitFlatJoinRow :: Schema -> JoinPath -> Text
emitFlatJoinRow _schema path =
  let models = joinPathModels path
      rowName = joinRowName path
      allFields =
        concat
          [ map (flatField model (idx > 0)) (fieldsForModel path model idx)
            | (idx, model) <- zip [0 ..] models
          ]
   in T.unlines
        [ "data " <> rowName <> " = " <> rowName,
          "  { " <> T.intercalate ",\n    " allFields,
          "  }",
          "  deriving (Show, Eq)",
          "",
          "instance FromRow " <> rowName <> " where",
          "  fromRow = " <> rowName <> " <$> " <> fromRowBody (length allFields)
        ]

emitColumnList :: Schema -> JoinPath -> Text
emitColumnList _schema path =
  case jpSteps path of
    [] -> error "emitColumnList: empty join path"
    [_single] ->
      let aliases = modelAliasesForPath path
          models = joinPathModels path
          name = columnListName path
          cols =
            concat
              [ map (columnExprWithAlias (lookupModelAlias aliases model) model) (fieldsForModel path model idx)
                | (idx, model) <- zip [0 ..] models
              ]
       in T.unlines
            [ name <> " :: [Text]",
              name <> " =",
              "  [ " <> T.intercalate ",\n    " cols,
              "  ]"
            ]
    steps ->
      let prefix = JoinPath {jpSteps = init steps}
          aliases = modelAliasesForPath path
          lastStep = last steps
          lastModel = psTo lastStep
          lastIdx = length steps
          name = columnListName path
          prefixName = columnListName prefix
          lastCols =
            map (columnExprWithAlias (lookupModelAlias aliases lastModel) lastModel) (fieldsForModel path lastModel lastIdx)
       in T.unlines
            [ name <> " :: [Text]",
              name <> " =",
              "  " <> prefixName,
              "    ++ [ " <> T.intercalate ",\n         " lastCols,
              "       ]"
            ]

modelAliasesForPath :: JoinPath -> [(Text, Text)]
modelAliasesForPath path =
  map (first modelName) $
    JoinAlias.assignModelAliases (joinPathModels path)

emitJoinBuilder :: Schema -> JoinPath -> Text
emitJoinBuilder _schema path =
  case jpSteps path of
    [] -> error "emitJoinBuilder: empty join path"
    [step] ->
      let root = psFrom step
          rootTable = modelName root <> "Table"
          builderName = joinBuilderName path
          colsName = columnListName path
          aliases = modelAliasesForPath path
          initial =
            emitRootJoinStep step (lookupModelAlias aliases (psFrom step)) (lookupModelAlias aliases (psTo step))
       in T.unlines
            [ builderName <> " :: JoinChain.JoinChain " <> rootTable,
              builderName <> " =",
              "  JoinChain.selectColumns " <> colsName <> " $",
              "    " <> initial
            ]
    steps ->
      let root = psFrom (head steps)
          rootTable = modelName root <> "Table"
          builderName = joinBuilderName path
          colsName = columnListName path
          aliases = modelAliasesForPath path
          prefix = JoinPath {jpSteps = init steps}
          innerName = joinBuilderName prefix
          lastStep = last steps
          wrapped =
            emitNextJoinStepWithBuilder aliases lastStep innerName
       in T.unlines
            [ builderName <> " :: JoinChain.JoinChain " <> rootTable,
              builderName <> " =",
              "  JoinChain.selectColumns " <> colsName <> " $",
              "    " <> wrapped
            ]

emitNextJoinStepWithBuilder :: [(Text, Text)] -> PathStep -> Text -> Text
emitNextJoinStepWithBuilder aliases PathStep {psFrom = from, psRel = rel, psTo = to} builder =
  let fromAlias = lookupModelAlias aliases from
      toAlias = lookupModelAlias aliases to
      fn = case relKind rel of
        RelBelongsTo -> "JoinChain.addBelongsToJoin"
        RelHasMany -> "JoinChain.addHasManyJoin"
   in fn <> " " <> relName rel <> " \"" <> fromAlias <> "\" \"" <> toAlias <> "\" " <> builder

emitRootJoinStep :: PathStep -> Text -> Text -> Text
emitRootJoinStep PathStep {psRel = rel} rootAlias childAlias =
  case relKind rel of
    RelBelongsTo -> error "emitJoinBuilder: BelongsTo root edge not supported yet"
    RelHasMany ->
      "JoinChain.buildJoinChainFromHasMany "
        <> relName rel
        <> " \""
        <> rootAlias
        <> "\" \""
        <> childAlias
        <> "\""

fieldsForModel :: JoinPath -> Model -> Int -> [FieldSpec]
fieldsForModel path model idx =
  case idx of
    0 -> modelFields model
    _ ->
      let step = jpSteps path !! (idx - 1)
       in filter (not . isRelationFk (psRel step)) (modelFields model)

columnExprWithAlias :: Text -> Model -> FieldSpec -> Text
columnExprWithAlias alias model f =
  "JoinChain.col \""
    <> alias
    <> "\" "
    <> modelName model
    <> "."
    <> fieldBinder model f

flatField :: Model -> Bool -> FieldSpec -> Text
flatField model asJoinSide f =
  binder <> " :: " <> ty
  where
    binder = fieldBinder model f
    base = hsType (fieldType f)
    asMaybe = asJoinSide || fieldNullable f
    ty = if asMaybe then "Maybe " <> base else base

isRelationFk :: RelationSpec -> FieldSpec -> Bool
isRelationFk rel f = fieldName f == relForeignField rel

fromRowBody :: Int -> Text
fromRowBody n =
  T.intercalate " <*> " (replicate n "field")
