{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.NestedWrite
  ( nestedWriteRelations,
    nestedWriteExportItems,
    emitNestedWriteTypes,
    emitNestedWriteHelpers,
    emitCreateFn,
    emitClientCreateManyFn,
    emitUpdateFn,
    emitUpdateManyFn,
    emitClientUpsertFn,
    nestedChildSchemaImports,
    nestedChildClientImports,
    nestedChildEnumImports,
    nestedWriteUsesTransaction,
    schemaModuleAlias,
    childHasForeignKey,
  )
where

import Data.List (nub, nubBy)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.EmitCommon
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupField, lookupModel)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

type NestedRel = (RelationSpec, Model)

nestedWriteRelations :: Schema -> Model -> [NestedRel]
nestedWriteRelations schema root =
  [ (rel, lookupModel schema (relToModel rel))
    | rel <- modelRelations root,
      relKind rel == RelHasMany,
      childHasForeignKey schema rel
  ]

childHasForeignKey :: Schema -> RelationSpec -> Bool
childHasForeignKey schema rel =
  any
    ((== relForeignField rel) . fieldName)
    (modelFields (lookupModel schema (relToModel rel)))

nestedWriteUsesTransaction :: Schema -> Model -> Bool
nestedWriteUsesTransaction schema model =
  not (null (nestedWriteRelations schema model))

schemaModuleAlias :: Model -> Text
schemaModuleAlias model = modelName model <> "Schema"

nestedWriteExportItems :: Schema -> Model -> [Text]
nestedWriteExportItems schema root =
  nub $
    case nestedWriteRelations schema root of
      [] -> []
      rels ->
        [ createTypeName root <> "Scalars,",
          updateTypeName root <> "Scalars,"
        ]
          ++ concat
            [ [ nestedCreateTypeName rels rel child <> " (..),",
                nestedUpsertTypeName child <> " (..),",
                nestedUpdateTypeName rel <> " (..),",
                emptyNestedUpdateName rel <> ","
              ]
              | (rel, child) <- rels
            ]

uniqueByCreateType :: [NestedRel] -> [NestedRel]
uniqueByCreateType rels =
  nubBy
    ( \(r1, c1) (r2, c2) ->
        nestedCreateTypeName rels r1 c1 == nestedCreateTypeName rels r2 c2
    )
    rels

uniqueByChild :: [NestedRel] -> [NestedRel]
uniqueByChild =
  nubBy (\(_, a) (_, b) -> modelName a == modelName b)

nestedCreateTypeName :: [NestedRel] -> RelationSpec -> Model -> Text
nestedCreateTypeName rels rel child =
  if length (nub [relForeignField r | (r, c) <- rels, modelName c == modelName child]) > 1
    then upperFirst (relName rel) <> "NestedCreate"
    else modelName child <> "NestedCreate"

nestedUpsertTypeName :: Model -> Text
nestedUpsertTypeName child = modelName child <> "NestedUpsert"

nestedUpdateTypeName :: RelationSpec -> Text
nestedUpdateTypeName rel = upperFirst (relName rel) <> "Update"

emptyNestedUpdateName :: RelationSpec -> Text
emptyNestedUpdateName rel = "empty" <> nestedUpdateTypeName rel

createCtorName :: Model -> Text
createCtorName child = "Create" <> modelName child

connectCtorName :: Model -> Text
connectCtorName child = "Connect" <> modelName child

applyCreateFnName :: RelationSpec -> Text
applyCreateFnName rel = "apply" <> upperFirst (relName rel) <> "Create"

applyUpdateFnName :: RelationSpec -> Text
applyUpdateFnName rel = "apply" <> nestedUpdateTypeName rel

insertCreatesFnName :: RelationSpec -> Text
insertCreatesFnName rel = "insert" <> upperFirst (relName rel)

replaceFnName :: RelationSpec -> Text
replaceFnName rel = "replace" <> upperFirst (relName rel)

deleteFnName :: RelationSpec -> Text
deleteFnName rel = "delete" <> upperFirst (relName rel)

updateChildFnName :: RelationSpec -> Text
updateChildFnName rel = "update" <> upperFirst (relName rel) <> "Rows"

upsertFnName :: RelationSpec -> Text
upsertFnName rel = "upsert" <> upperFirst (relName rel)

connectFnName :: RelationSpec -> Text
connectFnName rel = "connect" <> upperFirst (relName rel)

disconnectFnName :: RelationSpec -> Text
disconnectFnName rel = "disconnect" <> upperFirst (relName rel)

fkField :: Model -> RelationSpec -> FieldSpec
fkField child rel = lookupField child (relForeignField rel)

nestedPayloadFields :: Model -> RelationSpec -> [FieldSpec]
nestedPayloadFields child rel =
  [f | f <- modelFields child, fieldName f /= relForeignField rel]

pkFieldName :: Model -> Text
pkFieldName model = fieldName (primaryKeyField model)

childUniqueWhereRef :: Model -> Model -> Text
childUniqueWhereRef root child
  | modelName root == modelName child = uniqueWhereName child
  | otherwise = modelName child <> "." <> uniqueWhereName child

childUniqueTypeRef :: Model -> Model -> Text
childUniqueTypeRef root child
  | modelName root == modelName child = uniqueTypeName child
  | otherwise = modelName child <> "." <> uniqueTypeName child

childCreateTypeRef :: Model -> Model -> Text
childCreateTypeRef root child
  | modelName root == modelName child = schemaModuleAlias root <> "." <> createTypeName child
  | otherwise = createTypeName child

childUpdateTypeRef :: Model -> Model -> Text
childUpdateTypeRef root child
  | modelName root == modelName child = schemaModuleAlias root <> "." <> updateTypeName child
  | otherwise = updateTypeName child

toScalarsName :: Model -> Text
toScalarsName model = "to" <> createTypeName model <> "Scalars"

toUpdateScalarsName :: Model -> Text
toUpdateScalarsName model = "to" <> updateTypeName model <> "Scalars"

hasCreateNestedName :: Model -> Text
hasCreateNestedName model = "has" <> modelName model <> "NestedCreate"

hasUpdateNestedName :: Model -> Text
hasUpdateNestedName model = "has" <> modelName model <> "NestedUpdate"

uniqueConflictName :: Model -> Text
uniqueConflictName model = lowerFirst (modelName model) <> "ConflictCols"

leaveFkValue :: FieldSpec -> Text
leaveFkValue f
  | fieldNullable f = "Omit"
  | otherwise = "Nothing"

fkAssign :: FieldSpec -> Text
fkAssign f
  | fieldNullable f = "Value parentId"
  | otherwise = "parentId"

connectFkValue :: FieldSpec -> Text
connectFkValue f
  | fieldNullable f = "Value parentId"
  | otherwise = "Just parentId"

ownsParentPred :: Model -> RelationSpec -> Text
ownsParentPred child rel =
  let fk = fieldName (fkField child rel)
   in if fieldNullable (fkField child rel)
        then "row." <> fk <> " == Just parentId"
        else "row." <> fk <> " == parentId"

emitNestedWriteTypes :: Schema -> Model -> Text
emitNestedWriteTypes schema root =
  case nestedWriteRelations schema root of
    [] -> ""
    rels ->
      T.unlines $
        map
          T.strip
          ( map (emitNestedCreateType root rels) (uniqueByCreateType rels)
              ++ map (emitNestedUpsertType root rels) (uniqueByChild rels)
              ++ map (emitNestedUpdateType root rels) rels
              ++ [emitRootCreateType root rels, emitRootUpdateType root rels]
          )

emitNestedCreateType :: Model -> [NestedRel] -> NestedRel -> Text
emitNestedCreateType root rels (rel, child) =
  T.unlines
    [ "data " <> nestedCreateTypeName rels rel child,
      "  = " <> createCtorName child,
      "      { " <> T.intercalate ", " fieldLines,
      "      }",
      "  | " <> connectCtorName child <> " " <> childUniqueTypeRef root child,
      "  deriving (Show, Eq)"
    ]
  where
    fieldLines =
      [ fieldName f <> " :: " <> createHsType f
        | f <- nestedPayloadFields child rel
      ]

emitNestedUpsertType :: Model -> [NestedRel] -> NestedRel -> Text
emitNestedUpsertType root rels (rel, child) =
  T.unlines
    [ "data " <> nestedUpsertTypeName child <> " = " <> nestedUpsertTypeName child,
      "  { where_ :: " <> childUniqueTypeRef root child,
      "  , create :: " <> nestedCreateTypeName rels rel child,
      "  , update :: " <> childUpdateTypeRef root child,
      "  }",
      "  deriving (Show, Eq)"
    ]

emitNestedUpdateType :: Model -> [NestedRel] -> NestedRel -> Text
emitNestedUpdateType root rels (rel, child) =
  let createTy = nestedCreateTypeName rels rel child
      uniqueTy = childUniqueTypeRef root child
      nullableFk = fieldNullable (fkField child rel)
   in T.unlines $
        [ "data " <> nestedUpdateTypeName rel <> " = " <> nestedUpdateTypeName rel,
          "  { replaceWith :: Maybe [" <> createTy <> "]",
          "  , create :: [" <> createTy <> "]",
          "  , createMany :: [" <> createTy <> "]",
          "  , connect :: [" <> uniqueTy <> "]",
          "  , delete :: [" <> uniqueTy <> "]",
          "  , update :: [(" <> uniqueTy <> ", " <> childUpdateTypeRef root child <> ")]",
          "  , upsert :: [" <> nestedUpsertTypeName child <> "]"
        ]
          ++ ["  , disconnect :: [" <> uniqueTy <> "]" | nullableFk]
          ++ [ "  }",
               "  deriving (Show, Eq)",
               "",
               emptyNestedUpdateName rel <> " :: " <> nestedUpdateTypeName rel,
               emptyNestedUpdateName rel <> " =",
               "  " <> nestedUpdateTypeName rel,
               "    { replaceWith = Nothing",
               "    , create = []",
               "    , createMany = []",
               "    , connect = []",
               "    , delete = []",
               "    , update = []",
               "    , upsert = []"
             ]
          ++ ["    , disconnect = []" | nullableFk]
          ++ ["    }"]

emitRootCreateType :: Model -> [NestedRel] -> Text
emitRootCreateType root rels =
  T.unlines
    [ "data " <> createTypeName root <> " = " <> createTypeName root,
      "  { " <> T.intercalate ",\n    " (scalarFields ++ relFields),
      "  }",
      "  deriving (Show, Eq)",
      "",
      "type " <> createTypeName root <> "Scalars = " <> schemaModuleAlias root <> "." <> createTypeName root,
      "",
      toScalarsName root <> " :: " <> createTypeName root <> " -> " <> createTypeName root <> "Scalars",
      toScalarsName root <> " input =",
      "  " <> schemaModuleAlias root <> "." <> createTypeName root,
      "    { " <> T.intercalate ",\n      " assignFields,
      "    }"
    ]
  where
    scalarFields = [fieldName f <> " :: " <> createHsType f | f <- modelFields root]
    relFields =
      [ relName rel <> " :: [" <> nestedCreateTypeName rels rel child <> "]"
        | (rel, child) <- rels
      ]
    assignFields = [fieldName f <> " = input." <> fieldName f | f <- modelFields root]

emitRootUpdateType :: Model -> [NestedRel] -> Text
emitRootUpdateType root rels =
  T.unlines
    [ "data " <> updateTypeName root <> " = " <> updateTypeName root,
      "  { " <> T.intercalate ",\n    " (scalarFields ++ relFields),
      "  }",
      "  deriving (Show, Eq)",
      "",
      "type " <> updateTypeName root <> "Scalars = " <> schemaModuleAlias root <> "." <> updateTypeName root,
      "",
      toUpdateScalarsName root <> " :: " <> updateTypeName root <> " -> " <> updateTypeName root <> "Scalars",
      toUpdateScalarsName root <> " input =",
      "  " <> schemaModuleAlias root <> "." <> updateTypeName root,
      "    { " <> T.intercalate ",\n      " assignFields,
      "    }"
    ]
  where
    scalarFields = [fieldName f <> " :: " <> updateHsType f | f <- updateFields root]
    relFields = [relName rel <> " :: Maybe " <> nestedUpdateTypeName rel | (rel, _) <- rels]
    assignFields = [fieldName f <> " = input." <> fieldName f | f <- updateFields root]

emitCreateFn :: Schema -> Model -> Text
emitCreateFn schema model =
  case nestedWriteRelations schema model of
    [] ->
      T.unlines
        [ "create :: " <> createTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "create = Insert.insert @" <> tableTypeName model <> " @" <> rowTypeName model
        ]
    rels ->
      T.unlines
        [ "create :: " <> createTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "create input =",
          "  if " <> hasCreateNestedName model <> " input",
          "    then transactionEither (createWithNested input)",
          "    else Insert.insert @" <> table <> " @" <> row <> " (" <> toScalarsName model <> " input)",
          "",
          hasCreateNestedName model <> " :: " <> createTypeName model <> " -> Bool",
          hasCreateNestedName model <> " input =",
          "  " <> T.intercalate " || " ["not (null input." <> relName rel <> ")" | (rel, _) <- rels],
          "",
          "createWithNested :: " <> createTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "createWithNested input = do",
          "  rootResult <- Insert.insert @" <> table <> " @" <> row <> " (" <> toScalarsName model <> " input)",
          "  case rootResult of",
          "    Left err -> pure (Left err)",
          "    Right row -> do",
          "      nestedResult <-",
          "        sequenceNested",
          "          [ " <> T.intercalate "\n          , " applyCalls,
          "          ]",
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> pure (Right row)"
        ]
  where
    table = tableTypeName model
    row = rowTypeName model
    applyCalls =
      [ applyCreateFnName rel <> " row." <> pkFieldName model <> " input." <> relName rel
        | (rel, _) <- nestedWriteRelations schema model
      ]

emitClientCreateManyFn :: Schema -> Model -> Text
emitClientCreateManyFn schema model =
  T.unlines
    [ "createMany :: [" <> scalars <> "] -> Db (Either ORMError Int)",
      "createMany = Insert.insertMany @" <> tableTypeName model
    ]
  where
    scalars =
      if nestedWriteUsesTransaction schema model
        then createTypeName model <> "Scalars"
        else createTypeName model

emitUpdateFn :: Schema -> Model -> Text
emitUpdateFn schema model =
  case nestedWriteRelations schema model of
    [] ->
      T.unlines
        [ "update :: " <> uniqueTypeName model <> " -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "update key input =",
          "  Update.updateWhere @" <> tableTypeName model <> " @" <> rowTypeName model <> " (" <> uniqueWhereName model <> " key) input"
        ]
    rels ->
      T.unlines
        [ "update :: " <> uniqueTypeName model <> " -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "update key input =",
          "  if " <> hasUpdateNestedName model <> " input",
          "    then transactionEither (updateWithNested key input)",
          "    else Update.updateWhere @" <> table <> " @" <> row <> " (" <> uniqueWhereName model <> " key) (" <> toUpdateScalarsName model <> " input)",
          "",
          hasUpdateNestedName model <> " :: " <> updateTypeName model <> " -> Bool",
          hasUpdateNestedName model <> " input =",
          "  " <> T.intercalate " || " ["isJust input." <> relName rel | (rel, _) <- rels],
          "",
          "updateWithNested :: " <> uniqueTypeName model <> " -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
          "updateWithNested key input = do",
          "  updateResult <- Update.updateWhere @" <> table <> " @" <> row <> " (" <> uniqueWhereName model <> " key) (" <> toUpdateScalarsName model <> " input)",
          "  case updateResult of",
          "    Left err -> pure (Left err)",
          "    Right row -> do",
          "      nestedResult <-",
          "        sequenceNested",
          "          [ " <> T.intercalate "\n          , " applyCalls,
          "          ]",
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> pure (Right row)"
        ]
  where
    table = tableTypeName model
    row = rowTypeName model
    applyCalls =
      [ "maybe (pure (Right ())) (" <> applyUpdateFnName rel <> " row." <> pkFieldName model <> ") input." <> relName rel
        | (rel, _) <- nestedWriteRelations schema model
      ]

emitUpdateManyFn :: Schema -> Model -> Text
emitUpdateManyFn schema model =
  T.unlines
    [ "updateMany :: Where " <> tableTypeName model <> " -> " <> scalars <> " -> Db (Either ORMError Int)",
      "updateMany = Update.updateMany @" <> tableTypeName model
    ]
  where
    scalars =
      if nestedWriteUsesTransaction schema model
        then updateTypeName model <> "Scalars"
        else updateTypeName model

emitClientUpsertFn :: Schema -> Model -> Text
emitClientUpsertFn schema model =
  T.unlines
    [ "upsert :: " <> uniqueKeyTypeName model <> " -> " <> createTy <> " -> " <> updateTy <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "upsert key createInput updateInput =",
      "  Insert.upsert @" <> tableTypeName model <> " @" <> rowTypeName model <> " (" <> uniqueConflictName model <> " key) createInput updateInput"
    ]
  where
    nested = nestedWriteUsesTransaction schema model
    createTy = if nested then createTypeName model <> "Scalars" else createTypeName model
    updateTy = if nested then updateTypeName model <> "Scalars" else updateTypeName model

emitNestedWriteHelpers :: Schema -> Model -> Text
emitNestedWriteHelpers schema root =
  case nestedWriteRelations schema root of
    [] -> ""
    rels ->
      T.unlines $
        map T.strip $
          emitSequenceNested : concatMap (emitRelHelpers root rels) rels

emitSequenceNested :: Text
emitSequenceNested =
  T.unlines
    [ "sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())",
      "sequenceNested [] = pure (Right ())",
      "sequenceNested (action : rest) = do",
      "  result <- action",
      "  case result of",
      "    Left err -> pure (Left err)",
      "    Right () -> sequenceNested rest"
    ]

emitRelHelpers :: Model -> [NestedRel] -> NestedRel -> [Text]
emitRelHelpers root rels nested@(rel, child) =
  [ emitApplyCreate root rels nested,
    emitApplyUpdate root nested,
    emitReplaceFn root rels nested,
    emitInsertCreateFn root rels nested,
    emitDeleteFn root nested,
    emitUpdateChildFn root nested,
    emitUpsertFn root nested,
    emitConnectFn root nested
  ]
    ++ [emitDisconnectFn root nested | fieldNullable (fkField child rel)]

emitApplyCreate :: Model -> [NestedRel] -> NestedRel -> Text
emitApplyCreate root rels (rel, child) =
  T.unlines
    [ applyCreateFnName rel <> " :: " <> pkHsType root <> " -> [" <> nestedCreateTypeName rels rel child <> "] -> Db (Either ORMError ())",
      applyCreateFnName rel <> " = " <> insertCreatesFnName rel
    ]

emitApplyUpdate :: Model -> NestedRel -> Text
emitApplyUpdate root (rel, child) =
  T.unlines $
    [ applyUpdateFnName rel <> " :: " <> pkHsType root <> " -> " <> nestedUpdateTypeName rel <> " -> Db (Either ORMError ())",
      applyUpdateFnName rel <> " parentId ops = do",
      "  replaced <- case ops.replaceWith of",
      "    Nothing -> pure (Right ())",
      "    Just items -> " <> replaceFnName rel <> " parentId items",
      "  case replaced of",
      "    Left err -> pure (Left err)",
      "    Right () ->",
      "      sequenceNested",
      "        [ " <> deleteFnName rel <> " parentId ops.delete",
      "        , " <> updateChildFnName rel <> " parentId ops.update",
      "        , " <> upsertFnName rel <> " parentId ops.upsert",
      "        , " <> insertCreatesFnName rel <> " parentId ops.create",
      "        , " <> insertCreatesFnName rel <> " parentId ops.createMany",
      "        , " <> connectFnName rel <> " parentId ops.connect"
    ]
      ++ ["        , " <> disconnectFnName rel <> " parentId ops.disconnect" | fieldNullable (fkField child rel)]
      ++ ["        ]"]

emitReplaceFn :: Model -> [NestedRel] -> NestedRel -> Text
emitReplaceFn root rels (rel, child) =
  T.unlines
    [ replaceFnName rel <> " :: " <> pkHsType root <> " -> [" <> nestedCreateTypeName rels rel child <> "] -> Db (Either ORMError ())",
      replaceFnName rel <> " parentId items = do",
      "  result <-",
      "    Delete.deleteWhere $",
      "      Delete.whereDelete (fieldColumn " <> fkBinder <> " <> \" = ?\") [toField parentId] (Delete.emptyDelete @" <> tableTypeName child <> ")",
      "  case result of",
      "    Left err -> pure (Left err)",
      "    Right _ -> " <> insertCreatesFnName rel <> " parentId items"
    ]
  where
    fkBinder = fieldBinder child (fkField child rel)

emitInsertCreateFn :: Model -> [NestedRel] -> NestedRel -> Text
emitInsertCreateFn root rels (rel, child) =
  T.unlines
    [ insertCreatesFnName rel <> " :: " <> pkHsType root <> " -> [" <> nestedCreateTypeName rels rel child <> "] -> Db (Either ORMError ())",
      insertCreatesFnName rel <> " parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (nested : rest) = do",
      "      result <- case nested of",
      "        " <> connectCtorName child <> " key -> " <> connectFnName rel <> " parentId [key]",
      "        " <> createCtorName child <> " {" <> T.intercalate ", " binders <> "} -> do",
      "          inserted <- Insert.insert @" <> tableTypeName child <> " @" <> rowTypeName child <> " " <> childCreateTypeRef root child,
      "            { " <> T.intercalate ",\n              " assigns,
      "            }",
      "          pure $ case inserted of",
      "            Left err -> Left err",
      "            Right _ -> Right ()",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right () -> go rest"
    ]
  where
    payload = nestedPayloadFields child rel
    binders = map fieldName payload
    fk = fkField child rel
    assigns =
      [fieldName f <> " = " <> fieldName f | f <- payload]
        ++ [fieldName fk <> " = " <> fkAssign fk]

emitDeleteFn :: Model -> NestedRel -> Text
emitDeleteFn root (rel, child) =
  T.unlines
    [ deleteFnName rel <> " :: " <> pkHsType root <> " -> [" <> childUniqueTypeRef root child <> "] -> Db (Either ORMError ())",
      deleteFnName rel <> " _ [] = pure (Right ())",
      deleteFnName rel <> " parentId keys = sequenceNested (map deleteOne keys)",
      "  where",
      "    deleteOne key = do",
      "      result <- Delete.deleteMany @" <> tableTypeName child,
      "        (" <> childUniqueWhereRef root child <> " key `and_` eq " <> fkBinder <> " parentId)",
      "      pure $ case result of",
      "        Left err -> Left err",
      "        Right _ -> Right ()"
    ]
  where
    fkBinder = fieldBinder child (fkField child rel)

emitUpdateChildFn :: Model -> NestedRel -> Text
emitUpdateChildFn root (rel, child) =
  T.unlines
    [ updateChildFnName rel <> " :: " <> pkHsType root <> " -> [(" <> childUniqueTypeRef root child <> ", " <> childUpdateTypeRef root child <> ")] -> Db (Either ORMError ())",
      updateChildFnName rel <> " parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go ((key, nested) : rest) = do",
      "      let patched = " <> leaveFkUpdateExpr root child rel "nested",
      "      result <-",
      "        Update.updateWhere @" <> tableTypeName child <> " @" <> rowTypeName child,
      "          (" <> childUniqueWhereRef root child <> " key `and_` eq " <> fkBinder <> " parentId)",
      "          patched",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> go rest"
    ]
  where
    fkBinder = fieldBinder child (fkField child rel)

leaveFkUpdateExpr :: Model -> Model -> RelationSpec -> Text -> Text
leaveFkUpdateExpr root child rel source =
  childUpdateTypeRef root child
    <> " { "
    <> T.intercalate ", " fields
    <> " }"
  where
    fk = fkField child rel
    fields =
      [ if fieldName f == fieldName fk
          then fieldName f <> " = " <> leaveFkValue fk
          else fieldName f <> " = " <> source <> "." <> fieldName f
        | f <- updateFields child
      ]

emitUpsertFn :: Model -> NestedRel -> Text
emitUpsertFn root (rel, child) =
  T.unlines
    [ upsertFnName rel <> " :: " <> pkHsType root <> " -> [" <> nestedUpsertTypeName child <> "] -> Db (Either ORMError ())",
      upsertFnName rel <> " parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (item : rest) = do",
      "      existing <- Ops.findMany @" <> tableTypeName child <> " @" <> rowTypeName child <> " (matching (" <> childUniqueWhereRef root child <> " item.where_))",
      "      result <- case fromUniqueRows existing of",
      "        Left err -> pure (Left err)",
      "        Right Nothing -> " <> insertCreatesFnName rel <> " parentId [item.create]",
      "        Right (Just row) ->",
      "          if " <> ownsParentPred child rel,
      "            then do",
      "              let patched = " <> leaveFkUpdateExpr root child rel "item.update",
      "              updated <-",
      "                Update.updateWhere @" <> tableTypeName child <> " @" <> rowTypeName child,
      "                  (" <> childUniqueWhereRef root child <> " item.where_ `and_` eq " <> fkBinder <> " parentId)",
      "                  patched",
      "              pure $ case updated of",
      "                Left err -> Left err",
      "                Right _ -> Right ()",
      "            else pure (Left (UniqueViolation \"nested upsert would reparent a row owned by another parent\"))",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right () -> go rest"
    ]
  where
    fkBinder = fieldBinder child (fkField child rel)

emitConnectFn :: Model -> NestedRel -> Text
emitConnectFn root (rel, child) =
  T.unlines
    [ connectFnName rel <> " :: " <> pkHsType root <> " -> [" <> childUniqueTypeRef root child <> "] -> Db (Either ORMError ())",
      connectFnName rel <> " _ [] = pure (Right ())",
      connectFnName rel <> " parentId keys = sequenceNested (map connectOne keys)",
      "  where",
      "    connectOne key = do",
      "      result <-",
      "        Update.updateWhere @" <> tableTypeName child <> " @" <> rowTypeName child,
      "          (" <> childUniqueWhereRef root child <> " key)",
      "          (" <> childUpdateTypeRef root child,
      "            { " <> T.intercalate ",\n              " setFields,
      "            })",
      "      pure $ case result of",
      "        Left err -> Left err",
      "        Right _ -> Right ()"
    ]
  where
    fk = fkField child rel
    setFields =
      [ if fieldName f == fieldName fk
          then fieldName f <> " = " <> connectFkValue fk
          else fieldName f <> " = " <> leaveFkValue f
        | f <- updateFields child
      ]

emitDisconnectFn :: Model -> NestedRel -> Text
emitDisconnectFn root (rel, child) =
  T.unlines
    [ disconnectFnName rel <> " :: " <> pkHsType root <> " -> [" <> childUniqueTypeRef root child <> "] -> Db (Either ORMError ())",
      disconnectFnName rel <> " _ [] = pure (Right ())",
      disconnectFnName rel <> " parentId keys = sequenceNested (map disconnectOne keys)",
      "  where",
      "    disconnectOne key = do",
      "      result <-",
      "        Update.updateWhere @" <> tableTypeName child <> " @" <> rowTypeName child,
      "          (" <> childUniqueWhereRef root child <> " key `and_` eq " <> fkBinder <> " parentId)",
      "          (" <> childUpdateTypeRef root child,
      "            { " <> T.intercalate ",\n              " setFields,
      "            })",
      "      pure $ case result of",
      "        Left err -> Left err",
      "        Right _ -> Right ()"
    ]
  where
    fk = fkField child rel
    fkBinder = fieldBinder child fk
    setFields =
      [ if fieldName f == fieldName fk
          then fieldName f <> " = Null"
          else fieldName f <> " = " <> leaveFkValue f
        | f <- updateFields child
      ]

nestedChildSchemaImports :: Text -> Schema -> Model -> [Text]
nestedChildSchemaImports moduleName schema root =
  [ childSchemaImport moduleName nested
    | nested@(_, child) <- uniqueByChild (nestedWriteRelations schema root),
      modelName child /= modelName root
  ]

childSchemaImport :: Text -> NestedRel -> Text
childSchemaImport moduleName (rel, child) =
  "import "
    <> schemaModuleFor moduleName child
    <> " ("
    <> T.intercalate ", " names
    <> ")"
  where
    names =
      [ rowTypeName child <> " (..)",
        tableTypeName child,
        fieldBinder child (primaryKeyField child),
        fieldBinder child (fkField child rel)
      ]
        ++ [ createTypeName child <> " (..)",
             updateTypeName child <> " (..)"
           ]

nestedChildClientImports :: Text -> Schema -> Model -> [Text]
nestedChildClientImports moduleName schema root =
  [ childClientImport moduleName child
    | (_, child) <- uniqueByChild (nestedWriteRelations schema root),
      modelName child /= modelName root
  ]

childClientImport :: Text -> Model -> Text
childClientImport moduleName child =
  "import qualified "
    <> childClientModule moduleName child
    <> " as "
    <> modelName child
    <> " ("
    <> uniqueTypeName child
    <> " (..), "
    <> uniqueWhereName child
    <> ")"

childClientModule :: Text -> Model -> Text
childClientModule clientModule child =
  case T.breakOnEnd "." clientModule of
    (prefix, _) -> prefix <> modelName child

nestedChildEnumImports :: Text -> Schema -> Model -> [Text]
nestedChildEnumImports moduleName schema root =
  nub
    [ "import " <> clientSchemaPrefix moduleName <> enumName e <> " (" <> enumName e <> " (..))"
      | (_, child) <- uniqueByChild (nestedWriteRelations schema root),
        f <- modelFields child,
        TyEnum wanted <- [fieldType f],
        e <- schemaEnums schema,
        enumName e == wanted,
        isNothing (enumImport e)
    ]
