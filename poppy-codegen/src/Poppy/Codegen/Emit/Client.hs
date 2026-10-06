{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.Client
  ( emitClientModule,
    emitSimpleClientModule,
  )
where

import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.EmitCommon
  ( createTypeName,
    fieldBinder,
    hsType,
    includeFieldName,
    parsePickedName,
    pickedTypeName,
    primaryKeyField,
    rowTypeName,
    selectColumnsFnName,
    selectDefaultName,
    selectTypeName,
    tableTypeName,
    updateTypeName,
  )
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupField, lookupModel, lookupUniques)
import Poppy.Codegen.TextUtil (upperFirst)

emitClientModule :: Text -> Schema -> Model -> Text
emitClientModule moduleName schema model =
  if null (modelRelations model)
    then emitSimpleClientModule moduleName schema model
    else emitIncludeClientModule moduleName schema model

emitSimpleClientModule :: Text -> Schema -> Model -> Text
emitSimpleClientModule moduleName schema model =
  T.unlines
    [ "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE RecordWildCards #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module " <> moduleName,
      emitSimpleExports model,
      "where",
      "",
      "import Data.UUID (UUID)",
      "import Poppy.Db (Db)",
      "import Poppy.Errors (ORMError (..), requireFound)",
      "import qualified Poppy.Delete as Delete",
      "import qualified Poppy.Insert as Insert",
      "import qualified Poppy.Operations as Ops",
      "import Poppy.Query (OrderBy, QueryBuilder, applyQueryModifiers, matching, selectColumns)",
      "import Poppy.Select (OmitSelect (..), Picked (..))",
      "import Poppy.Where (Where)",
      "import "
        <> schemaModule moduleName model
        <> " ("
        <> createTypeName model
        <> " (..), "
        <> rowTypeName model
        <> " (..), "
        <> selectTypeName model
        <> " (..), "
        <> pickedTypeName model
        <> " (..), "
        <> selectDefaultName model
        <> ", "
        <> selectColumnsFnName model
        <> ", "
        <> parsePickedName model
        <> ", "
        <> tableTypeName model
        <> ", "
        <> updateTypeName model
        <> " (..), "
        <> fieldBinder model (primaryKeyField model)
        <> ")",
      "import qualified Poppy.Update as Update",
      "",
      emitCreateFn model,
      "",
      emitClientCreateManyFn model,
      "",
      emitUpdateFn model,
      "",
      emitUpdateManyFn model,
      "",
      emitClientUpsertFn schema model,
      "",
      emitQueryType model,
      "",
      emitEmptyQueryFn model,
      "",
      emitResolveSelect model,
      "",
      emitFindManyFn model,
      "",
      emitCountFn model,
      "",
      emitDeleteFn model,
      "",
      emitDeleteManyFn model
    ]

emitIncludeClientModule :: Text -> Schema -> Model -> Text
emitIncludeClientModule moduleName schema root =
  T.unlines
    ( [ "{-# LANGUAGE AllowAmbiguousTypes #-}",
        "{-# LANGUAGE DuplicateRecordFields #-}",
        "{-# LANGUAGE FlexibleContexts #-}",
        "{-# LANGUAGE FlexibleInstances #-}",
        "{-# LANGUAGE MultiParamTypeClasses #-}",
        "{-# LANGUAGE NamedFieldPuns #-}",
        "{-# LANGUAGE NoFieldSelectors #-}",
        "{-# LANGUAGE OverloadedRecordDot #-}",
        "{-# LANGUAGE RecordWildCards #-}",
        "{-# LANGUAGE TypeApplications #-}",
        "{-# LANGUAGE TypeFamilies #-}",
        "{-# LANGUAGE UndecidableInstances #-}",
        "",
        emitIncludeClientHeader schema root,
        "module " <> moduleName
      ]
        ++ exportLinesFromItems (includeClientExportItems schema root)
        ++ ["where", ""]
        ++ includeClientImportLines moduleName schema root
    )
    <> "\n"
    <> intercalateSections
      ( [ emitCreateFn root,
          emitClientCreateManyFn root,
          emitUpdateFn root,
          emitUpdateManyFn root,
          emitClientUpsertFn schema root
        ]
          ++ nestedWriteSections
          ++ emitIncludeReadDefinitions schema root
          ++ [ emitDeleteFn root,
               emitDeleteManyFn root
             ]
      )
  where
    nestedWriteSections =
      case nestedWriteRelation schema root of
        Nothing -> []
        Just (rel, child) ->
          [ emitWriteCreateType root child rel,
            emitWriteUpdateType root rel,
            emitNestedToChildCreate child rel,
            emitNestedToChildUpdate child rel,
            emitNestedWriteHelpers schema root child rel,
            emitWriteCreate schema root rel,
            emitWriteUpdate schema root rel
          ]

emitIncludeClientHeader :: Schema -> Model -> Text
emitIncludeClientHeader schema root =
  T.intercalate "\n" $
    ["{- | Generated Client. Do not edit."]
      ++ nestedDocs
      ++ ["-}"]
  where
    nestedDocs =
      case nestedWriteRelation schema root of
        Nothing -> []
        Just _ ->
          [ "",
            "Nested writes ('createNested' / 'updateNested'):",
            "  Set xs  — replace all children with xs",
            "  Ops o   — create, connect, disconnect, delete, update, upsert"
          ]

emitWriteCreateType :: Model -> Model -> RelationSpec -> Text
emitWriteCreateType root child rel =
  T.unlines
    [ emitNestedCreateType child rel,
      emitNestedOpsType child,
      "data " <> nestedWriteTypeName root rel <> " = Set [" <> nestedCreateTypeName child <> "] | Ops " <> nestedOpsTypeName child,
      "  deriving (Show, Eq)",
      "",
      emptyNestedOpsName child <> " :: " <> nestedOpsTypeName child,
      emptyNestedOpsName child <> " =",
      "  " <> nestedOpsTypeName child,
      "    { create = []",
      "    , createMany = []",
      "    , connect = []",
      "    , disconnect = []",
      "    , delete = []",
      "    , update = []",
      "    , upsert = []",
      "    }",
      "",
      "data " <> writeCreateTypeName root <> " = " <> writeCreateTypeName root,
      "  { root :: " <> createTypeName root,
      "  , " <> childrenFieldName root rel <> " :: " <> nestedWriteTypeName root rel,
      "  }",
      "  deriving (Show, Eq)"
    ]

emitWriteUpdateType :: Model -> RelationSpec -> Text
emitWriteUpdateType root rel =
  T.unlines
    [ "data " <> writeUpdateTypeName root <> " = " <> writeUpdateTypeName root,
      "  { root :: " <> updateTypeName root,
      "  , " <> childrenFieldName root rel <> " :: Maybe " <> nestedWriteTypeName root rel,
      "  }",
      "  deriving (Show, Eq)"
    ]

emitNestedOpsType :: Model -> Text
emitNestedOpsType child =
  T.unlines
    [ "data " <> nestedOpsTypeName child <> " = " <> nestedOpsTypeName child,
      "  { create :: [" <> nestedCreateTypeName child <> "]",
      "  , createMany :: [" <> nestedCreateTypeName child <> "]",
      "  , connect :: [UUID]",
      "  , disconnect :: [UUID]",
      "  , delete :: [UUID]",
      "  , update :: [(UUID, " <> nestedCreateTypeName child <> ")]",
      "  , upsert :: [" <> nestedCreateTypeName child <> "]",
      "  }",
      "  deriving (Show, Eq)"
    ]

emitNestedCreateType :: Model -> RelationSpec -> Text
emitNestedCreateType child rel =
  T.unlines
    [ "data " <> nestedCreateTypeName child <> " = " <> nestedCreateTypeName child,
      "  { " <> T.intercalate ", " (nestedFieldLines child rel),
      "  }",
      "  deriving (Show, Eq)"
    ]

nestedFieldLines :: Model -> RelationSpec -> [Text]
nestedFieldLines child rel =
  [ fieldName f <> " :: Maybe " <> hsType (fieldType f)
    | f <- modelFields child,
      fieldIsPrimaryKey f
  ]
    ++ [ fieldName f <> " :: " <> hsType (fieldType f)
         | f <- modelFields child,
           not (fieldIsPrimaryKey f),
           fieldName f /= relForeignField rel
       ]

emitNestedToChildCreate :: Model -> RelationSpec -> Text
emitNestedToChildCreate child rel =
  T.unlines $
    [ "to" <> createTypeName child <> " :: UUID -> " <> nestedCreateTypeName child <> " -> " <> createTypeName child,
      "to" <> createTypeName child <> " parentId nested =",
      "  " <> createTypeName child,
      "    { " <> fieldName pkField <> " = nested." <> fieldName pkField <> ","
    ]
      ++ [ "      " <> fieldName f <> " = nested." <> fieldName f <> ","
           | f <- modelFields child,
             not (fieldIsPrimaryKey f),
             fieldName f /= relForeignField rel
         ]
      ++ ["      " <> relForeignField rel <> " = " <> fkAssign, "    }"]
  where
    pkField = primaryKeyField child
    fkAssign =
      if fieldNullable (lookupField child (relForeignField rel))
        then "Value parentId"
        else "parentId"

emitNestedToChildUpdate :: Model -> RelationSpec -> Text
emitNestedToChildUpdate child rel =
  T.unlines
    [ "to" <> updateTypeName child <> " :: " <> nestedCreateTypeName child <> " -> " <> updateTypeName child,
      "to" <> updateTypeName child <> " nested =",
      "  " <> updateTypeName child,
      "    { " <> T.intercalate ",\n      " updateFields,
      "    }"
    ]
  where
    updateFields =
      [ if fieldName f == relForeignField rel
          then fieldName f <> " = " <> leaveUnchanged f
          else fieldName f <> " = " <> assignUpdate f
        | f <- modelFields child,
          not (fieldIsPrimaryKey f)
      ]
    leaveUnchanged f =
      if fieldNullable f then "Omit" else "Nothing"
    assignUpdate f =
      if fieldNullable f
        then "Value nested." <> fieldName f
        else "Just nested." <> fieldName f

emitNestedWriteHelpers :: Schema -> Model -> Model -> RelationSpec -> Text
emitNestedWriteHelpers schema root child rel =
  T.unlines
    [ "sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())",
      "sequenceNested [] = pure (Right ())",
      "sequenceNested (action : rest) = do",
      "  result <- action",
      "  case result of",
      "    Left err -> pure (Left err)",
      "    Right () -> sequenceNested rest",
      "",
      applyWrite <> " :: UUID -> " <> writeTy <> " -> Db (Either ORMError ())",
      applyWrite <> " parentId write =",
      "  case write of",
      "    Set items -> do",
      "      result <-",
      "        Delete.deleteWhere $",
      "          Delete.whereDelete (fieldColumn " <> fkBinder <> " <> \" = ?\") [toField parentId] (Delete.emptyDelete @" <> childTable <> ")",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> insertNestedCreates parentId items",
      "    Ops ops -> " <> applyOps <> " parentId ops",
      "",
      "insertNestedCreates :: UUID -> [" <> nestedCreateTypeName child <> "] -> Db (Either ORMError ())",
      "insertNestedCreates parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (nested : rest) = do",
      "      result <- Insert.insert @" <> childTable <> " @" <> childRow <> " (" <> toCreate <> " parentId nested)",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> go rest",
      "",
      applyOps <> " :: UUID -> " <> nestedOpsTypeName child <> " -> Db (Either ORMError ())",
      applyOps <> " parentId ops =",
      "  sequenceNested",
      "    [ deleteNested parentId ops.delete",
      "    , deleteNested parentId ops.disconnect",
      "    , updateChildRows parentId ops.update",
      "    , upsertNested parentId ops.upsert",
      "    , insertNestedCreates parentId ops.create",
      "    , insertNestedCreateMany parentId ops.createMany",
      "    , connectNested parentId ops.connect",
      "    ]",
      "",
      "deleteNested :: UUID -> [UUID] -> Db (Either ORMError ())",
      "deleteNested _ [] = pure (Right ())",
      "deleteNested parentId ids = do",
      "  result <- Delete.deleteMany @" <> childTable <> " (in_ " <> pkBinder <> " ids `and_` eq " <> fkBinder <> " parentId)",
      "  pure $ case result of",
      "    Left err -> Left err",
      "    Right _ -> Right ()",
      "",
      "updateChildRows :: UUID -> [(UUID, " <> nestedCreateTypeName child <> ")] -> Db (Either ORMError ())",
      "updateChildRows parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go ((childId, nested) : rest) = do",
      "      result <-",
      "        Update.updateBuilder @" <> childTable <> " @" <> childRow <> " $",
      "          Update.whereUpdate (fieldColumn " <> pkBinder <> " <> \" = ?\") [toField childId] $",
      "            Update.whereUpdate (fieldColumn " <> fkBinder <> " <> \" = ?\") [toField parentId] $",
      "              Update.toUpdateBuilder @" <> childTable <> " (" <> toUpdate <> " nested)",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> go rest",
      "",
      emitUpsertFn schema child rel,
      emitCreateManyFn schema child rel,
      "connectNested :: UUID -> [UUID] -> Db (Either ORMError ())",
      "connectNested parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (childId : rest) = do",
      "      result <-",
      "        Update.updateBuilder @" <> childTable <> " @" <> childRow <> " $",
      "          Update.setField " <> fkBinder <> " parentId $",
      "            Update.whereUpdate (fieldColumn " <> pkBinder <> " <> \" = ?\") [toField childId] $",
      "              Update.emptyUpdate @" <> childTable,
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> go rest"
    ]
  where
    writeTy = nestedWriteTypeName root rel
    applyWrite = applyWriteFnName root rel
    applyOps = applyOpsFnName child
    childTable = tableTypeName child
    childRow = rowTypeName child
    pkBinder = fieldBinder child (primaryKeyField child)
    fkBinder = fieldBinder child (lookupField child (relForeignField rel))
    toCreate = "to" <> createTypeName child
    toUpdate = "to" <> updateTypeName child

emitUpsertFn :: Schema -> Model -> RelationSpec -> Text
emitUpsertFn schema child _rel =
  T.unlines
    [ "upsertNested :: UUID -> [" <> nestedCreateTypeName child <> "] -> Db (Either ORMError ())",
      "upsertNested parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (nested : rest) = do",
      "      result <-",
      "        Insert.insertBuilder @" <> tableTypeName child <> " @" <> rowTypeName child <> " $",
      conflictLine,
      "          Insert.toInsertBuilder @" <> tableTypeName child <> " (to" <> createTypeName child <> " parentId nested)",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right _ -> go rest"
    ]
  where
    conflictLine = case uniqueCols schema child of
      [] -> "          Prelude.id $"
      cols ->
        case upsertSetCols schema child of
          [] -> "          Insert.onConflictDoNothing " <> listLit cols <> " $"
          setCols ->
            "          Insert.onConflictDoUpdate " <> listLit cols <> " " <> listLit setCols <> " $"

emitCreateManyFn :: Schema -> Model -> RelationSpec -> Text
emitCreateManyFn schema child _rel =
  T.unlines
    [ "insertNestedCreateMany :: UUID -> [" <> nestedCreateTypeName child <> "] -> Db (Either ORMError ())",
      "insertNestedCreateMany parentId = go",
      "  where",
      "    go [] = pure (Right ())",
      "    go (nested : rest) = do",
      "      result <-",
      "        Insert.tryExecuteInsert $",
      conflictLine,
      "          Insert.toInsertBuilder @" <> tableTypeName child <> " (to" <> createTypeName child <> " parentId nested)",
      "      case result of",
      "        Left err -> pure (Left err)",
      "        Right () -> go rest"
    ]
  where
    conflictLine = case uniqueCols schema child of
      [] -> "          Prelude.id $"
      cols ->
        "          Insert.onConflictDoNothing " <> listLit cols <> " $"

uniqueCols :: Schema -> Model -> [Text]
uniqueCols schema child =
  case lookupUniques schema (modelName child) of
    (constraint : _) -> map (fieldColumn . lookupField child) (uniqueFields constraint)
    [] -> []

upsertSetCols :: Schema -> Model -> [Text]
upsertSetCols schema child =
  case lookupUniques schema (modelName child) of
    (constraint : _) ->
      [ fieldColumn f
        | f <- modelFields child,
          not (fieldIsPrimaryKey f),
          fieldName f `notElem` uniqueFields constraint
      ]
    [] -> []

listLit :: [Text] -> Text
listLit cols =
  "[" <> T.intercalate ", " ["\"" <> col <> "\"" | col <- cols] <> "]"

emitWriteCreate :: Schema -> Model -> RelationSpec -> Text
emitWriteCreate _schema root rel =
  let childrenField = childrenFieldName root rel
      applyWrite = applyWriteFnName root rel
      table = tableTypeName root
      row = rowTypeName root
      createBody =
        [ "  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id",
          "  let rootInput =",
          "        " <> createRootInputExpr root,
          "  rootResult <-",
          "    Insert.insert @" <> table <> " @" <> row <> " rootInput",
          "  case rootResult of",
          "    Left err -> pure (Left err)",
          "    Right _ -> do",
          "      nestedResult <- " <> applyWrite <> " rootId input." <> childrenField,
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> reload include rootId"
        ]
   in T.unlines $
        [ "createNested ::",
          "  (" <> loadClass root <> ") =>",
          "  " <> includeApplied root <> " ->",
          "  " <> writeCreateTypeName root <> " ->",
          "  Db (Either ORMError (" <> withApplied root <> "))",
          "createNested include input = transactionEither $ do"
        ]
          ++ createBody

emitWriteUpdate :: Schema -> Model -> RelationSpec -> Text
emitWriteUpdate _schema root rel =
  let childrenField = childrenFieldName root rel
      applyWrite = applyWriteFnName root rel
      table = tableTypeName root
      row = rowTypeName root
      updateBody =
        [ "  updateResult <-",
          "    Update.update @" <> table <> " @" <> row <> " rootId input.root",
          "  case updateResult of",
          "    Left err -> pure (Left err)",
          "    Right _ -> do",
          "      nestedResult <- case input." <> childrenField <> " of",
          "        Nothing -> pure (Right ())",
          "        Just write -> " <> applyWrite <> " rootId write",
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> reload include rootId"
        ]
   in T.unlines $
        [ "updateNested ::",
          "  (" <> loadClass root <> ") =>",
          "  " <> includeApplied root <> " ->",
          "  UUID ->",
          "  " <> writeUpdateTypeName root <> " ->",
          "  Db (Either ORMError (" <> withApplied root <> "))",
          "updateNested include rootId input = transactionEither $ do"
        ]
          ++ updateBody

emitReload :: Model -> Text
emitReload root =
  T.unlines
    [ "reload ::",
      "  (" <> loadClass root <> ") =>",
      "  " <> includeApplied root <> " ->",
      "  UUID ->",
      "  Db (Either ORMError (" <> withApplied root <> "))",
      "reload include rootId = do",
      "  found <- Ops.findUnique @" <> tableTypeName root <> " @" <> rowTypeName root <> " rootId",
      "  case found of",
      "    Nothing -> pure (Left (RecordNotFound \"Record not found with primary key\"))",
      "    Just row -> do",
      "      loaded <- " <> loadMethod root <> " include [row]",
      "      pure $ case loaded of",
      "        (one : _) -> Right one",
      "        [] -> Left (RecordNotFound \"Record not found with primary key\")"
    ]

writeCreateTypeName :: Model -> Text
writeCreateTypeName model = modelName model <> "WriteCreate"

writeUpdateTypeName :: Model -> Text
writeUpdateTypeName model = modelName model <> "WriteUpdate"

nestedCreateTypeName :: Model -> Text
nestedCreateTypeName model = modelName model <> "NestedCreate"

nestedWriteTypeName :: Model -> RelationSpec -> Text
nestedWriteTypeName root rel = upperFirst (childrenFieldName root rel) <> "Write"

nestedOpsTypeName :: Model -> Text
nestedOpsTypeName child = modelName child <> "NestedOps"

emptyNestedOpsName :: Model -> Text
emptyNestedOpsName child = "empty" <> nestedOpsTypeName child

applyWriteFnName :: Model -> RelationSpec -> Text
applyWriteFnName root rel = "apply" <> nestedWriteTypeName root rel

applyOpsFnName :: Model -> Text
applyOpsFnName child = "apply" <> nestedOpsTypeName child

childrenFieldName :: Model -> RelationSpec -> Text
childrenFieldName = includeFieldName

-- Nested writes are inferred from hasMany + child FK (the field named on
-- the relation). `replaceChildren` was dropped; there is no opt-in for
-- non-FK patterns.
nestedWriteRelation :: Schema -> Model -> Maybe (RelationSpec, Model)
nestedWriteRelation schema root =
  case [ (rel, lookupModel schema (relToModel rel))
         | rel <- modelRelations root,
           relKind rel == RelHasMany,
           childHasForeignKey schema rel
       ] of
    (found : _) -> Just found
    [] -> Nothing

childHasForeignKey :: Schema -> RelationSpec -> Bool
childHasForeignKey schema rel =
  any
    ((== relForeignField rel) . fieldName)
    (modelFields (lookupModel schema (relToModel rel)))

createRootInputExpr :: Model -> Text
createRootInputExpr model =
  let cn = createTypeName model
      fieldLines =
        map createRootFieldLine (modelFields model)
   in cn
        <> " { "
        <> T.intercalate ", " fieldLines
        <> " }"

createRootFieldLine :: FieldSpec -> Text
createRootFieldLine f
  | fieldIsPrimaryKey f = fieldName f <> " = Just rootId"
  | otherwise = fieldName f <> " = input.root." <> fieldName f

nestedEnumImports :: Text -> Schema -> Model -> [Text]
nestedEnumImports clientModule schema child =
  nubImports
    [ "import " <> clientSchemaPrefix clientModule <> enumName e <> " (" <> enumName e <> " (..))"
      | f <- modelFields child,
        TyEnum wanted <- [fieldType f],
        e <- schemaEnums schema,
        enumName e == wanted,
        isNothing (enumImport e)
    ]
  where
    nubImports = foldr go []
      where
        go imp acc | imp `elem` acc = acc
        go imp acc = imp : acc

emitSimpleExports :: Model -> Text
emitSimpleExports model =
  T.unlines
    [ "  ( create,",
      "    createMany,",
      "    update,",
      "    updateMany,",
      "    upsert,",
      "    findMany,",
      "    findUnique,",
      "    findUniqueOrFail,",
      "    findFirst,",
      "    findFirstOrFail,",
      "    count,",
      "    delete,",
      "    deleteMany,",
      "    " <> createTypeName model <> " (..),",
      "    " <> rowTypeName model <> " (..),",
      "    " <> selectTypeName model <> " (..),",
      "    " <> pickedTypeName model <> " (..),",
      "    " <> selectDefaultName model <> ",",
      "    OmitSelect (..),",
      "    Picked (..),",
      "    ResolveSelect,",
      "    " <> updateTypeName model <> " (..),",
      "    " <> tableTypeName model <> ",",
      "    " <> queryTypeName model <> " (..),",
      "    emptyQuery,",
      "    " <> fieldBinder model (primaryKeyField model),
      "  )"
    ]

exportLinesFromItems :: [Text] -> [Text]
exportLinesFromItems items =
  case items of
    [] -> ["  ()"]
    (first : rest) ->
      ("  ( " <> first) : map ("    " <>) rest ++ ["  )"]

includeClientExportItems :: Schema -> Model -> [Text]
includeClientExportItems schema root =
  includeReadFunctionExportItems
    ++ [ "create,",
         "createMany,",
         "update,",
         "updateMany,",
         "upsert,",
         "delete,",
         "deleteMany,"
       ]
    ++ nestedWriteExportItems schema root
    ++ [ queryTypeName root <> " (..),",
         "emptyQuery,",
         "OmitSelect (..),",
         "Picked (..),",
         createTypeName root <> " (..),",
         rowTypeName root <> " (..),",
         selectTypeName root <> " (..),",
         pickedTypeName root <> " (..),",
         selectDefaultName root <> ",",
         updateTypeName root <> " (..),",
         tableTypeName root <> ",",
         fieldBinder root (primaryKeyField root)
       ]

nestedWriteExportItems :: Schema -> Model -> [Text]
nestedWriteExportItems schema root =
  case nestedWriteRelation schema root of
    Nothing -> []
    Just (rel, child) ->
      [ "createNested,",
        "updateNested,",
        writeCreateTypeName root <> " (..),",
        writeUpdateTypeName root <> " (..),",
        nestedCreateTypeName child <> " (..),",
        nestedWriteTypeName root rel <> " (..),",
        nestedOpsTypeName child <> " (..),",
        emptyNestedOpsName child <> ","
      ]

includeReadFunctionExportItems :: [Text]
includeReadFunctionExportItems =
  [ "findMany,",
    "findUnique,",
    "findUniqueOrFail,",
    "findFirst,",
    "findFirstOrFail,",
    "count,"
  ]

includeClientImportLines :: Text -> Schema -> Model -> [Text]
includeClientImportLines moduleName schema root =
  nestedPreludeImports nested
    ++ [ dbImport nested,
         "import qualified Poppy.Delete as Delete",
         "import Poppy.Errors (ORMError (..), requireFound)",
         "import qualified Poppy.Insert as Insert",
         "import qualified Poppy.Operations as Ops",
         "import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)",
         "import Poppy.Select (OmitSelect (..), Picked (..))",
         "import Poppy.SelectIn (prepareIncludeRootQuery)",
         "import qualified Poppy.Update as Update",
         whereImport nested,
         "import "
           <> schemaModule moduleName root
           <> " ("
           <> createTypeName root
           <> " (..), "
           <> rowTypeName root
           <> " (..), "
           <> selectTypeName root
           <> " (..), "
           <> pickedTypeName root
           <> " (..), "
           <> selectDefaultName root
           <> ", "
           <> selectColumnsFnName root
           <> ", "
           <> parsePickedName root
           <> ", "
           <> tableTypeName root
           <> ", "
           <> updateTypeName root
           <> " (..), "
           <> fieldBinder root (primaryKeyField root)
           <> ")",
         includeModuleImportLine moduleName root
       ]
    ++ nestedChildImport moduleName nested
    ++ nestedChildEnumImports moduleName schema nested
  where
    nested = nestedWriteRelation schema root

includeModuleImportLine :: Text -> Model -> Text
includeModuleImportLine moduleName model =
  "import "
    <> includeModule moduleName model
    <> " ("
    <> T.intercalate ", " names
    <> ")"
  where
    names =
      [ "Load" <> modelName model <> " (..)",
        includeType model <> " (..)",
        readType model,
        withType model <> " (..)",
        toWithPickedName model
      ]

nestedPreludeImports :: Maybe (RelationSpec, Model) -> [Text]
nestedPreludeImports Nothing =
  ["import Data.UUID (UUID)"]
nestedPreludeImports (Just (rel, child)) =
  [ "import Data.Text (Text)",
    "import Data.UUID (UUID)",
    "import qualified Data.UUID.V4 as V4",
    "import Poppy.Core (" <> T.intercalate ", " coreNames <> ")",
    "import Poppy.PG (toField)"
  ]
  where
    coreNames =
      "fieldColumn"
        : [ "NullableValue (Omit, Value)"
            | fieldNullable (lookupField child (relForeignField rel))
          ]

dbImport :: Maybe (RelationSpec, Model) -> Text
dbImport Nothing = "import Poppy.Db (Db)"
dbImport (Just _) = "import Poppy.Db (Db, liftIO, transactionEither)"

whereImport :: Maybe (RelationSpec, Model) -> Text
whereImport Nothing = "import Poppy.Where (Where)"
whereImport (Just _) = "import Poppy.Where (Where, and_, eq, in_)"

nestedChildImport :: Text -> Maybe (RelationSpec, Model) -> [Text]
nestedChildImport _ Nothing = []
nestedChildImport moduleName (Just (rel, child)) =
  [ "import "
      <> schemaModule moduleName child
      <> " ("
      <> createTypeName child
      <> " (..), "
      <> rowTypeName child
      <> " (..), "
      <> updateTypeName child
      <> " (..), "
      <> tableTypeName child
      <> ", "
      <> fieldBinder child (primaryKeyField child)
      <> ", "
      <> fieldBinder child (lookupField child (relForeignField rel))
      <> ")"
  ]

nestedChildEnumImports :: Text -> Schema -> Maybe (RelationSpec, Model) -> [Text]
nestedChildEnumImports _ _ Nothing = []
nestedChildEnumImports moduleName schema (Just (_, child)) =
  nestedEnumImports moduleName schema child

intercalateSections :: [Text] -> Text
intercalateSections sections =
  T.intercalate "\n\n" (map T.strip sections)

emitCreateFn :: Model -> Text
emitCreateFn model =
  T.unlines
    [ "create :: " <> createTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "create = Insert.insert @" <> tableTypeName model <> " @" <> rowTypeName model
    ]

emitClientCreateManyFn :: Model -> Text
emitClientCreateManyFn model =
  T.unlines
    [ "createMany :: [" <> createTypeName model <> "] -> Db (Either ORMError Int)",
      "createMany = Insert.insertMany @" <> tableTypeName model
    ]

emitUpdateFn :: Model -> Text
emitUpdateFn model =
  T.unlines
    [ "update :: UUID -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "update = Update.update @" <> tableTypeName model <> " @" <> rowTypeName model
    ]

emitUpdateManyFn :: Model -> Text
emitUpdateManyFn model =
  T.unlines
    [ "updateMany :: Where " <> tableTypeName model <> " -> " <> updateTypeName model <> " -> Db (Either ORMError Int)",
      "updateMany = Update.updateMany @" <> tableTypeName model
    ]

emitClientUpsertFn :: Schema -> Model -> Text
emitClientUpsertFn schema model =
  T.unlines
    [ "upsert :: " <> createTypeName model <> " -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "upsert = Insert.upsert @" <> tableTypeName model <> " @" <> rowTypeName model <> " " <> listLit (upsertConflictCols schema model)
    ]

upsertConflictCols :: Schema -> Model -> [Text]
upsertConflictCols schema model =
  case uniqueCols schema model of
    [] -> [fieldColumn (primaryKeyField model)]
    cols -> cols

emitFindManyFn :: Model -> Text
emitFindManyFn model =
  T.unlines $
    [ "class Read" <> modelName model <> " select where",
      "  findMany :: " <> queryTypeName model <> " select -> Db [ResolveSelect select]",
      "  findUnique :: " <> queryTypeName model <> " select -> Db (Either ORMError (Maybe (ResolveSelect select)))",
      "  findUniqueOrFail :: " <> queryTypeName model <> " select -> Db (Either ORMError (ResolveSelect select))",
      "  findFirst :: " <> queryTypeName model <> " select -> Db (Maybe (ResolveSelect select))",
      "  findFirstOrFail :: " <> queryTypeName model <> " select -> Db (Either ORMError (ResolveSelect select))",
      "",
      "instance Read" <> modelName model <> " OmitSelect where",
      "  findMany q =",
      "    Ops.findMany @" <> tableTypeName model <> " @" <> rowTypeName model <> " (applyQuery q)",
      "  findUnique " <> queryTypeName model <> " {where_} =",
      "    Ops.findUniqueWhere @" <> tableTypeName model <> " @" <> rowTypeName model <> " where_"
    ]
      ++ emitFindUniqueOrFailLines
      ++ [ "  findFirst q =",
           "    Ops.findFirst @" <> tableTypeName model <> " @" <> rowTypeName model <> " (applyQuery q)"
         ]
      ++ emitFindFirstOrFailLines
      ++ [ "",
           "instance Read" <> modelName model <> " " <> selectTypeName model <> " where",
           "  findMany q@" <> queryTypeName model <> " {select_} =",
           "    Ops.findManyWith",
           "      (" <> parsePickedName model <> " select_)",
           "      (selectColumns (" <> selectColumnsFnName model <> " select_) . applyQuery q)"
         ]
      ++ emitFindUniqueByRowsLines
        (tableTypeName model)
        (queryTypeName model <> " {select_, where_}")
        [ "rows <-",
          "  Ops.findManyWith",
          "    (" <> parsePickedName model <> " select_)",
          "    (selectColumns (" <> selectColumnsFnName model <> " select_) . matching w)"
        ]
      ++ emitFindUniqueOrFailLines
      ++ [ "  findFirst q@" <> queryTypeName model <> " {select_} =",
           "    Ops.findFirstWith",
           "      (" <> parsePickedName model <> " select_)",
           "      (selectColumns (" <> selectColumnsFnName model <> " select_) . applyQuery q)"
         ]
      ++ emitFindFirstOrFailLines
      ++ [ "",
           "applyQuery :: " <> queryTypeName model <> " select -> QueryBuilder " <> tableTypeName model <> " -> QueryBuilder " <> tableTypeName model,
           "applyQuery " <> queryTypeName model <> " {where_, orderBy_, limit_, offset_} =",
           "  applyQueryModifiers where_ orderBy_ limit_ offset_"
         ]

emitFindUniqueOrFailLines :: [Text]
emitFindUniqueOrFailLines =
  [ "  findUniqueOrFail q = do",
    "    result <- findUnique q",
    "    case result of",
    "      Left err -> pure (Left err)",
    "      Right found -> pure $ requireFound found (RecordNotFound \"No record found matching query\")"
  ]

emitFindFirstOrFailLines :: [Text]
emitFindFirstOrFailLines =
  [ "  findFirstOrFail q = do",
    "    result <- findFirst q",
    "    pure $ requireFound result (RecordNotFound \"No record found matching query\")"
  ]

emitCountFn :: Model -> Text
emitCountFn model =
  T.unlines
    [ "count :: " <> queryTypeName model <> " select -> Db Int",
      "count q = Ops.count @" <> tableTypeName model <> " (applyQuery q)"
    ]

emitIncludeCountFn :: Model -> Text
emitIncludeCountFn model =
  T.unlines
    [ "count :: " <> queryTypeName model <> " include select -> Db Int",
      "count " <> queryTypeName model <> " {where_, orderBy_, limit_, offset_} =",
      "  Ops.count @" <> tableTypeName model <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
    ]

emitFindUniqueByRowsLines :: Text -> Text -> [Text] -> [Text]
emitFindUniqueByRowsLines table binding rowLines =
  [ "  findUnique " <> binding <> " =",
    "    case Ops.requireUniqueWhere @" <> table <> " where_ of",
    "      Left err -> pure (Left err)",
    "      Right w -> do"
  ]
    ++ map ("        " <>) rowLines
    ++ [ "        pure $ case rows of",
         "          [] -> Right Nothing",
         "          [row] -> Right (Just row)",
         "          _ -> Left (MultipleRecordsFound \"findUnique matched multiple rows\")"
       ]

emitResolveSelect :: Model -> Text
emitResolveSelect model =
  T.unlines
    [ "type family ResolveSelect select",
      "type instance ResolveSelect OmitSelect = " <> rowTypeName model,
      "type instance ResolveSelect " <> selectTypeName model <> " = " <> pickedTypeName model
    ]

emitEmptyQueryFn :: Model -> Text
emitEmptyQueryFn model =
  T.unlines
    [ "emptyQuery :: " <> queryTypeName model <> " OmitSelect",
      "emptyQuery =",
      "  " <> queryTypeName model <> " {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}"
    ]

emitQueryType :: Model -> Text
emitQueryType model =
  T.unlines
    [ "data " <> queryTypeName model <> " select = " <> queryTypeName model,
      "  { select_ :: select",
      "  , where_ :: Maybe (Where " <> tableTypeName model <> ")",
      "  , orderBy_ :: [OrderBy " <> tableTypeName model <> "]",
      "  , limit_ :: Maybe Int",
      "  , offset_ :: Maybe Int",
      "  }"
    ]

emitDeleteFn :: Model -> Text
emitDeleteFn model =
  T.unlines
    [ "delete :: UUID -> Db (Either ORMError Int)",
      "delete = Ops.delete @" <> tableTypeName model
    ]

emitDeleteManyFn :: Model -> Text
emitDeleteManyFn model =
  T.unlines
    [ "deleteMany :: Where " <> tableTypeName model <> " -> Db (Either ORMError Int)",
      "deleteMany = Delete.deleteMany @" <> tableTypeName model
    ]

emitIncludeReadDefinitions :: Schema -> Model -> [Text]
emitIncludeReadDefinitions schema model =
  [ emitIncludeQueryType model,
    emitIncludeEmptyQueryFn model,
    emitIncludeFindManyClass model,
    emitIncludeFindManyInstances model,
    emitIncludeCountFn model
  ]
    ++ case nestedWriteRelation schema model of
      Nothing -> []
      Just _ -> [emitReload model]

emitIncludeQueryType :: Model -> Text
emitIncludeQueryType model =
  T.unlines
    [ "data " <> queryTypeName model <> " include select = " <> queryTypeName model,
      "  { include_ :: include",
      "  , select_ :: select",
      "  , where_ :: Maybe (Where " <> tableTypeName model <> ")",
      "  , orderBy_ :: [OrderBy " <> tableTypeName model <> "]",
      "  , limit_ :: Maybe Int",
      "  , offset_ :: Maybe Int",
      "  }"
    ]

emitIncludeEmptyQueryFn :: Model -> Text
emitIncludeEmptyQueryFn model =
  T.unlines
    [ "emptyQuery :: " <> queryTypeName model <> " () OmitSelect",
      "emptyQuery =",
      "  " <> queryTypeName model <> " {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}"
    ]

emitIncludeFindManyClass :: Model -> Text
emitIncludeFindManyClass model =
  let query = queryTypeName model
      result = readType model <> " include select"
   in T.unlines
        [ "class Read" <> modelName model <> " include select where",
          "  findMany :: " <> query <> " include select -> Db [" <> result <> "]",
          "  findUnique :: " <> query <> " include select -> Db (Either ORMError (Maybe (" <> result <> ")))",
          "  findUniqueOrFail :: " <> query <> " include select -> Db (Either ORMError (" <> result <> "))",
          "  findFirst :: " <> query <> " include select -> Db (Maybe (" <> result <> "))",
          "  findFirstOrFail :: " <> query <> " include select -> Db (Either ORMError (" <> result <> "))"
        ]

emitIncludeFindManyInstances :: Model -> Text
emitIncludeFindManyInstances model =
  let table = tableTypeName model
      row = rowTypeName model
      query = queryTypeName model
      selectTy = selectTypeName model
      parseFn = parsePickedName model
      colsFn = selectColumnsFnName model
      toPicked = toWithPickedName model
      modelNm = modelName model
      includeTy = includeApplied model
      load = loadMethod model
      rootFetch modifier =
        "Ops.findMany @" <> table <> " @" <> row <> " (prepareIncludeRootQuery @" <> table <> " (" <> modifier <> "))"
      uniqueFromInclude =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, where_}")
          [ "roots <- " <> rootFetch "matching w",
            "rows <- " <> load <> " include_ roots"
          ]
          ++ emitFindUniqueOrFailLines
          ++ emitLoadedFindFirstFor model load False
          ++ emitFindFirstOrFailLines
      uniqueFromWhere =
        [ "  findUnique " <> query <> " {where_} =",
          "    Ops.findUniqueWhere @" <> table <> " @" <> row <> " where_"
        ]
          ++ emitFindUniqueOrFailLines
          ++ [ "  findFirst " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findFirst @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ emitFindFirstOrFailLines
      uniqueFromSelect =
        emitFindUniqueByRowsLines
          table
          (query <> " {select_, where_}")
          [ "rows <-",
            "  Ops.findManyWith",
            "    (" <> parseFn <> " select_)",
            "    (selectColumns (" <> colsFn <> " select_) . matching w)"
          ]
          ++ emitFindUniqueOrFailLines
          ++ [ "  findFirst " <> query <> " {select_, where_, orderBy_, limit_, offset_} =",
               "    Ops.findFirstWith",
               "      (" <> parseFn <> " select_)",
               "      (selectColumns (" <> colsFn <> " select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ emitFindFirstOrFailLines
      uniqueFromIncludeSelect =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, select_, where_}")
          [ "roots <- " <> rootFetch "matching w",
            "loaded <- " <> load <> " include_ roots",
            "let rows = map (" <> toPicked <> " select_) loaded"
          ]
          ++ emitFindUniqueOrFailLines
          ++ emitLoadedFindFirstFor model load True
          ++ emitFindFirstOrFailLines
   in T.unlines $
        [ "instance (" <> loadClass model <> ") => Read" <> modelNm <> " (" <> includeTy <> ") OmitSelect where",
          "  findMany " <> query <> " {include_, where_, orderBy_, limit_, offset_} = do",
          "    roots <- " <> rootFetch "applyQueryModifiers where_ orderBy_ limit_ offset_",
          "    " <> load <> " include_ roots"
        ]
          ++ uniqueFromInclude
          ++ [ "",
               "instance Read" <> modelNm <> " () OmitSelect where",
               "  findMany " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findMany @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ uniqueFromWhere
          ++ [ "",
               "instance (" <> loadClass model <> ") => Read" <> modelNm <> " (" <> includeTy <> ") " <> selectTy <> " where",
               "  findMany " <> query <> " {include_, select_, where_, orderBy_, limit_, offset_} = do",
               "    roots <- " <> rootFetch "applyQueryModifiers where_ orderBy_ limit_ offset_",
               "    loaded <- " <> load <> " include_ roots",
               "    pure $ map (" <> toPicked <> " select_) loaded"
             ]
          ++ uniqueFromIncludeSelect
          ++ [ "",
               "instance Read" <> modelNm <> " () " <> selectTy <> " where",
               "  findMany " <> query <> " {select_, where_, orderBy_, limit_, offset_} =",
               "    Ops.findManyWith",
               "      (" <> parseFn <> " select_)",
               "      (selectColumns (" <> colsFn <> " select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ uniqueFromSelect

emitLoadedFindFirstFor :: Model -> Text -> Bool -> [Text]
emitLoadedFindFirstFor model load picked =
  [ "  findFirst " <> query <> " {" <> T.intercalate ", " fields <> "} = do",
    "    roots <- Ops.findMany @" <> table <> " @" <> row <> " (prepareIncludeRootQuery @" <> table <> " (applyQueryModifiers where_ orderBy_ (Just 1) offset_))",
    "    loaded <- " <> load <> " include_ roots",
    "    pure $ case loaded of",
    "      [] -> Nothing",
    "      (row : _) -> Just " <> value
  ]
  where
    query = queryTypeName model
    table = tableTypeName model
    row = rowTypeName model
    fields = (["select_" | picked]) ++ ["include_", "where_", "orderBy_", "offset_"]
    value =
      if picked
        then "(" <> toWithPickedName model <> " select_ row)"
        else "row"

queryTypeName :: Model -> Text
queryTypeName model = modelName model <> "Query"

schemaModule :: Text -> Model -> Text
schemaModule clientModule model =
  clientSchemaPrefix clientModule <> modelName model

includeModule :: Text -> Model -> Text
includeModule clientModule model =
  clientSchemaPrefix clientModule <> "Include." <> modelName model

paramsOf :: Model -> Text
paramsOf model = T.unwords (map relName (modelRelations model))

includeType :: Model -> Text
includeType model = modelName model <> "Include"

includeApplied :: Model -> Text
includeApplied model = includeType model <> " " <> paramsOf model

withType :: Model -> Text
withType model = modelName model <> "With"

withApplied :: Model -> Text
withApplied model = withType model <> " " <> paramsOf model

withPickedType :: Model -> Text
withPickedType model = modelName model <> "WithPicked"

readType :: Model -> Text
readType model = modelName model <> "Read"

loadClass :: Model -> Text
loadClass model = "Load" <> modelName model <> " " <> paramsOf model

loadMethod :: Model -> Text
loadMethod model = "load" <> modelName model

toWithPickedName :: Model -> Text
toWithPickedName model = "to" <> withPickedType model

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
