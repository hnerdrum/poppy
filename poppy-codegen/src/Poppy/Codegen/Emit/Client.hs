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
    includeRecordValue,
    parsePickedName,
    pickedTypeName,
    primaryKeyField,
    resultTypeName,
    rowTypeName,
    selectColumnsFnName,
    selectDefaultName,
    selectTypeName,
    tableTypeName,
    toPickedName,
    updateTypeName,
  )
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupField, lookupModel, lookupRelation, lookupUniques)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

emitClientModule :: Text -> Schema -> Model -> Text
emitClientModule moduleName schema model =
  case lookupModelInclude schema model of
    Just incl -> emitIncludeClientModule moduleName schema incl
    Nothing -> emitSimpleClientModule moduleName model

lookupModelInclude :: Schema -> Model -> Maybe ModelInclude
lookupModelInclude schema model =
  case [incl | incl <- schemaIncludes schema, includeRootModel incl == modelName model] of
    (incl : _) -> Just incl
    [] -> Nothing

emitSimpleClientModule :: Text -> Model -> Text
emitSimpleClientModule moduleName model =
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
      emitUpdateFn model,
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

emitIncludeClientModule :: Text -> Schema -> ModelInclude -> Text
emitIncludeClientModule moduleName schema incl =
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
        "",
        emitIncludeClientHeader schema root incl,
        "module " <> moduleName
      ]
        ++ exportLinesFromItems (includeClientExportItems schema incl)
        ++ ["where", ""]
        ++ includeClientImportLines moduleName schema incl
    )
    <> "\n"
    <> intercalateSections
      ( [ emitCreateFn root,
          emitUpdateFn root
        ]
          ++ nestedWriteSections
          ++ emitIncludeReadDefinitions schema incl False
          ++ [ emitDeleteFn root,
               emitDeleteManyFn root
             ]
      )
  where
    root = lookupModel schema (includeRootModel incl)
    nestedWriteSections =
      case nestedWriteRelation schema root incl of
        Nothing -> []
        Just (rel, child) ->
          [ emitWriteCreateType root child rel,
            emitWriteUpdateType root rel,
            emitNestedToChildCreate child rel,
            emitNestedToChildUpdate child rel,
            emitNestedWriteHelpers schema root child rel,
            emitWriteCreate schema root incl rel,
            emitWriteUpdate schema root incl rel
          ]

emitIncludeClientHeader :: Schema -> Model -> ModelInclude -> Text
emitIncludeClientHeader schema root incl =
  T.intercalate "\n" $
    ["{- | Generated Client. Do not edit."]
      ++ nestedDocs
      ++ ["-}"]
  where
    nestedDocs =
      case nestedWriteRelation schema root incl of
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
      ++ ["      " <> relForeignField rel <> " = parentId", "    }"]
  where
    pkField = primaryKeyField child

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
          then fieldName f <> " = Nothing"
          else fieldName f <> " = Just nested." <> fieldName f
        | f <- modelFields child,
          not (fieldIsPrimaryKey f)
      ]

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

emitWriteCreate :: Schema -> Model -> ModelInclude -> RelationSpec -> Text
emitWriteCreate _schema root _incl rel =
  let childrenField = childrenFieldName root rel
      applyWrite = applyWriteFnName root rel
      table = tableTypeName root
      createBody =
        [ "  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id",
          "  let rootInput =",
          "        " <> createRootInputExpr root,
          "  rootResult <-",
          "    Insert.insert @" <> table <> " @" <> rowTypeName root <> " rootInput",
          "  case rootResult of",
          "    Left err -> pure (Left err)",
          "    Right _ -> do",
          "      nestedResult <- " <> applyWrite <> " rootId input." <> childrenField,
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> Include.findUniqueOrFail @" <> table <> " include rootId"
        ]
   in T.unlines $
        [ "createNested ::",
          "  (Include.ExecuteInclude " <> table <> " include result) =>",
          "  include ->",
          "  " <> writeCreateTypeName root <> " ->",
          "  Db (Either ORMError result)",
          "createNested include input = transactionEither $ do"
        ]
          ++ createBody

emitWriteUpdate :: Schema -> Model -> ModelInclude -> RelationSpec -> Text
emitWriteUpdate _schema root _incl rel =
  let childrenField = childrenFieldName root rel
      applyWrite = applyWriteFnName root rel
      table = tableTypeName root
      updateBody =
        [ "  updateResult <-",
          "    Update.update @" <> table <> " @" <> rowTypeName root <> " rootId input.root",
          "  case updateResult of",
          "    Left err -> pure (Left err)",
          "    Right _ -> do",
          "      nestedResult <- case input." <> childrenField <> " of",
          "        Nothing -> pure (Right ())",
          "        Just write -> " <> applyWrite <> " rootId write",
          "      case nestedResult of",
          "        Left err -> pure (Left err)",
          "        Right () -> Include.findUniqueOrFail @" <> table <> " include rootId"
        ]
   in T.unlines $
        [ "updateNested ::",
          "  (Include.ExecuteInclude " <> table <> " include result) =>",
          "  include ->",
          "  UUID ->",
          "  " <> writeUpdateTypeName root <> " ->",
          "  Db (Either ORMError result)",
          "updateNested include rootId input = transactionEither $ do"
        ]
          ++ updateBody

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
nestedWriteRelation :: Schema -> Model -> ModelInclude -> Maybe (RelationSpec, Model)
nestedWriteRelation schema root incl =
  case [ (rel, lookupModel schema (relToModel rel))
         | edge <- includeTree incl,
           let rel = lookupRelation root (includeRelation edge),
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
      "    update,",
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

includeClientExportItems :: Schema -> ModelInclude -> [Text]
includeClientExportItems schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in includeReadFunctionExportItems
        ++ [ "create,",
             "update,",
             "delete,",
             "deleteMany,"
           ]
        ++ nestedWriteExportItems schema root incl
        ++ includeReadPresetExportItems schema incl
        ++ includeRecordExportItems schema incl
        ++ [ "NoInclude (..),",
             "ResolveInclude,",
             queryTypeName root <> " (..),",
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
             fieldBinder root (primaryKeyField root) <> ",",
             pickedIncludeTypeName root incl <> " (..)"
           ]

nestedWriteExportItems :: Schema -> Model -> ModelInclude -> [Text]
nestedWriteExportItems schema root incl =
  case nestedWriteRelation schema root incl of
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

includeReadPresetExportItems :: Schema -> ModelInclude -> [Text]
includeReadPresetExportItems schema incl =
  [ fullPresetName schema incl <> ",",
    "noInclude,"
  ]

includeRecordExportItems :: Schema -> ModelInclude -> [Text]
includeRecordExportItems schema incl =
  (includeName incl <> " (..),") : [name <> " (..)," | name <- nestedIncludeTypeNames schema incl]

includeClientImportLines :: Text -> Schema -> ModelInclude -> [Text]
includeClientImportLines moduleName schema incl =
  nestedPreludeImports nested
    ++ [ dbImport nested,
         "import qualified Poppy.Delete as Delete",
         "import Poppy.Errors (ORMError (..), requireFound)",
         "import qualified Poppy.Include as Include",
         "import qualified Poppy.Insert as Insert",
         "import qualified Poppy.Operations as Ops",
         "import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)",
         "import Poppy.Select (OmitSelect (..), Picked (..))",
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
           <> toPickedName root
           <> ", "
           <> tableTypeName root
           <> ", "
           <> updateTypeName root
           <> " (..), "
           <> fieldBinder root (primaryKeyField root)
           <> ")",
         includeModuleImportLine moduleName schema incl
       ]
    ++ nestedChildImport moduleName nested
    ++ includeLeafRowImportLines moduleName schema root incl excludeNames
    ++ nestedChildEnumImports moduleName schema nested
  where
    root = lookupModel schema (includeRootModel incl)
    nested = nestedWriteRelation schema root incl
    excludeNames = maybe [] (\(_, child) -> [modelName child]) nested

nestedPreludeImports :: Maybe (RelationSpec, Model) -> [Text]
nestedPreludeImports Nothing =
  ["import Data.UUID (UUID)"]
nestedPreludeImports (Just _) =
  [ "import Data.Text (Text)",
    "import Data.UUID (UUID)",
    "import qualified Data.UUID.V4 as V4",
    "import Poppy.Core (fieldColumn)",
    "import Poppy.PG (toField)"
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

includeLeafRowImportLines :: Text -> Schema -> Model -> ModelInclude -> [Text] -> [Text]
includeLeafRowImportLines moduleName schema root incl excludeNames =
  [ "import "
      <> schemaModule moduleName model
      <> " ("
      <> rowTypeName model
      <> " (..))"
    | model <- leafRowModels schema root (includeTree incl),
      modelName model `notElem` excludeNames
  ]

leafRowModels :: Schema -> Model -> [IncludeTree] -> [Model]
leafRowModels schema parent edges =
  [ lookupModel schema (relToModel (lookupRelation parent (includeRelation edge)))
    | edge <- edges,
      null (includeChildren edge)
  ]

includeModuleImportLine :: Text -> Schema -> ModelInclude -> Text
includeModuleImportLine moduleName schema incl =
  let root = lookupModel schema (includeRootModel incl)
      includeTypes =
        [includeName incl <> " (..)"]
          ++ [name <> " (..)" | name <- nestedIncludeTypeNames schema incl]
          ++ [resultTypeName schema root (includeTree incl) <> " (..)"]
          ++ nestedWithTypeImportNames schema root (includeTree incl)
          ++ ["NoInclude (..)", "ResolveInclude"]
   in "import "
        <> includeModule moduleName incl
        <> " ( "
        <> T.intercalate ", " includeTypes
        <> ")"

nestedWithTypeImportNames :: Schema -> Model -> [IncludeTree] -> [Text]
nestedWithTypeImportNames schema parent edges =
  [ resultTypeName schema child kids <> " (..)"
    | edge <- edges,
      let rel = lookupRelation parent (includeRelation edge),
      let child = lookupModel schema (relToModel rel),
      let kids = includeChildren edge,
      not (null kids)
  ]

nestedIncludeTypeNames :: Schema -> ModelInclude -> [Text]
nestedIncludeTypeNames schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in collectNestedIncludeNames schema root (includeTree incl)

collectNestedIncludeNames :: Schema -> Model -> [IncludeTree] -> [Text]
collectNestedIncludeNames schema current =
  concatMap go
  where
    go edge =
      let rel = lookupRelation current (includeRelation edge)
          child = lookupModel schema (relToModel rel)
          kids = includeChildren edge
       in [modelName child <> "Include" | not (null kids)]
            ++ collectNestedIncludeNames schema child kids

emitIncludeReadDefinitions :: Schema -> ModelInclude -> Bool -> [Text]
emitIncludeReadDefinitions schema incl useRecordDot =
  let root = lookupModel schema (includeRootModel incl)
   in [ emitFullIncludePreset schema incl,
        emitNoIncludePreset schema incl,
        emitPickedIncludeType schema root incl useRecordDot,
        emitIncludeQueryType root incl,
        emitIncludeEmptyQueryFn root,
        emitIncludeQueryResolveInstances schema root incl,
        emitIncludeFindManyClass root incl,
        emitIncludeFindManyInstances schema root incl,
        emitIncludeCountFn root
      ]

emitPickedIncludeType :: Schema -> Model -> ModelInclude -> Bool -> Text
emitPickedIncludeType schema model incl useRecordDot =
  let pickedName = pickedIncludeTypeName model incl
      nested = includeResultName model incl
      rootVar = lowerFirst (modelName model)
      relFields = map (pickedIncludeRelField schema model) (includeTree incl)
      copyFields = map (includeRelName schema model) (includeTree incl)
      pickedBody
        | useRecordDot =
            [ toPickedIncludeFnName model incl <> " select_ nested =",
              "  " <> pickedName,
              "    { " <> rootVar <> " = " <> toPickedName model <> " select_ nested." <> rootVar,
              if null copyFields
                then "    }"
                else "    , " <> T.intercalate "\n    , " (map (\f -> f <> " = nested." <> f) copyFields) <> "\n    }"
            ]
        | otherwise =
            [ toPickedIncludeFnName model incl <> " select_ " <> nested <> " {" <> T.intercalate ", " (rootVar : copyFields) <> "} =",
              "  " <> pickedName,
              "    { " <> rootVar <> " = " <> toPickedName model <> " select_ " <> rootVar,
              if null copyFields
                then "    }"
                else "    , " <> T.intercalate "\n    , " (map (\f -> f <> " = " <> f) copyFields) <> "\n    }"
            ]
   in T.unlines $
        [ "data " <> pickedName <> " = " <> pickedName,
          "  { " <> T.intercalate ",\n    " ((rootVar <> " :: " <> pickedTypeName model) : relFields),
          "  }",
          "  deriving (Show, Eq)",
          "",
          toPickedIncludeFnName model incl <> " :: " <> selectTypeName model <> " -> " <> nested <> " -> " <> pickedName
        ]
          ++ pickedBody

includeRelName :: Schema -> Model -> IncludeTree -> Text
includeRelName _schema parent edge =
  includeFieldName parent (lookupRelation parent (includeRelation edge))

pickedIncludeRelField :: Schema -> Model -> IncludeTree -> Text
pickedIncludeRelField schema parent edge =
  name <> " :: " <> ty
  where
    rel = lookupRelation parent (includeRelation edge)
    child = lookupModel schema (relToModel rel)
    name = includeFieldName parent rel
    kids = includeChildren edge
    ty
      | not (null kids) =
          "[" <> resultTypeName schema child kids <> "]"
      | RelHasMany <- relKind rel =
          "[" <> modelName child <> "Row]"
      | otherwise =
          "Maybe " <> modelName child <> "Row"

pickedIncludeTypeName :: Model -> ModelInclude -> Text
pickedIncludeTypeName model incl = includeResultName model incl <> "Picked"

toPickedIncludeFnName :: Model -> ModelInclude -> Text
toPickedIncludeFnName model incl = "to" <> includeResultName model incl <> "Picked"

emitIncludeQueryResolveInstances :: Schema -> Model -> ModelInclude -> Text
emitIncludeQueryResolveInstances _schema model incl =
  let includeTy = includeName incl
      nested = includeResultName model incl
      nestedPicked = pickedIncludeTypeName model incl
      row = rowTypeName model
      picked = pickedTypeName model
      query = queryTypeName model
   in T.unlines
        [ "type instance ResolveInclude (" <> query <> " " <> includeTy <> " OmitSelect) = " <> nested,
          "type instance ResolveInclude (" <> query <> " NoInclude OmitSelect) = " <> row,
          "type instance ResolveInclude (" <> query <> " " <> includeTy <> " " <> selectTypeName model <> ") = " <> nestedPicked,
          "type instance ResolveInclude (" <> query <> " NoInclude " <> selectTypeName model <> ") = " <> picked
        ]

intercalateSections :: [Text] -> Text
intercalateSections sections =
  T.intercalate "\n\n" (map T.strip sections)

emitCreateFn :: Model -> Text
emitCreateFn model =
  T.unlines
    [ "create :: " <> createTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "create = Insert.insert @" <> tableTypeName model <> " @" <> rowTypeName model
    ]

emitUpdateFn :: Model -> Text
emitUpdateFn model =
  T.unlines
    [ "update :: UUID -> " <> updateTypeName model <> " -> Db (Either ORMError " <> rowTypeName model <> ")",
      "update = Update.update @" <> tableTypeName model <> " @" <> rowTypeName model
    ]

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

emitFindFirstFromIncludeLines :: Text -> Text -> Text -> Text -> [Text] -> [Text]
emitFindFirstFromIncludeLines table query includeExpr toRow extraFields =
  [ "  findFirst " <> query <> " {" <> T.intercalate ", " fields <> "} = do",
    "    rows <- Include.findMany @" <> table <> " " <> includeExpr <> " (applyQueryModifiers where_ orderBy_ (Just 1) offset_)",
    "    pure $ case rows of",
    "      [] -> Nothing",
    "      (row : _) -> Just " <> toRow
  ]
  where
    fields = extraFields ++ ["include_", "where_", "orderBy_", "offset_"]

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

emitFullIncludePreset :: Schema -> ModelInclude -> Text
emitFullIncludePreset schema incl =
  T.unlines
    [ fullPresetName schema incl <> " :: " <> includeName incl,
      fullPresetName schema incl <> " =",
      "  " <> fullIncludeValue schema incl
    ]

emitNoIncludePreset :: Schema -> ModelInclude -> Text
emitNoIncludePreset schema incl =
  T.unlines
    [ "noInclude :: NoInclude",
      "noInclude = NoInclude " <> noIncludeValue schema incl
    ]

emitIncludeQueryType :: Model -> ModelInclude -> Text
emitIncludeQueryType model _incl =
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
    [ "emptyQuery :: " <> queryTypeName model <> " NoInclude OmitSelect",
      "emptyQuery =",
      "  " <> queryTypeName model <> " {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}"
    ]

emitIncludeFindManyClass :: Model -> ModelInclude -> Text
emitIncludeFindManyClass model _incl =
  let query = queryTypeName model
   in T.unlines
        [ "class Read" <> modelName model <> " include select where",
          "  findMany :: " <> query <> " include select -> Db [ResolveInclude (" <> query <> " include select)]",
          "  findUnique :: " <> query <> " include select -> Db (Either ORMError (Maybe (ResolveInclude (" <> query <> " include select))))",
          "  findUniqueOrFail :: " <> query <> " include select -> Db (Either ORMError (ResolveInclude (" <> query <> " include select)))",
          "  findFirst :: " <> query <> " include select -> Db (Maybe (ResolveInclude (" <> query <> " include select)))",
          "  findFirstOrFail :: " <> query <> " include select -> Db (Either ORMError (ResolveInclude (" <> query <> " include select)))"
        ]

emitIncludeFindManyInstances :: Schema -> Model -> ModelInclude -> Text
emitIncludeFindManyInstances _schema model incl =
  let table = tableTypeName model
      row = rowTypeName model
      includeTy = includeName incl
      query = queryTypeName model
      selectTy = selectTypeName model
      parseFn = parsePickedName model
      colsFn = selectColumnsFnName model
      toPickedIncl = toPickedIncludeFnName model incl
      modelNm = modelName model
      uniqueFromInclude =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, where_}")
          [ "rows <- Include.findMany @" <> table <> " include_ (matching w)"
          ]
          ++ emitFindUniqueOrFailLines
          ++ emitFindFirstFromIncludeLines table query "include_" "row" []
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
          [ "nested <- Include.findMany @" <> table <> " include_ (matching w)",
            "let rows = map (" <> toPickedIncl <> " select_) nested"
          ]
          ++ emitFindUniqueOrFailLines
          ++ emitFindFirstFromIncludeLines table query "include_" ("(" <> toPickedIncl <> " select_ row)") ["select_"]
          ++ emitFindFirstOrFailLines
   in T.unlines $
        [ "instance Read" <> modelNm <> " " <> includeTy <> " OmitSelect where",
          "  findMany " <> query <> " {include_, where_, orderBy_, limit_, offset_} =",
          "    Include.findMany @" <> table <> " include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)"
        ]
          ++ uniqueFromInclude
          ++ [ "",
               "instance Read" <> modelNm <> " NoInclude OmitSelect where",
               "  findMany " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findMany @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ uniqueFromWhere
          ++ [ "",
               "instance Read" <> modelNm <> " " <> includeTy <> " " <> selectTy <> " where",
               "  findMany " <> query <> " {include_, select_, where_, orderBy_, limit_, offset_} = do",
               "    rows <- Include.findMany @" <> table <> " include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)",
               "    pure $ map (" <> toPickedIncl <> " select_) rows"
             ]
          ++ uniqueFromIncludeSelect
          ++ [ "",
               "instance Read" <> modelNm <> " NoInclude " <> selectTy <> " where",
               "  findMany " <> query <> " {select_, where_, orderBy_, limit_, offset_} =",
               "    Ops.findManyWith",
               "      (" <> parseFn <> " select_)",
               "      (selectColumns (" <> colsFn <> " select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ uniqueFromSelect

queryTypeName :: Model -> Text
queryTypeName model = modelName model <> "Query"

includeResultName :: Model -> ModelInclude -> Text
includeResultName model incl =
  resultTypeName stubSchema model (includeTree incl)
  where
    stubSchema = Schema {schemaEnums = [], schemaModels = [model], schemaIncludes = [incl], schemaUniques = []}

fullIncludeValue :: Schema -> ModelInclude -> Text
fullIncludeValue schema incl =
  includeRecordValue schema root (includeTree incl)
  where
    root = lookupModel schema (includeRootModel incl)

noIncludeValue :: Schema -> ModelInclude -> Text
noIncludeValue schema incl =
  includeRecordValue schema root []
  where
    root = lookupModel schema (includeRootModel incl)

fullPresetName :: Schema -> ModelInclude -> Text
fullPresetName schema incl =
  "with"
    <> T.concat
      [ upperFirst (includeFieldName root rel)
        | let root = lookupModel schema (includeRootModel incl),
          edge <- includeTree incl,
          let rel = lookupRelation root (includeRelation edge)
      ]

schemaModule :: Text -> Model -> Text
schemaModule clientModule model =
  clientSchemaPrefix clientModule <> modelName model

includeModule :: Text -> ModelInclude -> Text
includeModule clientModule incl =
  clientSchemaPrefix clientModule <> includeName incl

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
