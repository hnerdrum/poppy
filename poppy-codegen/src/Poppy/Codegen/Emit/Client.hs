{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.Client
  ( emitSimpleClientModule,
    emitIncludeReadClientModule,
    emitWriteClientModule,
    nestedWriteRelation,
  )
where

import Data.List (nub)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.Combinator
  ( CombinatorTree (..),
    includeRecordValue,
    isFullGraphTree,
    nestTokens,
    resultADTName,
    tagTypeName,
  )
import Poppy.Codegen.Emit.Combinator (combinatorImportItems, rootCombinatorTrees)
import Poppy.Codegen.EmitCommon
  ( createTypeName,
    fieldBinder,
    hsType,
    includeFieldName,
    leafResultTypeName,
    leafTagTypeName,
    parsePickedName,
    pickedTypeName,
    primaryKeyField,
    resultTypeName,
    rowTypeName,
    selectColumnsFnName,
    selectDefaultName,
    selectTypeName,
    singlePrefixEdge,
    tableTypeName,
    toPickedName,
    updateTypeName,
  )
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupField, lookupModel, lookupRelation, lookupUniques)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

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
      "import Poppy.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)",
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
      emitDeleteFn model,
      "",
      emitDeleteManyFn model
    ]

emitIncludeReadClientModule :: Text -> Schema -> ModelInclude -> Text
emitIncludeReadClientModule moduleName schema incl =
  T.unlines
    ( [ "{-# LANGUAGE FlexibleContexts #-}",
        "{-# LANGUAGE FlexibleInstances #-}",
        "{-# LANGUAGE MultiParamTypeClasses #-}",
        "{-# LANGUAGE NamedFieldPuns #-}",
        "{-# LANGUAGE RecordWildCards #-}",
        "{-# LANGUAGE TypeApplications #-}",
        "{-# LANGUAGE TypeFamilies #-}",
        "",
        "module " <> moduleName
      ]
        ++ exportLinesFromItems (includeReadExportItems schema incl)
        ++ ["where", ""]
        ++ includeReadImportLines moduleName schema incl
    )
    <> "\n"
    <> intercalateSections (emitIncludeReadDefinitions schema incl False)

emitWriteClientModule :: Text -> Schema -> ModelInclude -> Text
emitWriteClientModule moduleName schema incl =
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
        "module " <> moduleName
      ]
        ++ exportLinesFromItems
          ( nubExportItems $
              includeReadFunctionExportItems
                ++ [ "create,",
                     "update,",
                     "delete,",
                     "deleteMany,",
                     writeCreateTypeName root <> " (..),",
                     writeUpdateTypeName root <> " (..),",
                     nestedCreateTypeName child <> " (..),",
                     nestedWriteTypeName root rel <> " (..),",
                     nestedOpsTypeName child <> " (..),",
                     emptyNestedOpsName child <> ","
                   ]
                ++ includeCombinatorExportItems schema incl
                ++ [ "ResolveInclude,",
                     queryTypeName root <> " (..),",
                     "emptyQuery,",
                     fieldBinder root (primaryKeyField root) <> ","
                   ]
                ++ writeClientTypeReexportItems schema root incl
          )
        ++ ["where", ""]
        ++ [ "import Data.UUID (UUID)",
             "import qualified Data.UUID.V4 as V4",
             "import Poppy.PG (toField)",
             "import Poppy.Core (fieldColumn)",
             "import Poppy.Db (Db, liftIO, transaction)",
             "import qualified Poppy.Delete as Delete",
             "import Poppy.Errors (ORMError (..), requireFound)",
             "import qualified Poppy.Include as Include",
             "import qualified Poppy.Insert as Insert",
             "import "
               <> schemaModule moduleName root
               <> " ( "
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
             includeModuleImportLine moduleName schema incl True,
             "import "
               <> schemaModule moduleName child
               <> " ( "
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
               <> ")",
             "import qualified Poppy.Operations as Ops",
             "import Poppy.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)",
             "import Poppy.Select (OmitSelect (..), Picked (..))",
             "import Poppy.Where (Where, and_, eq, in_)",
             "import qualified Poppy.Update as Update"
           ]
        ++ writeClientRelatedRowImportLines moduleName schema root child incl
        ++ nestedEnumImports moduleName schema child
    )
    <> "\n"
    <> intercalateSections
      ( [ emitWriteCreateType root child rel,
          emitWriteUpdateType root rel,
          emitNestedToChildCreate child rel,
          emitNestedToChildUpdate child rel,
          emitNestedWriteHelpers schema root child rel
        ]
          ++ emitIncludeReadDefinitions schema incl True
          ++ [ emitWriteCreate schema root incl rel,
               emitWriteUpdate schema root incl rel,
               emitDeleteFn root,
               emitDeleteManyFn root
             ]
      )
  where
    root = lookupModel schema (includeRootModel incl)
    (rel, child) =
      case nestedWriteRelation schema root incl of
        Just found -> found
        Nothing ->
          error $
            "emitWriteClientModule: include "
              <> T.unpack (includeName incl)
              <> " has no owned-child hasMany (child FK matching the relation)"

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
  [ fieldName f <> " :: " <> hsType (fieldType f)
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
      "    { id = Nothing,"
    ]
      ++ [ "      " <> fieldName f <> " = nested." <> fieldName f <> ","
           | f <- modelFields child,
             not (fieldIsPrimaryKey f),
             fieldName f /= relForeignField rel
         ]
      ++ ["      " <> relForeignField rel <> " = parentId", "    }"]

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
      "      _ <-",
      "        Delete.deleteWhere $",
      "          Delete.whereDelete (fieldColumn " <> fkBinder <> " <> \" = ?\") [toField parentId] (Delete.emptyDelete @" <> childTable <> ")",
      "      insertNestedCreates parentId items",
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
      "    , updateNested parentId ops.update",
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
      "updateNested :: UUID -> [(UUID, " <> nestedCreateTypeName child <> ")] -> Db (Either ORMError ())",
      "updateNested parentId = go",
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
        [ "create ::",
          "  (Include.ExecuteInclude " <> table <> " include result) =>",
          "  include ->",
          "  " <> writeCreateTypeName root <> " ->",
          "  Db (Either ORMError result)",
          "create include input = transaction $ do"
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
        [ "update ::",
          "  (Include.ExecuteInclude " <> table <> " include result) =>",
          "  include ->",
          "  UUID ->",
          "  " <> writeUpdateTypeName root <> " ->",
          "  Db (Either ORMError result)",
          "update include rootId input = transaction $ do"
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

includeReadExportItems :: Schema -> ModelInclude -> [Text]
includeReadExportItems schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in includeReadFunctionExportItems
        ++ includeReadPresetExportItems schema incl
        ++ [ presetTypeName schema incl <> " (..),",
             "NoInclude (..),",
             "ResolveInclude,",
             queryTypeName root <> " (..),",
             "emptyQuery,",
             "OmitSelect (..),",
             "Picked (..),",
             selectTypeName root <> " (..),",
             pickedTypeName root <> " (..),",
             selectDefaultName root <> ",",
             pickedIncludeTypeName root incl <> " (..)"
           ]

includeReadFunctionExportItems :: [Text]
includeReadFunctionExportItems =
  [ "findMany,",
    "findUnique,",
    "findUniqueOrFail,"
  ]

includeReadPresetExportItems :: Schema -> ModelInclude -> [Text]
includeReadPresetExportItems schema incl =
  [ fullPresetName schema incl <> ",",
    "noInclude,"
  ]

includeCombinatorExportItems :: Schema -> ModelInclude -> [Text]
includeCombinatorExportItems schema incl =
  [ item <> ","
    | item <- combinatorImportItems schema incl
  ]
    ++ [ name <> ","
         | (name, _) <- clientTokenValues schema incl
       ]

clientTokenValues :: Schema -> ModelInclude -> [(Text, Text)]
clientTokenValues schema incl =
  nub
    [ token
      | tree <- rootCombinatorTrees schema incl,
        token <- nestTokens schema tree
    ]

writeClientTypeReexportItems :: Schema -> Model -> ModelInclude -> [Text]
writeClientTypeReexportItems schema root incl =
  nubExportItems $
    [ createTypeName root <> " (..),",
      rowTypeName root <> " (..),",
      selectTypeName root <> " (..),",
      pickedTypeName root <> " (..),",
      selectDefaultName root <> ",",
      "OmitSelect (..),",
      "Picked (..),",
      pickedIncludeTypeName root incl <> " (..),",
      updateTypeName root <> " (..),",
      includeResultName root incl <> " (..),"
    ]
      ++ prefixResultExportItems schema root incl
      ++ concatMap (includeNestedTypeReexports schema root) (includeTree incl)
      ++ inlineEnumReexportItems schema

includeNestedTypeReexports :: Schema -> Model -> IncludeTree -> [Text]
includeNestedTypeReexports schema parent edge =
  let rel = lookupRelation parent (includeRelation edge)
      child = lookupModel schema (relToModel rel)
      kids = includeChildren edge
      nestedWith
        | not (null kids) =
            [resultTypeName schema child kids <> " (..),"]
        | otherwise = []
   in nestedWith
        ++ [rowTypeName child <> " (..),"]
        ++ concatMap (includeNestedTypeReexports schema child) kids

inlineEnumReexportItems :: Schema -> [Text]
inlineEnumReexportItems schema =
  [enumName enum <> " (..)," | enum <- schemaEnums schema, isNothing (enumImport enum)]

nubExportItems :: [Text] -> [Text]
nubExportItems = foldr go []
  where
    go item acc | item `elem` acc = acc
    go item acc = item : acc

writePrefixEdge :: Schema -> Model -> ModelInclude -> Maybe IncludeTree
writePrefixEdge schema root incl = do
  edge <- singlePrefixEdge incl
  _ <- singleNestCombinator schema incl
  _ <- nestedWriteRelation schema root incl
  pure edge

prefixResultExportItems :: Schema -> Model -> ModelInclude -> [Text]
prefixResultExportItems schema root incl =
  case writePrefixEdge schema root incl of
    Just edge ->
      let result = leafResultTypeName schema root edge
       in [result <> " (..),", result <> "Picked (..),"]
    Nothing ->
      []

prefixIncludeImportItems :: Schema -> Model -> ModelInclude -> [Text]
prefixIncludeImportItems schema root incl =
  case writePrefixEdge schema root incl of
    Just edge ->
      let tag = leafTagTypeName root edge
          result = leafResultTypeName schema root edge
       in [tag <> " (..)", result <> " (..)"]
    Nothing ->
      []

includeReadImportLines :: Text -> Schema -> ModelInclude -> [Text]
includeReadImportLines moduleName schema incl =
  [ "import Data.UUID (UUID)",
    "import Poppy.Db (Db)",
    "import Poppy.Errors (ORMError (..), requireFound)",
    "import qualified Poppy.Include as Include",
    "import qualified Poppy.Operations as Ops",
    "import Poppy.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)",
    "import Poppy.Select (OmitSelect (..), Picked (..))",
    "import Poppy.Where (Where)",
    "import "
      <> schemaModule moduleName root
      <> " ("
      <> tableTypeName root
      <> ", "
      <> rowTypeName root
      <> ", "
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
      <> ")",
    includeModuleImportLine moduleName schema incl False
  ]
  where
    root = lookupModel schema (includeRootModel incl)

includeModuleImportLine :: Text -> Schema -> ModelInclude -> Bool -> Text
includeModuleImportLine moduleName schema incl withPrefix =
  let root = lookupModel schema (includeRootModel incl)
      typeExports = nestedIncludeTypeExports schema incl
      withPreset = presetTypeName schema incl
      nestedWithImports = nestedWithTypeImportNames schema root (includeTree incl)
      includeTypes =
        nub $
          [includeName incl <> " (..)"]
            ++ [item | not withPrefix, item <- typeExports]
            ++ [resultTypeName schema root (includeTree incl) <> " (..)"]
            ++ nestedWithImports
            ++ [withPreset <> " (..)", "NoInclude (..)", "ResolveInclude", "unwrap" <> withPreset]
            ++ [item | withPrefix, item <- prefixIncludeImportItems schema root incl]
            ++ combinatorImportItems schema incl
   in "import "
        <> includeModule moduleName incl
        <> " ( "
        <> T.intercalate ", " includeTypes
        <> ")"

nestedWithTypeImportNames :: Schema -> Model -> [IncludeTree] -> [Text]
nestedWithTypeImportNames schema parent =
  concatMap (nestedWithTypeImportNamesForEdge schema parent)

nestedWithTypeImportNamesForEdge :: Schema -> Model -> IncludeTree -> [Text]
nestedWithTypeImportNamesForEdge schema parent edge =
  let rel = lookupRelation parent (includeRelation edge)
      child = lookupModel schema (relToModel rel)
      kids = includeChildren edge
      withType
        | not (null kids) = [resultTypeName schema child kids <> " (..)"]
        | otherwise = []
   in withType ++ nestedWithTypeImportNames schema child kids

writeClientRelatedRowImportLines :: Text -> Schema -> Model -> Model -> ModelInclude -> [Text]
writeClientRelatedRowImportLines moduleName schema root child incl =
  [ "import "
      <> schemaModule moduleName model
      <> " ("
      <> rowTypeName model
      <> " (..))"
    | model <- includeTreeModels schema root (includeTree incl),
      modelName model `notElem` [modelName root, modelName child]
  ]

includeTreeModels :: Schema -> Model -> [IncludeTree] -> [Model]
includeTreeModels schema parent =
  concatMap (includeEdgeModels schema parent)

includeEdgeModels :: Schema -> Model -> IncludeTree -> [Model]
includeEdgeModels schema parent edge =
  let rel = lookupRelation parent (includeRelation edge)
      child = lookupModel schema (relToModel rel)
   in child : includeTreeModels schema child (includeChildren edge)

nestedIncludeTypeExports :: Schema -> ModelInclude -> [Text]
nestedIncludeTypeExports schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in [ modelName nested <> "Include (..)"
        | edge <- includeTree incl,
          let rel = lookupRelation root (includeRelation edge),
          let nested = lookupModel schema (relToModel rel),
          not (null (includeChildren edge))
      ]

emitIncludeReadDefinitions :: Schema -> ModelInclude -> Bool -> [Text]
emitIncludeReadDefinitions schema incl emitCombinators =
  let root = lookupModel schema (includeRootModel incl)
   in [emitFullIncludePreset schema incl | not emitCombinators]
        ++ [emitNoIncludePreset schema incl]
        ++ [ emitPickedIncludeType schema root incl emitCombinators
           ]
        ++ [emitPrefixPickedIncludeType schema root incl | emitCombinators]
        ++ [emitClientTokenBindings schema incl | emitCombinators]
        ++ [ emitIncludeQueryType root incl,
             emitIncludeEmptyQueryFn root,
             emitIncludeQueryResolveInstances schema root incl emitCombinators,
             emitIncludeFindManyClass root incl,
             emitIncludeFindManyInstances schema root incl emitCombinators
           ]

emitClientTokenBindings :: Schema -> ModelInclude -> Text
emitClientTokenBindings schema incl =
  T.intercalate "\n" $
    map emitToken (clientTokenValues schema incl)
  where
    emitToken (name, ty) =
      T.unlines
        [ name <> " :: " <> ty,
          name <> " = " <> ty
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

emitPrefixPickedIncludeType :: Schema -> Model -> ModelInclude -> Text
emitPrefixPickedIncludeType schema model incl =
  case singlePrefixEdge incl of
    Nothing -> ""
    Just edge ->
      let result = leafResultTypeName schema model edge
          pickedName = result <> "Picked"
          toPickedFn = "to" <> pickedName
          rootVar = lowerFirst (modelName model)
          relName = includeRelName schema model edge
          child = lookupModel schema (relToModel (lookupRelation model (includeRelation edge)))
       in T.unlines
            [ "data " <> pickedName <> " = " <> pickedName,
              "  { " <> rootVar <> " :: " <> pickedTypeName model <> ",",
              "    " <> relName <> " :: [" <> modelName child <> "Row]",
              "  }",
              "  deriving (Show, Eq)",
              "",
              toPickedFn <> " :: " <> selectTypeName model <> " -> " <> result <> " -> " <> pickedName,
              toPickedFn <> " select_ nested =",
              "  " <> pickedName,
              "    { " <> rootVar <> " = " <> toPickedName model <> " select_ nested." <> rootVar,
              "    , " <> relName <> " = nested." <> relName,
              "    }"
            ]

emitIncludeQueryResolveInstances :: Schema -> Model -> ModelInclude -> Bool -> Text
emitIncludeQueryResolveInstances schema model incl emitCombinators =
  let withPreset = presetTypeName schema incl
      nested = includeResultName model incl
      nestedPicked = pickedIncludeTypeName model incl
      row = rowTypeName model
      picked = pickedTypeName model
      query = queryTypeName model
   in T.unlines $
        [ "type instance ResolveInclude (" <> query <> " " <> withPreset <> " OmitSelect) = " <> nested,
          "type instance ResolveInclude (" <> query <> " NoInclude OmitSelect) = " <> row,
          "type instance ResolveInclude (" <> query <> " " <> withPreset <> " " <> selectTypeName model <> ") = " <> nestedPicked,
          "type instance ResolveInclude (" <> query <> " NoInclude " <> selectTypeName model <> ") = " <> picked
        ]
          ++ [line | emitCombinators, line <- combinatorQueryResolveLines schema model incl query]

combinatorQueryResolveLines :: Schema -> Model -> ModelInclude -> Text -> [Text]
combinatorQueryResolveLines schema model incl query =
  concatMap (treeQueryResolve schema model query) (partialClientTrees schema incl)

partialClientTrees :: Schema -> ModelInclude -> [CombinatorTree]
partialClientTrees schema incl =
  filter (not . isFullGraphTree schema) (rootCombinatorTrees schema incl)

treeQueryResolve :: Schema -> Model -> Text -> CombinatorTree -> [Text]
treeQueryResolve schema model query tree =
  let tag = tagTypeName schema tree
      result = resultADTName schema tree
      pickedName = result <> "Picked"
      selectTy = selectTypeName model
   in [ "type instance ResolveInclude (" <> query <> " " <> tag <> " OmitSelect) = " <> result,
        "type instance ResolveInclude (" <> query <> " " <> tag <> " " <> selectTy <> ") = " <> pickedName
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
      "",
      "instance Read" <> modelName model <> " OmitSelect where",
      "  findMany q =",
      "    Ops.findMany @" <> tableTypeName model <> " @" <> rowTypeName model <> " (applyQuery q)",
      "  findUnique " <> queryTypeName model <> " {where_} =",
      "    Ops.findUniqueWhere @" <> tableTypeName model <> " @" <> rowTypeName model <> " where_"
    ]
      ++ emitFindUniqueOrFailLines
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
      "  " <> queryTypeName model <> " {select_ = OmitSelect, where_ = Nothing, orderBy_ = Nothing, limit_ = Nothing, offset_ = Nothing}"
    ]

emitQueryType :: Model -> Text
emitQueryType model =
  T.unlines
    [ "data " <> queryTypeName model <> " select = " <> queryTypeName model,
      "  { select_ :: select",
      "  , where_ :: Maybe (Where " <> tableTypeName model <> ")",
      "  , orderBy_ :: Maybe (QueryBuilder " <> tableTypeName model <> " -> QueryBuilder " <> tableTypeName model <> ")",
      "  , limit_ :: Maybe Int",
      "  , offset_ :: Maybe Int",
      "  }"
    ]

emitDeleteFn :: Model -> Text
emitDeleteFn model =
  T.unlines
    [ "delete :: UUID -> Db Int",
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
  let withPreset = presetTypeName schema incl
   in T.unlines
        [ fullPresetName schema incl <> " :: " <> withPreset,
          fullPresetName schema incl <> " =",
          "  " <> withPreset <> " " <> fullIncludeValue schema incl
        ]

emitNoIncludePreset :: Schema -> ModelInclude -> Text
emitNoIncludePreset schema incl =
  T.unlines
    [ "noInclude :: NoInclude",
      "noInclude = NoInclude " <> noIncludeValue schema incl
    ]

data AppliedCombinator = AppliedCombinator
  { acParentName :: Text,
    acChildName :: Text,
    acTokenType :: Text
  }

singleNestCombinator :: Schema -> ModelInclude -> Maybe AppliedCombinator
singleNestCombinator schema incl =
  case includeTree incl of
    [parentEdge] ->
      case includeChildren parentEdge of
        [childEdge]
          | null (includeChildren childEdge) ->
              let root = lookupModel schema (includeRootModel incl)
                  parentRel = lookupRelation root (includeRelation parentEdge)
                  childModel = lookupModel schema (relToModel parentRel)
                  childRel = lookupRelation childModel (includeRelation childEdge)
                  childName = includeFieldName childModel childRel
               in Just
                    AppliedCombinator
                      { acParentName = includeFieldName root parentRel,
                        acChildName = childName,
                        acTokenType = upperFirst childName
                      }
        _ -> Nothing
    _ -> Nothing

emitIncludeQueryType :: Model -> ModelInclude -> Text
emitIncludeQueryType model _incl =
  T.unlines
    [ "data " <> queryTypeName model <> " include select = " <> queryTypeName model,
      "  { include_ :: include",
      "  , select_ :: select",
      "  , where_ :: Maybe (Where " <> tableTypeName model <> ")",
      "  , orderBy_ :: Maybe (QueryBuilder " <> tableTypeName model <> " -> QueryBuilder " <> tableTypeName model <> ")",
      "  , limit_ :: Maybe Int",
      "  , offset_ :: Maybe Int",
      "  }"
    ]

emitIncludeEmptyQueryFn :: Model -> Text
emitIncludeEmptyQueryFn model =
  T.unlines
    [ "emptyQuery :: " <> queryTypeName model <> " NoInclude OmitSelect",
      "emptyQuery =",
      "  " <> queryTypeName model <> " {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = Nothing, limit_ = Nothing, offset_ = Nothing}"
    ]

emitIncludeFindManyClass :: Model -> ModelInclude -> Text
emitIncludeFindManyClass model _incl =
  let query = queryTypeName model
   in T.unlines
        [ "class Read" <> modelName model <> " include select where",
          "  findMany :: " <> query <> " include select -> Db [ResolveInclude (" <> query <> " include select)]",
          "  findUnique :: " <> query <> " include select -> Db (Either ORMError (Maybe (ResolveInclude (" <> query <> " include select))))",
          "  findUniqueOrFail :: " <> query <> " include select -> Db (Either ORMError (ResolveInclude (" <> query <> " include select)))"
        ]

emitIncludeFindManyInstances :: Schema -> Model -> ModelInclude -> Bool -> Text
emitIncludeFindManyInstances schema model incl emitCombinators =
  let table = tableTypeName model
      row = rowTypeName model
      withPreset = presetTypeName schema incl
      unwrapFn = "unwrap" <> withPreset
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
          [ "rows <- Include.findMany @" <> table <> " (" <> unwrapFn <> " include_) (matching w)"
          ]
          ++ emitFindUniqueOrFailLines
      uniqueFromWhere =
        [ "  findUnique " <> query <> " {where_} =",
          "    Ops.findUniqueWhere @" <> table <> " @" <> row <> " where_"
        ]
          ++ emitFindUniqueOrFailLines
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
      uniqueFromIncludeSelect =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, select_, where_}")
          [ "nested <- Include.findMany @" <> table <> " (" <> unwrapFn <> " include_) (matching w)",
            "let rows = map (" <> toPickedIncl <> " select_) nested"
          ]
          ++ emitFindUniqueOrFailLines
   in T.unlines $
        [ "instance Read" <> modelNm <> " " <> withPreset <> " OmitSelect where",
          "  findMany " <> query <> " {include_, where_, orderBy_, limit_, offset_} =",
          "    Include.findMany @" <> table <> " (" <> unwrapFn <> " include_) (applyQueryModifiers where_ orderBy_ limit_ offset_)"
        ]
          ++ uniqueFromInclude
          ++ [ "",
               "instance Read" <> modelNm <> " NoInclude OmitSelect where",
               "  findMany " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findMany @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ uniqueFromWhere
          ++ [ "",
               "instance Read" <> modelNm <> " " <> withPreset <> " " <> selectTy <> " where",
               "  findMany " <> query <> " {include_, select_, where_, orderBy_, limit_, offset_} = do",
               "    rows <- Include.findMany @" <> table <> " (" <> unwrapFn <> " include_) (applyQueryModifiers where_ orderBy_ limit_ offset_)",
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
          ++ [inst | emitCombinators, inst <- combinatorReadInstances schema model incl]

combinatorReadInstances :: Schema -> Model -> ModelInclude -> [Text]
combinatorReadInstances schema model incl =
  concatMap (treeReadInstances schema model) (partialClientTrees schema incl)

treeReadInstances :: Schema -> Model -> CombinatorTree -> [Text]
treeReadInstances schema model tree =
  let table = tableTypeName model
      query = queryTypeName model
      tag = tagTypeName schema tree
      selectTy = selectTypeName model
      result = resultADTName schema tree
      toPickedFn = "to" <> result <> "Picked"
      modelNm = modelName model
      uniqueLeaf =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, where_}")
          [ "rows <- Include.findMany @" <> table <> " include_ (matching w)"
          ]
          ++ emitFindUniqueOrFailLines
      uniqueLeafSelect =
        emitFindUniqueByRowsLines
          table
          (query <> " {include_, select_, where_}")
          [ "nested <- Include.findMany @" <> table <> " include_ (matching w)",
            "let rows = map (" <> toPickedFn <> " select_) nested"
          ]
          ++ emitFindUniqueOrFailLines
   in [ "",
        "instance Read" <> modelNm <> " " <> tag <> " OmitSelect where",
        "  findMany " <> query <> " {include_, where_, orderBy_, limit_, offset_} =",
        "    Include.findMany @" <> table <> " include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)"
      ]
        ++ uniqueLeaf
        ++ [ "",
             "instance Read" <> modelNm <> " " <> tag <> " " <> selectTy <> " where",
             "  findMany " <> query <> " {include_, select_, where_, orderBy_, limit_, offset_} = do",
             "    rows <- Include.findMany @" <> table <> " include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)",
             "    pure $ map (" <> toPickedFn <> " select_) rows"
           ]
        ++ uniqueLeafSelect

presetTypeName :: Schema -> ModelInclude -> Text
presetTypeName schema incl =
  case T.stripPrefix "with" (fullPresetName schema incl) of
    Just rest -> "With" <> rest
    Nothing -> "With" <> upperFirst (fullPresetName schema incl)

queryTypeName :: Model -> Text
queryTypeName model = modelName model <> "Query"

includeResultName :: Model -> ModelInclude -> Text
includeResultName model incl =
  resultTypeName stubSchema model (includeTree incl)
  where
    stubSchema = Schema {schemaEnums = [], schemaModels = [model], schemaIncludes = [incl], schemaUniques = []}

fullIncludeValue :: Schema -> ModelInclude -> Text
fullIncludeValue schema incl =
  includeRecordValue schema (CombinatorTree root (includeTree incl))
  where
    root = lookupModel schema (includeRootModel incl)

noIncludeValue :: Schema -> ModelInclude -> Text
noIncludeValue schema incl =
  includeRecordValue schema (CombinatorTree root [])
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
