{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.Client
  ( emitClientModule,
    emitSimpleClientModule,
  )
where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.Emit.NestedWrite
  ( emitClientCreateManyFn,
    emitClientUpsertFn,
    emitCreateFn,
    emitNestedWriteHelpers,
    emitNestedWriteTypes,
    emitUpdateFn,
    emitUpdateManyFn,
    nestedChildClientImports,
    nestedChildEnumImports,
    nestedChildSchemaImports,
    nestedWriteExportItems,
    nestedWriteRelations,
    nestedWriteUsesTransaction,
    schemaModuleAlias,
  )
import Poppy.Codegen.Emit.Schema (enumImportLine)
import Poppy.Codegen.EmitCommon
  ( clientSchemaPrefix,
    createTypeName,
    fieldBinder,
    hsType,
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
import Poppy.Codegen.Lookup (lookupField, lookupUniques)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

emitClientModule :: Text -> Schema -> Model -> Text
emitClientModule moduleName schema model =
  if null (modelRelations model)
    then emitSimpleClientModule moduleName schema model
    else emitIncludeClientModule moduleName schema model

emitSimpleClientModule :: Text -> Schema -> Model -> Text
emitSimpleClientModule moduleName schema model =
  T.unlines
    [ "{-# LANGUAGE DuplicateRecordFields #-}",
      "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE LambdaCase #-}",
      "{-# LANGUAGE NamedFieldPuns #-}",
      "{-# LANGUAGE RecordWildCards #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module " <> moduleName,
      emitSimpleExports model,
      "where",
      "",
      valueImportsBlock schema moduleName model,
      simpleGeneratedImports schema model,
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
        <> uniqueFieldImportList schema model
        <> ")",
      "",
      emitUniqueDecls schema model,
      "",
      emitCreateFn schema model,
      "",
      emitClientCreateManyFn schema model,
      "",
      emitUpdateFn schema model,
      "",
      emitUpdateManyFn schema model,
      "",
      emitClientUpsertFn schema model,
      "",
      emitQueryType model,
      "",
      emitUniqueQueryType False model,
      "",
      emitEmptyQueryFn model,
      "",
      emitUniqueQueryFn False model,
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
        "{-# LANGUAGE LambdaCase #-}",
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
      ( emitUniqueDecls schema root
          : filter
            (not . T.null)
            ( emitNestedWriteTypes schema root
                : [ emitCreateFn schema root,
                    emitClientCreateManyFn schema root,
                    emitUpdateFn schema root,
                    emitUpdateManyFn schema root,
                    emitClientUpsertFn schema root,
                    emitNestedWriteHelpers schema root
                  ]
                ++ emitIncludeReadDefinitions schema root
                ++ [ emitDeleteFn root,
                     emitDeleteManyFn root
                   ]
            )
      )

emitIncludeClientHeader :: Schema -> Model -> Text
emitIncludeClientHeader schema root =
  T.intercalate "\n" $
    ["{- | Generated Client. Do not edit."]
      ++ nestedDocs
      ++ ["-}"]
  where
    nestedDocs =
      case nestedWriteRelations schema root of
        [] -> []
        _ ->
          [ "",
            "Nested writes live on 'create' / 'update'.",
            "  create-time relation fields are [CreateChild | ConnectChild unique]",
            "  update-time relation fields are Maybe <Rel>Update (replaceWith, create, connect, delete, update, upsert;",
            "  disconnect when the child foreign key is nullable)"
          ]

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
      "    " <> uniqueTypeName model <> " (..),",
      "    " <> uniqueKeyTypeName model <> " (..),",
      "    " <> uniqueQueryName model <> " (..),",
      "    emptyQuery,",
      "    uniqueQuery,",
      "    " <> uniqueWhereName model <> ",",
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
         uniqueTypeName root <> " (..),",
         uniqueKeyTypeName root <> " (..),",
         uniqueQueryName root <> " (..),",
         "emptyQuery,",
         "uniqueQuery,",
         uniqueWhereName root <> ",",
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
  nestedPreludeImports schema root
    ++ [ includeGeneratedImports schema root,
         rootSchemaImport moduleName schema root,
         includeModuleImportLine moduleName root
       ]
    ++ nestedChildSchemaImports moduleName schema root
    ++ nestedChildClientImports moduleName schema root
    ++ nestedChildEnumImports moduleName schema root
    ++ extraValueImports moduleName schema root

rootSchemaImport :: Text -> Schema -> Model -> Text
rootSchemaImport moduleName schema root
  | nestedWriteUsesTransaction schema root =
      T.unlines
        [ "import "
            <> schemaModule moduleName root
            <> " ("
            <> T.intercalate ", " (rootSchemaNames False)
            <> ")",
          "import qualified "
            <> schemaModule moduleName root
            <> " as "
            <> schemaModuleAlias root
            <> " ("
            <> createTypeName root
            <> " (..), "
            <> updateTypeName root
            <> " (..))"
        ]
  | otherwise =
      "import "
        <> schemaModule moduleName root
        <> " ("
        <> T.intercalate ", " (rootSchemaNames True)
        <> ")"
  where
    rootSchemaNames withCreateUpdate =
      nub $
        [n | withCreateUpdate, n <- [createTypeName root <> " (..)", updateTypeName root <> " (..)"]]
          ++ [ rowTypeName root <> " (..)",
               selectTypeName root <> " (..)",
               pickedTypeName root <> " (..)",
               selectDefaultName root,
               selectColumnsFnName root,
               parsePickedName root,
               tableTypeName root,
               uniqueFieldImportList schema root
             ]
          ++ [ fieldBinder child (lookupField child (relForeignField rel))
               | (rel, child) <- nestedWriteRelations schema root,
                 modelName child == modelName root
             ]

includeModuleImportLine :: Text -> Model -> Text
includeModuleImportLine moduleName model =
  "import "
    <> includeModule moduleName model
    <> " ("
    <> T.intercalate
      ", "
      [ "Load" <> modelName model <> " (..)",
        includeType model <> " (..)",
        readType model,
        toWithPickedName model
      ]
    <> ")"

nestedPreludeImports :: Schema -> Model -> [Text]
nestedPreludeImports schema root
  | nestedWriteUsesTransaction schema root =
      [ "import Data.Maybe (isJust)",
        "import Data.Text (Text)",
        "import Data.UUID (UUID)"
      ]
  | otherwise = ["import Data.UUID (UUID)"]

simpleGeneratedImports :: Schema -> Model -> Text
simpleGeneratedImports schema model =
  T.unlines
    [ "import Poppy.Internal.Generated",
      "  ( Db,",
      "    ORMError (..),",
      "    fromUniqueRows,",
      "    requireFound,",
      "    uniqueOrFail,",
      "    OrderBy,",
      "    QueryBuilder,",
      "    applyQueryModifiers,",
      "    matching,",
      "    selectColumns,",
      "    OmitSelect (..),",
      "    Picked (..),",
      "    " <> T.intercalate ",\n    " (whereNames schema model) <> "",
      "  )",
      "import qualified Poppy.Internal.Generated as Delete",
      "  ( deleteMany",
      "  )",
      "import qualified Poppy.Internal.Generated as Insert",
      "  ( insert,",
      "    insertMany,",
      "    upsert",
      "  )",
      "import qualified Poppy.Internal.Generated as Ops",
      "  ( findMany,",
      "    findManyWith,",
      "    findFirst,",
      "    findFirstWith,",
      "    count",
      "  )",
      "import qualified Poppy.Internal.Generated as Update",
      "  ( updateWhere,",
      "    updateMany",
      "  )"
    ]

includeGeneratedImports :: Schema -> Model -> Text
includeGeneratedImports schema root =
  let dbNames =
        if nestedWriteUsesTransaction schema root
          then ["Db", "transactionEither"]
          else ["Db"]
      nestedNames =
        if nestedWriteUsesTransaction schema root
          then
            ["fieldColumn", "toField"]
              ++ [ "NullableValue (..)"
                   | any
                       ( \(rel, child) ->
                           fieldNullable (lookupField child (relForeignField rel))
                       )
                       (nestedWriteRelations schema root)
                 ]
          else []
      wherePart = whereNames schema root
      selectIn = ["prepareIncludeRootQuery"]
   in T.unlines T.unlines
        [ "import Poppy.Internal.Generated",
          "  ( " <> T.intercalate ",\n    " (dbNames ++ nestedNames ++ ["ORMError (..)", "fromUniqueRows", "requireFound", "uniqueOrFail", "OrderBy", "applyQueryModifiers", "matching", "selectColumns", "OmitSelect (..)", "Picked (..)"] ++ selectIn ++ wherePart) <> "",
          "  )",
          "import qualified Poppy.Internal.Generated as Delete",
          "  ( deleteMany,",
          "    deleteWhere,",
          "    whereDelete,",
          "    emptyDelete",
          "  )",
          "import qualified Poppy.Internal.Generated as Insert",
          "  ( insert,",
          "    insertMany,",
          "    upsert",
          "  )",
          "import qualified Poppy.Internal.Generated as Ops",
          "  ( findMany,",
          "    findManyWith,",
          "    findFirst,",
          "    findFirstWith,",
          "    count",
          "  )",
          "import qualified Poppy.Internal.Generated as Update",
          "  ( updateWhere,",
          "    updateMany",
          "  )"
        ]

whereNames :: Schema -> Model -> [Text]
whereNames schema model
  | nestedWriteUsesTransaction schema model = ["Where", "and_", "eq"]
  | otherwise =
      let needsAnd = any ((> 1) . length . emittedFields) (modelUniqueKeys schema model)
       in ["Where", "eq"] ++ ["and_" | needsAnd]

intercalateSections :: [Text] -> Text
intercalateSections sections =
  T.intercalate "\n\n" (map T.strip sections)

emitFindManyFn :: Model -> Text
emitFindManyFn model =
  let q = queryTypeName model
      uq = uniqueQueryName model
      table = tableTypeName model
      row = rowTypeName model
      whereFn = uniqueWhereName model
   in T.unlines $
        [ "class Read" <> modelName model <> " select where",
          "  findMany :: " <> q <> " select -> Db [ResolveSelect select]",
          "  findUnique :: " <> uq <> " select -> Db (Either ORMError (Maybe (ResolveSelect select)))",
          "  findUniqueOrFail :: " <> uq <> " select -> Db (Either ORMError (ResolveSelect select))",
          "  findFirst :: " <> q <> " select -> Db (Maybe (ResolveSelect select))",
          "  findFirstOrFail :: " <> q <> " select -> Db (Either ORMError (ResolveSelect select))",
          "",
          "instance Read" <> modelName model <> " OmitSelect where",
          "  findMany q =",
          "    Ops.findMany @" <> table <> " @" <> row <> " (applyQuery q)"
        ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_}")
            ["rows <- Ops.findMany @" <> table <> " @" <> row <> " (matching w)"]
          ++ emitFindUniqueOrFailLines
          ++ [ "  findFirst q =",
               "    Ops.findFirst @" <> table <> " @" <> row <> " (applyQuery q)"
             ]
          ++ emitFindFirstOrFailLines
          ++ [ "",
               "instance Read" <> modelName model <> " " <> selectTypeName model <> " where",
               "  findMany q@" <> q <> " {select_} =",
               "    Ops.findManyWith",
               "      (" <> parsePickedName model <> " select_)",
               "      (selectColumns (" <> selectColumnsFnName model <> " select_) . applyQuery q)"
             ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_, select_}")
            [ "rows <-",
              "  Ops.findManyWith",
              "    (" <> parsePickedName model <> " select_)",
              "    (selectColumns (" <> selectColumnsFnName model <> " select_) . matching w)"
            ]
          ++ emitFindUniqueOrFailLines
          ++ [ "  findFirst q@" <> q <> " {select_} =",
               "    Ops.findFirstWith",
               "      (" <> parsePickedName model <> " select_)",
               "      (selectColumns (" <> selectColumnsFnName model <> " select_) . applyQuery q)"
             ]
          ++ emitFindFirstOrFailLines
          ++ [ "",
               "applyQuery :: " <> q <> " select -> QueryBuilder " <> table <> " -> QueryBuilder " <> table,
               "applyQuery " <> q <> " {where_, orderBy_, limit_, offset_} =",
               "  applyQueryModifiers where_ orderBy_ limit_ offset_"
             ]

emitFindUniqueOrFailLines :: [Text]
emitFindUniqueOrFailLines =
  ["  findUniqueOrFail q = uniqueOrFail <$> findUnique q"]

emitFindUniqueByRowsLines :: Text -> Text -> [Text] -> [Text]
emitFindUniqueByRowsLines whereFn binding rowLines =
  [ "  findUnique " <> binding <> " = do",
    "    let w = " <> whereFn <> " where_"
  ]
    ++ map ("    " <>) rowLines
    ++ ["    pure (fromUniqueRows rows)"]

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
    [ "delete :: " <> uniqueTypeName model <> " -> Db (Either ORMError Int)",
      "delete key =",
      "  Delete.deleteMany @" <> tableTypeName model <> " (" <> uniqueWhereName model <> " key)"
    ]

emitDeleteManyFn :: Model -> Text
emitDeleteManyFn model =
  T.unlines
    [ "deleteMany :: Where " <> tableTypeName model <> " -> Db (Either ORMError Int)",
      "deleteMany = Delete.deleteMany @" <> tableTypeName model
    ]

emitIncludeReadDefinitions :: Schema -> Model -> [Text]
emitIncludeReadDefinitions _schema model =
  [ emitIncludeQueryType model,
    emitUniqueQueryType True model,
    emitIncludeEmptyQueryFn model,
    emitUniqueQueryFn True model,
    emitIncludeFindManyClass model,
    emitIncludeFindManyInstances model,
    emitIncludeCountFn model
  ]

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

emitUniqueQueryType :: Bool -> Model -> Text
emitUniqueQueryType withInclude model =
  let name = uniqueQueryName model
      params = if withInclude then " include select" else " select"
      fields =
        ["include_ :: include" | withInclude]
          ++ [ "select_ :: select",
               "where_ :: " <> uniqueTypeName model
             ]
   in T.unlines $
        ["data " <> name <> params <> " = " <> name]
          ++ zipWith fieldLine [0 :: Int ..] fields
          ++ ["  }"]
  where
    fieldLine 0 f = "  { " <> f
    fieldLine _ f = "  , " <> f

emitUniqueQueryFn :: Bool -> Model -> Text
emitUniqueQueryFn withInclude model =
  let name = uniqueQueryName model
      result =
        if withInclude
          then name <> " () OmitSelect"
          else name <> " OmitSelect"
      fields =
        ["include_ = ()" | withInclude]
          ++ ["select_ = OmitSelect", "where_ = key"]
   in T.unlines
        [ "uniqueQuery :: " <> uniqueTypeName model <> " -> " <> result,
          "uniqueQuery key =",
          "  " <> name <> " {" <> T.intercalate ", " fields <> "}"
        ]

emitIncludeFindManyClass :: Model -> Text
emitIncludeFindManyClass model =
  let query = queryTypeName model
      uniqueQuery = uniqueQueryName model
      result = readType model <> " include select"
   in T.unlines
        [ "class Read" <> modelName model <> " include select where",
          "  findMany :: " <> query <> " include select -> Db [" <> result <> "]",
          "  findUnique :: " <> uniqueQuery <> " include select -> Db (Either ORMError (Maybe (" <> result <> ")))",
          "  findUniqueOrFail :: " <> uniqueQuery <> " include select -> Db (Either ORMError (" <> result <> "))",
          "  findFirst :: " <> query <> " include select -> Db (Maybe (" <> result <> "))",
          "  findFirstOrFail :: " <> query <> " include select -> Db (Either ORMError (" <> result <> "))"
        ]

emitIncludeFindManyInstances :: Model -> Text
emitIncludeFindManyInstances model =
  let table = tableTypeName model
      row = rowTypeName model
      query = queryTypeName model
      uq = uniqueQueryName model
      selectTy = selectTypeName model
      parseFn = parsePickedName model
      colsFn = selectColumnsFnName model
      toPicked = toWithPickedName model
      modelNm = modelName model
      includeTy = includeApplied model
      load = loadMethod model
      whereFn = uniqueWhereName model
      rootFetch modifier =
        "Ops.findMany @" <> table <> " @" <> row <> " (prepareIncludeRootQuery @" <> table <> " (" <> modifier <> "))"
   in T.unlines $
        [ "instance (" <> loadClass model <> ") => Read" <> modelNm <> " (" <> includeTy <> ") OmitSelect where",
          "  findMany " <> query <> " {include_, where_, orderBy_, limit_, offset_} = do",
          "    roots <- " <> rootFetch "applyQueryModifiers where_ orderBy_ limit_ offset_",
          "    " <> load <> " include_ roots"
        ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_, include_}")
            [ "roots <- " <> rootFetch "matching w",
              "rows <- " <> load <> " include_ roots"
            ]
          ++ emitFindUniqueOrFailLines
          ++ emitLoadedFindFirstFor model load False
          ++ emitFindFirstOrFailLines
          ++ [ "",
               "instance Read" <> modelNm <> " () OmitSelect where",
               "  findMany " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findMany @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_}")
            ["rows <- Ops.findMany @" <> table <> " @" <> row <> " (matching w)"]
          ++ emitFindUniqueOrFailLines
          ++ [ "  findFirst " <> query <> " {where_, orderBy_, limit_, offset_} =",
               "    Ops.findFirst @" <> table <> " @" <> row <> " (applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ emitFindFirstOrFailLines
          ++ [ "",
               "instance (" <> loadClass model <> ") => Read" <> modelNm <> " (" <> includeTy <> ") " <> selectTy <> " where",
               "  findMany " <> query <> " {include_, select_, where_, orderBy_, limit_, offset_} = do",
               "    roots <- " <> rootFetch "applyQueryModifiers where_ orderBy_ limit_ offset_",
               "    loaded <- " <> load <> " include_ roots",
               "    pure $ map (" <> toPicked <> " select_) loaded"
             ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_, include_, select_}")
            [ "roots <- " <> rootFetch "matching w",
              "loaded <- " <> load <> " include_ roots",
              "let rows = map (" <> toPicked <> " select_) loaded"
            ]
          ++ emitFindUniqueOrFailLines
          ++ emitLoadedFindFirstFor model load True
          ++ emitFindFirstOrFailLines
          ++ [ "",
               "instance Read" <> modelNm <> " () " <> selectTy <> " where",
               "  findMany " <> query <> " {select_, where_, orderBy_, limit_, offset_} =",
               "    Ops.findManyWith",
               "      (" <> parseFn <> " select_)",
               "      (selectColumns (" <> colsFn <> " select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)"
             ]
          ++ emitFindUniqueByRowsLines
            whereFn
            (uq <> " {where_, select_}")
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

data EmittedUnique = EmittedUnique
  { emittedCtor :: Text,
    emittedKeyCtor :: Text,
    emittedFields :: [FieldSpec]
  }

modelUniqueKeys :: Schema -> Model -> [EmittedUnique]
modelUniqueKeys schema model =
  emittedFrom (upperFirst (fieldName primary)) [primary]
    : map fromConstraint (lookupUniques schema (modelName model))
  where
    primary = primaryKeyField model
    fromConstraint constraint =
      let fields = map (lookupField model) (uniqueFields constraint)
       in emittedFrom (T.concat (map (upperFirst . fieldName) fields)) fields
    emittedFrom label fields =
      EmittedUnique
        { emittedCtor = "By" <> label,
          emittedKeyCtor = "On" <> label,
          emittedFields = fields
        }

uniqueTypeName :: Model -> Text
uniqueTypeName model = modelName model <> "Unique"

uniqueQueryName :: Model -> Text
uniqueQueryName model = modelName model <> "UniqueQuery"

uniqueKeyTypeName :: Model -> Text
uniqueKeyTypeName model = modelName model <> "UniqueKey"

uniqueWhereName :: Model -> Text
uniqueWhereName model = lowerFirst (modelName model) <> "UniqueWhere"

uniqueConflictName :: Model -> Text
uniqueConflictName model = lowerFirst (modelName model) <> "ConflictCols"

uniqueFieldImportList :: Schema -> Model -> Text
uniqueFieldImportList schema model =
  T.intercalate ", " $
    nub
      [ fieldBinder model spec
        | key <- modelUniqueKeys schema model,
          spec <- emittedFields key
      ]

valueImportsBlock :: Schema -> Text -> Model -> Text
valueImportsBlock schema moduleName model =
  T.intercalate "\n" (valueImportLines schema moduleName model)

valueImportLines :: Schema -> Text -> Model -> [Text]
valueImportLines schema moduleName model =
  nub $
    "import Data.Text (Text)"
      : concatMap (valueImport schema moduleName) (concatMap emittedFields (modelUniqueKeys schema model))

extraValueImports :: Text -> Schema -> Model -> [Text]
extraValueImports moduleName schema model =
  filter (`notElem` nestedPreludeImports schema model) $
    nub $
      valueImportLines schema moduleName model
        ++ concat
          [ concatMap (valueImport schema moduleName) nonEnumFields
            | (rel, child) <- nestedWriteRelations schema model,
              let nonEnumFields =
                    [ f
                      | f <- modelFields child ++ [f' | f' <- modelFields child, fieldName f' /= relForeignField rel],
                        case fieldType f of
                          TyEnum _ -> False
                          _ -> True
                    ]
          ]

valueImport :: Schema -> Text -> FieldSpec -> [Text]
valueImport schema moduleName spec =
  case fieldType spec of
    TyText -> []
    TyUuid -> ["import Data.UUID (UUID)"]
    TyNumeric -> ["import Data.Scientific (Scientific)"]
    TyJsonb -> ["import Data.Aeson (Value)"]
    TyTimestamptz -> ["import Data.Time (UTCTime)"]
    TyInt -> []
    TyBool -> []
    TyEnum name ->
      [ enumImportLine (clientSchemaPrefix moduleName) enumSpec
        | enumSpec <- schemaEnums schema,
          enumName enumSpec == name
      ]

emitUniqueDecls :: Schema -> Model -> Text
emitUniqueDecls schema model =
  let keys = modelUniqueKeys schema model
   in T.unlines $
        emitSum (uniqueTypeName model) (map ctorLine keys)
          ++ [""]
          ++ emitSum (uniqueKeyTypeName model) (map emittedKeyCtor keys)
          ++ [""]
          ++ emitUniqueWhere model keys
          ++ [""]
          ++ emitConflictCols model keys
  where
    ctorLine key = emittedCtor key <> " " <> T.unwords (map emittedArgType (emittedFields key))

emitSum :: Text -> [Text] -> [Text]
emitSum name alts =
  ["data " <> name]
    ++ zipWith arm [0 :: Int ..] alts
    ++ ["  deriving (Eq, Show)"]
  where
    arm 0 alt = "  = " <> alt
    arm _ alt = "  | " <> alt

emitUniqueWhere :: Model -> [EmittedUnique] -> [Text]
emitUniqueWhere model keys =
  let fn = uniqueWhereName model
   in [ fn <> " :: " <> uniqueTypeName model <> " -> Where " <> tableTypeName model,
        fn <> " = \\case"
      ]
        ++ map arm keys
  where
    arm key =
      let binders = zipWith (\i _ -> "v" <> T.pack (show i)) [1 :: Int ..] (emittedFields key)
          preds =
            zipWith
              (\spec binder -> "eq " <> fieldBinder model spec <> " " <> binder)
              (emittedFields key)
              binders
       in "  "
            <> emittedCtor key
            <> " "
            <> T.unwords binders
            <> " -> "
            <> T.intercalate " `and_` " preds

emitConflictCols :: Model -> [EmittedUnique] -> [Text]
emitConflictCols model keys =
  let fn = uniqueConflictName model
   in [ fn <> " :: " <> uniqueKeyTypeName model <> " -> [Text]",
        fn <> " = \\case"
      ]
        ++ map arm keys
  where
    arm key =
      "  "
        <> emittedKeyCtor key
        <> " -> "
        <> listLit (map fieldColumn (emittedFields key))

listLit :: [Text] -> Text
listLit cols =
  "[" <> T.intercalate ", " ["\"" <> col <> "\"" | col <- cols] <> "]"

emittedArgType :: FieldSpec -> Text
emittedArgType spec =
  let base = hsType (fieldType spec)
   in if fieldNullable spec then "(Maybe " <> base <> ")" else base
