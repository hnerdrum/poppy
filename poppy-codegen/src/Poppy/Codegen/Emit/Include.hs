{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.Include
  ( emitIncludeADTs,
    emitResultADTs,
    emitIncludesJoin,
    emitExecuteInclude,
    emitNestInclude,
    emitIncludePresetTypes,
    emitIncludeModule,
  )
where

import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.Combinator (combinePairs)
import Poppy.Codegen.Emit.Combinator
  ( emitCombinatorAPI,
    emitCombinatorExecute,
    emitCombinatorExports,
    emitCombinatorLoaders,
    emitCombinatorResolveLines,
    emitCombinatorResultADTs,
    emitCombinatorTags,
  )
import Poppy.Codegen.Emit.SelectIn (emitExecuteInclude, emitLoadHelpers)
import Poppy.Codegen.EmitCommon
  ( includeFieldName,
    resultTypeName,
  )
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupModel, lookupRelation)
import Poppy.Codegen.TextUtil (lowerFirst)

data IncludeTypeDecl = IncludeTypeDecl
  { itdName :: Text,
    itdParent :: Model,
    itdEdges :: [IncludeTree]
  }

emitIncludeADTs :: Schema -> ModelInclude -> Text
emitIncludeADTs schema incl =
  T.intercalate "\n" $
    map (emitTypeDecl schema) (collectIncludeTypes schema incl)

emitResultADTs :: Schema -> ModelInclude -> Text
emitResultADTs schema incl =
  T.intercalate "\n" $
    map (emitResultTypeDecl schema) (collectResultTypes schema incl)

emitNestInclude :: Schema -> ModelInclude -> Text
emitNestInclude schema incl =
  T.unlines
    [ "instance NestInclude " <> includeName incl <> " " <> resultName <> " where",
      "  type RootRow " <> includeName incl <> " = " <> rootRow,
      "  wrapRoot _ " <> rootVar <> " = " <> resultName <> " {" <> rootVar <> ", " <> emptyFields <> "}"
    ]
  where
    root = lookupModel schema (includeRootModel incl)
    resultName = resultTypeName schema root (includeTree incl)
    rootRow = modelName root <> "Row"
    rootVar = lowerFirst (modelName root)
    emptyFields =
      T.intercalate ", " $
        map (emptyIncludeField schema root) (includeTree incl)

emptyIncludeField :: Schema -> Model -> IncludeTree -> Text
emptyIncludeField _schema parent edge =
  name <> " = " <> emptyVal
  where
    rel = lookupRelation parent (includeRelation edge)
    name = includeFieldName parent rel
    emptyVal = case relKind rel of
      RelHasMany -> "[]"
      RelBelongsTo -> "Nothing"

emitIncludesJoin :: Schema -> ModelInclude -> Text
emitIncludesJoin schema incl =
  T.unlines
    [ "instance IncludesJoin " <> includeName incl <> " where",
      "  includesJoin include = " <> expr
    ]
  where
    root = lookupModel schema (includeRootModel incl)
    edges = includeTree incl
    expr = T.intercalate " || " (map (edgeJoinExpr schema root) edges)

edgeField :: Schema -> Model -> IncludeTree -> Text
edgeField _schema parent edge =
  includeFieldName parent (lookupRelation parent (includeRelation edge))

edgeJoinExpr :: Schema -> Model -> IncludeTree -> Text
edgeJoinExpr schema parent edge =
  if null (includeChildren edge)
    then "include." <> fld
    else "isJust include." <> fld
  where
    fld = edgeField schema parent edge

emitIncludePresetTypes :: Schema -> ModelInclude -> Text
emitIncludePresetTypes schema incl =
  let root = lookupModel schema (includeRootModel incl)
      inclName = includeName incl
      rootRow = modelName root <> "Row"
   in T.unlines $
        [ "type family ResolveInclude preset :: Type",
          "type instance ResolveInclude NoInclude = " <> rootRow
        ]
          ++ emitCombinatorResolveLines schema incl
          ++ [ "",
               "newtype NoInclude = NoInclude " <> inclName,
               "  deriving (Show, Eq)",
               "",
               "unwrapNoInclude :: NoInclude -> " <> inclName,
               "unwrapNoInclude (NoInclude include) = include"
             ]

emitIncludeModule :: Text -> Schema -> ModelInclude -> Text
emitIncludeModule moduleName schema incl =
  T.unlines $
    concat
      [ includePragmas,
        [ "module " <> moduleName,
          emitIncludeExports schema incl,
          "where",
          ""
        ]
          ++ includePrelude schema incl
          ++ [ emitIncludeImports moduleName schema incl,
               ""
             ],
        [ emitIncludeADTs schema incl,
          emitCombinatorResultADTs schema incl,
          emitCombinatorTags schema incl,
          emitIncludePresetTypes schema incl,
          emitIncludesJoin schema incl,
          emitNestInclude schema incl,
          emitLoadHelpers schema incl,
          emitCombinatorLoaders schema incl,
          emitExecuteInclude schema incl,
          emitCombinatorExecute schema incl,
          emitCombinatorAPI schema incl
        ]
      ]

includePragmas :: [Text]
includePragmas =
  [ "{-# LANGUAGE AllowAmbiguousTypes #-}",
    "{-# LANGUAGE DuplicateRecordFields #-}",
    "{-# LANGUAGE FlexibleContexts #-}",
    "{-# LANGUAGE FlexibleInstances #-}",
    "{-# LANGUAGE MultiParamTypeClasses #-}",
    "{-# LANGUAGE NoFieldSelectors #-}",
    "{-# LANGUAGE OverloadedRecordDot #-}",
    "{-# LANGUAGE TypeApplications #-}",
    "{-# LANGUAGE TypeFamilies #-}",
    "{-# LANGUAGE TypeOperators #-}",
    "{-# LANGUAGE UndecidableInstances #-}",
    ""
  ]

includePrelude :: Schema -> ModelInclude -> [Text]
includePrelude schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in ["import Prelude hiding ((<>))" | not (null (combinePairs schema root))]

emitIncludeExports :: Schema -> ModelInclude -> Text
emitIncludeExports schema incl =
  "  ( "
    <> T.intercalate ",\n    " items
    <> "\n  )"
  where
    nestedExport =
      [ itdName d <> " (..)"
        | d <- collectIncludeTypes schema incl,
          itdName d /= includeName incl
      ]
    combinatorExports = emitCombinatorExports schema incl
    items =
      [includeName incl <> " (..)"]
        ++ presetExportItems schema incl
        ++ combinatorExports
        ++ nestedExport

presetExportItems :: Schema -> ModelInclude -> [Text]
presetExportItems _schema _incl =
  [ "NoInclude (..)",
    "ResolveInclude",
    "unwrapNoInclude"
  ]

emitIncludeImports :: Text -> Schema -> ModelInclude -> Text
emitIncludeImports moduleName schema incl =
  T.intercalate "\n" $
    filter
      (not . T.null)
      [ "import Data.Kind (Type)",
        if (not . all (null . includeChildren)) (includeTree incl) then "import Data.Maybe (isJust)" else "",
        "import Poppy.Db (Db)",
        "import Poppy.Include",
        "  ( ExecuteInclude (..),",
        "    IncludesJoin (..),",
        "    NestInclude (..)",
        "  )",
        "import Poppy.Operations (findMany)",
        selectInImport schema incl
      ]
      ++ map (unqualifiedRowImport moduleName) models
      ++ map (qualifiedImport moduleName) childModels
  where
    root = lookupModel schema (includeRootModel incl)
    models = includeTreeModels schema root (includeTree incl)
    childModels = filter ((/= modelName root) . modelName) models

unqualifiedRowImport :: Text -> Model -> Text
unqualifiedRowImport moduleName model =
  "import "
    <> schemaModulePrefix moduleName
    <> modelName model
    <> " ("
    <> tableTypeNameFor model
    <> ", "
    <> modelName model
    <> "Row (..))"

tableTypeNameFor :: Model -> Text
tableTypeNameFor model = modelName model <> "Table"

selectInImport :: Schema -> ModelInclude -> Text
selectInImport schema incl =
  "import Poppy.SelectIn\n  ( "
    <> T.intercalate ",\n    " names
    <> "\n  )"
  where
    names =
      ["findByIn", "prepareIncludeRootQuery"]
        ++ hasManyImports
        ++ belongsToImports
    hasManyImports =
      if treeHasKind schema incl RelHasMany
        then ["emptyGroups", "indexHasMany", "lookupGroups"]
        else []
    belongsToImports =
      if treeHasKind schema incl RelBelongsTo
        then ["emptyByPk", "indexByPk", "lookupByPk"]
        else []

treeHasKind :: Schema -> ModelInclude -> RelationKind -> Bool
treeHasKind schema incl wanted =
  let root = lookupModel schema (includeRootModel incl)
   in anyEdgeKind schema root (includeTree incl) wanted

anyEdgeKind :: Schema -> Model -> [IncludeTree] -> RelationKind -> Bool
anyEdgeKind schema current edges wanted =
  any go edges
  where
    go edge =
      let rel = lookupRelation current (includeRelation edge)
          child = lookupModel schema (relToModel rel)
       in relKind rel == wanted || anyEdgeKind schema child (includeChildren edge) wanted

qualifiedImport :: Text -> Model -> Text
qualifiedImport moduleName model =
  "import qualified "
    <> schemaModulePrefix moduleName
    <> modelName model
    <> " as "
    <> modelName model

schemaModulePrefix :: Text -> Text
schemaModulePrefix moduleName =
  case T.breakOnEnd "." moduleName of
    (prefix, _) | not (T.null prefix) -> prefix
    _ -> ""

includeTreeModels :: Schema -> Model -> [IncludeTree] -> [Model]
includeTreeModels schema root edges =
  nubBy (\a b -> modelName a == modelName b) $
    root : concatMap (go root) edges
  where
    go current edge =
      let rel = lookupRelation current (includeRelation edge)
          child = lookupModel schema (relToModel rel)
       in child : concatMap (go child) (includeChildren edge)

collectIncludeTypes :: Schema -> ModelInclude -> [IncludeTypeDecl]
collectIncludeTypes schema incl =
  nested ++ [rootDecl]
  where
    root = lookupModel schema (includeRootModel incl)
    rootDecl =
      IncludeTypeDecl
        { itdName = includeName incl,
          itdParent = root,
          itdEdges = includeTree incl
        }
    nested = collectNested schema root (includeTree incl)

collectNested :: Schema -> Model -> [IncludeTree] -> [IncludeTypeDecl]
collectNested schema current = concatMap go
  where
    go edge =
      let rel = lookupRelation current (includeRelation edge)
          child = lookupModel schema (relToModel rel)
          kids = includeChildren edge
          deeper = collectNested schema child kids
          self =
            [ IncludeTypeDecl
                { itdName = modelName child <> "Include",
                  itdParent = child,
                  itdEdges = kids
                }
              | not (null kids)
            ]
       in deeper ++ self

emitTypeDecl :: Schema -> IncludeTypeDecl -> Text
emitTypeDecl schema IncludeTypeDecl {itdName, itdParent, itdEdges} =
  T.unlines
    [ keyword <> " " <> itdName <> " = " <> itdName,
      "  { " <> T.intercalate ",\n    " (map (emitField schema itdParent) itdEdges),
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    keyword = if length itdEdges == 1 then "newtype" else "data"

emitField :: Schema -> Model -> IncludeTree -> Text
emitField schema parent edge =
  name <> " :: " <> ty
  where
    rel = lookupRelation parent (includeRelation edge)
    child = lookupModel schema (relToModel rel)
    name = includeFieldName parent rel
    ty =
      if null (includeChildren edge)
        then "Bool"
        else "Maybe " <> modelName child <> "Include"

collectResultTypes :: Schema -> ModelInclude -> [IncludeTypeDecl]
collectResultTypes schema incl =
  nested ++ [rootDecl]
  where
    root = lookupModel schema (includeRootModel incl)
    rootDecl =
      IncludeTypeDecl
        { itdName = resultTypeName schema root (includeTree incl),
          itdParent = root,
          itdEdges = includeTree incl
        }
    nested =
      [ IncludeTypeDecl
          { itdName = resultTypeName schema (itdParent d) (itdEdges d),
            itdParent = itdParent d,
            itdEdges = itdEdges d
          }
        | d <- collectNested schema root (includeTree incl)
      ]

emitResultTypeDecl :: Schema -> IncludeTypeDecl -> Text
emitResultTypeDecl schema IncludeTypeDecl {itdName, itdParent, itdEdges} =
  T.unlines
    [ "data " <> itdName <> " = " <> itdName,
      "  { " <> T.intercalate ",\n    " fields,
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    rootField =
      lowerFirst (modelName itdParent) <> " :: " <> modelName itdParent <> "Row"
    fields = rootField : map (emitResultField schema itdParent) itdEdges

emitResultField :: Schema -> Model -> IncludeTree -> Text
emitResultField schema parent edge =
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