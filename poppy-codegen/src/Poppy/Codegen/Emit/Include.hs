{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Emit.Include
  ( emitIncludeModule,
  )
where

import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.EmitCommon
  ( fieldBinder,
    pickedTypeName,
    primaryKeyField,
    rowTypeName,
    selectTypeName,
    tableTypeName,
    toPickedName,
  )
import Poppy.Codegen.IR
import Poppy.Codegen.Lookup (lookupField, lookupModel)
import Poppy.Codegen.TextUtil (lowerFirst, upperFirst)

emitIncludeModule :: Text -> Schema -> Model -> Text
emitIncludeModule moduleName schema model
  | modelName model /= modelName (representative schema model) =
      emitReexport moduleName schema model
  | otherwise =
      emitComponent moduleName schema (componentModels schema model)

emitReexport :: Text -> Schema -> Model -> Text
emitReexport moduleName schema model =
  T.unlines $
    [ "module " <> moduleName
    ]
      ++ exportLines (modelExportNames model)
      ++ [ "where",
           "",
           "import " <> repModule <> " (" <> T.intercalate ", " (modelExportNames model) <> ")"
         ]
  where
    rep = representative schema model
    repModule = includeModuleName (modulePrefix moduleName model) rep

emitComponent :: Text -> Schema -> [Model] -> Text
emitComponent moduleName schema models =
  T.unlines $
    [ "{-# LANGUAGE DataKinds #-}",
      "{-# LANGUAGE DuplicateRecordFields #-}",
      "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE MultiParamTypeClasses #-}",
      "{-# LANGUAGE NoFieldSelectors #-}",
      "{-# LANGUAGE OverloadedRecordDot #-}",
      "{-# LANGUAGE StandaloneDeriving #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "{-# LANGUAGE UndecidableInstances #-}",
      "{-# OPTIONS_GHC -Wno-redundant-constraints #-}",
      "",
      "module " <> moduleName
    ]
      ++ exportLines (concatMap modelExportNames models)
      ++ ["where", ""]
      ++ emitImports moduleName schema models
      ++ [""]
      ++ concatMap (\model -> emitModelTypes schema model ++ [""]) models
      ++ concatMap (\model -> emitModelLoad schema model ++ [""]) models

emitImports :: Text -> Schema -> [Model] -> [Text]
emitImports moduleName schema models =
  [ "import Poppy.Db (Db)",
    "import Poppy.Include (IncludeFor, Load (..), Skip (..), Skipped, ValidEdge, skipped" <> requireImport <> ")",
    "import Poppy.Select (OmitSelect (..))"
  ]
    ++ selectInImport schema models
    ++ concatMap (emitSchemaImport prefix models childNames) (importModels schema models)
    ++ map (emitIncludeImport prefix) (externalChildren schema models)
  where
    prefix = modulePrefix moduleName (head models)
    childNames =
      [ modelName (childModel schema rel)
        | model <- models,
          rel <- modelRelations model
      ]
    requireImport =
      if any (any (isRequiredBelongsTo schema) . modelRelations) models
        then ", requireRelated"
        else ""

selectInImport :: Schema -> [Model] -> [Text]
selectInImport schema models =
  [ "import Poppy.SelectIn (" <> T.intercalate ", " names <> ")"
    | not (null names)
  ]
  where
    rels = concatMap modelRelations models
    names =
      ["findByIn" | not (null rels)]
        ++ ["indexByPk" | any (\rel -> relKind rel == RelBelongsTo) rels]
        ++ ["indexHasMany" | any (isPlainHasMany schema) rels]
        ++ ["indexHasManyMaybe" | any (isNullableHasMany schema) rels]
        ++ ["lookupByPk" | any (\rel -> relKind rel == RelBelongsTo) rels]
        ++ ["lookupGroups" | any (\rel -> relKind rel == RelHasMany) rels]

emitSchemaImport :: Text -> [Model] -> [Text] -> Model -> [Text]
emitSchemaImport prefix component childNames model =
  [ "import qualified " <> modName <> " as " <> modelName model
    | isChild
  ]
    ++ [ "import " <> modName <> " (" <> T.intercalate ", " names <> ")"
         | not (null names)
       ]
  where
    modName = prefix <> modelName model
    inComponent = modelName model `elem` map modelName component
    isChild = modelName model `elem` childNames
    names =
      concat
        [ [pickedTypeName model | inComponent],
          [rowTypeName model <> " (..)"],
          [selectTypeName model | inComponent],
          [tableTypeName model | isChild],
          [toPickedName model | inComponent]
        ]

emitIncludeImport :: Text -> Model -> Text
emitIncludeImport prefix child =
  "import "
    <> includeModuleName prefix child
    <> " ("
    <> T.intercalate ", " names
    <> ")"
  where
    names =
      [ modelName child <> "Include (..)",
        modelName child <> "Result",
        modelName child <> "With (..)",
        "Load" <> modelName child <> " (..)"
      ]

emitModelTypes :: Schema -> Model -> [Text]
emitModelTypes schema model =
  concat
    [ emitIncludeData model,
      [""],
      emitWithData model False,
      [""],
      emitWithData model True,
      [""],
      emitToPicked model,
      [""],
      concatMap (\rel -> emitEdgeFamily schema model rel ++ [""]) (modelRelations model),
      emitResultFamily model,
      [""],
      emitReadFamily model
    ]

emitIncludeData :: Model -> [Text]
emitIncludeData model =
  [ "data " <> includeType model <> " " <> params model <> " = " <> includeType model,
    "  { " <> T.intercalate ",\n    " [relName rel <> " :: " <> relName rel | rel <- modelRelations model],
    "  }",
    "  deriving (Show, Eq)"
  ]

emitWithData :: Model -> Bool -> [Text]
emitWithData model picked =
  [ "data " <> withName <> " " <> params model <> " = " <> withName,
    "  { " <> T.intercalate ",\n    " fields,
    "  }",
    "",
    "deriving instance (" <> ctx "Eq" <> ") => Eq (" <> applied <> ")",
    "deriving instance (" <> ctx "Show" <> ") => Show (" <> applied <> ")"
  ]
  where
    withName = if picked then withPickedType model else withType model
    rootTy = if picked then pickedTypeName model else rowTypeName model
    applied = withName <> " " <> params model
    fields =
      (rootVar model <> " :: " <> rootTy)
        : [ relName rel <> " :: " <> edgeFamily model rel <> " " <> relName rel
            | rel <- modelRelations model
          ]
    ctx cls =
      T.intercalate
        ", "
        ( (cls <> " " <> rootTy)
            : [ cls <> " (" <> edgeFamily model rel <> " " <> relName rel <> ")"
                | rel <- modelRelations model
              ]
        )

emitToPicked :: Model -> [Text]
emitToPicked model =
  [ toWithPickedName model
      <> " :: "
      <> selectTypeName model
      <> " -> "
      <> withType model
      <> " "
      <> params model
      <> " -> "
      <> withPickedType model
      <> " "
      <> params model,
    toWithPickedName model <> " select_ nested =",
    "  " <> withPickedType model,
    "    { " <> T.intercalate ",\n      " fields,
    "    }"
  ]
  where
    fields =
      (rootVar model <> " = " <> toPickedName model <> " select_ nested." <> rootVar model)
        : [relName rel <> " = nested." <> relName rel | rel <- modelRelations model]

emitEdgeFamily :: Schema -> Model -> RelationSpec -> [Text]
emitEdgeFamily schema model rel =
  [ "type family " <> fam <> " edge where",
    "  " <> fam <> " Skip = Skipped \"" <> relName rel <> "\" " <> skipTy,
    "  " <> fam <> " " <> loadPat <> " = " <> loadedTy
  ]
  where
    fam = edgeFamily model rel
    child = childModel schema rel
    skipTy = wrapSkip (plainLoaded schema rel)
    (loadPat, loadedTy)
      | hasInclude child =
          ("(Load include)", loadedRhs schema rel "include")
      | otherwise =
          ("(Load ())", loadedRhs schema rel "")

emitResultFamily :: Model -> [Text]
emitResultFamily model =
  [ "type family " <> modelName model <> "Result include where",
    "  " <> modelName model <> "Result () = " <> rowTypeName model,
    "  "
      <> modelName model
      <> "Result ("
      <> includeType model
      <> " "
      <> params model
      <> ") = "
      <> withType model
      <> " "
      <> params model
  ]

emitReadFamily :: Model -> [Text]
emitReadFamily model =
  [ "type family " <> modelName model <> "Read include select where",
    "  " <> modelName model <> "Read () OmitSelect = " <> rowTypeName model,
    "  " <> modelName model <> "Read () " <> selectTypeName model <> " = " <> pickedTypeName model,
    "  "
      <> modelName model
      <> "Read ("
      <> includeType model
      <> " "
      <> params model
      <> ") OmitSelect = "
      <> withType model
      <> " "
      <> params model,
    "  "
      <> modelName model
      <> "Read ("
      <> includeType model
      <> " "
      <> params model
      <> ") "
      <> selectTypeName model
      <> " = "
      <> withPickedType model
      <> " "
      <> params model
  ]

emitModelLoad :: Schema -> Model -> [Text]
emitModelLoad schema model =
  concatMap (\rel -> emitEdgeLoad schema model rel ++ [""]) (modelRelations model)
    ++ emitModelLoadClass model
    ++ [""]
    ++ emitIncludeFor model

emitEdgeLoad :: Schema -> Model -> RelationSpec -> [Text]
emitEdgeLoad schema model rel =
  emitEdgeClass model rel
    ++ [""]
    ++ emitSkipInstance model rel
    ++ [""]
    ++ emitUnitInstance schema model rel
    ++ nested
    ++ [""]
    ++ emitRejectedInstance model rel
  where
    child = childModel schema rel
    nested
      | hasInclude child = "" : emitNestedInstance schema model rel child
      | otherwise = []

emitEdgeClass :: Model -> RelationSpec -> [Text]
emitEdgeClass model rel =
  [ "class " <> edgeClass model rel <> " edge where",
    "  "
      <> edgeMethod model rel
      <> " :: edge -> ["
      <> rowTypeName model
      <> "] -> Db ["
      <> edgeFamily model rel
      <> " edge]"
  ]

emitSkipInstance :: Model -> RelationSpec -> [Text]
emitSkipInstance model rel =
  [ "instance " <> edgeClass model rel <> " Skip where",
    "  " <> edgeMethod model rel <> " Skip roots = pure (map (const skipped) roots)"
  ]

emitUnitInstance :: Schema -> Model -> RelationSpec -> [Text]
emitUnitInstance schema model rel =
  [ "instance " <> edgeClass model rel <> " (Load ()) where",
    "  " <> edgeMethod model rel <> " (Load ()) roots = do"
  ]
    ++ emitFetch schema model rel
    ++ emitGroup schema model rel False

emitNestedInstance :: Schema -> Model -> RelationSpec -> Model -> [Text]
emitNestedInstance schema model rel child =
  [ "instance (" <> loadClass child <> " " <> params child <> ") => " <> edgeClass model rel <> " (Load (" <> includeType child <> " " <> params child <> ")) where",
    "  " <> edgeMethod model rel <> " (Load nested) roots = do"
  ]
    ++ emitFetch schema model rel
    ++ ["    loaded <- " <> loadMethod child <> " nested rows"]
    ++ emitGroup schema model rel True

emitRejectedInstance :: Model -> RelationSpec -> [Text]
emitRejectedInstance model rel =
  [ "instance {-# OVERLAPPABLE #-} (ValidEdge \"" <> relToModel rel <> "\" edge) => " <> edgeClass model rel <> " edge where",
    "  " <> edgeMethod model rel <> " _ roots = pure (map (const skipped) roots)"
  ]

emitFetch :: Schema -> Model -> RelationSpec -> [Text]
emitFetch schema model rel =
  case relKind rel of
    RelHasMany ->
      [ "    rows <- findByIn @" <> tableTypeName child <> " @" <> rowTypeName child <> " " <> fkBinder <> " (map (." <> parentPk <> ") roots)"
      ]
    RelBelongsTo
      | isNullableBelongsTo schema rel ->
          [ "    let keys = [key | root <- roots, Just key <- [root." <> fkName <> "]]",
            "    rows <- findByIn @" <> tableTypeName child <> " @" <> rowTypeName child <> " " <> pkBinder <> " keys"
          ]
      | otherwise ->
          [ "    rows <- findByIn @" <> tableTypeName child <> " @" <> rowTypeName child <> " " <> pkBinder <> " (map (." <> fkName <> ") roots)"
          ]
  where
    child = childModel schema rel
    parentPk = fieldName (primaryKeyField model)
    fkName = relForeignField rel
    fkField = lookupField child (relForeignField rel)
    fkBinder = modelName child <> "." <> fieldBinder child fkField
    pkField = lookupField child (relLocalField rel)
    pkBinder = modelName child <> "." <> fieldBinder child pkField

emitGroup :: Schema -> Model -> RelationSpec -> Bool -> [Text]
emitGroup schema model rel nested =
  case relKind rel of
    RelHasMany ->
      [ "    let grouped = " <> indexFn <> " " <> keyExpr <> " " <> source,
        "    pure [lookupGroups root." <> parentPk <> " grouped | root <- roots]"
      ]
    RelBelongsTo
      | isNullableBelongsTo schema rel ->
          [ "    let indexed = indexByPk " <> keyExpr <> " " <> source,
            "    pure [root." <> fkName <> " >>= \\key -> lookupByPk key indexed | root <- roots]"
          ]
      | otherwise ->
          [ "    let indexed = indexByPk " <> keyExpr <> " " <> source,
            "    pure [requireRelated \"" <> relName rel <> "\" (lookupByPk root." <> fkName <> " indexed) | root <- roots]"
          ]
  where
    child = childModel schema rel
    parentPk = fieldName (primaryKeyField model)
    fkName = relForeignField rel
    source = if nested then "loaded" else "rows"
    indexFn
      | isNullableHasMany schema rel = "indexHasManyMaybe"
      | otherwise = "indexHasMany"
    keyExpr
      | nested && relKind rel == RelHasMany =
          keyOn (childFkName schema rel) (lowerFirst (modelName child))
      | nested =
          keyOn (fieldName (primaryKeyField child)) (lowerFirst (modelName child))
      | relKind rel == RelHasMany =
          "(." <> childFkName schema rel <> ")"
      | otherwise =
          "(." <> fieldName (primaryKeyField child) <> ")"

emitModelLoadClass :: Model -> [Text]
emitModelLoadClass model =
  [ "class " <> loadClass model <> " " <> params model <> " where",
    "  "
      <> loadMethod model
      <> " :: "
      <> includeType model
      <> " "
      <> params model
      <> " -> ["
      <> rowTypeName model
      <> "] -> Db ["
      <> withType model
      <> " "
      <> params model
      <> "]",
    "",
    "instance (" <> T.intercalate ", " constraints <> ") => " <> loadClass model <> " " <> params model <> " where",
    "  " <> loadMethod model <> " include roots = do"
  ]
    ++ concatMap edgeBind (modelRelations model)
    ++ [ "    pure",
         "      [ " <> withType model,
         "          { " <> T.intercalate ",\n            " fields,
         "          }",
         "      | (n, root) <- zip [0 :: Int ..] roots",
         "      ]"
       ]
  where
    constraints =
      [edgeClass model rel <> " " <> relName rel | rel <- modelRelations model]
        ++ [validEdge model rel | rel <- modelRelations model]
    edgeBind rel =
      [ "    "
          <> relName rel
          <> "Loaded <- "
          <> edgeMethod model rel
          <> " include."
          <> relName rel
          <> " roots"
      ]
    fields =
      (rootVar model <> " = root")
        : [relName rel <> " = " <> relName rel <> "Loaded !! n" | rel <- modelRelations model]

emitIncludeFor :: Model -> [Text]
emitIncludeFor model =
  [ "instance (" <> T.intercalate ", " [validEdge model rel | rel <- modelRelations model] <> ") => IncludeFor \"" <> modelName model <> "\" (" <> includeType model <> " " <> params model <> ")"
  ]

validEdge :: Model -> RelationSpec -> Text
validEdge _model rel =
  "ValidEdge \"" <> relToModel rel <> "\" " <> relName rel

childModel :: Schema -> RelationSpec -> Model
childModel schema rel = lookupModel schema (relToModel rel)

hasInclude :: Model -> Bool
hasInclude model = not (null (modelRelations model))

isRequiredBelongsTo :: Schema -> RelationSpec -> Bool
isRequiredBelongsTo schema rel =
  relKind rel == RelBelongsTo && not (fieldNullable (lookupField (lookupModel schema (relFromModel rel)) (relForeignField rel)))

isNullableBelongsTo :: Schema -> RelationSpec -> Bool
isNullableBelongsTo schema rel =
  relKind rel == RelBelongsTo && not (isRequiredBelongsTo schema rel)

isNullableHasMany :: Schema -> RelationSpec -> Bool
isNullableHasMany schema rel =
  relKind rel == RelHasMany && fieldNullable (lookupField (childModel schema rel) (relForeignField rel))

isPlainHasMany :: Schema -> RelationSpec -> Bool
isPlainHasMany schema rel =
  relKind rel == RelHasMany && not (isNullableHasMany schema rel)

plainLoaded :: Schema -> RelationSpec -> Text
plainLoaded schema rel =
  case relKind rel of
    RelHasMany -> "[" <> rowTypeName child <> "]"
    RelBelongsTo
      | isNullableBelongsTo schema rel -> "Maybe " <> rowTypeName child
      | otherwise -> rowTypeName child
  where
    child = childModel schema rel

wrapSkip :: Text -> Text
wrapSkip ty
  | " " `T.isInfixOf` ty = "(" <> ty <> ")"
  | otherwise = ty

loadedRhs :: Schema -> RelationSpec -> Text -> Text
loadedRhs schema rel includeVar =
  case relKind rel of
    RelHasMany -> "[" <> childTy <> "]"
    RelBelongsTo
      | isNullableBelongsTo schema rel -> "Maybe (" <> childTy <> ")"
      | otherwise -> childTy
  where
    child = childModel schema rel
    childTy
      | hasInclude child = modelName child <> "Result" <> includeArg
      | otherwise = rowTypeName child
    includeArg
      | T.null includeVar = ""
      | otherwise = " " <> includeVar

childFkName :: Schema -> RelationSpec -> Text
childFkName schema rel =
  fieldName (lookupField (childModel schema rel) (relForeignField rel))

keyOn :: Text -> Text -> Text
keyOn keyField accessor = "((." <> keyField <> ") . (." <> accessor <> "))"

params :: Model -> Text
params model = T.unwords (map relName (modelRelations model))

includeType :: Model -> Text
includeType model = modelName model <> "Include"

withType :: Model -> Text
withType model = modelName model <> "With"

withPickedType :: Model -> Text
withPickedType model = modelName model <> "WithPicked"

edgeFamily :: Model -> RelationSpec -> Text
edgeFamily model rel = modelName model <> upperFirst (relName rel)

edgeClass :: Model -> RelationSpec -> Text
edgeClass model rel = "Load" <> edgeFamily model rel

edgeMethod :: Model -> RelationSpec -> Text
edgeMethod model rel = "load" <> edgeFamily model rel

loadClass :: Model -> Text
loadClass model = "Load" <> modelName model

loadMethod :: Model -> Text
loadMethod model = "load" <> modelName model

toWithPickedName :: Model -> Text
toWithPickedName model = "to" <> withPickedType model

rootVar :: Model -> Text
rootVar model = lowerFirst (modelName model)

modelExportNames :: Model -> [Text]
modelExportNames model =
  [ includeType model <> " (..)",
    withType model <> " (..)",
    withPickedType model <> " (..)"
  ]
    ++ [edgeFamily model rel | rel <- modelRelations model]
    ++ [ modelName model <> "Result",
         modelName model <> "Read",
         loadClass model <> " (..)",
         toWithPickedName model
       ]

exportLines :: [Text] -> [Text]
exportLines [] = ["  ()"]
exportLines (first : rest) =
  ("  ( " <> first) : map ("  , " <>) rest ++ ["  )"]

includeModels :: Schema -> [Model]
includeModels schema = filter hasInclude (schemaModels schema)

componentModels :: Schema -> Model -> [Model]
componentModels schema model =
  [m | m <- includeModels schema, modelName m `elem` componentNames]
  where
    componentNames =
      head
        [ names
          | names <- stronglyConnected (map modelName (includeModels schema)) (neighborNames schema),
            modelName model `elem` names
        ]

representative :: Schema -> Model -> Model
representative schema model = head (componentModels schema model)

neighborNames :: Schema -> Text -> [Text]
neighborNames schema name =
  map modelName (deps schema (lookupModel schema name))

deps :: Schema -> Model -> [Model]
deps schema model =
  nubBy (\a b -> modelName a == modelName b) $
    [ child
      | rel <- modelRelations model,
        let child = childModel schema rel,
        modelName child /= modelName model,
        hasInclude child
    ]

importModels :: Schema -> [Model] -> [Model]
importModels schema models =
  nubBy (\a b -> modelName a == modelName b) $
    models ++ concatMap (map (childModel schema) . modelRelations) models

externalChildren :: Schema -> [Model] -> [Model]
externalChildren schema models =
  nubBy (\a b -> modelName a == modelName b) $
    [ child
      | model <- models,
        rel <- modelRelations model,
        let child = childModel schema rel,
        hasInclude child,
        modelName child `notElem` map modelName models
    ]

modulePrefix :: Text -> Model -> Text
modulePrefix moduleName model =
  fromMaybe
    "Schema."
    (T.stripSuffix ("Include." <> modelName model) moduleName)

includeModuleName :: Text -> Model -> Text
includeModuleName prefix model = prefix <> "Include." <> modelName model

stronglyConnected :: [Text] -> (Text -> [Text]) -> [[Text]]
stronglyConnected nodes neighbors =
  let (_, finish) = dfs neighbors [] [] nodes
   in walk (transpose nodes neighbors) [] [] finish

dfs :: (Text -> [Text]) -> [Text] -> [Text] -> [Text] -> ([Text], [Text])
dfs _ seen order [] = (seen, order)
dfs neighbors seen order (name : rest)
  | name `elem` seen = dfs neighbors seen order rest
  | otherwise =
      let (seen1, order1) = dfs neighbors (name : seen) order (neighbors name)
       in dfs neighbors seen1 (name : order1) rest

walk :: (Text -> [Text]) -> [Text] -> [[Text]] -> [Text] -> [[Text]]
walk _ _ comps [] = comps
walk incoming seen comps (name : rest)
  | name `elem` seen = walk incoming seen comps rest
  | otherwise =
      let (members, seen1) = collect incoming [] (name : seen) [name]
       in walk incoming seen1 (members : comps) rest

collect :: (Text -> [Text]) -> [Text] -> [Text] -> [Text] -> ([Text], [Text])
collect _ acc seen [] = (acc, seen)
collect incoming acc seen (name : stack) =
  let more = [next | next <- incoming name, next `notElem` seen]
   in collect incoming (name : acc) (more ++ seen) (more ++ stack)

transpose :: [Text] -> (Text -> [Text]) -> Text -> [Text]
transpose nodes neighbors name =
  [other | other <- nodes, name `elem` neighbors other]
