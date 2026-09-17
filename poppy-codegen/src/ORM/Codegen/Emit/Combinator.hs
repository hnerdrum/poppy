{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Emit.Combinator
  ( emitCombinatorResultADTs,
    emitCombinatorTags,
    emitCombinatorResolveLines,
    emitCombinatorLoaders,
    emitCombinatorExecute,
    emitCombinatorAPI,
    emitCombinatorExports,
    combinatorImportItems,
    rootCombinatorTrees,
    singleRelationTrees,
  )
where

import Data.List (nub, nubBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.Combinator
import ORM.Codegen.EmitCommon
  ( includeFieldName,
    primaryKeyField,
    rowTypeName,
    tableTypeName,
  )
import ORM.Codegen.IR
import ORM.Codegen.Lookup (lookupField, lookupModel, lookupRelation)
import ORM.Codegen.TextUtil (lowerFirst, upperFirst)

rootCombinatorTrees :: Schema -> ModelInclude -> [CombinatorTree]
rootCombinatorTrees schema incl =
  combinatorTrees schema (lookupModel schema (includeRootModel incl))

singleRelationTrees :: Schema -> CombinatorTree -> Bool
singleRelationTrees _schema tree =
  length (ctEdges tree) == 1

emitCombinatorExports :: Schema -> ModelInclude -> [Text]
emitCombinatorExports schema incl =
  nub
    ( concatMap (tagExportItems schema) (rootCombinatorTrees schema incl)
        ++ concatMap (resultExportItems schema) (uniqueResults schema incl)
        ++ combinatorNameExports schema incl
    )

tagExportItems :: Schema -> CombinatorTree -> [Text]
tagExportItems schema tree =
  let tag = tagTypeName schema tree
   in [tag <> " (..)", "unwrap" <> tag]

resultExportItems :: Schema -> CombinatorTree -> [Text]
resultExportItems schema tree =
  [resultADTName schema tree <> " (..)"]

combinatorNameExports :: Schema -> ModelInclude -> [Text]
combinatorNameExports schema incl =
  let root = lookupModel schema (includeRootModel incl)
      trees = combinatorTrees schema root
      relClasses =
        nub
          [ "Include" <> upperFirst name <> " (..)"
            | tree <- trees,
              singleRelationTrees schema tree,
              edge <- ctEdges tree,
              let name = combinatorFieldName root edge
          ]
      tokenTypes =
        nub
          [ ty <> " (..)"
            | tree <- trees,
              (_, ty) <- nestTokens schema tree
          ]
      tokenClasses =
        nub
          [ "Include" <> upperFirst name <> " (..)"
            | (name, _, applied) <- groupNestTokens (concatMap (nestTokens schema) trees),
              not (null applied)
          ]
      combine =
        ["CombineInclude (..)" | not (null (combinePairs schema root))]
   in relClasses ++ tokenClasses ++ tokenTypes ++ combine

combinatorImportItems :: Schema -> ModelInclude -> [Text]
combinatorImportItems schema incl =
  combinatorNameExports schema incl
    ++ concatMap (tagExportItems schema) (rootCombinatorTrees schema incl)
    ++ concatMap (resultExportItems schema) (uniqueResults schema incl)

emitCombinatorResultADTs :: Schema -> ModelInclude -> Text
emitCombinatorResultADTs schema incl =
  T.intercalate "\n" $
    map (emitResultADT schema) (uniqueResults schema incl)

uniqueResults :: Schema -> ModelInclude -> [CombinatorTree]
uniqueResults schema incl =
  nubBy (\a b -> resultADTName schema a == resultADTName schema b) $
    concatMap (treeAndNested schema) (rootCombinatorTrees schema incl)

treeAndNested :: Schema -> CombinatorTree -> [CombinatorTree]
treeAndNested schema tree =
  tree : concatMap (nestedFromEdge schema (ctRoot tree)) (ctEdges tree)

nestedFromEdge :: Schema -> Model -> IncludeTree -> [CombinatorTree]
nestedFromEdge schema parent edge =
  case includeChildren edge of
    [] -> []
    kids ->
      let child = childOf schema parent edge
          nested = CombinatorTree child kids
       in nested : concatMap (nestedFromEdge schema child) kids

emitResultADT :: Schema -> CombinatorTree -> Text
emitResultADT schema tree =
  T.unlines
    [ "data " <> name <> " = " <> name,
      "  { " <> T.intercalate ",\n    " fields,
      "  }",
      "  deriving (Show, Eq)"
    ]
  where
    name = resultADTName schema tree
    parent = ctRoot tree
    rootField = lowerFirst (modelName parent) <> " :: " <> modelName parent <> "Row"
    fields = rootField : map (resultField schema parent) (ctEdges tree)

resultField :: Schema -> Model -> IncludeTree -> Text
resultField schema parent edge =
  fld <> " :: " <> ty
  where
    rel = lookupRelation parent (includeRelation edge)
    child = childOf schema parent edge
    fld = includeFieldName parent rel
    kids = includeChildren edge
    ty
      | not (null kids) =
          "[" <> nestedResultName schema child kids <> "]"
      | RelHasMany <- relKind rel =
          "[" <> modelName child <> "Row]"
      | otherwise =
          "Maybe " <> modelName child <> "Row"

emitCombinatorTags :: Schema -> ModelInclude -> Text
emitCombinatorTags schema incl =
  T.intercalate "\n" $
    map (emitTag schema incl) (rootCombinatorTrees schema incl)

emitTag :: Schema -> ModelInclude -> CombinatorTree -> Text
emitTag schema incl tree =
  T.unlines
    [ "newtype " <> tag <> " = " <> tag <> " " <> includeName incl,
      "  deriving (Show, Eq)",
      "",
      "unwrap" <> tag <> " :: " <> tag <> " -> " <> includeName incl,
      "unwrap" <> tag <> " (" <> tag <> " include) = include"
    ]
  where
    tag = tagTypeName schema tree

emitCombinatorResolveLines :: Schema -> ModelInclude -> [Text]
emitCombinatorResolveLines schema incl =
  [ "type instance ResolveInclude " <> tagTypeName schema tree <> " = " <> resultADTName schema tree
    | tree <- rootCombinatorTrees schema incl
  ]

emitCombinatorLoaders :: Schema -> ModelInclude -> Text
emitCombinatorLoaders schema incl =
  T.intercalate "\n" $
    map (emitLoader schema incl) (partialResults schema incl)

partialResults :: Schema -> ModelInclude -> [CombinatorTree]
partialResults schema incl =
  nubBy (\a b -> resultADTName schema a == resultADTName schema b) $
    concatMap (treeAndNested schema) $
      filter (not . isFullGraphTree schema) (rootCombinatorTrees schema incl)

emitLoader :: Schema -> ModelInclude -> CombinatorTree -> Text
emitLoader schema incl tree =
  T.unlines $
    [ loadFn <> " :: " <> includeName incl <> " -> [" <> parentRow <> "] -> Db [" <> result <> "]",
      loadFn <> " " <> includePat <> " roots = do"
    ]
      ++ map (indent 2) (concatMap (emitEdgeLoad schema parent) (ctEdges tree))
      ++ [indent 2 "pure", indent 4 "["]
      ++ emitResultRecord schema parent (ctEdges tree) result
      ++ [indent 4 "| root <- roots", indent 4 "]"]
  where
    parent = ctRoot tree
    parentRow = rowTypeName parent
    result = resultADTName schema tree
    loadFn = "load" <> result
    includePat
      | not (all (null . includeChildren) (ctEdges tree)) = "include"
      | otherwise = "_include"

emitEdgeLoad :: Schema -> Model -> IncludeTree -> [Text]
emitEdgeLoad schema parent edge =
  case relKind rel of
    RelHasMany -> emitHasManyLoad schema parent edge rel child
    RelBelongsTo -> emitBelongsToLoad schema parent edge rel child
  where
    rel = lookupRelation parent (includeRelation edge)
    child = childOf schema parent edge

emitHasManyLoad :: Schema -> Model -> IncludeTree -> RelationSpec -> Model -> [Text]
emitHasManyLoad schema parent edge rel child =
  case includeChildren edge of
    [] ->
      [ mapName <> " <-",
        "  indexHasMany (." <> fkName <> ") <$> findByIn @" <> childTable <> " @" <> childRow <> " " <> fkBinder <> " (map (." <> parentPk <> ") roots)"
      ]
    kids ->
      [ mapName <> " <- do",
        "  rows <- findByIn @" <> childTable <> " @" <> childRow <> " " <> fkBinder <> " (map (." <> parentPk <> ") roots)",
        "  nested <- load" <> nestedResultName schema child kids <> " include rows",
        "  pure $ indexHasMany ((" <> "." <> fkName <> ") . (." <> childVar <> ")) nested"
      ]
  where
    fld = includeFieldName parent rel
    mapName = fld <> "Map"
    childTable = tableTypeName child
    childRow = rowTypeName child
    fkField = lookupField child (relForeignField rel)
    fkName = fieldName fkField
    fkBinder = modelName child <> "." <> fieldBinderName child fkField
    parentPk = fieldName (primaryKeyField parent)
    childVar = lowerFirst (modelName child)

emitBelongsToLoad :: Schema -> Model -> IncludeTree -> RelationSpec -> Model -> [Text]
emitBelongsToLoad schema parent edge rel child =
  case includeChildren edge of
    [] ->
      [ mapName <> " <-",
        "  indexByPk (." <> childPk <> ") <$> findByIn @" <> childTable <> " @" <> childRow <> " " <> pkBinder <> " (map (." <> fkName <> ") roots)"
      ]
    kids ->
      [ mapName <> " <- do",
        "  rows <- findByIn @" <> childTable <> " @" <> childRow <> " " <> pkBinder <> " (map (." <> fkName <> ") roots)",
        "  nested <- load" <> nestedResultName schema child kids <> " include rows",
        "  pure $ indexByPk ((" <> "." <> childPk <> ") . (." <> childVar <> ")) nested"
      ]
  where
    fld = includeFieldName parent rel
    mapName = fld <> "Map"
    childTable = tableTypeName child
    childRow = rowTypeName child
    childPkField = lookupField child (relLocalField rel)
    childPk = fieldName childPkField
    pkBinder = modelName child <> "." <> fieldBinderName child childPkField
    fkName = relForeignField rel
    childVar = lowerFirst (modelName child)

fieldBinderName :: Model -> FieldSpec -> Text
fieldBinderName model f =
  lowerFirst (modelName model) <> upperFirst (fieldName f)

emitResultRecord :: Schema -> Model -> [IncludeTree] -> Text -> [Text]
emitResultRecord schema parent edges resultName =
  [indent 6 (resultName <> " {"), indent 8 (rootVar <> " = root" <> if null edges then "" else ",")]
    ++ zipWith (emitResultFieldLine schema parent (length edges)) [1 ..] edges
    ++ [indent 6 "}"]
  where
    rootVar = lowerFirst (modelName parent)

emitResultFieldLine :: Schema -> Model -> Int -> Int -> IncludeTree -> Text
emitResultFieldLine _schema parent total idx edge =
  indent 8 (fld <> " = " <> value <> suffix)
  where
    rel = lookupRelation parent (includeRelation edge)
    fld = includeFieldName parent rel
    suffix = if idx < total then "," else ""
    value = case relKind rel of
      RelHasMany -> "lookupGroups root." <> fieldName (primaryKeyField parent) <> " " <> fld <> "Map"
      RelBelongsTo -> "lookupByPk root." <> relForeignField rel <> " " <> fld <> "Map"

emitCombinatorExecute :: Schema -> ModelInclude -> Text
emitCombinatorExecute schema incl =
  T.intercalate "\n" $
    map (emitExecute schema incl) (rootCombinatorTrees schema incl)

emitExecute :: Schema -> ModelInclude -> CombinatorTree -> Text
emitExecute schema incl tree
  | isFullGraphTree schema tree =
      T.unlines
        [ "instance {-# OVERLAPPING #-} ExecuteInclude " <> rootTable <> " " <> tag <> " " <> result <> " where",
          "  executeInclude (" <> tag <> " include) modifier = executeInclude include modifier"
        ]
  | otherwise =
      T.unlines
        [ "instance {-# OVERLAPPING #-} ExecuteInclude " <> rootTable <> " " <> tag <> " " <> result <> " where",
          "  executeInclude (" <> tag <> " include) modifier = do",
          "    roots <- findMany @" <> rootTable <> " @" <> rootRow <> " (prepareIncludeRootQuery @" <> rootTable <> " modifier)",
          "    load" <> result <> " include roots"
        ]
  where
    root = lookupModel schema (includeRootModel incl)
    rootTable = tableTypeName root
    rootRow = rowTypeName root
    tag = tagTypeName schema tree
    result = resultADTName schema tree

emitCombinatorAPI :: Schema -> ModelInclude -> Text
emitCombinatorAPI schema incl =
  T.intercalate "\n" $
    filter
      (not . T.null)
      [ emitTokenTypes schema incl,
        emitNestBindings schema incl,
        emitRelationClasses schema incl,
        emitCombine schema incl
      ]

emitTokenTypes :: Schema -> ModelInclude -> Text
emitTokenTypes schema incl =
  T.intercalate "\n" $
    map emitTokenType (nub (map snd (allNestTokens schema incl)))
  where
    emitTokenType ty =
      T.unlines ["data " <> ty <> " = " <> ty]

allNestTokens :: Schema -> ModelInclude -> [(Text, Text)]
allNestTokens schema incl =
  let root = lookupModel schema (includeRootModel incl)
   in concatMap (nestTokens schema) (combinatorTrees schema root)

emitNestBindings :: Schema -> ModelInclude -> Text
emitNestBindings schema incl =
  T.intercalate "\n" $
    filter (not . T.null) $
      map emitBinding (groupNestTokens (allNestTokens schema incl))
  where
    emitBinding (_name, _leafTy, []) = ""
    emitBinding (name, leafTy, applied) =
      T.unlines $
        [ "class Include" <> upperFirst name <> " include where",
          "  " <> name <> " :: include",
          "",
          "instance Include" <> upperFirst name <> " " <> leafTy <> " where",
          "  " <> name <> " = " <> leafTy,
          ""
        ]
          ++ concatMap (appliedInstance name leafTy) applied
    appliedInstance name leafTy resultTy =
      let argTy = fromMaybe resultTy (T.stripPrefix leafTy resultTy)
       in [ "instance Include" <> upperFirst name <> " (" <> argTy <> " -> " <> resultTy <> ") where",
            "  " <> name <> " " <> argTy <> " = " <> resultTy,
            ""
          ]

groupNestTokens :: [(Text, Text)] -> [(Text, Text, [Text])]
groupNestTokens pairs =
  [ (name, leafTy, applied)
    | name <- nub (map fst pairs),
      let types = nub [ty | (n, ty) <- pairs, n == name],
      let leafTy = upperFirst name,
      let applied = [ty | ty <- types, ty /= leafTy]
  ]

emitRelationClasses :: Schema -> ModelInclude -> Text
emitRelationClasses schema incl =
  T.intercalate "\n" $
    map (emitRelationClass schema incl) (rootRelations schema incl)

rootRelations :: Schema -> ModelInclude -> [RelationSpec]
rootRelations schema incl =
  modelRelations (lookupModel schema (includeRootModel incl))

emitRelationClass :: Schema -> ModelInclude -> RelationSpec -> Text
emitRelationClass schema incl rel =
  T.unlines $
    [ "class Include" <> upperFirst fld <> " include where",
      "  " <> fld <> " :: include",
      ""
    ]
      ++ concatMap (emitRelationInstance schema incl rel fld) (treesForRel schema incl rel)
  where
    root = lookupModel schema (includeRootModel incl)
    fld = includeFieldName root rel

treesForRel :: Schema -> ModelInclude -> RelationSpec -> [CombinatorTree]
treesForRel schema incl rel =
  [ tree
    | tree <- rootCombinatorTrees schema incl,
      [edge] <- [ctEdges tree],
      includeRelation edge == relName rel
  ]

emitRelationInstance :: Schema -> ModelInclude -> RelationSpec -> Text -> CombinatorTree -> [Text]
emitRelationInstance schema _incl _rel fld tree =
  case includeChildren (head (ctEdges tree)) of
    [] ->
      [ "instance Include" <> upperFirst fld <> " " <> tag <> " where",
        "  " <> fld <> " =",
        "    " <> tag <> " " <> includeRecordValue schema tree,
        ""
      ]
    kids ->
      let root = ctRoot tree
          child = childOf schema root (head (ctEdges tree))
          argType = T.concat (map (edgeTagName schema child) kids)
       in [ "instance (include ~ " <> tag <> ") => Include" <> upperFirst fld <> " (" <> argType <> " -> include) where",
            "  " <> fld <> " " <> argType <> " =",
            "    " <> tag <> " " <> includeRecordValue schema tree,
            ""
          ]
  where
    tag = tagTypeName schema tree

edgeTagName :: Schema -> Model -> IncludeTree -> Text
edgeTagName schema parent edge
  | null (includeChildren edge) = upperFirst (edgeFieldName parent edge)
  | otherwise =
      upperFirst (edgeFieldName parent edge)
        <> T.concat (map (edgeTagName schema (childOf schema parent edge)) (includeChildren edge))

emitCombine :: Schema -> ModelInclude -> Text
emitCombine schema incl =
  case combinePairs schema root of
    [] -> ""
    pairs ->
      T.unlines
        ( [ "class CombineInclude a b where",
            "  type CombinedInclude a b :: Type",
            "  (<>) :: a -> b -> CombinedInclude a b",
            ""
          ]
            ++ concatMap (emitCombineInstance schema) pairs
        )
  where
    root = lookupModel schema (includeRootModel incl)

emitCombineInstance :: Schema -> (CombinatorTree, CombinatorTree, CombinatorTree) -> [Text]
emitCombineInstance schema (left, right, merged) =
  [ "instance CombineInclude " <> leftTag <> " " <> rightTag <> " where",
    "  type CombinedInclude " <> leftTag <> " " <> rightTag <> " = " <> mergedTag,
    "  _ <> _ =",
    "    " <> mergedTag <> " " <> includeRecordValue schema merged,
    ""
  ]
  where
    leftTag = tagTypeName schema left
    rightTag = tagTypeName schema right
    mergedTag = tagTypeName schema merged

indent :: Int -> Text -> Text
indent n line = T.replicate n " " <> line
