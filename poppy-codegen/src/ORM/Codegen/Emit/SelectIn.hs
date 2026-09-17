{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Emit.SelectIn
  ( emitExecuteInclude,
    emitLoadHelpers,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.EmitCommon
  ( fieldBinder,
    includeFieldName,
    primaryKeyField,
    resultTypeName,
    rowTypeName,
    tableTypeName,
  )
import ORM.Codegen.IR
import ORM.Codegen.Lookup (lookupField, lookupModel, lookupRelation)
import ORM.Codegen.TextUtil (lowerFirst)

data IncludeTypeDecl = IncludeTypeDecl
  { itdName :: Text,
    itdParent :: Model,
    itdEdges :: [IncludeTree]
  }

emitExecuteInclude :: Schema -> ModelInclude -> Text
emitExecuteInclude schema incl =
  T.unlines
    [ "instance {-# OVERLAPPING #-} ExecuteInclude " <> rootTable <> " " <> inclName <> " " <> resultName <> " where",
      "  executeInclude include modifier = do",
      "    roots <- findMany @" <> rootTable <> " @" <> rootRow <> " (prepareIncludeRootQuery @" <> rootTable <> " modifier)",
      "    " <> loadFn <> " include roots"
    ]
  where
    root = lookupModel schema (includeRootModel incl)
    rootTable = tableTypeName root
    rootRow = rowTypeName root
    inclName = includeName incl
    resultName = resultTypeName schema root (includeTree incl)
    loadFn = loadFnName inclName

emitLoadHelpers :: Schema -> ModelInclude -> Text
emitLoadHelpers schema incl =
  T.intercalate "\n" $
    map (emitLoadFn schema) (collectIncludeTypes schema incl)

loadFnName :: Text -> Text
loadFnName typeName = "load" <> typeName

emitLoadFn :: Schema -> IncludeTypeDecl -> Text
emitLoadFn schema IncludeTypeDecl {itdName, itdParent, itdEdges} =
  T.unlines $
    [ loadFn <> " :: " <> itdName <> " -> [" <> parentRow <> "] -> Db [" <> resultName <> "]",
      loadFn <> " include roots = do"
    ]
      ++ map (indent 2) (concatMap (emitEdgeLoad schema itdParent) itdEdges)
      ++ [indent 2 "pure", indent 4 "["]
      ++ emitResultRecord schema itdParent itdEdges resultName
      ++ [indent 4 "| root <- roots", indent 4 "]"]
  where
    loadFn = loadFnName itdName
    parentRow = rowTypeName itdParent
    resultName = resultTypeName schema itdParent itdEdges

emitEdgeLoad :: Schema -> Model -> IncludeTree -> [Text]
emitEdgeLoad schema parent edge =
  case relKind rel of
    RelHasMany -> emitHasManyLoad schema parent edge rel child
    RelBelongsTo -> emitBelongsToLoad schema parent edge rel child
  where
    rel = lookupRelation parent (includeRelation edge)
    child = lookupModel schema (relToModel rel)

emitHasManyLoad :: Schema -> Model -> IncludeTree -> RelationSpec -> Model -> [Text]
emitHasManyLoad _schema parent edge rel child =
  case includeChildren edge of
    [] ->
      [ mapName <> " <-",
        "  if include." <> fld,
        "    then indexHasMany (." <> fkName <> ") <$> findByIn @" <> childTable <> " @" <> childRow <> " " <> fkBinder <> " (map (." <> parentPk <> ") roots)",
        "    else pure emptyGroups"
      ]
    _ ->
      [ mapName <> " <- case include." <> fld <> " of",
        "  Nothing -> pure emptyGroups",
        "  Just nestedInclude -> do",
        "    rows <- findByIn @" <> childTable <> " @" <> childRow <> " " <> fkBinder <> " (map (." <> parentPk <> ") roots)",
        "    nested <- " <> loadFnName (modelName child <> "Include") <> " nestedInclude rows",
        "    pure $ indexHasMany ((" <> "." <> fkName <> ") . (." <> childVar <> ")) nested"
      ]
  where
    fld = includeFieldName parent rel
    mapName = fld <> "Map"
    childTable = tableTypeName child
    childRow = rowTypeName child
    fkField = lookupField child (relForeignField rel)
    fkName = fieldName fkField
    fkBinder = modelName child <> "." <> fieldBinder child fkField
    parentPk = fieldName (primaryKeyField parent)
    childVar = lowerFirst (modelName child)

emitBelongsToLoad :: Schema -> Model -> IncludeTree -> RelationSpec -> Model -> [Text]
emitBelongsToLoad _schema parent edge rel child =
  case includeChildren edge of
    [] ->
      [ mapName <> " <-",
        "  if include." <> fld,
        "    then indexByPk (." <> childPk <> ") <$> findByIn @" <> childTable <> " @" <> childRow <> " " <> pkBinder <> " (map (." <> fkName <> ") roots)",
        "    else pure emptyByPk"
      ]
    _ ->
      [ mapName <> " <- case include." <> fld <> " of",
        "  Nothing -> pure emptyByPk",
        "  Just nestedInclude -> do",
        "    rows <- findByIn @" <> childTable <> " @" <> childRow <> " " <> pkBinder <> " (map (." <> fkName <> ") roots)",
        "    nested <- " <> loadFnName (modelName child <> "Include") <> " nestedInclude rows",
        "    pure $ indexByPk ((" <> "." <> childPk <> ") . (." <> childVar <> ")) nested"
      ]
  where
    fld = includeFieldName parent rel
    mapName = fld <> "Map"
    childTable = tableTypeName child
    childRow = rowTypeName child
    childPkField = lookupField child (relLocalField rel)
    childPk = fieldName childPkField
    pkBinder = modelName child <> "." <> fieldBinder child childPkField
    fkName = relForeignField rel
    childVar = lowerFirst (modelName child)

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

indent :: Int -> Text -> Text
indent n line = T.replicate n " " <> line

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
