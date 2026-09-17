module ORM.Codegen.Combinator
  ( CombinatorTree (..),
    combinatorTrees,
    tagTypeName,
    resultADTName,
    nestedResultName,
    includeRecordValue,
    combinatorFieldName,
    edgeFieldName,
    childOf,
    isFullGraphTree,
    isSingleLeafTree,
    combinePairs,
    nestTokens,
    fullPresetTypeName,
  )
where

import Data.List (elemIndex, nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.EmitCommon
  ( includeFieldName,
    leafResultTypeName,
    leafTagTypeName,
    resultTypeName,
  )
import ORM.Codegen.IR
import ORM.Codegen.Lookup (lookupModel, lookupRelation)
import ORM.Codegen.Schema (isFullGraphInclude)
import ORM.Codegen.TextUtil (upperFirst)

data CombinatorTree = CombinatorTree
  { ctRoot :: Model,
    ctEdges :: [IncludeTree]
  }
  deriving (Show, Eq)

combinatorTrees :: Schema -> Model -> [CombinatorTree]
combinatorTrees schema root =
  [ CombinatorTree root edges
    | edges <- filter (not . null) (subtrees schema [] root)
  ]

subtrees :: Schema -> [Text] -> Model -> [[IncludeTree]]
subtrees schema path current =
  map concat (mapM (choices schema nextPath current) (modelRelations current))
  where
    nextPath = modelName current : path

choices :: Schema -> [Text] -> Model -> RelationSpec -> [[IncludeTree]]
choices schema path _current rel =
  [] : leaf : map nest childTrees
  where
    leaf = [IncludeTree {includeRelation = relName rel, includeChildren = []}]
    childName = relToModel rel
    childTrees
      | childName `elem` path = []
      | otherwise =
          filter (not . null) (subtrees schema path (lookupModel schema childName))
    nest kids =
      [IncludeTree {includeRelation = relName rel, includeChildren = kids}]

isFullGraphTree :: Schema -> CombinatorTree -> Bool
isFullGraphTree schema tree =
  isFullGraphInclude
    (schemaModels schema)
    ModelInclude
      { includeName = modelName (ctRoot tree) <> "Include",
        includeRootModel = modelName (ctRoot tree),
        includeTree = ctEdges tree
      }

isSingleLeafTree :: CombinatorTree -> Bool
isSingleLeafTree tree =
  case ctEdges tree of
    [edge] -> null (includeChildren edge)
    _ -> False

tagTypeName :: Schema -> CombinatorTree -> Text
tagTypeName schema tree
  | isFullGraphTree schema tree = fullPresetTypeName schema (ctRoot tree) (ctEdges tree)
  | isSingleLeafTree tree = leafTagTypeName (ctRoot tree) (head (ctEdges tree))
  | otherwise = T.concat (map (edgeTag schema (ctRoot tree)) (ctEdges tree))

fullPresetTypeName :: Schema -> Model -> [IncludeTree] -> Text
fullPresetTypeName _schema root edges =
  "With"
    <> T.concat
      [ upperFirst (includeFieldName root (lookupRelation root (includeRelation edge)))
        | edge <- edges
      ]

resultADTName :: Schema -> CombinatorTree -> Text
resultADTName schema tree
  | isFullGraphTree schema tree = resultTypeName schema (ctRoot tree) (ctEdges tree)
  | isSingleLeafTree tree = leafResultTypeName schema (ctRoot tree) (head (ctEdges tree))
  | otherwise =
      modelName (ctRoot tree)
        <> "With"
        <> T.concat (map (edgeResultLabel schema (ctRoot tree)) (ctEdges tree))

nestedResultName :: Schema -> Model -> [IncludeTree] -> Text
nestedResultName schema parent edges =
  resultADTName schema (CombinatorTree parent edges)

edgeTag :: Schema -> Model -> IncludeTree -> Text
edgeTag schema parent edge
  | null (includeChildren edge) = upperFirst (edgeFieldName parent edge)
  | otherwise =
      upperFirst (edgeFieldName parent edge)
        <> T.concat (map (edgeTag schema child) (includeChildren edge))
  where
    child = childOf schema parent edge

edgeResultLabel :: Schema -> Model -> IncludeTree -> Text
edgeResultLabel schema parent edge
  | null (includeChildren edge) = modelName (childOf schema parent edge)
  | otherwise =
      upperFirst (edgeFieldName parent edge)
        <> T.concat (map (edgeResultLabel schema child) (includeChildren edge))
  where
    child = childOf schema parent edge

edgeFieldName :: Model -> IncludeTree -> Text
edgeFieldName parent edge =
  includeFieldName parent (lookupRelation parent (includeRelation edge))

combinatorFieldName :: Model -> IncludeTree -> Text
combinatorFieldName = edgeFieldName

childOf :: Schema -> Model -> IncludeTree -> Model
childOf schema parent edge =
  lookupModel schema (relToModel (lookupRelation parent (includeRelation edge)))

-- Build the full-graph include record with flags set for this combinator tree.
includeRecordValue :: Schema -> CombinatorTree -> Text
includeRecordValue schema tree =
  includeRecordLiteral schema (ctRoot tree) fullEdges (ctEdges tree)
  where
    fullEdges = includeTree (fullInclude schema (ctRoot tree))

fullInclude :: Schema -> Model -> ModelInclude
fullInclude schema model =
  case [incl | incl <- schemaIncludes schema, includeRootModel incl == modelName model] of
    (incl : _) -> incl
    [] ->
      ModelInclude
        { includeName = modelName model <> "Include",
          includeRootModel = modelName model,
          includeTree = []
        }

includeRecordLiteral :: Schema -> Model -> [IncludeTree] -> [IncludeTree] -> Text
includeRecordLiteral schema parent fullEdges selected =
  includeNameFor parent
    <> " {"
    <> T.intercalate ", " (map (fieldAssign schema parent selected) fullEdges)
    <> "}"

includeNameFor :: Model -> Text
includeNameFor model = modelName model <> "Include"

fieldAssign :: Schema -> Model -> [IncludeTree] -> IncludeTree -> Text
fieldAssign schema parent selected fullEdge =
  fld <> " = " <> value
  where
    fld = edgeFieldName parent fullEdge
    child = childOf schema parent fullEdge
    match = findEdge (includeRelation fullEdge) selected
    value = case match of
      Nothing
        | null (includeChildren fullEdge) -> "False"
        | otherwise -> "Nothing"
      Just sel
        | null (includeChildren fullEdge) -> "True"
        | null (includeChildren sel) ->
            "Just ("
              <> includeRecordLiteral schema child (includeChildren fullEdge) []
              <> ")"
        | otherwise ->
            "Just ("
              <> includeRecordLiteral schema child (includeChildren fullEdge) (includeChildren sel)
              <> ")"

findEdge :: Text -> [IncludeTree] -> Maybe IncludeTree
findEdge name = foldr go Nothing
  where
    go edge acc
      | includeRelation edge == name = Just edge
      | otherwise = acc

-- Sibling pairs on the same root with disjoint top-level relations.
combinePairs :: Schema -> Model -> [(CombinatorTree, CombinatorTree, CombinatorTree)]
combinePairs schema root =
  [ (left, right, merged)
    | left <- trees,
      right <- trees,
      disjoint (ctEdges left) (ctEdges right),
      firstRelIndex root left < firstRelIndex root right,
      let merged = CombinatorTree root (mergeEdges root (ctEdges left) (ctEdges right))
  ]
  where
    trees = combinatorTrees schema root

firstRelIndex :: Model -> CombinatorTree -> Int
firstRelIndex root tree =
  case ctEdges tree of
    [] -> maxBound
    (edge : _) ->
      fromMaybe
        maxBound
        ( elemIndex (includeRelation edge) (map relName (modelRelations root))
        )

disjoint :: [IncludeTree] -> [IncludeTree] -> Bool
disjoint left right =
  null [() | l <- left, r <- right, includeRelation l == includeRelation r]

mergeEdges :: Model -> [IncludeTree] -> [IncludeTree] -> [IncludeTree]
mergeEdges root left right =
  [ edge
    | rel <- modelRelations root,
      edge <- left ++ right,
      includeRelation edge == relName rel
  ]

nestTokens :: Schema -> CombinatorTree -> [(Text, Text)]
nestTokens schema tree =
  nub (concatMap (tokensIn schema (ctRoot tree)) (ctEdges tree))

tokensIn :: Schema -> Model -> IncludeTree -> [(Text, Text)]
tokensIn schema parent edge =
  case includeChildren edge of
    [] -> []
    kids ->
      let child = childOf schema parent edge
          tokenType = tokenTypeName schema child kids
          tokenVal = combinatorFieldName child (head kids)
       in (tokenVal, tokenType) : concatMap (tokensIn schema child) kids

tokenTypeName :: Schema -> Model -> [IncludeTree] -> Text
tokenTypeName schema parent edges =
  T.concat (map (edgeTag schema parent) edges)
