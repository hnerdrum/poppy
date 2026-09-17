{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.IncludePath
  ( defaultMaxIncludeDepth,
    PathStep (..),
    JoinPath (..),
    joinPathModels,
    joinPathSteps,
    joinPathTake,
    includeMaxDepth,
    includeLongestJoinPath,
    includePresetJoinPaths,
    joinRowName,
    joinRowNameFromModels,
    columnListName,
    joinBuilderName,
    lookupModelAlias,
    assignIncludeModelAliases,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.IR
import Poppy.Codegen.JoinAlias (assignModelAliases)
import Poppy.Codegen.Lookup (lookupModel, lookupRelation)
import Poppy.Codegen.TextUtil (lowerFirst)

defaultMaxIncludeDepth :: Int
defaultMaxIncludeDepth = 5

data PathStep = PathStep
  { psFrom :: Model,
    psRel :: RelationSpec,
    psTo :: Model
  }
  deriving (Show)

newtype JoinPath = JoinPath
  { jpSteps :: [PathStep]
  }
  deriving (Show)

joinPathModels :: JoinPath -> [Model]
joinPathModels JoinPath {jpSteps = steps} =
  case steps of
    [] -> []
    (PathStep {psFrom = root} : _) ->
      root : map psTo steps

joinPathSteps :: JoinPath -> [PathStep]
joinPathSteps = jpSteps

joinPathTake :: Int -> JoinPath -> JoinPath
joinPathTake n JoinPath {jpSteps = steps} =
  JoinPath {jpSteps = take n steps}

includeMaxDepth :: Schema -> ModelInclude -> Int
includeMaxDepth schema incl =
  includeTreeDepth schema (lookupModel schema (includeRootModel incl)) (includeTree incl)

includeTreeDepth :: Schema -> Model -> [IncludeTree] -> Int
includeTreeDepth _ _ [] = 1
includeTreeDepth schema model edges =
  1
    + maximum
      [ includeTreeDepth schema child (includeChildren edge)
        | edge <- edges,
          let rel = lookupRelation model (includeRelation edge),
          let child = lookupModel schema (relToModel rel)
      ]

includeLongestJoinPath :: Schema -> ModelInclude -> JoinPath
includeLongestJoinPath schema incl =
  case includeTree incl of
    [] ->
      error $
        "includeLongestJoinPath: include "
          <> T.unpack (includeName incl)
          <> " has empty tree"
    edges ->
      let root = lookupModel schema (includeRootModel incl)
       in JoinPath {jpSteps = longestPathSteps schema root edges}

includePresetJoinPaths :: Schema -> ModelInclude -> [JoinPath]
includePresetJoinPaths schema incl =
  let full = includeLongestJoinPath schema incl
      maxSteps = length (jpSteps full)
   in [joinPathTake n full | n <- [1 .. maxSteps]]

longestPathSteps :: Schema -> Model -> [IncludeTree] -> [PathStep]
longestPathSteps schema fromModel edges =
  case edges of
    [] -> []
    (edge : _) ->
      let rel = lookupRelation fromModel (includeRelation edge)
          toModel = lookupModel schema (relToModel rel)
          step = PathStep {psFrom = fromModel, psRel = rel, psTo = toModel}
          deeper =
            case includeChildren edge of
              [] -> []
              childEdges -> longestPathSteps schema toModel childEdges
       in step : deeper

joinRowNameFromModels :: [Model] -> Text
joinRowNameFromModels models =
  T.concat (map modelName models) <> "JoinRow"

joinRowName :: JoinPath -> Text
joinRowName = joinRowNameFromModels . joinPathModels

columnListName :: JoinPath -> Text
columnListName path =
  case joinPathModels path of
    [] -> error "columnListName: empty join path"
    (root : rest) ->
      lowerFirst (modelName root) <> T.concat (map modelName rest) <> "Columns"

joinBuilderName :: JoinPath -> Text
joinBuilderName path =
  "build" <> T.concat (map modelName (joinPathModels path)) <> "Join"

assignIncludeModelAliases :: Schema -> ModelInclude -> [(Text, Text)]
assignIncludeModelAliases schema incl =
  map (first modelName) $
    assignModelAliases (joinPathModels (includeLongestJoinPath schema incl))

lookupModelAlias :: [(Text, Text)] -> Model -> Text
lookupModelAlias aliases model =
  case lookup (modelName model) aliases of
    Just alias -> alias
    Nothing ->
      error $
        "lookupModelAlias: no alias for model "
          <> T.unpack (modelName model)
