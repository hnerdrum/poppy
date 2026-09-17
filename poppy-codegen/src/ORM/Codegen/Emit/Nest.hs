{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Emit.Nest
  ( emitNestHelpersForPath,
    nestRowsFnName,
    includePatternForPath,
    nestedIncludePatternForPath,
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ORM.Codegen.EmitCommon (fieldBinder, includeFieldName, primaryKeyField, resultTypeName)
import ORM.Codegen.IR
import ORM.Codegen.IncludePath
  ( JoinPath (..),
    PathStep (..),
    includeMaxDepth,
    joinPathModels,
    joinRowName,
  )
import ORM.Codegen.Lookup (lookupModel, lookupRelation)
import ORM.Codegen.TextUtil (lowerFirst, upperFirst)

emitNestHelpersForPath :: Schema -> ModelInclude -> JoinPath -> Text
emitNestHelpersForPath schema incl path =
  T.intercalate
    "\n"
    $ filter
      (not . T.null)
      [ emitTopNestFn schema incl path,
        emitRootFromGroupFn schema incl path,
        emitInnerFromGroupFns schema incl path,
        emitEmptyWrapWhenPathShorterThanTree schema incl path,
        emitToRowFns schema incl path
      ]

nestRowsFnName :: JoinPath -> Text
nestRowsFnName path =
  case joinPathModels path of
    (root : _)
      | length (joinPathModels path) <= 2 ->
          "nest" <> modelName root <> "Rows"
    models ->
      "nest" <> T.concat (map modelName models) <> "Rows"

emitTopNestFn :: Schema -> ModelInclude -> JoinPath -> Text
emitTopNestFn schema incl path =
  let root = head (joinPathModels path)
      flatRow = joinRowName path
      resultName = resultTypeName schema root (includeTree incl)
      nestFn = nestRowsFnName path
      groupFn = rootFromGroupName schema incl path
      rootPkBinder = fieldBinder root (primaryKeyField root)
   in T.unlines
        [ nestFn <> " :: [" <> flatRow <> "] -> [" <> resultName <> "]",
          nestFn <> " =",
          "  map " <> groupFn <> " . groupByKey (." <> rootPkBinder <> ")"
        ]

rootFromGroupName :: Schema -> ModelInclude -> JoinPath -> Text
rootFromGroupName schema incl path =
  let models = joinPathModels path
   in case models of
        [_] -> error "rootFromGroupName: path must include at least one join step"
        [_, _] -> fromGroupName schema (head models) (includeTree incl)
        _ ->
          let root = head models
              nextModel = models !! 1
              nestedEdges = includeChildren (head (includeTree incl))
              suffix =
                if length models > 3
                  then
                    T.concat
                      [ upperFirst (includeFieldName nextModel (lookupRelation nextModel (includeRelation e)))
                        | e <- nestedEdges
                      ]
                      <> disambiguationSuffixFrom schema incl path 2
                  else
                    T.concat
                      [ upperFirst (includeFieldName nextModel (lookupRelation nextModel (includeRelation e)))
                        | e <- nestedEdges
                      ]
           in lowerFirst (modelName root)
                <> "With"
                <> suffix
                <> "From"
                <> modelName root
                <> "Group"

disambiguationSuffixFrom :: Schema -> ModelInclude -> JoinPath -> Int -> Text
disambiguationSuffixFrom schema incl path depth =
  let models = joinPathModels path
   in if length models <= depth + 1
        then ""
        else
          let model = models !! depth
              edge = edgeAtDepthTree incl depth
              rel = lookupRelation model (includeRelation edge)
           in upperFirst (includeFieldName model rel)
                <> disambiguationSuffixFrom schema incl path (depth + 1)

emitRootFromGroupFn :: Schema -> ModelInclude -> JoinPath -> Text
emitRootFromGroupFn schema incl path =
  let root = head (joinPathModels path)
      flatRow = joinRowName path
      resultName = resultTypeName schema root (includeTree incl)
      groupFn = rootFromGroupName schema incl path
      rootVar = lowerFirst (modelName root)
      toRootFn = toRowFnForModel path 0 root
      edge = head (includeTree incl)
      rel = lookupRelation root (includeRelation edge)
      nestedField = includeFieldName root rel
      nestedExpr = nestedFieldExprAt schema incl path 0 root edge
   in T.unlines
        [ groupFn <> " :: [" <> flatRow <> "] -> " <> resultName,
          groupFn <> " rows@(firstRow : _) =",
          "  " <> resultName,
          "    { " <> rootVar <> " = " <> toRootFn <> " firstRow,",
          "      " <> nestedField <> " = " <> nestedExpr,
          "    }",
          groupFn <> " [] =",
          "  error \"" <> groupFn <> ": empty group\""
        ]

emitInnerFromGroupFns :: Schema -> ModelInclude -> JoinPath -> Text
emitInnerFromGroupFns schema incl path =
  let models = joinPathModels path
      steps = jpSteps path
   in T.intercalate "\n" $
        [ emitInnerFromGroupFn
            schema
            incl
            path
            levelIdx
            (models !! levelIdx)
            (includeChildren (edgeAtDepthTree incl levelIdx))
            (psRel (steps !! (levelIdx - 1)))
          | levelIdx <- [1 .. length models - 2]
        ]

edgeAtDepthTree :: ModelInclude -> Int -> IncludeTree
edgeAtDepthTree incl 0 =
  case includeTree incl of
    (edge : _) -> edge
    _ -> error "edgeAtDepthTree: empty include tree"
edgeAtDepthTree incl depth =
  let prev = edgeAtDepthTree incl (depth - 1)
   in case includeChildren prev of
        (edge : _) -> edge
        _ ->
          error $
            "edgeAtDepthTree: missing nested edge at depth "
              <> show depth
              <> " in "
              <> T.unpack (includeName incl)

emitInnerFromGroupFn ::
  Schema ->
  ModelInclude ->
  JoinPath ->
  Int ->
  Model ->
  [IncludeTree] ->
  RelationSpec ->
  Text
emitInnerFromGroupFn schema incl path levelIdx model _nestedEdges joinRel =
  let flatRow = joinRowName path
      resultEdge = edgeAtDepthTree incl levelIdx
      resultType = resultTypeName schema model [resultEdge]
      groupFn = fromGroupNameForPath path schema model [resultEdge]
      modelVar = lowerFirst (modelName model)
      groupKeyBinder = fieldBinder model (groupKeyFieldForJoinSide model joinRel)
      toRowFnName = toRowFnForModel path levelIdx model
      nextRel = psRel (jpSteps path !! levelIdx)
      nextField = includeFieldName model nextRel
      nextNestedExpr = nestedFieldExprBelow schema incl path levelIdx nextRel
      groupPattern =
        if nestedExprUsesRows nextNestedExpr
          then groupFn <> " rows@(firstRow : _)"
          else groupFn <> " (firstRow : _)"
   in T.unlines
        [ groupFn <> " :: [" <> flatRow <> "] -> Maybe " <> resultType,
          groupPattern,
          "  | Just _ <- firstRow." <> groupKeyBinder <> " =",
          "      Just",
          "        " <> resultType,
          "          { " <> modelVar <> " = " <> toRowFnName <> " firstRow,",
          "            " <> nextField <> " = " <> nextNestedExpr,
          "          }",
          "  | otherwise = Nothing",
          groupFn <> " [] = Nothing"
        ]

nestedExprUsesRows :: Text -> Bool
nestedExprUsesRows expr = " rows" `T.isInfixOf` expr

nestedFieldExprAt :: Schema -> ModelInclude -> JoinPath -> Int -> Model -> IncludeTree -> Text
nestedFieldExprAt schema incl path levelIdx parent edge =
  let rel = lookupRelation parent (includeRelation edge)
      nextModel = lookupModel schema (relToModel rel)
      deeperEdges = includeChildren edge
      pathDepth = length (joinPathModels path)
      toNextRowFn = toRowFnForModel path (levelIdx + 1) nextModel
   in if reachesPathLeaf levelIdx pathDepth
        then
          if null deeperEdges
            then "mapMaybe " <> toNextRowFn <> " rows"
            else
              "map "
                <> emptyWrapFn nextModel deeperEdges
                <> " $ mapMaybe "
                <> toNextRowFn
                <> " rows"
        else
          let resultEdge = edgeAtDepthTree incl (levelIdx + 1)
              groupFn = fromGroupNameForPath path schema nextModel [resultEdge]
              groupKeyBinder = fieldBinder nextModel (groupKeyFieldForJoinSide nextModel rel)
           in "mapMaybe "
                <> groupFn
                <> " $ groupByKey (."
                <> groupKeyBinder
                <> ") rows"

nestedFieldExprBelow :: Schema -> ModelInclude -> JoinPath -> Int -> RelationSpec -> Text
nestedFieldExprBelow schema incl path levelIdx nextRel =
  let nextModel = lookupModel schema (relToModel nextRel)
      pathDepth = length (joinPathModels path)
      toNextRowFn = toRowFnForModel path (levelIdx + 1) nextModel
   in if reachesPathLeaf levelIdx pathDepth
        then case relKind nextRel of
          RelBelongsTo -> toNextRowFn <> " firstRow"
          RelHasMany ->
            let stepEdge = edgeAtDepthTree incl levelIdx
                deeperEdges = includeChildren stepEdge
             in if null deeperEdges
                  then "mapMaybe " <> toNextRowFn <> " rows"
                  else
                    "map "
                      <> emptyWrapFn nextModel deeperEdges
                      <> " $ mapMaybe "
                      <> toNextRowFn
                      <> " rows"
        else
          let edge = edgeAtDepthTree incl (levelIdx + 1)
              groupFn = fromGroupNameForPath path schema nextModel [edge]
              groupKeyBinder = fieldBinder nextModel (groupKeyFieldForJoinSide nextModel nextRel)
           in "mapMaybe "
                <> groupFn
                <> " $ groupByKey (."
                <> groupKeyBinder
                <> ") rows"

reachesPathLeaf :: Int -> Int -> Bool
reachesPathLeaf levelIdx pathDepth = levelIdx + 2 >= pathDepth

emitEmptyWrapWhenPathShorterThanTree :: Schema -> ModelInclude -> JoinPath -> Text
emitEmptyWrapWhenPathShorterThanTree schema incl path =
  let models = joinPathModels path
      pathDepth = length models
      treeDepth = includeMaxDepth schema incl
   in if treeDepth <= pathDepth || pathDepth < 2
        then ""
        else
          let leafModel = last models
              edge = edgeAtDepthTree incl (pathDepth - 1)
              wrapFn = emptyWrapFn leafModel [edge]
              wrapResult = resultTypeName schema leafModel [edge]
              modelVar = lowerFirst (modelName leafModel)
              emptyFields = emptyNestedField schema leafModel edge
           in T.unlines
                [ wrapFn <> " :: " <> modelName leafModel <> "Row -> " <> wrapResult,
                  wrapFn <> " " <> modelVar <> " = " <> wrapResult <> " {" <> modelVar <> ", " <> emptyFields <> "}"
                ]

emitToRowFns :: Schema -> ModelInclude -> JoinPath -> Text
emitToRowFns schema incl path =
  T.intercalate "\n" $
    zipWith (emitToRowFn schema incl path) [0 ..] (joinPathModels path)

emitToRowFn :: Schema -> ModelInclude -> JoinPath -> Int -> Model -> Text
emitToRowFn schema incl path idx model =
  let models = joinPathModels path
      flatRow = joinRowName path
      isLeaf = idx == length models - 1
      toFn = toRowFnForModel path idx model
   in if isLeaf
        then emitLeafToRow schema incl path model flatRow toFn
        else
          if idx == 0
            then emitRootToRow schema path model flatRow toFn
            else emitInnerToRow schema path idx model flatRow toFn

emitRootToRow :: Schema -> JoinPath -> Model -> Text -> Text -> Text
emitRootToRow _schema _path model flatRow toFn =
  let rootRow = modelName model <> "Row"
      assigns =
        [ fieldName f <> " = row." <> fieldBinder model f
          | f <- modelFields model
        ]
   in T.unlines
        [ toFn <> " :: " <> flatRow <> " -> " <> rootRow,
          toFn <> " row = " <> rootRow <> " {" <> T.intercalate ", " assigns <> "}"
        ]

emitInnerToRow :: Schema -> JoinPath -> Int -> Model -> Text -> Text -> Text
emitInnerToRow _ path idx model flatRow toFn =
  let models = joinPathModels path
      rowType = modelName model <> "Row"
      step = jpSteps path !! (idx - 1)
      rel = psRel step
      joinedFields = fieldsForModel path model idx
      fkField =
        fromMaybe (error "emitInnerToRow: missing FK on joined model") $
          find ((== relForeignField rel) . fieldName) (modelFields model)
      parent = models !! (idx - 1)
      localRootField =
        fromMaybe (error "emitInnerToRow: missing local field on parent") $
          find ((== relLocalField rel) . fieldName) (modelFields parent)
      assigns =
        [ fieldName f
            <> " = fromMaybe (error \""
            <> toFn
            <> ": missing "
            <> fieldName f
            <> "\") row."
            <> fieldBinder model f
          | f <- joinedFields
        ]
          ++ [ fieldName fkField
                 <> " = "
                 <> if idx > 1
                   then
                     "fromMaybe (error \""
                       <> toFn
                       <> ": missing "
                       <> fieldName fkField
                       <> "\") row."
                       <> fieldBinder parent localRootField
                   else "row." <> fieldBinder parent localRootField
             ]
   in T.unlines
        [ toFn <> " :: " <> flatRow <> " -> " <> rowType,
          toFn <> " row =",
          "  " <> rowType,
          "    { " <> T.intercalate ",\n      " assigns,
          "    }"
        ]

emitLeafToRow :: Schema -> ModelInclude -> JoinPath -> Model -> Text -> Text -> Text
emitLeafToRow _ _incl path model flatRow toFn =
  let rowType = modelName model <> "Row"
      idx = length (joinPathModels path) - 1
      fields = fieldsForModel path model idx
      parentIdx = idx - 1
      parent = joinPathModels path !! parentIdx
      step = jpSteps path !! parentIdx
      rel = psRel step
      (justBinds, assigns) =
        if relKind rel == RelBelongsTo
          then
            ( [ "Just " <> patVar model f <> " <- row." <> fieldBinder model f
                | f <- fields
              ],
              [ fieldName f <> " = " <> patVar model f
                | f <- fields
              ]
            )
          else
            if idx == 1
              then
                let fkField =
                      fromMaybe (error "emitLeafToRow: missing FK on leaf") $
                        find ((== relForeignField rel) . fieldName) (modelFields model)
                    localRootField =
                      fromMaybe (error "emitLeafToRow: missing local field on parent") $
                        find ((== relLocalField rel) . fieldName) (modelFields parent)
                 in ( [ "Just " <> patVar model f <> " <- row." <> fieldBinder model f
                        | f <- fields
                      ],
                      [ fieldName f <> " = " <> patVar model f
                        | f <- fields
                      ]
                        ++ [fieldName fkField <> " = row." <> fieldBinder parent localRootField]
                    )
              else
                let leafFkField =
                      fromMaybe (error "emitLeafToRow: missing FK on leaf") $
                        find ((== relForeignField rel) . fieldName) (modelFields model)
                    parentPk = primaryKeyField parent
                 in ( [ "Just " <> patVar model f <> " <- row." <> fieldBinder model f
                        | f <- fields
                      ]
                        ++ [ "Just "
                               <> patVar parent parentPk
                               <> " <- row."
                               <> fieldBinder parent parentPk
                           ],
                      [ fieldName f <> " = " <> patVar model f
                        | f <- fields
                      ]
                        ++ [fieldName leafFkField <> " = " <> patVar parent parentPk]
                    )
   in T.unlines
        [ toFn <> " :: " <> flatRow <> " -> Maybe " <> rowType,
          toFn <> " row",
          "  | " <> T.intercalate ",\n    " justBinds <> " =",
          "      Just " <> rowType <> " {" <> T.intercalate ", " assigns <> "}",
          "  | otherwise = Nothing"
        ]

toRowFnForModel :: JoinPath -> Int -> Model -> Text
toRowFnForModel path idx model =
  let models = joinPathModels path
      isLeaf = idx == length models - 1
   in if isLeaf
        then toRowFn model
        else
          if length models > 2
            then toRowFn model <> "From" <> modelName (last models)
            else toRowFn model

fieldsForModel :: JoinPath -> Model -> Int -> [FieldSpec]
fieldsForModel path model idx =
  case idx of
    0 -> modelFields model
    _ ->
      let step = jpSteps path !! (idx - 1)
       in filter (not . isRelationFk (psRel step)) (modelFields model)

isRelationFk :: RelationSpec -> FieldSpec -> Bool
isRelationFk rel f = fieldName f == relForeignField rel

includePatternForPath :: Schema -> ModelInclude -> JoinPath -> Text
includePatternForPath schema incl path =
  let root = lookupModel schema (includeRootModel incl)
      targetDepth = length (jpSteps path)
   in includeName incl <> " {" <> emitPatternFields schema root (includeTree incl) 0 targetDepth <> "}"

nestedIncludePatternForPath :: Schema -> ModelInclude -> JoinPath -> Text
nestedIncludePatternForPath schema incl path =
  case includeTree incl of
    (edge : _) ->
      let root = lookupModel schema (includeRootModel incl)
          rel = lookupRelation root (includeRelation edge)
          nextModel = lookupModel schema (relToModel rel)
          targetDepth = length (jpSteps path)
       in if null (includeChildren edge)
            then
              if targetDepth > 0
                then "True"
                else "False"
            else
              let nested =
                    emitPatternFields schema nextModel (includeChildren edge) 1 targetDepth
               in "Just "
                    <> modelName nextModel
                    <> "Include {"
                    <> nested
                    <> "}"
    _ -> error "nestedIncludePatternForPath: empty include tree"

emitPatternFields :: Schema -> Model -> [IncludeTree] -> Int -> Int -> Text
emitPatternFields schema parent edges currentDepth targetDepth =
  T.intercalate ", " $
    map (emitPatternField schema parent currentDepth targetDepth) edges

emitPatternField :: Schema -> Model -> Int -> Int -> IncludeTree -> Text
emitPatternField schema parent currentDepth targetDepth edge =
  let rel = lookupRelation parent (includeRelation edge)
      name = includeFieldName parent rel
      deeperEdges = includeChildren edge
   in if null deeperEdges
        then
          name
            <> " = "
            <> if targetDepth > currentDepth
              then "True"
              else "False"
        else
          let nestedModel = lookupModel schema (relToModel rel)
              nestedIncludeName = modelName nestedModel <> "Include"
              nestedPattern =
                if targetDepth > currentDepth + 1
                  then nestedIncludeName <> " {" <> emitPatternFields schema nestedModel deeperEdges (currentDepth + 1) targetDepth <> "}"
                  else nestedIncludeName <> " {" <> emitPatternFields schema nestedModel deeperEdges targetDepth targetDepth <> "}"
           in name
                <> " = Just "
                <> nestedPattern

fromGroupName :: Schema -> Model -> [IncludeTree] -> Text
fromGroupName schema root edges =
  lowerFirst (resultTypeName schema root edges) <> "FromGroup"

fromGroupNameForPath :: JoinPath -> Schema -> Model -> [IncludeTree] -> Text
fromGroupNameForPath path schema model edges =
  let base = fromGroupName schema model edges
      pathModels = joinPathModels path
      leaf = last pathModels
   in if length pathModels > 3 && modelName model /= modelName leaf
        then
          lowerFirst (resultTypeName schema model edges)
            <> "From"
            <> modelName leaf
            <> "Group"
        else base

emptyWrapFn :: Model -> [IncludeTree] -> Text
emptyWrapFn model edges =
  lowerFirst (modelName model)
    <> "WithNo"
    <> T.concat (map (upperFirst . includeFieldNameFromEdge model) edges)
  where
    includeFieldNameFromEdge parent edge =
      case T.stripPrefix (lowerFirst (modelName parent)) (includeRelation edge) of
        Just rest | not (T.null rest) -> lowerFirst rest
        _ -> lowerFirst (includeRelation edge)

emptyNestedField :: Schema -> Model -> IncludeTree -> Text
emptyNestedField _schema parent edge =
  name <> " = " <> emptyVal
  where
    rel = lookupRelation parent (includeRelation edge)
    name = includeFieldName parent rel
    emptyVal = case relKind rel of
      RelHasMany -> "[]"
      RelBelongsTo -> "Nothing"

groupKeyFieldForJoinSide :: Model -> RelationSpec -> FieldSpec
groupKeyFieldForJoinSide model rel =
  let pkField = primaryKeyField model
   in if fieldName pkField == relForeignField rel
        then
          fromMaybe pkField $
            find ((/= fieldName pkField) . fieldName) (modelFields model)
        else pkField

patVar :: Model -> FieldSpec -> Text
patVar model f
  | fieldName f == "id" =
      T.toLower (T.take 1 (modelName model)) <> "id"
  | otherwise = fieldName f

toRowFn :: Model -> Text
toRowFn model = "to" <> modelName model <> "Row"
