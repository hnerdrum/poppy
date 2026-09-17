{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.IncludePathSpec
  ( includePathSpec,
    fourLevelPathSpec,
  )
where

import Poppy.Codegen.IR
import Poppy.Codegen.IncludePath
  ( columnListName,
    includeLongestJoinPath,
    includeMaxDepth,
    includePresetJoinPaths,
    joinBuilderName,
    joinPathModels,
    joinPathTake,
    joinRowName,
  )
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.ShelfDeep (shelfDeepSchema)
import Test.Hspec

includePathSpec :: Spec
includePathSpec =
  describe "Poppy.Codegen.IncludePath" $ do
    it "computes longest Shelf path as Shelf → Book → Chapter → Section" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = includeLongestJoinPath shelfSchema incl
      map modelName (joinPathModels path) `shouldBe` ["Shelf", "Book", "Chapter", "Section"]

    it "computes preset paths for shallow and deep joins" $ do
      let incl = head (schemaIncludes shelfSchema)
          paths = includePresetJoinPaths shelfSchema incl
      length paths `shouldBe` 3
      joinRowName (head paths) `shouldBe` "ShelfBookJoinRow"
      joinRowName (last paths) `shouldBe` "ShelfBookChapterSectionJoinRow"

    it "names join builders from model path" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = includeLongestJoinPath shelfSchema incl
      joinBuilderName (joinPathTake 1 path) `shouldBe` "buildShelfBookJoin"
      joinBuilderName path `shouldBe` "buildShelfBookChapterSectionJoin"

    it "names column lists from model path" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = includeLongestJoinPath shelfSchema incl
      columnListName (joinPathTake 1 path) `shouldBe` "shelfBookColumns"
      columnListName path `shouldBe` "shelfBookChapterSectionColumns"

    it "reports include depth within default cap" $ do
      let incl = head (schemaIncludes shelfSchema)
      includeMaxDepth shelfSchema incl `shouldBe` 4

    it "includeMaxDepth uses the deepest sibling branch" $ do
      includeMaxDepth shelfSchema (head (schemaIncludes shelfSchema)) `shouldBe` 4
      includeMaxDepth siblingDepthSchema siblingDepthInclude `shouldBe` 3

fourLevelPathSpec :: Spec
fourLevelPathSpec =
  describe "Poppy.Codegen.IncludePath four-level fixture" $ do
    it "supports a 4-model join path" $ do
      let path = includeLongestJoinPath shelfDeepSchema shelfDeepInclude
      map modelName (joinPathModels path) `shouldBe` ["Shelf", "Book", "Chapter", "Section"]
      joinRowName path `shouldBe` "ShelfBookChapterSectionJoinRow"

shelfDeepInclude :: ModelInclude
shelfDeepInclude = head (schemaIncludes shelfDeepSchema)

-- First edge is a leaf; the deeper path is a sibling. First-child-only
-- depth would report 2.
siblingDepthSchema :: Schema
siblingDepthSchema =
  Schema
    { schemaEnums = [],
      schemaModels = [parentModel, leafModel, midModel, deepModel],
      schemaIncludes = [siblingDepthInclude],
      schemaUniques = []
    }

parentModel :: Model
parentModel =
  Model
    { modelName = "Parent",
      modelTable = "parent",
      modelFields = [pk (uuid "id")],
      modelRelations =
        [ hasMany "parentLeaves" "Parent" "Leaf" "id" "parentId",
          hasMany "parentMids" "Parent" "Mid" "id" "parentId"
        ]
    }

leafModel :: Model
leafModel =
  Model
    { modelName = "Leaf",
      modelTable = "leaf",
      modelFields = [pk (uuid "id"), column "parent_id" (uuid "parentId")],
      modelRelations = []
    }

midModel :: Model
midModel =
  Model
    { modelName = "Mid",
      modelTable = "mid",
      modelFields = [pk (uuid "id"), column "parent_id" (uuid "parentId")],
      modelRelations = [hasMany "midDeeps" "Mid" "Deep" "id" "midId"]
    }

deepModel :: Model
deepModel =
  Model
    { modelName = "Deep",
      modelTable = "deep",
      modelFields = [pk (uuid "id"), column "mid_id" (uuid "midId")],
      modelRelations = []
    }

siblingDepthInclude :: ModelInclude
siblingDepthInclude =
  ModelInclude
    { includeName = "ParentInclude",
      includeRootModel = "Parent",
      includeTree =
        [ IncludeTree {includeRelation = "parentLeaves", includeChildren = []},
          IncludeTree
            { includeRelation = "parentMids",
              includeChildren = [IncludeTree {includeRelation = "midDeeps", includeChildren = []}]
            }
        ]
    }
