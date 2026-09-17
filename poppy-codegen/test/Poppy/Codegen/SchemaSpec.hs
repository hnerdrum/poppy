{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.SchemaSpec
  ( schemaSpec,
  )
where

import Poppy.Codegen.Schema
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

schemaSpec :: Spec
schemaSpec =
  describe "Poppy.Codegen.Schema" $ do
    it "auto-emits {Model}Include when a model has relations" $ do
      let parent =
            model
              "Parent"
              [uuid "id" & pk]
              [hasMany "parentKids" "Kid" "parentId"]
          child =
            model
              "Kid"
              [uuid "id" & pk, uuid "parentId"]
              []
          built = schema [] [parent, child] []
          incl = head (schemaIncludes built)
      map includeName (schemaIncludes built) `shouldBe` ["ParentInclude"]
      includeRootModel incl `shouldBe` "Parent"
      map includeRelation (includeTree incl) `shouldBe` ["parentKids"]

    it "derives Shelf includes from relations only" $ do
      let names = map includeName (schemaIncludes shelfSchema)
          shelfIncl = head (schemaIncludes shelfSchema)
      names `shouldBe` ["ShelfInclude", "BookInclude", "ChapterInclude"]
      includeRootModel shelfIncl `shouldBe` "Shelf"
      map includeRelation (includeTree shelfIncl) `shouldBe` ["shelfBooks", "shelfTags"]
