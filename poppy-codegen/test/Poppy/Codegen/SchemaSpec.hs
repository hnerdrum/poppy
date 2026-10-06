{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.SchemaSpec
  ( schemaSpec,
  )
where

import Poppy.Codegen.Schema
import Poppy.Codegen.Spec.Editor (editorSchema)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

schemaSpec :: Spec
schemaSpec =
  describe "Poppy.Codegen.Schema" $ do
    it "keeps relation names as declared" $ do
      let parent =
            model
              "Parent"
              [uuid "id" & pk]
              [hasMany "kids" "Kid" "parentId"]
          child =
            model
              "Kid"
              [uuid "id" & pk, uuid "parentId"]
              []
          built = schema [] [parent, child] []
          parentModel = head (schemaModels built)
      map relName (modelRelations parentModel) `shouldBe` ["kids"]

    it "derives Shelf relations from the model" $ do
      let shelf = head [m | m <- schemaModels shelfSchema, modelName m == "Shelf"]
      map relName (modelRelations shelf) `shouldBe` ["books", "tags"]

    it "keeps two relations to the same model as distinct edges" $ do
      let editor = head [m | m <- schemaModels editorSchema, modelName m == "Editor"]
      map relName (modelRelations editor) `shouldBe` ["writtenPosts", "editedPosts"]
