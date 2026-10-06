{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.EmitIncludeSpec
  ( emitIncludeSpec,
  )
where

import qualified Data.Text as T
import Poppy.Codegen.Emit.Include (emitIncludeModule)
import Poppy.Codegen.IR (modelName, schemaModels)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitIncludeSpec :: Spec
emitIncludeSpec =
  describe "Poppy.Codegen.Emit.Include" $ do
    it "emits one Shelf include module with uniform edges" $ do
      let shelf = head [m | m <- schemaModels shelfSchema, modelName m == "Shelf"]
          actual = emitIncludeModule "Schema.Include.Shelf" shelfSchema shelf
      actual `shouldSatisfy` T.isInfixOf "module Schema.Include.Shelf"
      actual `shouldSatisfy` T.isInfixOf "data ShelfInclude books tags"
      actual `shouldSatisfy` T.isInfixOf "ShelfBooks Skip = Skipped \"books\""
      actual `shouldSatisfy` T.isInfixOf "loadShelf"
      actual `shouldSatisfy` T.isInfixOf "IncludeFor \"Shelf\""
