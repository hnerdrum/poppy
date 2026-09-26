{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.EmitClientSpec
  ( emitClientSpec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Poppy.Codegen.Emit.Client
  ( emitClientModule,
    emitSimpleClientModule,
  )
import Poppy.Codegen.IR
  ( Schema (..),
    modelName,
  )
import Poppy.Codegen.Spec.Example (taskModel)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitClientSpec :: Spec
emitClientSpec =
  describe "Poppy.Codegen.Emit.Client" $ do
    it "emits Client for the canonical Example Task model" $ do
      let actual = emitSimpleClientModule "Poppy.Client.Task" taskModel
      actual `shouldSatisfy` T.isInfixOf "data TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "findMany :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "findFirst :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "count :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "emptyQuery"

    it "emits Shelf Client with include_ and writes" $ do
      expected <- TIO.readFile "test/Poppy/Codegen/golden/ShelfReadClient.hs.golden"
      let shelf = head [m | m <- schemaModels shelfSchema, modelName m == "Shelf"]
          actual = emitClientModule "Schema.Client.Shelf" shelfSchema shelf
      T.strip actual `shouldBe` T.strip expected
      actual `shouldSatisfy` T.isInfixOf "create ::"
      actual `shouldSatisfy` T.isInfixOf "createNested ::"
      actual `shouldSatisfy` T.isInfixOf "updateNested ::"
      actual `shouldSatisfy` T.isInfixOf "data BooksWrite"
      actual `shouldSatisfy` T.isInfixOf "Set xs"
      actual `shouldSatisfy` T.isInfixOf "include_ :: include"
