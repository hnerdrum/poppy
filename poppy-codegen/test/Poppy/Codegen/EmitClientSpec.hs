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
import Poppy.Codegen.Spec.Example (exampleSchema, taskModel)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitClientSpec :: Spec
emitClientSpec =
  describe "Poppy.Codegen.Emit.Client" $ do
    it "emits Client for the canonical Example Task model" $ do
      let actual = emitSimpleClientModule "Poppy.Client.Task" exampleSchema taskModel
      actual `shouldSatisfy` T.isInfixOf "data TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "findMany :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "findFirst :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "count :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "createMany ::"
      actual `shouldSatisfy` T.isInfixOf "updateMany ::"
      actual `shouldSatisfy` T.isInfixOf "upsert ::"
      actual `shouldSatisfy` T.isInfixOf "emptyQuery"
      actual `shouldSatisfy` T.isInfixOf "data TaskUniqueQuery"
      actual `shouldSatisfy` T.isInfixOf "uniqueQuery ::"

    it "emits Shelf Client with include_ and writes" $ do
      expected <- TIO.readFile "test/Poppy/Codegen/golden/ShelfReadClient.hs.golden"
      let shelf = head [m | m <- schemaModels shelfSchema, modelName m == "Shelf"]
          actual = emitClientModule "Schema.Client.Shelf" shelfSchema shelf
      T.strip actual `shouldBe` T.strip expected
      actual `shouldSatisfy` T.isInfixOf "create ::"
      actual `shouldSatisfy` T.isInfixOf "createMany ::"
      actual `shouldSatisfy` T.isInfixOf "updateMany ::"
      actual `shouldSatisfy` T.isInfixOf "upsert ::"
      actual `shouldSatisfy` T.isInfixOf "data BookNestedCreate"
      actual `shouldSatisfy` T.isInfixOf "data BooksUpdate"
      actual `shouldSatisfy` T.isInfixOf "replaceWith ::"
      actual `shouldSatisfy` T.isInfixOf "ShelfCreateScalars"
      actual `shouldSatisfy` T.isInfixOf "include_ :: include"
      actual `shouldSatisfy` T.isInfixOf "include_ :: include"
