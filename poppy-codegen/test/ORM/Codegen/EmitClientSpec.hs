{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.EmitClientSpec
  ( emitClientSpec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ORM.Codegen.Emit.Client
  ( emitIncludeReadClientModule,
    emitSimpleClientModule,
  )
import ORM.Codegen.IR
  ( Schema (..),
  )
import ORM.Codegen.Spec.Example (taskModel)
import ORM.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitClientSpec :: Spec
emitClientSpec =
  describe "ORM.Codegen.Emit.Client" $ do
    it "emits Client for the canonical Example Task model" $ do
      let actual = emitSimpleClientModule "ORM.Client.Task" taskModel
      actual `shouldSatisfy` T.isInfixOf "data TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "findMany :: TaskQuery"
      actual `shouldSatisfy` T.isInfixOf "emptyQuery"

    it "emits Shelf read Client with include helpers" $ do
      expected <- TIO.readFile "test/ORM/Codegen/golden/ShelfReadClient.hs.golden"
      let incl = head (schemaIncludes shelfSchema)
          actual = emitIncludeReadClientModule "Schema.Client.Shelf" shelfSchema incl
      T.strip actual `shouldBe` T.strip expected
