{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.EmitSpec
  ( emitSpec,
  )
where

import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Poppy.Codegen.Emit.Schema (emitModelModule)
import Poppy.Codegen.IR
  ( Model (..),
    modelName,
    schemaModels,
  )
import Poppy.Codegen.Spec.Example (exampleSchema, taskModel)
import Poppy.Codegen.Spec.Flag (flagModel, flagSchema)
import Poppy.Codegen.Spec.Packet (packetModel, packetSchema)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.Widget (widgetModel, widgetSchema)
import Test.Hspec

emitSpec :: Spec
emitSpec = do
  describe "Poppy.Codegen.Emit.Schema" $ do
    it "emits the canonical Example Task module" $ do
      let actual = emitModelModule "Schema.Task" exampleSchema taskModel
      actual `shouldSatisfy` T.isInfixOf "data TaskRow"
      actual `shouldSatisfy` T.isInfixOf "done :: Bool"
      actual `shouldSatisfy` T.isInfixOf "tableName = \"task\""

    it "emits Widget matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Widget.hs.golden"
      let actual = emitModelModule "Schema.Widget" widgetSchema widgetModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Flag matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Flag.hs.golden"
      let actual = emitModelModule "Schema.Flag" flagSchema flagModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Packet matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Packet.hs.golden"
      let actual = emitModelModule "Schema.Packet" packetSchema packetModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Shelf matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Shelf.hs.golden"
      let actual = emitModelModule "Schema.Shelf" shelfSchema shelfModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Book matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Book.hs.golden"
      let actual = emitModelModule "Schema.Book" shelfSchema bookModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Chapter matching the golden file" $ do
      expected <- T.readFile "test/Poppy/Codegen/golden/Chapter.hs.golden"
      let actual = emitModelModule "Schema.Chapter" shelfSchema chapterModel
      T.strip actual `shouldBe` T.strip expected

shelfModel :: Model
shelfModel =
  case find ((== "Shelf") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "shelfModel: Shelf missing"

bookModel :: Model
bookModel =
  case find ((== "Book") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "bookModel: Book missing"

chapterModel :: Model
chapterModel =
  case find ((== "Chapter") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "chapterModel: Chapter missing"
