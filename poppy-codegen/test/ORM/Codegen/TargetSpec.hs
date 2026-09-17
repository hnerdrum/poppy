{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.TargetSpec
  ( targetSpec,
  )
where

import Data.List (nub, sort)
import qualified Data.Text as T
import ORM.Codegen.Run (allOutputs, schemasForTargets)
import ORM.Codegen.Spec.Shelf (shelfSchema)
import ORM.Codegen.Spec.Widget (widgetSchema)
import ORM.Codegen.Target
  ( CodegenTarget,
    GenOutput (..),
    targetOutputs,
  )
import ORM.Codegen.TestTarget (testTarget)
import Test.Hspec

allTargets :: [CodegenTarget]
allTargets = [testTarget]

targetSpec :: Spec
targetSpec =
  describe "ORM.Codegen.Target" $ do
    it "validates every schema referenced by a target" $ do
      schemasForTargets allTargets
        `shouldMatchList` [ widgetSchema,
                            shelfSchema
                          ]

    it "derives 18 outputs across the test target" $ do
      length (concatMap targetOutputs allTargets) `shouldBe` 18
      length (allOutputs allTargets) `shouldBe` 18

    it "derives expected output paths" $ do
      let paths = sort (nub (map outputPath (allOutputs allTargets)))
      paths
        `shouldBe` sort
          [ "test/ORM/Codegen/golden/Book.hs.golden",
            "test/ORM/Codegen/golden/BookInclude.hs.golden",
            "test/ORM/Codegen/golden/Chapter.hs.golden",
            "test/ORM/Codegen/golden/ChapterInclude.hs.golden",
            "test/ORM/Codegen/golden/Section.hs.golden",
            "test/ORM/Codegen/golden/Shelf.hs.golden",
            "test/ORM/Codegen/golden/ShelfInclude.hs.golden",
            "test/ORM/Codegen/golden/Tag.hs.golden",
            "test/ORM/Codegen/golden/Widget.hs.golden",
            "test/Schema/Book.hs",
            "test/Schema/BookInclude.hs",
            "test/Schema/Chapter.hs",
            "test/Schema/ChapterInclude.hs",
            "test/Schema/Section.hs",
            "test/Schema/Shelf.hs",
            "test/Schema/ShelfInclude.hs",
            "test/Schema/Tag.hs",
            "test/Schema/Widget.hs"
          ]

    it "keeps generated text byte-stable for golden outputs" $ do
      mapM_ assertGoldenStable goldenOutputPaths
  where
    goldenOutputPaths =
      map outputPath (filter isGoldenOutput (allOutputs allTargets))

    isGoldenOutput output =
      "test/ORM/Codegen/golden/" `T.isPrefixOf` T.pack (outputPath output)

    assertGoldenStable path = do
      expected <- readFile path
      let output =
            head
              [ out
                | out <- allOutputs allTargets,
                  outputPath out == path
              ]
          actual = T.unpack (outputText output)
      actual `shouldBe` expected
