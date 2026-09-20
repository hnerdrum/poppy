{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.TargetSpec
  ( targetSpec,
  )
where

import Data.List (nub, sort)
import qualified Data.Text as T
import Poppy.Codegen.Run (allOutputs, schemasForTargets)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.Widget (widgetSchema)
import Poppy.Codegen.Target
  ( CodegenTarget,
    GenOutput (..),
    simpleTarget,
    targetOutputs,
  )
import Poppy.Codegen.TestTarget (testTargets)
import Test.Hspec

allTargets :: [CodegenTarget]
allTargets = testTargets

schemaGoldenNames :: [FilePath]
schemaGoldenNames =
  [ "Book",
    "BookInclude",
    "Chapter",
    "ChapterInclude",
    "Section",
    "Shelf",
    "ShelfInclude",
    "Tag",
    "Widget"
  ]

targetSpec :: Spec
targetSpec =
  describe "Poppy.Codegen.Target" $ do
    it "validates every schema referenced by a target" $ do
      schemasForTargets allTargets
        `shouldMatchList` [ widgetSchema,
                            shelfSchema
                          ]

    it "writes table types under the output dir and clients under Client/" $ do
      let paths = sort (nub (map outputPath (allOutputs allTargets)))
      filter (not . isClientPath) paths
        `shouldBe` sort
          [ "test/Schema/Book.hs",
            "test/Schema/BookInclude.hs",
            "test/Schema/Chapter.hs",
            "test/Schema/ChapterInclude.hs",
            "test/Schema/Section.hs",
            "test/Schema/Shelf.hs",
            "test/Schema/ShelfInclude.hs",
            "test/Schema/Tag.hs",
            "test/Schema/Widget.hs"
          ]
      filter isClientPath paths
        `shouldNotBe` []

    it "keeps generated schema modules byte-stable" $ do
      mapM_ assertGoldenStable schemaGoldenNames

    it "nests clients under the module prefix" $ do
      let target = simpleTarget "src/Schema" widgetSchema
      outputPaths target
        `shouldMatchList` [ "src/Schema/Widget.hs",
                            "src/Schema/Client/Widget.hs"
                          ]
      outputTextFor "src/Schema/Widget.hs" target
        `shouldSatisfy` ("module Schema.Widget" `T.isInfixOf`)
      outputTextFor "src/Schema/Client/Widget.hs" target
        `shouldSatisfy` ("module Schema.Client.Widget" `T.isInfixOf`)

    it "keeps nested module prefixes from the path" $ do
      outputPaths (simpleTarget "src/MyApp/Schema" widgetSchema)
        `shouldMatchList` [ "src/MyApp/Schema/Widget.hs",
                            "src/MyApp/Schema/Client/Widget.hs"
                          ]
      outputTextFor "src/MyApp/Schema/Widget.hs" (simpleTarget "src/MyApp/Schema" widgetSchema)
        `shouldSatisfy` ("module MyApp.Schema.Widget" `T.isInfixOf`)
  where
    isClientPath path = "/Client/" `T.isInfixOf` T.pack path

    assertGoldenStable name = do
      expected <- readFile ("test/Poppy/Codegen/golden/" <> name <> ".hs.golden")
      let path = "test/Schema/" <> name <> ".hs"
          actual =
            T.unpack $
              outputText $
                head
                  [ out
                    | out <- allOutputs allTargets,
                      outputPath out == path
                  ]
      actual `shouldBe` expected

    outputPaths target = map outputPath (targetOutputs target)

    outputTextFor path target =
      outputText $
        head
          [ out
            | out <- targetOutputs target,
              outputPath out == path
          ]
