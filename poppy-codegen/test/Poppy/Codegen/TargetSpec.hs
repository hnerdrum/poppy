{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.TargetSpec
  ( targetSpec,
  )
where

import Data.List (nub, sort)
import qualified Data.Text as T
import Poppy.Codegen.Run (allOutputs, schemasForTargets)
import Poppy.Codegen.Spec.Author (authorSchema)
import Poppy.Codegen.Spec.Comment (commentSchema)
import Poppy.Codegen.Spec.Editor (editorSchema)
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
  [ "Article",
    "Author",
    "Book",
    "Chapter",
    "Comment",
    "Editor",
    "Include/Author",
    "Include/Book",
    "Include/Chapter",
    "Include/Comment",
    "Include/Editor",
    "Include/Post",
    "Include/Shelf",
    "Post",
    "PostStatus",
    "Section",
    "Shelf",
    "Tag",
    "Widget"
  ]

targetSpec :: Spec
targetSpec =
  describe "Poppy.Codegen.Target" $ do
    it "validates every schema referenced by a target" $ do
      schemasForTargets allTargets
        `shouldMatchList` [ widgetSchema,
                            shelfSchema,
                            authorSchema,
                            editorSchema,
                            commentSchema
                          ]

    it "writes table types under the output dir and clients under Client/" $ do
      let paths = sort (nub (map outputPath (allOutputs allTargets)))
      filter (not . isClientPath) paths
        `shouldBe` sort
          [ "test/Schema/Article.hs",
            "test/Schema/Author.hs",
            "test/Schema/Book.hs",
            "test/Schema/Chapter.hs",
            "test/Schema/Comment.hs",
            "test/Schema/Editor.hs",
            "test/Schema/Include/Author.hs",
            "test/Schema/Include/Book.hs",
            "test/Schema/Include/Chapter.hs",
            "test/Schema/Include/Comment.hs",
            "test/Schema/Include/Editor.hs",
            "test/Schema/Include/Post.hs",
            "test/Schema/Include/Shelf.hs",
            "test/Schema/Post.hs",
            "test/Schema/PostStatus.hs",
            "test/Schema/Section.hs",
            "test/Schema/Shelf.hs",
            "test/Schema/Tag.hs",
            "test/Schema/Widget.hs"
          ]
      filter isClientPath paths
        `shouldNotBe` []

    it "keeps generated schema modules byte-stable" $ do
      mapM_ assertGoldenStable schemaGoldenNames

    it "emits one Client per model" $ do
      outputPaths (simpleTarget "src/Schema" shelfSchema)
        `shouldMatchList` [ "src/Schema/Book.hs",
                            "src/Schema/Chapter.hs",
                            "src/Schema/Include/Book.hs",
                            "src/Schema/Include/Chapter.hs",
                            "src/Schema/Include/Shelf.hs",
                            "src/Schema/Section.hs",
                            "src/Schema/Shelf.hs",
                            "src/Schema/Tag.hs",
                            "src/Schema/Client/Book.hs",
                            "src/Schema/Client/Chapter.hs",
                            "src/Schema/Client/Section.hs",
                            "src/Schema/Client/Shelf.hs",
                            "src/Schema/Client/Tag.hs"
                          ]

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
