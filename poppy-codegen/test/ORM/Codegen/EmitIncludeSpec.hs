{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.EmitIncludeSpec
  ( emitIncludeSpec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ORM.Codegen.Emit.Include
  ( emitExecuteInclude,
    emitIncludeADTs,
    emitIncludeModule,
    emitIncludesJoin,
    emitNestInclude,
    emitResultADTs,
  )
import ORM.Codegen.IR (Schema (..))
import ORM.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitIncludeSpec :: Spec
emitIncludeSpec =
  describe "ORM.Codegen.Emit.Include" $ do
    it "emits ShelfInclude and BookInclude ADTs" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitIncludeADTs shelfSchema incl)
        `shouldBe` T.strip expectedShelfIncludeADTs

    it "emits ShelfWithBooksTags and nested result ADTs" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitResultADTs shelfSchema incl)
        `shouldBe` T.strip expectedShelfResultADTs

    it "emits IncludesJoin for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitIncludesJoin shelfSchema incl)
        `shouldBe` T.strip expectedIncludesJoin

    it "emits NestInclude for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitNestInclude shelfSchema incl)
        `shouldBe` T.strip expectedNestInclude

    it "emits OVERLAPPING ExecuteInclude for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitExecuteInclude shelfSchema incl)
        `shouldBe` T.strip expectedExecuteInclude

    it "emits ShelfInclude module matching the golden file" $ do
      expected <- TIO.readFile "test/ORM/Codegen/golden/ShelfInclude.hs.golden"
      let incl = head (schemaIncludes shelfSchema)
          actual = emitIncludeModule "Schema.ShelfInclude" shelfSchema incl
      T.strip actual `shouldBe` T.strip expected

    it "emits sibling IncludesJoin for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
      T.strip (emitIncludesJoin shelfSchema incl)
        `shouldBe` T.strip expectedSiblingIncludesJoin

expectedShelfIncludeADTs :: T.Text
expectedShelfIncludeADTs =
  T.unlines
    [ "newtype ChapterInclude = ChapterInclude",
      "  { sections :: Bool",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "newtype BookInclude = BookInclude",
      "  { chapters :: Maybe ChapterInclude",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "data ShelfInclude = ShelfInclude",
      "  { books :: Maybe BookInclude,",
      "    tags :: Bool",
      "  }",
      "  deriving (Show, Eq)"
    ]

expectedShelfResultADTs :: T.Text
expectedShelfResultADTs =
  T.unlines
    [ "data ChapterWithSections = ChapterWithSections",
      "  { chapter :: ChapterRow,",
      "    sections :: [SectionRow]",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "data BookWithChapters = BookWithChapters",
      "  { book :: BookRow,",
      "    chapters :: [ChapterWithSections]",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "data ShelfWithBooksTags = ShelfWithBooksTags",
      "  { shelf :: ShelfRow,",
      "    books :: [BookWithChapters],",
      "    tags :: [TagRow]",
      "  }",
      "  deriving (Show, Eq)"
    ]

expectedIncludesJoin :: T.Text
expectedIncludesJoin =
  T.unlines
    [ "instance IncludesJoin ShelfInclude where",
      "  includesJoin include = isJust include.books || include.tags"
    ]

expectedSiblingIncludesJoin :: T.Text
expectedSiblingIncludesJoin =
  T.unlines
    [ "instance IncludesJoin ShelfInclude where",
      "  includesJoin include = isJust include.books || include.tags"
    ]

expectedNestInclude :: T.Text
expectedNestInclude =
  T.unlines
    [ "instance NestInclude ShelfInclude ShelfWithBooksTags where",
      "  type RootRow ShelfInclude = ShelfRow",
      "  wrapRoot _ shelf = ShelfWithBooksTags {shelf, books = [], tags = []}"
    ]

expectedExecuteInclude :: T.Text
expectedExecuteInclude =
  T.unlines
    [ "instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable ShelfInclude ShelfWithBooksTags where",
      "  executeInclude include modifier = do",
      "    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)",
      "    loadShelfInclude include roots"
    ]
