{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.EmitFlatRowSpec
  ( emitFlatRowSpec,
  )
where

import qualified Data.Text as T
import ORM.Codegen.Emit.FlatRow
  ( emitColumnList,
    emitFlatJoinRow,
  )
import ORM.Codegen.IR (Schema (..))
import ORM.Codegen.IncludePath
  ( includeLongestJoinPath,
    joinPathTake,
  )
import ORM.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

emitFlatRowSpec :: Spec
emitFlatRowSpec =
  describe "ORM.Codegen.Emit.FlatRow" $ do
    it "emits ShelfBookJoinRow for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = joinPathTake 1 (includeLongestJoinPath shelfSchema incl)
      T.strip (emitFlatJoinRow shelfSchema path)
        `shouldBe` T.strip expectedShelfBookJoinRow

    it "emits shelfBookColumns for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = joinPathTake 1 (includeLongestJoinPath shelfSchema incl)
      T.strip (emitColumnList shelfSchema path)
        `shouldBe` T.strip expectedShelfBookColumns

    it "emits ShelfBookChapterJoinRow for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = joinPathTake 2 (includeLongestJoinPath shelfSchema incl)
      T.strip (emitFlatJoinRow shelfSchema path)
        `shouldBe` T.strip expectedShelfBookChapterJoinRow

    it "emits shelfBookChapterColumns for ShelfInclude" $ do
      let incl = head (schemaIncludes shelfSchema)
          path = joinPathTake 2 (includeLongestJoinPath shelfSchema incl)
      T.strip (emitColumnList shelfSchema path)
        `shouldBe` T.strip expectedShelfBookChapterColumns

expectedShelfBookJoinRow :: T.Text
expectedShelfBookJoinRow =
  T.unlines
    [ "data ShelfBookJoinRow = ShelfBookJoinRow",
      "  { shelfId :: UUID,",
      "    shelfName :: Text,",
      "    bookId :: Maybe UUID,",
      "    bookTitle :: Maybe Text",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "instance FromRow ShelfBookJoinRow where",
      "  fromRow = ShelfBookJoinRow <$> field <*> field <*> field <*> field"
    ]

expectedShelfBookColumns :: T.Text
expectedShelfBookColumns =
  T.unlines
    [ "shelfBookColumns :: [Text]",
      "shelfBookColumns =",
      "  [ JoinChain.col \"s\" Shelf.shelfId,",
      "    JoinChain.col \"s\" Shelf.shelfName,",
      "    JoinChain.col \"b\" Book.bookId,",
      "    JoinChain.col \"b\" Book.bookTitle",
      "  ]"
    ]

expectedShelfBookChapterJoinRow :: T.Text
expectedShelfBookChapterJoinRow =
  T.unlines
    [ "data ShelfBookChapterJoinRow = ShelfBookChapterJoinRow",
      "  { shelfId :: UUID,",
      "    shelfName :: Text,",
      "    bookId :: Maybe UUID,",
      "    bookTitle :: Maybe Text,",
      "    chapterId :: Maybe UUID,",
      "    chapterHeading :: Maybe Text",
      "  }",
      "  deriving (Show, Eq)",
      "",
      "instance FromRow ShelfBookChapterJoinRow where",
      "  fromRow = ShelfBookChapterJoinRow <$> field <*> field <*> field <*> field <*> field <*> field"
    ]

expectedShelfBookChapterColumns :: T.Text
expectedShelfBookChapterColumns =
  T.unlines
    [ "shelfBookChapterColumns :: [Text]",
      "shelfBookChapterColumns =",
      "  shelfBookColumns",
      "    ++ [ JoinChain.col \"c\" Chapter.chapterId,",
      "         JoinChain.col \"c\" Chapter.chapterHeading",
      "       ]"
    ]
