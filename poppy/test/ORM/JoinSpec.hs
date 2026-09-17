{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module ORM.JoinSpec
  ( joinSpec,
  )
where

import Data.List (sort)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.ToField (toField)
import ORM (runDb)
import qualified ORM.JoinChain as JoinChain
import ORM.PG (FromRow (..), field)
import ORM.Relation (HasMany (..), JoinType (..))
import qualified ORM.ShelfFixtures as ShelfFixtures
import qualified Schema.Book as Book
import qualified Schema.Chapter as Chapter
import qualified Schema.Shelf as Shelf
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

data ShelfBookJoinRow = ShelfBookJoinRow
  { shelfId :: UUID,
    shelfName :: Text,
    bookId :: Maybe UUID,
    bookTitle :: Maybe Text
  }
  deriving (Show, Eq)

instance FromRow ShelfBookJoinRow where
  fromRow = ShelfBookJoinRow <$> field <*> field <*> field <*> field

data ShelfBookChapterJoinRow = ShelfBookChapterJoinRow
  { shelfId :: UUID,
    shelfName :: Text,
    bookId :: Maybe UUID,
    bookTitle :: Maybe Text,
    chapterId :: Maybe UUID,
    chapterHeading :: Maybe Text
  }
  deriving (Show, Eq)

instance FromRow ShelfBookChapterJoinRow where
  fromRow = ShelfBookChapterJoinRow <$> field <*> field <*> field <*> field <*> field <*> field

selectShelfBookColumns :: JoinChain.JoinChain Shelf.ShelfTable -> JoinChain.JoinChain Shelf.ShelfTable
selectShelfBookColumns =
  JoinChain.selectColumns
    [ JoinChain.col "s" Shelf.shelfId,
      JoinChain.col "s" Shelf.shelfName,
      JoinChain.col "b" Book.bookId,
      JoinChain.col "b" Book.bookTitle
    ]

shelfBookJoin :: JoinChain.JoinChain Shelf.ShelfTable
shelfBookJoin =
  selectShelfBookColumns $
    JoinChain.buildJoinChainFromHasMany Shelf.shelfBooks "s" "b"

joinSpec :: SpecWith TestEnv
joinSpec =
  describe "ORM.JoinChain" $ do
    it "leftJoin via HasMany returns one row per book on a shelf" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      rows <- runDb pool (JoinChain.runJoinChain @ShelfBookJoinRow shelfBookJoin)
      length rows `shouldBe` 2
      map (.bookId) rows `shouldBe` sort (map (.bookId) rows)
      sort (map (.bookTitle) rows) `shouldBe` sort [Just "Dune", Just "Neuromancer"]
      map (.shelfId) rows `shouldBe` [shelf.id, shelf.id]

    it "leftJoin returns a shelf with no books as one row with null book fields" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "empty"
      (row :: ShelfBookJoinRow) : _ <- runDb pool (JoinChain.runJoinChain @ShelfBookJoinRow shelfBookJoin)
      row.shelfId `shouldBe` shelf.id
      row.shelfName `shouldBe` "empty"
      row.bookId `shouldBe` Nothing
      row.bookTitle `shouldBe` Nothing

    it "innerJoin excludes shelves without books" $ \TestEnv {envPool = pool} -> do
      emptyShelf <- ShelfFixtures.insertShelf pool "empty"
      stockedShelf <- ShelfFixtures.insertShelf pool "stocked"
      _ <- ShelfFixtures.insertBook pool stockedShelf.id "Book"
      let innerRelation = Shelf.shelfBooks {joinType = InnerJoin}
          innerJoin =
            selectShelfBookColumns $
              JoinChain.buildJoinChainFromHasMany innerRelation "s" "b"
      rows <- runDb pool (JoinChain.runJoinChain @ShelfBookJoinRow innerJoin)
      map (.shelfId) rows `shouldBe` [stockedShelf.id]
      map (.shelfId) rows `shouldSatisfy` (notElem emptyShelf.id)

    it "whereJoin filters by left alias" $ \TestEnv {envPool = pool} -> do
      fiction <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertShelf pool "nonfiction"
      _ <- ShelfFixtures.insertBook pool fiction.id "Dune"
      rows <-
        runDb
          pool
          ( JoinChain.runJoinChain @ShelfBookJoinRow $
              JoinChain.whereJoin "s.name = ?" [toField ("fiction" :: Text)] shelfBookJoin
          )
      length rows `shouldBe` 1
      (head rows).shelfName `shouldBe` "fiction"

    it "whereJoin filters by right alias" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      rows <-
        runDb
          pool
          ( JoinChain.runJoinChain @ShelfBookJoinRow $
              JoinChain.whereJoin "b.title = ?" [toField ("Dune" :: Text)] shelfBookJoin
          )
      length rows `shouldBe` 1
      (head rows).bookTitle `shouldBe` Just "Dune"

    it "whereJoin can return zero rows" $ \TestEnv {envPool = pool} -> do
      _ <- ShelfFixtures.insertShelf pool "fiction"
      rows <-
        runDb
          pool
          ( JoinChain.runJoinChain @ShelfBookJoinRow $
              JoinChain.whereJoin "s.name = ?" [toField ("nonexistent" :: Text)] shelfBookJoin
          )
      rows `shouldBe` []

    it "chained leftJoin returns one row per chapter" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertChapter pool book.id "Arrakis"
      _ <- ShelfFixtures.insertChapter pool book.id "Caladan"
      let shelfBookChapterJoin =
            JoinChain.selectColumns
              [ JoinChain.col "s" Shelf.shelfId,
                JoinChain.col "s" Shelf.shelfName,
                JoinChain.col "b" Book.bookId,
                JoinChain.col "b" Book.bookTitle,
                JoinChain.col "c" Chapter.chapterId,
                JoinChain.col "c" Chapter.chapterHeading
              ]
              (JoinChain.addHasManyJoin Book.bookChapters "b" "c" shelfBookJoin)
      rows <- runDb pool (JoinChain.runJoinChain @ShelfBookChapterJoinRow shelfBookChapterJoin)
      length rows `shouldBe` 2
      map (.chapterId) rows `shouldBe` sort (map (.chapterId) rows)
      sort (map (.chapterHeading) rows) `shouldBe` sort [Just "Arrakis", Just "Caladan"]
