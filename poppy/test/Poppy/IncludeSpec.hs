{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.IncludeSpec
  ( includeSpec,
  )
where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, nil)
import GHC.Records (HasField)
import Poppy (asc, desc, load, loadWith, runDb, skip)
import Poppy.Include (Load (..))
import Poppy.IncludeUpdate (updatedChapters)
import qualified Poppy.Operations as Ops
import qualified Poppy.ShelfFixtures as ShelfFixtures
import Poppy.Where (eq, neq)
import Schema.Book (BookRow (..), bookId, bookTitle)
import Schema.Chapter (ChapterRow (..))
import qualified Schema.Client.Book as Book
import qualified Schema.Client.Shelf as Shelf
import Schema.Include.Book (BookInclude (..), BookWith (..))
import Schema.Include.Chapter (ChapterInclude (..), ChapterWith (..))
import Schema.Include.Shelf (ShelfInclude (..), ShelfWith (..))
import Schema.Section (SectionRow (..))
import Schema.Shelf (ShelfRow (..), ShelfTable, shelfId, shelfName)
import Schema.Tag (TagRow (..))
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

includeBooks =
  ShelfInclude {books = loadWith (BookInclude {chapters = skip}), tags = skip}

includeBooksChapters =
  ShelfInclude
    { books = loadWith (BookInclude {chapters = loadWith (ChapterInclude {sections = skip})}),
      tags = skip
    }

includeBooksAndTags =
  ShelfInclude {books = loadWith (BookInclude {chapters = skip}), tags = load}

includeBooksChaptersAndTags =
  ShelfInclude
    { books = loadWith (BookInclude {chapters = loadWith (ChapterInclude {sections = skip})}),
      tags = load
    }

includeBooksChaptersSections =
  ShelfInclude
    { books = loadWith (BookInclude {chapters = loadWith (ChapterInclude {sections = load})}),
      tags = skip
    }

shelves include = Shelf.findMany (Shelf.emptyQuery {Shelf.include_ = include})

shelvesWhere include predicate =
  Shelf.findMany (Shelf.emptyQuery {Shelf.include_ = include, Shelf.where_ = Just predicate})

shelfById include pk =
  Shelf.findUnique (Shelf.emptyQuery {Shelf.include_ = include, Shelf.where_ = Just (eq shelfId pk)})

includeSpec :: SpecWith TestEnv
includeSpec =
  describe "Poppy.Include" $ do
    it "findMany nests books under their shelf" $ \TestEnv {envPool = pool} -> do
      fiction <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertShelf pool "nonfiction"
      _ <- ShelfFixtures.insertBook pool fiction.id "Dune"
      _ <- ShelfFixtures.insertBook pool fiction.id "Neuromancer"
      results <- runDb pool (shelves includeBooks)
      length results `shouldBe` 2
      fictionResult <- lookupShelf fiction.id results
      let bookIds = map ((.id) . (.book)) fictionResult.books
      bookIds `shouldBe` sort bookIds
      sort (map ((.title) . (.book)) fictionResult.books) `shouldBe` ["Dune", "Neuromancer"]

    it "findMany includes a shelf with no books as empty list" $ \TestEnv {envPool = pool} -> do
      empty <- ShelfFixtures.insertShelf pool "empty"
      stocked <- ShelfFixtures.insertShelf pool "stocked"
      _ <- ShelfFixtures.insertBook pool stocked.id "Book"
      results <- runDb pool (shelves includeBooks)
      emptyResult <- lookupShelf empty.id results
      length emptyResult.books `shouldBe` 0

    it "findMany without include uses single-table read" $ \TestEnv {envPool = pool} -> do
      _ <- ShelfFixtures.insertShelf pool "alpha"
      _ <- ShelfFixtures.insertShelf pool "beta"
      results <- runDb pool (Ops.findMany @ShelfTable @ShelfRow id)
      length results `shouldBe` 2
      sort (map (.name) results) `shouldBe` ["alpha", "beta"]

    it "findMany returns included roots in primary-key order" $ \TestEnv {envPool = pool} -> do
      firstShelf <- ShelfFixtures.insertShelf pool "first"
      secondShelf <- ShelfFixtures.insertShelf pool "second"
      results <- runDb pool (shelves includeBooks)
      map ((.id) . (.shelf)) results `shouldBe` sort [firstShelf.id, secondShelf.id]

    it "findMany applies root filter before nesting" $ \TestEnv {envPool = pool} -> do
      fiction <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertShelf pool "nonfiction"
      _ <- ShelfFixtures.insertBook pool fiction.id "Dune"
      results <-
        runDb pool (shelvesWhere includeBooks (eq shelfName "fiction"))
      length results `shouldBe` 1
      (head results).shelf.name `shouldBe` "fiction"
      map ((.title) . (.book)) (head results).books `shouldBe` ["Dune"]

    it "findMany nests chapters under books" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertChapter pool book.id "Arrakis"
      _ <- ShelfFixtures.insertChapter pool book.id "Caladan"
      results <- runDb pool (shelves includeBooksChapters)
      [result] <- pure results
      result.shelf.name `shouldBe` "fiction"
      [bookWithChapters] <- pure result.books
      (.title) bookWithChapters.book `shouldBe` "Dune"
      let chapterIds = map ((.id) . (.chapter)) bookWithChapters.chapters
      chapterIds `shouldBe` sort chapterIds
      sort (map ((.heading) . (.chapter)) bookWithChapters.chapters) `shouldBe` ["Arrakis", "Caladan"]

    it "findMany includes a book with no chapters as empty list" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      _ <- ShelfFixtures.insertChapter pool book.id "Chiba"
      results <- runDb pool (shelves includeBooksChapters)
      [result] <- pure results
      dune <- lookupBook "Dune" result
      neuromancer <- lookupBook "Neuromancer" result
      map ((.heading) . (.chapter)) dune.chapters `shouldBe` ["Chiba"]
      length neuromancer.chapters `shouldBe` 0

    it "findMany returns all nested results" $ \TestEnv {envPool = pool} -> do
      _ <- ShelfFixtures.insertShelf pool "one"
      _ <- ShelfFixtures.insertShelf pool "two"
      results <- runDb pool (shelves includeBooks)
      length results `shouldBe` 2

    it "findUnique returns Just for an existing primary key" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      Right result <- runDb pool (shelfById includeBooks shelf.id)
      fmap ((.name) . (.shelf)) result `shouldBe` Just "fiction"

    it "findUnique returns all children for a parent with several has-many rows" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      Right result <- runDb pool (shelfById includeBooks shelf.id)
      let bookIds = maybe [] (map ((.id) . (.book)) . (.books)) result
      bookIds `shouldBe` sort bookIds
      fmap (sort . map ((.title) . (.book)) . (.books)) result
        `shouldBe` Just ["Dune", "Neuromancer"]

    it "findUnique includes a shelf with no books as Just with an empty list" $ \TestEnv {envPool = pool} -> do
      empty <- ShelfFixtures.insertShelf pool "empty"
      Right result <- runDb pool (shelfById includeBooks empty.id)
      fmap ((.name) . (.shelf)) result `shouldBe` Just "empty"
      fmap (length . (.books)) result `shouldBe` Just 0

    it "findUnique returns Nothing when missing" $ \TestEnv {envPool = pool} -> do
      Right result <- runDb pool (shelfById includeBooks (nil :: UUID))
      case result of
        Nothing -> pure ()
        Just _ -> fail "expected Nothing"

    it "findMany with include paginates parent rows" $ \TestEnv {envPool = pool} -> do
      mapM_ (\n -> ShelfFixtures.insertShelf pool ("shelf-" `T.append` T.pack (show n))) ([1 .. 12] :: [Int])
      results <-
        runDb
          pool
          (Shelf.findMany (Shelf.emptyQuery {Shelf.include_ = includeBooks, Shelf.limit_ = Just 10}))
      length results `shouldBe` 10

    it "findMany with include applies offset to parent rows" $ \TestEnv {envPool = pool} -> do
      _ <- ShelfFixtures.insertShelf pool "alpha"
      _ <- ShelfFixtures.insertShelf pool "bravo"
      _ <- ShelfFixtures.insertShelf pool "charlie"
      results <-
        runDb
          pool
          ( Shelf.findMany
              Shelf.emptyQuery
                { Shelf.include_ = includeBooks,
                  Shelf.orderBy_ = [asc shelfName],
                  Shelf.limit_ = Just 1,
                  Shelf.offset_ = Just 1
                }
          )
      length results `shouldBe` 1
      (head results).shelf.name `shouldBe` "bravo"

    it "findMany with include orders parent rows" $ \TestEnv {envPool = pool} -> do
      _ <- ShelfFixtures.insertShelf pool "zeta"
      _ <- ShelfFixtures.insertShelf pool "alpha"
      results <-
        runDb
          pool
          (Shelf.findMany (Shelf.emptyQuery {Shelf.include_ = includeBooks, Shelf.orderBy_ = [asc shelfName]}))
      map ((.name) . (.shelf)) results `shouldBe` ["alpha", "zeta"]

    it "findMany loads sibling hasMany collections independently" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      _ <- ShelfFixtures.insertTag pool shelf.id "scifi"
      _ <- ShelfFixtures.insertTag pool shelf.id "classic"
      results <- runDb pool (shelves includeBooksAndTags)
      [result] <- pure results
      sort (map ((.title) . (.book)) result.books) `shouldBe` ["Dune", "Neuromancer"]
      sort (map (.label) result.tags) `shouldBe` ["classic", "scifi"]

    it "findMany combines nested books with sibling tags" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertChapter pool book.id "Arrakis"
      _ <- ShelfFixtures.insertTag pool shelf.id "scifi"
      results <- runDb pool (shelves includeBooksChaptersAndTags)
      [result] <- pure results
      [bookWithChapters] <- pure result.books
      (.title) bookWithChapters.book `shouldBe` "Dune"
      map ((.heading) . (.chapter)) bookWithChapters.chapters `shouldBe` ["Arrakis"]
      map (.label) result.tags `shouldBe` ["scifi"]

    it "findMany nests sections under chapters at four levels deep" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      chapter <- ShelfFixtures.insertChapter pool book.id "Arrakis"
      _ <- ShelfFixtures.insertSection pool chapter.id "Desert"
      _ <- ShelfFixtures.insertSection pool chapter.id "Sietch"
      results <- runDb pool (shelves includeBooksChaptersSections)
      [result] <- pure results
      [bookWithChapters] <- pure result.books
      [chapterWithSections] <- pure bookWithChapters.chapters
      let sectionIds = map (.id) chapterWithSections.sections
      sectionIds `shouldBe` sort sectionIds
      sort (map (.label) chapterWithSections.sections) `shouldBe` ["Desert", "Sietch"]

    it "one renderer accepts a book loaded on its own and under a shelf" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      book <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertChapter pool book.id "Arrakis"
      let bookInclude = BookInclude {chapters = load}
      [fromBook] <-
        runDb
          pool
          (Book.findMany (Book.emptyQuery {Book.include_ = bookInclude, Book.where_ = Just (eq bookId book.id)}))
      [fromShelf] <-
        runDb
          pool
          (shelves (ShelfInclude {books = loadWith bookInclude, tags = skip}))
      renderBook fromBook `shouldBe` ["Arrakis"]
      renderBook (head fromShelf.books) `shouldBe` ["Arrakis"]

    it "updates a book include from the caller when the field name is unique" $ \_ ->
      updatedChapters `shouldBe` load

    it "findMany includes only books matching where_" $ \TestEnv {envPool = pool} -> do
      fiction <- ShelfFixtures.insertShelf pool "fiction"
      mystery <- ShelfFixtures.insertShelf pool "mystery"
      _ <- ShelfFixtures.insertBook pool fiction.id "Dune"
      _ <- ShelfFixtures.insertBook pool fiction.id "Neuromancer"
      _ <- ShelfFixtures.insertBook pool mystery.id "Rebecca"
      let include =
            ShelfInclude
              { books =
                  (loadWith (BookInclude {chapters = skip}))
                    { where_ = Just (eq bookTitle "Dune")
                    },
                tags = skip
              }
      results <- runDb pool (shelves include)
      fictionResult <- lookupShelf fiction.id results
      mysteryResult <- lookupShelf mystery.id results
      map ((.title) . (.book)) fictionResult.books `shouldBe` ["Dune"]
      length mysteryResult.books `shouldBe` 0

    it "findMany orders included books" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Contact"
      let include =
            ShelfInclude
              { books =
                  (loadWith (BookInclude {chapters = skip}))
                    { orderBy_ = [desc bookTitle]
                    },
                tags = skip
              }
      results <- runDb pool (shelves include)
      [result] <- pure results
      map ((.title) . (.book)) result.books `shouldBe` ["Neuromancer", "Dune", "Contact"]

    it "findMany takes two ordered books per shelf" $ \TestEnv {envPool = pool} -> do
      empty <- ShelfFixtures.insertShelf pool "empty"
      fiction <- ShelfFixtures.insertShelf pool "fiction"
      history <- ShelfFixtures.insertShelf pool "history"
      mapM_ (ShelfFixtures.insertBook pool fiction.id) ["alpha", "bravo", "charlie", "zzz"]
      mapM_ (ShelfFixtures.insertBook pool history.id) ["m", "n", "zzz"]
      let include =
            ShelfInclude
              { books =
                  (loadWith (BookInclude {chapters = skip}))
                    { where_ = Just (neq bookTitle "zzz"),
                      orderBy_ = [asc bookTitle],
                      take_ = Just 2
                    },
                tags = skip
              }
      results <- runDb pool (shelves include)
      emptyResult <- lookupShelf empty.id results
      fictionResult <- lookupShelf fiction.id results
      historyResult <- lookupShelf history.id results
      length emptyResult.books `shouldBe` 0
      map ((.title) . (.book)) fictionResult.books `shouldBe` ["alpha", "bravo"]
      map ((.title) . (.book)) historyResult.books `shouldBe` ["m", "n"]

renderBook :: (HasField "chapters" row [ChapterRow]) => row -> [Text]
renderBook row = map (.heading) row.chapters

lookupShelf wanted results =
  case filter ((== wanted) . (.id) . (.shelf)) results of
    [shelf] -> pure shelf
    _ -> fail "expected exactly one shelf in results"

lookupBook title result =
  case filter ((== title) . (.title) . (.book)) result.books of
    [book] -> pure book
    _ -> fail "expected exactly one book in results"
