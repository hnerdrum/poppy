{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema.ShelfInclude
  ( ShelfInclude (..),
    NoInclude (..),
    ResolveInclude,
    unwrapNoInclude,
    Tags (..),
    unwrapTags,
    Books (..),
    unwrapBooks,
    BooksTags (..),
    unwrapBooksTags,
    BooksChapters (..),
    unwrapBooksChapters,
    BooksChaptersTags (..),
    unwrapBooksChaptersTags,
    BooksChaptersSections (..),
    unwrapBooksChaptersSections,
    WithBooksTags (..),
    unwrapWithBooksTags,
    ShelfWithTag (..),
    ShelfWithBook (..),
    ShelfWithBookTag (..),
    ShelfWithBooksChapter (..),
    BookWithChapter (..),
    ShelfWithBooksChapterTag (..),
    ShelfWithBooksChaptersSection (..),
    BookWithChapters (..),
    ChapterWithSections (..),
    ShelfWithBooksTags (..),
    IncludeTags (..),
    IncludeBooks (..),
    IncludeChapters (..),
    Chapters (..),
    ChaptersSections (..),
    Sections (..),
    CombineInclude (..),
    ChapterInclude (..),
    BookInclude (..)
  )
where

import Prelude hiding ((<>))
import Data.Kind (Type)
import Data.Maybe (isJust)
import ORM.Db (Db)
import ORM.Include
  ( ExecuteInclude (..),
    IncludesJoin (..),
    NestInclude (..)
  )
import ORM.Operations (findMany)
import ORM.SelectIn
  ( findByIn,
    prepareIncludeRootQuery,
    emptyGroups,
    indexHasMany,
    lookupGroups
  )
import Schema.Shelf (ShelfTable, ShelfRow (..))
import Schema.Book (BookTable, BookRow (..))
import Schema.Chapter (ChapterTable, ChapterRow (..))
import Schema.Section (SectionTable, SectionRow (..))
import Schema.Tag (TagTable, TagRow (..))
import qualified Schema.Book as Book
import qualified Schema.Chapter as Chapter
import qualified Schema.Section as Section
import qualified Schema.Tag as Tag

newtype ChapterInclude = ChapterInclude
  { sections :: Bool
  }
  deriving (Show, Eq)

newtype BookInclude = BookInclude
  { chapters :: Maybe ChapterInclude
  }
  deriving (Show, Eq)

data ShelfInclude = ShelfInclude
  { books :: Maybe BookInclude,
    tags :: Bool
  }
  deriving (Show, Eq)

data ShelfWithTag = ShelfWithTag
  { shelf :: ShelfRow,
    tags :: [TagRow]
  }
  deriving (Show, Eq)

data ShelfWithBook = ShelfWithBook
  { shelf :: ShelfRow,
    books :: [BookRow]
  }
  deriving (Show, Eq)

data ShelfWithBookTag = ShelfWithBookTag
  { shelf :: ShelfRow,
    books :: [BookRow],
    tags :: [TagRow]
  }
  deriving (Show, Eq)

data ShelfWithBooksChapter = ShelfWithBooksChapter
  { shelf :: ShelfRow,
    books :: [BookWithChapter]
  }
  deriving (Show, Eq)

data BookWithChapter = BookWithChapter
  { book :: BookRow,
    chapters :: [ChapterRow]
  }
  deriving (Show, Eq)

data ShelfWithBooksChapterTag = ShelfWithBooksChapterTag
  { shelf :: ShelfRow,
    books :: [BookWithChapter],
    tags :: [TagRow]
  }
  deriving (Show, Eq)

data ShelfWithBooksChaptersSection = ShelfWithBooksChaptersSection
  { shelf :: ShelfRow,
    books :: [BookWithChapters]
  }
  deriving (Show, Eq)

data BookWithChapters = BookWithChapters
  { book :: BookRow,
    chapters :: [ChapterWithSections]
  }
  deriving (Show, Eq)

data ChapterWithSections = ChapterWithSections
  { chapter :: ChapterRow,
    sections :: [SectionRow]
  }
  deriving (Show, Eq)

data ShelfWithBooksTags = ShelfWithBooksTags
  { shelf :: ShelfRow,
    books :: [BookWithChapters],
    tags :: [TagRow]
  }
  deriving (Show, Eq)

newtype Tags = Tags ShelfInclude
  deriving (Show, Eq)

unwrapTags :: Tags -> ShelfInclude
unwrapTags (Tags include) = include

newtype Books = Books ShelfInclude
  deriving (Show, Eq)

unwrapBooks :: Books -> ShelfInclude
unwrapBooks (Books include) = include

newtype BooksTags = BooksTags ShelfInclude
  deriving (Show, Eq)

unwrapBooksTags :: BooksTags -> ShelfInclude
unwrapBooksTags (BooksTags include) = include

newtype BooksChapters = BooksChapters ShelfInclude
  deriving (Show, Eq)

unwrapBooksChapters :: BooksChapters -> ShelfInclude
unwrapBooksChapters (BooksChapters include) = include

newtype BooksChaptersTags = BooksChaptersTags ShelfInclude
  deriving (Show, Eq)

unwrapBooksChaptersTags :: BooksChaptersTags -> ShelfInclude
unwrapBooksChaptersTags (BooksChaptersTags include) = include

newtype BooksChaptersSections = BooksChaptersSections ShelfInclude
  deriving (Show, Eq)

unwrapBooksChaptersSections :: BooksChaptersSections -> ShelfInclude
unwrapBooksChaptersSections (BooksChaptersSections include) = include

newtype WithBooksTags = WithBooksTags ShelfInclude
  deriving (Show, Eq)

unwrapWithBooksTags :: WithBooksTags -> ShelfInclude
unwrapWithBooksTags (WithBooksTags include) = include

type family ResolveInclude preset :: Type
type instance ResolveInclude NoInclude = ShelfRow
type instance ResolveInclude Tags = ShelfWithTag
type instance ResolveInclude Books = ShelfWithBook
type instance ResolveInclude BooksTags = ShelfWithBookTag
type instance ResolveInclude BooksChapters = ShelfWithBooksChapter
type instance ResolveInclude BooksChaptersTags = ShelfWithBooksChapterTag
type instance ResolveInclude BooksChaptersSections = ShelfWithBooksChaptersSection
type instance ResolveInclude WithBooksTags = ShelfWithBooksTags

newtype NoInclude = NoInclude ShelfInclude
  deriving (Show, Eq)

unwrapNoInclude :: NoInclude -> ShelfInclude
unwrapNoInclude (NoInclude include) = include

instance IncludesJoin ShelfInclude where
  includesJoin include = isJust include.books || include.tags

instance NestInclude ShelfInclude ShelfWithBooksTags where
  type RootRow ShelfInclude = ShelfRow
  wrapRoot _ shelf = ShelfWithBooksTags {shelf, books = [], tags = []}

loadChapterInclude :: ChapterInclude -> [ChapterRow] -> Db [ChapterWithSections]
loadChapterInclude include roots = do
  sectionsMap <-
    if include.sections
      then indexHasMany (.chapterRef) <$> findByIn @SectionTable @SectionRow Section.sectionChapterRef (map (.id) roots)
      else pure emptyGroups
  pure
    [
      ChapterWithSections {
        chapter = root,
        sections = lookupGroups root.id sectionsMap
      }
    | root <- roots
    ]

loadBookInclude :: BookInclude -> [BookRow] -> Db [BookWithChapters]
loadBookInclude include roots = do
  chaptersMap <- case include.chapters of
    Nothing -> pure emptyGroups
    Just nestedInclude -> do
      rows <- findByIn @ChapterTable @ChapterRow Chapter.chapterBookRef (map (.id) roots)
      nested <- loadChapterInclude nestedInclude rows
      pure $ indexHasMany ((.bookRef) . (.chapter)) nested
  pure
    [
      BookWithChapters {
        book = root,
        chapters = lookupGroups root.id chaptersMap
      }
    | root <- roots
    ]

loadShelfInclude :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBooksTags]
loadShelfInclude include roots = do
  booksMap <- case include.books of
    Nothing -> pure emptyGroups
    Just nestedInclude -> do
      rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
      nested <- loadBookInclude nestedInclude rows
      pure $ indexHasMany ((.shelfId) . (.book)) nested
  tagsMap <-
    if include.tags
      then indexHasMany (.shelfId) <$> findByIn @TagTable @TagRow Tag.tagShelfId (map (.id) roots)
      else pure emptyGroups
  pure
    [
      ShelfWithBooksTags {
        shelf = root,
        books = lookupGroups root.id booksMap,
        tags = lookupGroups root.id tagsMap
      }
    | root <- roots
    ]

loadShelfWithTag :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithTag]
loadShelfWithTag _include roots = do
  tagsMap <-
    indexHasMany (.shelfId) <$> findByIn @TagTable @TagRow Tag.tagShelfId (map (.id) roots)
  pure
    [
      ShelfWithTag {
        shelf = root,
        tags = lookupGroups root.id tagsMap
      }
    | root <- roots
    ]

loadShelfWithBook :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBook]
loadShelfWithBook _include roots = do
  booksMap <-
    indexHasMany (.shelfId) <$> findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
  pure
    [
      ShelfWithBook {
        shelf = root,
        books = lookupGroups root.id booksMap
      }
    | root <- roots
    ]

loadShelfWithBookTag :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBookTag]
loadShelfWithBookTag _include roots = do
  booksMap <-
    indexHasMany (.shelfId) <$> findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
  tagsMap <-
    indexHasMany (.shelfId) <$> findByIn @TagTable @TagRow Tag.tagShelfId (map (.id) roots)
  pure
    [
      ShelfWithBookTag {
        shelf = root,
        books = lookupGroups root.id booksMap,
        tags = lookupGroups root.id tagsMap
      }
    | root <- roots
    ]

loadShelfWithBooksChapter :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBooksChapter]
loadShelfWithBooksChapter include roots = do
  booksMap <- do
    rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
    nested <- loadBookWithChapter include rows
    pure $ indexHasMany ((.shelfId) . (.book)) nested
  pure
    [
      ShelfWithBooksChapter {
        shelf = root,
        books = lookupGroups root.id booksMap
      }
    | root <- roots
    ]

loadBookWithChapter :: ShelfInclude -> [BookRow] -> Db [BookWithChapter]
loadBookWithChapter _include roots = do
  chaptersMap <-
    indexHasMany (.bookRef) <$> findByIn @ChapterTable @ChapterRow Chapter.chapterBookRef (map (.id) roots)
  pure
    [
      BookWithChapter {
        book = root,
        chapters = lookupGroups root.id chaptersMap
      }
    | root <- roots
    ]

loadShelfWithBooksChapterTag :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBooksChapterTag]
loadShelfWithBooksChapterTag include roots = do
  booksMap <- do
    rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
    nested <- loadBookWithChapter include rows
    pure $ indexHasMany ((.shelfId) . (.book)) nested
  tagsMap <-
    indexHasMany (.shelfId) <$> findByIn @TagTable @TagRow Tag.tagShelfId (map (.id) roots)
  pure
    [
      ShelfWithBooksChapterTag {
        shelf = root,
        books = lookupGroups root.id booksMap,
        tags = lookupGroups root.id tagsMap
      }
    | root <- roots
    ]

loadShelfWithBooksChaptersSection :: ShelfInclude -> [ShelfRow] -> Db [ShelfWithBooksChaptersSection]
loadShelfWithBooksChaptersSection include roots = do
  booksMap <- do
    rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
    nested <- loadBookWithChapters include rows
    pure $ indexHasMany ((.shelfId) . (.book)) nested
  pure
    [
      ShelfWithBooksChaptersSection {
        shelf = root,
        books = lookupGroups root.id booksMap
      }
    | root <- roots
    ]

loadBookWithChapters :: ShelfInclude -> [BookRow] -> Db [BookWithChapters]
loadBookWithChapters include roots = do
  chaptersMap <- do
    rows <- findByIn @ChapterTable @ChapterRow Chapter.chapterBookRef (map (.id) roots)
    nested <- loadChapterWithSections include rows
    pure $ indexHasMany ((.bookRef) . (.chapter)) nested
  pure
    [
      BookWithChapters {
        book = root,
        chapters = lookupGroups root.id chaptersMap
      }
    | root <- roots
    ]

loadChapterWithSections :: ShelfInclude -> [ChapterRow] -> Db [ChapterWithSections]
loadChapterWithSections _include roots = do
  sectionsMap <-
    indexHasMany (.chapterRef) <$> findByIn @SectionTable @SectionRow Section.sectionChapterRef (map (.id) roots)
  pure
    [
      ChapterWithSections {
        chapter = root,
        sections = lookupGroups root.id sectionsMap
      }
    | root <- roots
    ]

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable ShelfInclude ShelfWithBooksTags where
  executeInclude include modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfInclude include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable Tags ShelfWithTag where
  executeInclude (Tags include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithTag include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable Books ShelfWithBook where
  executeInclude (Books include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithBook include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable BooksTags ShelfWithBookTag where
  executeInclude (BooksTags include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithBookTag include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable BooksChapters ShelfWithBooksChapter where
  executeInclude (BooksChapters include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithBooksChapter include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable BooksChaptersTags ShelfWithBooksChapterTag where
  executeInclude (BooksChaptersTags include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithBooksChapterTag include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable BooksChaptersSections ShelfWithBooksChaptersSection where
  executeInclude (BooksChaptersSections include) modifier = do
    roots <- findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable modifier)
    loadShelfWithBooksChaptersSection include roots

instance {-# OVERLAPPING #-} ExecuteInclude ShelfTable WithBooksTags ShelfWithBooksTags where
  executeInclude (WithBooksTags include) modifier = executeInclude include modifier

data Chapters = Chapters

data ChaptersSections = ChaptersSections

data Sections = Sections

class IncludeChapters include where
  chapters :: include

instance IncludeChapters Chapters where
  chapters = Chapters

instance IncludeChapters (Sections -> ChaptersSections) where
  chapters Sections = ChaptersSections


class IncludeBooks include where
  books :: include

instance IncludeBooks Books where
  books =
    Books ShelfInclude {books = Just (BookInclude {chapters = Nothing}), tags = False}

instance (include ~ BooksChapters) => IncludeBooks (Chapters -> include) where
  books Chapters =
    BooksChapters ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = False})}), tags = False}

instance (include ~ BooksChaptersSections) => IncludeBooks (ChaptersSections -> include) where
  books ChaptersSections =
    BooksChaptersSections ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}), tags = False}


class IncludeTags include where
  tags :: include

instance IncludeTags Tags where
  tags =
    Tags ShelfInclude {books = Nothing, tags = True}


class CombineInclude a b where
  type CombinedInclude a b :: Type
  (<>) :: a -> b -> CombinedInclude a b

instance CombineInclude Books Tags where
  type CombinedInclude Books Tags = BooksTags
  _ <> _ =
    BooksTags ShelfInclude {books = Just (BookInclude {chapters = Nothing}), tags = True}

instance CombineInclude BooksChapters Tags where
  type CombinedInclude BooksChapters Tags = BooksChaptersTags
  _ <> _ =
    BooksChaptersTags ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = False})}), tags = True}

instance CombineInclude BooksChaptersSections Tags where
  type CombinedInclude BooksChaptersSections Tags = WithBooksTags
  _ <> _ =
    WithBooksTags ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}), tags = True}


