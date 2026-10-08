{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Schema.Include.Book
  ( BookInclude (..)
  , BookWith (..)
  , BookWithPicked (..)
  , BookChapters
  , BookResult
  , BookRead
  , LoadBook (..)
  , toBookWithPicked
  )
where

import Poppy.Internal.Generated
  ( Db,
    IncludeFor,
    Load (..),
    Skip (..),
    Skipped,
    ValidEdge,
    skipped,
    OmitSelect (..),
    findByIn,
    indexHasMany,
    lookupGroups
  )
import Schema.Book (BookPicked, BookRow (..), BookSelect, toBookPicked)
import qualified Schema.Chapter as Chapter
import Schema.Chapter (ChapterRow (..), ChapterTable)
import Schema.Include.Chapter (ChapterInclude (..), ChapterResult, ChapterWith (..), LoadChapter (..))

data BookInclude chapters = BookInclude
  { chapters :: chapters
  }
  deriving (Show, Eq)

data BookWith chapters = BookWith
  { book :: BookRow,
    chapters :: BookChapters chapters
  }

deriving instance (Eq BookRow, Eq (BookChapters chapters)) => Eq (BookWith chapters)
deriving instance (Show BookRow, Show (BookChapters chapters)) => Show (BookWith chapters)

data BookWithPicked chapters = BookWithPicked
  { book :: BookPicked,
    chapters :: BookChapters chapters
  }

deriving instance (Eq BookPicked, Eq (BookChapters chapters)) => Eq (BookWithPicked chapters)
deriving instance (Show BookPicked, Show (BookChapters chapters)) => Show (BookWithPicked chapters)

toBookWithPicked :: BookSelect -> BookWith chapters -> BookWithPicked chapters
toBookWithPicked select_ nested =
  BookWithPicked
    { book = toBookPicked select_ nested.book,
      chapters = nested.chapters
    }

type family BookChapters edge where
  BookChapters Skip = Skipped "chapters" [ChapterRow]
  BookChapters (Load ChapterTable include) = [ChapterResult include]

type family BookResult include where
  BookResult () = BookRow
  BookResult (BookInclude chapters) = BookWith chapters

type family BookRead include select where
  BookRead () OmitSelect = BookRow
  BookRead () BookSelect = BookPicked
  BookRead (BookInclude chapters) OmitSelect = BookWith chapters
  BookRead (BookInclude chapters) BookSelect = BookWithPicked chapters

class LoadBookChapters edge where
  loadBookChapters :: edge -> [BookRow] -> Db [BookChapters edge]

instance LoadBookChapters Skip where
  loadBookChapters Skip roots = pure (map (const skipped) roots)

instance LoadBookChapters (Load ChapterTable ()) where
  loadBookChapters edge roots = do
    rows <- findByIn @ChapterTable @ChapterRow Chapter.chapterBookRef (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    let grouped = indexHasMany (.bookRef) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance (LoadChapter sections) => LoadBookChapters (Load ChapterTable (ChapterInclude sections)) where
  loadBookChapters edge roots = do
    rows <- findByIn @ChapterTable @ChapterRow Chapter.chapterBookRef (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    loaded <- loadChapter edge.include_ rows
    let grouped = indexHasMany ((.bookRef) . (.chapter)) loaded
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Chapter" edge) => LoadBookChapters edge where
  loadBookChapters _ roots = pure (map (const skipped) roots)

class LoadBook chapters where
  loadBook :: BookInclude chapters -> [BookRow] -> Db [BookWith chapters]

instance (LoadBookChapters chapters, ValidEdge "Chapter" chapters) => LoadBook chapters where
  loadBook include roots = do
    chaptersLoaded <- loadBookChapters include.chapters roots
    pure
      [ BookWith
          { book = root,
            chapters = chaptersLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Chapter" chapters) => IncludeFor "Book" (BookInclude chapters)

