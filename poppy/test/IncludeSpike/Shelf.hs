{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module IncludeSpike.Shelf
  ( BookInclude (..),
    BookWith (..),
    BookResult,
    ChapterResult,
    TagInclude (..),
    TagResult,
    ShelfInclude (..),
    ShelfWith (..),
    ShelfQuery (..),
    ShelfResult,
    emptyQuery,
    findMany,
  )
where

import Data.Kind (Constraint, Type)
import IncludeSpike.Rows (BookRow, ChapterRow, ShelfRow (..), TagRow)
import IncludeSpike.Runtime (IncludeFor, Load, Skip, Skipped, ValidEdge)

data BookInclude chapters = BookInclude
  { chapters :: chapters
  }
  deriving (Show, Eq)

data BookWith chapters = BookWith
  { book :: BookRow,
    chapters :: BookChapters chapters
  }

type family BookChapters edge where
  BookChapters Skip = Skipped "chapters" [ChapterRow]
  BookChapters (Load include) = [ChapterResult include]

type family BookResult include :: Type where
  BookResult () = BookRow
  BookResult (BookInclude chapters) = BookWith chapters

type family ChapterResult include :: Type where
  ChapterResult () = ChapterRow

data TagInclude = TagInclude
  deriving (Show, Eq)

type family TagResult include :: Type where
  TagResult () = TagRow
  TagResult TagInclude = TagRow

data ShelfInclude books tags = ShelfInclude
  { books :: books,
    tags :: tags
  }
  deriving (Show, Eq)

data ShelfWith books tags = ShelfWith
  { shelf :: ShelfRow,
    books :: ShelfBooks books,
    tags :: ShelfTags tags
  }

type family ShelfBooks edge where
  ShelfBooks Skip = Skipped "books" [BookRow]
  ShelfBooks (Load include) = [BookResult include]

type family ShelfTags edge where
  ShelfTags Skip = Skipped "tags" [TagRow]
  ShelfTags (Load include) = [TagResult include]

type family ShelfResult include :: Type where
  ShelfResult () = ShelfRow
  ShelfResult (ShelfInclude books tags) = ShelfWith books tags

data ShelfQuery include select = ShelfQuery
  { include_ :: include,
    select_ :: select
  }

emptyQuery :: ShelfQuery () ()
emptyQuery = ShelfQuery {include_ = (), select_ = ()}

findMany :: (ValidShelf include) => ShelfQuery include select -> [ShelfResult include]
findMany _ = []

type family ValidShelf include :: Constraint where
  ValidShelf () = ()
  ValidShelf (ShelfInclude books tags) =
    (ValidEdge "Book" books, ValidEdge "Tag" tags)

instance (ValidEdge "Chapter" chapters) => IncludeFor "Book" (BookInclude chapters)

instance IncludeFor "Tag" TagInclude
