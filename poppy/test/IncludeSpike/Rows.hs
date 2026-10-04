module IncludeSpike.Rows
  ( AuthorRow (..),
    PostRow (..),
    ShelfRow (..),
    BookRow (..),
    ChapterRow (..),
    TagRow (..),
  )
where

data AuthorRow = AuthorRow
  deriving (Show, Eq)

data PostRow = PostRow
  deriving (Show, Eq)

data ShelfRow = ShelfRow
  deriving (Show, Eq)

data BookRow = BookRow
  deriving (Show, Eq)

data ChapterRow = ChapterRow
  deriving (Show, Eq)

data TagRow = TagRow
  deriving (Show, Eq)
