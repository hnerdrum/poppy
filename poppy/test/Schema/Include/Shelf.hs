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

module Schema.Include.Shelf
  ( ShelfInclude (..)
  , ShelfWith (..)
  , ShelfWithPicked (..)
  , ShelfBooks
  , ShelfTags
  , ShelfResult
  , ShelfRead
  , LoadShelf (..)
  , toShelfWithPicked
  )
where

import Poppy.Db (Db)
import Poppy.Include (IncludeFor, Load (..), Skip (..), Skipped, ValidEdge, skipped)
import Poppy.Select (OmitSelect (..))
import Poppy.SelectIn (findByIn, indexHasMany, lookupGroups)
import Schema.Shelf (ShelfPicked, ShelfRow (..), ShelfSelect, toShelfPicked)
import qualified Schema.Book as Book
import Schema.Book (BookRow (..), BookTable)
import qualified Schema.Tag as Tag
import Schema.Tag (TagRow (..), TagTable)
import Schema.Include.Book (BookInclude (..), BookResult, BookWith (..), LoadBook (..))

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

deriving instance (Eq ShelfRow, Eq (ShelfBooks books), Eq (ShelfTags tags)) => Eq (ShelfWith books tags)
deriving instance (Show ShelfRow, Show (ShelfBooks books), Show (ShelfTags tags)) => Show (ShelfWith books tags)

data ShelfWithPicked books tags = ShelfWithPicked
  { shelf :: ShelfPicked,
    books :: ShelfBooks books,
    tags :: ShelfTags tags
  }

deriving instance (Eq ShelfPicked, Eq (ShelfBooks books), Eq (ShelfTags tags)) => Eq (ShelfWithPicked books tags)
deriving instance (Show ShelfPicked, Show (ShelfBooks books), Show (ShelfTags tags)) => Show (ShelfWithPicked books tags)

toShelfWithPicked :: ShelfSelect -> ShelfWith books tags -> ShelfWithPicked books tags
toShelfWithPicked select_ nested =
  ShelfWithPicked
    { shelf = toShelfPicked select_ nested.shelf,
      books = nested.books,
      tags = nested.tags
    }

type family ShelfBooks edge where
  ShelfBooks Skip = Skipped "books" [BookRow]
  ShelfBooks (Load include) = [BookResult include]

type family ShelfTags edge where
  ShelfTags Skip = Skipped "tags" [TagRow]
  ShelfTags (Load ()) = [TagRow]

type family ShelfResult include where
  ShelfResult () = ShelfRow
  ShelfResult (ShelfInclude books tags) = ShelfWith books tags

type family ShelfRead include select where
  ShelfRead () OmitSelect = ShelfRow
  ShelfRead () ShelfSelect = ShelfPicked
  ShelfRead (ShelfInclude books tags) OmitSelect = ShelfWith books tags
  ShelfRead (ShelfInclude books tags) ShelfSelect = ShelfWithPicked books tags

class LoadShelfBooks edge where
  loadShelfBooks :: edge -> [ShelfRow] -> Db [ShelfBooks edge]

instance LoadShelfBooks Skip where
  loadShelfBooks Skip roots = pure (map (const skipped) roots)

instance LoadShelfBooks (Load ()) where
  loadShelfBooks (Load ()) roots = do
    rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
    let grouped = indexHasMany (.shelfId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance (LoadBook chapters) => LoadShelfBooks (Load (BookInclude chapters)) where
  loadShelfBooks (Load nested) roots = do
    rows <- findByIn @BookTable @BookRow Book.bookShelfId (map (.id) roots)
    loaded <- loadBook nested rows
    let grouped = indexHasMany ((.shelfId) . (.book)) loaded
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Book" edge) => LoadShelfBooks edge where
  loadShelfBooks _ roots = pure (map (const skipped) roots)

class LoadShelfTags edge where
  loadShelfTags :: edge -> [ShelfRow] -> Db [ShelfTags edge]

instance LoadShelfTags Skip where
  loadShelfTags Skip roots = pure (map (const skipped) roots)

instance LoadShelfTags (Load ()) where
  loadShelfTags (Load ()) roots = do
    rows <- findByIn @TagTable @TagRow Tag.tagShelfId (map (.id) roots)
    let grouped = indexHasMany (.shelfId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Tag" edge) => LoadShelfTags edge where
  loadShelfTags _ roots = pure (map (const skipped) roots)

class LoadShelf books tags where
  loadShelf :: ShelfInclude books tags -> [ShelfRow] -> Db [ShelfWith books tags]

instance (LoadShelfBooks books, LoadShelfTags tags, ValidEdge "Book" books, ValidEdge "Tag" tags) => LoadShelf books tags where
  loadShelf include roots = do
    booksLoaded <- loadShelfBooks include.books roots
    tagsLoaded <- loadShelfTags include.tags roots
    pure
      [ ShelfWith
          { shelf = root,
            books = booksLoaded !! n,
            tags = tagsLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Book" books, ValidEdge "Tag" tags) => IncludeFor "Shelf" (ShelfInclude books tags)

