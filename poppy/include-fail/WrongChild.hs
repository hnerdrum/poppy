{-# LANGUAGE DuplicateRecordFields #-}

module WrongChild where

import Poppy (loadWith, skip)
import qualified Schema.Client.Shelf as Shelf
import Schema.Include.Shelf (ShelfInclude (..))

bad =
  Shelf.findMany
    Shelf.emptyQuery
      { Shelf.include_ =
          ShelfInclude
            { books = loadWith (ShelfInclude {books = skip, tags = skip}),
              tags = skip
            }
      }
