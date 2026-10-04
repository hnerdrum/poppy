{-# LANGUAGE DuplicateRecordFields #-}

module WrongChild where

import IncludeSpike.Runtime (loadWith, skip)
import IncludeSpike.Shelf (ShelfInclude (..), ShelfQuery (..), TagInclude (..), findMany)

bad =
  findMany
    ShelfQuery
      { include_ = ShelfInclude {books = loadWith TagInclude, tags = skip},
        select_ = ()
      }
