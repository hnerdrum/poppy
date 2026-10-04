{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module RecordUpdateAmbiguous where

import IncludeSpike.Runtime (load, loadWith, skip)
import IncludeSpike.Shelf (BookInclude (..), ShelfInclude (..))

data Other = Other {tags :: Int}

updated =
  nested {tags = load}
  where
    nested =
      ShelfInclude
        { books = loadWith BookInclude {chapters = load},
          tags = skip
        }
