{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module RecordUpdateOther where

import IncludeSpike.Runtime (load, loadWith, skip)
import IncludeSpike.Shelf (BookInclude (..), ShelfInclude (..))

updated =
  nested {tags = load}
  where
    nested =
      ShelfInclude
        { books = loadWith BookInclude {chapters = load},
          tags = skip
        }
