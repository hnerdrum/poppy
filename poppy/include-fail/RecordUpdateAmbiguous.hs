{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module RecordUpdateAmbiguous where

import Poppy (load, skip)
import Schema.Include.Book (BookInclude (..))

data Other = Other {chapters :: Int}

updated =
  nested {chapters = load}
  where
    nested = BookInclude {chapters = skip}
