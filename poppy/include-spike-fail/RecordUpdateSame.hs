{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module RecordUpdateSame where

import IncludeSpike.Runtime (Load, load, loadWith, skip)

data BookInclude chapters = BookInclude {chapters :: chapters}

data ShelfInclude books tags = ShelfInclude {books :: books, tags :: tags}

updated :: ShelfInclude (Load (BookInclude (Load ()))) (Load ())
updated =
  nested {tags = load}
  where
    nested =
      ShelfInclude
        { books = loadWith BookInclude {chapters = load},
          tags = skip
        }
