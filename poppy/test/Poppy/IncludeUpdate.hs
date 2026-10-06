module Poppy.IncludeUpdate
  ( updatedChapters,
  )
where

import Poppy (load, skip)
import Poppy.Include (Load)
import Schema.Include.Book (BookInclude (..))

updatedChapters :: Load ()
updatedChapters =
  let BookInclude {chapters} = BookInclude {chapters = skip} {chapters = load}
   in chapters
