module Poppy.IncludeUpdate
  ( updatedChapters,
  )
where

import Poppy (load, skip)
import Poppy.Include (Load)
import Schema.Chapter (ChapterTable)
import Schema.Include.Book (BookInclude (..))

updatedChapters :: Load ChapterTable ()
updatedChapters =
  let BookInclude {chapters} = BookInclude {chapters = skip} {chapters = load}
   in chapters
