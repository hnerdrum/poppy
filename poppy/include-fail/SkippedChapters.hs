{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SkippedChapters where

import Poppy.Internal.Include (Skip)
import Schema.Chapter (ChapterRow)
import Schema.Include.Book (BookWith (..))

bad :: BookWith Skip -> [ChapterRow]
bad row = row.chapters
