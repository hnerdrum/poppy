{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module SkippedChaptersNoSelectors where

import Poppy.Include (Skip)
import Schema.Chapter (ChapterRow)
import Schema.Include.Book (BookWith (..))

bad :: BookWith Skip -> [ChapterRow]
bad row = row.chapters
