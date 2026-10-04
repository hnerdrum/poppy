{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SkippedPosts where

import IncludeSpike.Author (AuthorWith (..))
import IncludeSpike.Rows (PostRow)
import IncludeSpike.Runtime (Skip)

bad :: AuthorWith Skip Skip -> [PostRow]
bad row = row.posts
