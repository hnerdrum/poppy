{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module SkippedPostsNoSelectors where

import IncludeSpike.Author (AuthorWith (..))
import IncludeSpike.Rows (PostRow)
import IncludeSpike.Runtime (Skip)

bad :: AuthorWith Skip Skip -> [PostRow]
bad row = row.posts
