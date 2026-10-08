{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Schema.Include.Comment
  ( CommentInclude (..)
  , CommentWith (..)
  , CommentWithPicked (..)
  , CommentReplies
  , CommentResult
  , CommentRead
  , LoadComment (..)
  , toCommentWithPicked
  )
where

import Poppy.Internal.Generated
  ( Db,
    IncludeFor,
    Load (..),
    Skip (..),
    Skipped,
    ValidEdge,
    skipped,
    OmitSelect (..),
    findByIn,
    indexHasManyMaybe,
    lookupGroups
  )
import qualified Schema.Comment as Comment
import Schema.Comment (CommentPicked, CommentRow (..), CommentSelect, CommentTable, toCommentPicked)

data CommentInclude replies = CommentInclude
  { replies :: replies
  }
  deriving (Show, Eq)

data CommentWith replies = CommentWith
  { comment :: CommentRow,
    replies :: CommentReplies replies
  }

deriving instance (Eq CommentRow, Eq (CommentReplies replies)) => Eq (CommentWith replies)
deriving instance (Show CommentRow, Show (CommentReplies replies)) => Show (CommentWith replies)

data CommentWithPicked replies = CommentWithPicked
  { comment :: CommentPicked,
    replies :: CommentReplies replies
  }

deriving instance (Eq CommentPicked, Eq (CommentReplies replies)) => Eq (CommentWithPicked replies)
deriving instance (Show CommentPicked, Show (CommentReplies replies)) => Show (CommentWithPicked replies)

toCommentWithPicked :: CommentSelect -> CommentWith replies -> CommentWithPicked replies
toCommentWithPicked select_ nested =
  CommentWithPicked
    { comment = toCommentPicked select_ nested.comment,
      replies = nested.replies
    }

type family CommentReplies edge where
  CommentReplies Skip = Skipped "replies" [CommentRow]
  CommentReplies (Load CommentTable include) = [CommentResult include]

type family CommentResult include where
  CommentResult () = CommentRow
  CommentResult (CommentInclude replies) = CommentWith replies

type family CommentRead include select where
  CommentRead () OmitSelect = CommentRow
  CommentRead () CommentSelect = CommentPicked
  CommentRead (CommentInclude replies) OmitSelect = CommentWith replies
  CommentRead (CommentInclude replies) CommentSelect = CommentWithPicked replies

class LoadCommentReplies edge where
  loadCommentReplies :: edge -> [CommentRow] -> Db [CommentReplies edge]

instance LoadCommentReplies Skip where
  loadCommentReplies Skip roots = pure (map (const skipped) roots)

instance LoadCommentReplies (Load CommentTable ()) where
  loadCommentReplies edge roots = do
    rows <- findByIn @CommentTable @CommentRow Comment.commentParentId (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    let grouped = indexHasManyMaybe (.parentId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance (LoadComment replies) => LoadCommentReplies (Load CommentTable (CommentInclude replies)) where
  loadCommentReplies edge roots = do
    rows <- findByIn @CommentTable @CommentRow Comment.commentParentId (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    loaded <- loadComment edge.include_ rows
    let grouped = indexHasManyMaybe ((.parentId) . (.comment)) loaded
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Comment" edge) => LoadCommentReplies edge where
  loadCommentReplies _ roots = pure (map (const skipped) roots)

class LoadComment replies where
  loadComment :: CommentInclude replies -> [CommentRow] -> Db [CommentWith replies]

instance (LoadCommentReplies replies, ValidEdge "Comment" replies) => LoadComment replies where
  loadComment include roots = do
    repliesLoaded <- loadCommentReplies include.replies roots
    pure
      [ CommentWith
          { comment = root,
            replies = repliesLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Comment" replies) => IncludeFor "Comment" (CommentInclude replies)

