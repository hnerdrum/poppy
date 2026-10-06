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

module Schema.Include.Author
  ( AuthorInclude (..)
  , AuthorWith (..)
  , AuthorWithPicked (..)
  , AuthorPosts
  , AuthorResult
  , AuthorRead
  , LoadAuthor (..)
  , toAuthorWithPicked
  )
where

import Poppy.Db (Db)
import Poppy.Include (IncludeFor, Load (..), Skip (..), Skipped, ValidEdge, skipped)
import Poppy.Select (OmitSelect (..))
import Poppy.SelectIn (findByIn, indexHasMany, lookupGroups)
import Schema.Author (AuthorPicked, AuthorRow (..), AuthorSelect, toAuthorPicked)
import qualified Schema.Post as Post
import Schema.Post (PostRow (..), PostTable)

data AuthorInclude posts = AuthorInclude
  { posts :: posts
  }
  deriving (Show, Eq)

data AuthorWith posts = AuthorWith
  { author :: AuthorRow,
    posts :: AuthorPosts posts
  }

deriving instance (Eq AuthorRow, Eq (AuthorPosts posts)) => Eq (AuthorWith posts)
deriving instance (Show AuthorRow, Show (AuthorPosts posts)) => Show (AuthorWith posts)

data AuthorWithPicked posts = AuthorWithPicked
  { author :: AuthorPicked,
    posts :: AuthorPosts posts
  }

deriving instance (Eq AuthorPicked, Eq (AuthorPosts posts)) => Eq (AuthorWithPicked posts)
deriving instance (Show AuthorPicked, Show (AuthorPosts posts)) => Show (AuthorWithPicked posts)

toAuthorWithPicked :: AuthorSelect -> AuthorWith posts -> AuthorWithPicked posts
toAuthorWithPicked select_ nested =
  AuthorWithPicked
    { author = toAuthorPicked select_ nested.author,
      posts = nested.posts
    }

type family AuthorPosts edge where
  AuthorPosts Skip = Skipped "posts" [PostRow]
  AuthorPosts (Load PostTable ()) = [PostRow]

type family AuthorResult include where
  AuthorResult () = AuthorRow
  AuthorResult (AuthorInclude posts) = AuthorWith posts

type family AuthorRead include select where
  AuthorRead () OmitSelect = AuthorRow
  AuthorRead () AuthorSelect = AuthorPicked
  AuthorRead (AuthorInclude posts) OmitSelect = AuthorWith posts
  AuthorRead (AuthorInclude posts) AuthorSelect = AuthorWithPicked posts

class LoadAuthorPosts edge where
  loadAuthorPosts :: edge -> [AuthorRow] -> Db [AuthorPosts edge]

instance LoadAuthorPosts Skip where
  loadAuthorPosts Skip roots = pure (map (const skipped) roots)

instance LoadAuthorPosts (Load PostTable ()) where
  loadAuthorPosts edge roots = do
    rows <- findByIn @PostTable @PostRow Post.postAuthorId (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    let grouped = indexHasMany (.authorId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Post" edge) => LoadAuthorPosts edge where
  loadAuthorPosts _ roots = pure (map (const skipped) roots)

class LoadAuthor posts where
  loadAuthor :: AuthorInclude posts -> [AuthorRow] -> Db [AuthorWith posts]

instance (LoadAuthorPosts posts, ValidEdge "Post" posts) => LoadAuthor posts where
  loadAuthor include roots = do
    postsLoaded <- loadAuthorPosts include.posts roots
    pure
      [ AuthorWith
          { author = root,
            posts = postsLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Post" posts) => IncludeFor "Author" (AuthorInclude posts)

