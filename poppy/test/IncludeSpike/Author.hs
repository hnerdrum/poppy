{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module IncludeSpike.Author
  ( AuthorInclude (..),
    AuthorWith (..),
    AuthorQuery (..),
    AuthorResult,
    emptyQuery,
    findMany,
    PostInclude (..),
    PostWith (..),
    PostQuery (..),
    PostResult,
    emptyPostQuery,
    findManyPosts,
  )
where

import Data.Kind (Constraint, Type)
import IncludeSpike.Rows (AuthorRow (..), PostRow)
import IncludeSpike.Runtime (IncludeFor, Load, Skip, Skipped, ValidEdge)

data AuthorInclude posts drafts = AuthorInclude
  { posts :: posts,
    drafts :: drafts
  }
  deriving (Show, Eq)

data AuthorWith posts drafts = AuthorWith
  { author :: AuthorRow,
    posts :: AuthorPosts posts,
    drafts :: AuthorDrafts drafts
  }

type family AuthorPosts edge where
  AuthorPosts Skip = Skipped "posts" [PostRow]
  AuthorPosts (Load include) = [PostResult include]

type family AuthorDrafts edge where
  AuthorDrafts Skip = Skipped "drafts" [PostRow]
  AuthorDrafts (Load include) = [PostResult include]

type family AuthorResult include :: Type where
  AuthorResult () = AuthorRow
  AuthorResult (AuthorInclude posts drafts) = AuthorWith posts drafts

data AuthorQuery include select = AuthorQuery
  { include_ :: include,
    select_ :: select
  }

emptyQuery :: AuthorQuery () ()
emptyQuery = AuthorQuery {include_ = (), select_ = ()}

findMany :: (ValidAuthor include) => AuthorQuery include select -> [AuthorResult include]
findMany _ = []

type family ValidAuthor include :: Constraint where
  ValidAuthor () = ()
  ValidAuthor (AuthorInclude posts drafts) =
    (ValidEdge "Post" posts, ValidEdge "Post" drafts)

data PostInclude author reviewer = PostInclude
  { author :: author,
    reviewer :: reviewer
  }
  deriving (Show, Eq)

data PostWith author reviewer = PostWith
  { post :: PostRow,
    author :: PostAuthor author,
    reviewer :: PostReviewer reviewer
  }

type family PostAuthor edge where
  PostAuthor Skip = Skipped "author" AuthorRow
  PostAuthor (Load include) = AuthorResult include

type family PostReviewer edge where
  PostReviewer Skip = Skipped "reviewer" (Maybe AuthorRow)
  PostReviewer (Load include) = Maybe (AuthorResult include)

type family PostResult include :: Type where
  PostResult () = PostRow
  PostResult (PostInclude author reviewer) = PostWith author reviewer

data PostQuery include select = PostQuery
  { include_ :: include,
    select_ :: select
  }

emptyPostQuery :: PostQuery () ()
emptyPostQuery = PostQuery {include_ = (), select_ = ()}

findManyPosts :: (ValidPost include) => PostQuery include select -> [PostResult include]
findManyPosts _ = []

type family ValidPost include :: Constraint where
  ValidPost () = ()
  ValidPost (PostInclude author reviewer) =
    (ValidEdge "Author" author, ValidEdge "Author" reviewer)

instance
  (ValidEdge "Post" posts, ValidEdge "Post" drafts) =>
  IncludeFor "Author" (AuthorInclude posts drafts)

instance
  (ValidEdge "Author" author, ValidEdge "Author" reviewer) =>
  IncludeFor "Post" (PostInclude author reviewer)
