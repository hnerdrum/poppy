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
  , PostInclude (..)
  , PostWith (..)
  , PostWithPicked (..)
  , PostAuthor
  , PostResult
  , PostRead
  , LoadPost (..)
  , toPostWithPicked
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
    requireRelated,
    findByIn,
    indexByPk,
    indexHasMany,
    lookupByPk,
    lookupGroups
  )
import qualified Schema.Author as Author
import Schema.Author (AuthorPicked, AuthorRow (..), AuthorSelect, AuthorTable, toAuthorPicked)
import qualified Schema.Post as Post
import Schema.Post (PostPicked, PostRow (..), PostSelect, PostTable, toPostPicked)

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
  AuthorPosts (Load PostTable include) = [PostResult include]

type family AuthorResult include where
  AuthorResult () = AuthorRow
  AuthorResult (AuthorInclude posts) = AuthorWith posts

type family AuthorRead include select where
  AuthorRead () OmitSelect = AuthorRow
  AuthorRead () AuthorSelect = AuthorPicked
  AuthorRead (AuthorInclude posts) OmitSelect = AuthorWith posts
  AuthorRead (AuthorInclude posts) AuthorSelect = AuthorWithPicked posts

data PostInclude author = PostInclude
  { author :: author
  }
  deriving (Show, Eq)

data PostWith author = PostWith
  { post :: PostRow,
    author :: PostAuthor author
  }

deriving instance (Eq PostRow, Eq (PostAuthor author)) => Eq (PostWith author)
deriving instance (Show PostRow, Show (PostAuthor author)) => Show (PostWith author)

data PostWithPicked author = PostWithPicked
  { post :: PostPicked,
    author :: PostAuthor author
  }

deriving instance (Eq PostPicked, Eq (PostAuthor author)) => Eq (PostWithPicked author)
deriving instance (Show PostPicked, Show (PostAuthor author)) => Show (PostWithPicked author)

toPostWithPicked :: PostSelect -> PostWith author -> PostWithPicked author
toPostWithPicked select_ nested =
  PostWithPicked
    { post = toPostPicked select_ nested.post,
      author = nested.author
    }

type family PostAuthor edge where
  PostAuthor Skip = Skipped "author" AuthorRow
  PostAuthor (Load AuthorTable include) = AuthorResult include

type family PostResult include where
  PostResult () = PostRow
  PostResult (PostInclude author) = PostWith author

type family PostRead include select where
  PostRead () OmitSelect = PostRow
  PostRead () PostSelect = PostPicked
  PostRead (PostInclude author) OmitSelect = PostWith author
  PostRead (PostInclude author) PostSelect = PostWithPicked author

class LoadAuthorPosts edge where
  loadAuthorPosts :: edge -> [AuthorRow] -> Db [AuthorPosts edge]

instance LoadAuthorPosts Skip where
  loadAuthorPosts Skip roots = pure (map (const skipped) roots)

instance LoadAuthorPosts (Load PostTable ()) where
  loadAuthorPosts edge roots = do
    rows <- findByIn @PostTable @PostRow Post.postAuthorId (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    let grouped = indexHasMany (.authorId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance (LoadPost author) => LoadAuthorPosts (Load PostTable (PostInclude author)) where
  loadAuthorPosts edge roots = do
    rows <- findByIn @PostTable @PostRow Post.postAuthorId (map (.id) roots) edge.where_ edge.orderBy_ edge.take_
    loaded <- loadPost edge.include_ rows
    let grouped = indexHasMany ((.authorId) . (.post)) loaded
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

class LoadPostAuthor edge where
  loadPostAuthor :: edge -> [PostRow] -> Db [PostAuthor edge]

instance LoadPostAuthor Skip where
  loadPostAuthor Skip roots = pure (map (const skipped) roots)

instance LoadPostAuthor (Load AuthorTable ()) where
  loadPostAuthor edge roots = do
    rows <- findByIn @AuthorTable @AuthorRow Author.authorId (map (.authorId) roots) edge.where_ edge.orderBy_ edge.take_
    let indexed = indexByPk (.id) rows
    pure [requireRelated "author" (lookupByPk root.authorId indexed) | root <- roots]

instance (LoadAuthor posts) => LoadPostAuthor (Load AuthorTable (AuthorInclude posts)) where
  loadPostAuthor edge roots = do
    rows <- findByIn @AuthorTable @AuthorRow Author.authorId (map (.authorId) roots) edge.where_ edge.orderBy_ edge.take_
    loaded <- loadAuthor edge.include_ rows
    let indexed = indexByPk ((.id) . (.author)) loaded
    pure [requireRelated "author" (lookupByPk root.authorId indexed) | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Author" edge) => LoadPostAuthor edge where
  loadPostAuthor _ roots = pure (map (const skipped) roots)

class LoadPost author where
  loadPost :: PostInclude author -> [PostRow] -> Db [PostWith author]

instance (LoadPostAuthor author, ValidEdge "Author" author) => LoadPost author where
  loadPost include roots = do
    authorLoaded <- loadPostAuthor include.author roots
    pure
      [ PostWith
          { post = root,
            author = authorLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Author" author) => IncludeFor "Post" (PostInclude author)

