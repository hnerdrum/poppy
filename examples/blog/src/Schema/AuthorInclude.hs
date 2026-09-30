{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.AuthorInclude
  ( AuthorInclude (..),
    NoInclude (..),
    ResolveInclude,
    unwrapNoInclude,
    AuthorWithPosts (..)
  )
where

import Data.Kind (Type)
import Poppy.Db (Db)
import Poppy.Include
  ( ExecuteInclude (..),
    IncludesJoin (..),
    NestInclude (..)
  )
import Poppy.Operations (findMany)
import Poppy.SelectIn
  ( findByIn,
    prepareIncludeRootQuery,
    emptyGroups,
    indexHasMany,
    lookupGroups
  )
import Schema.Author (AuthorTable, AuthorRow (..))
import Schema.Post (PostTable, PostRow (..))
import qualified Schema.Post as Post

newtype AuthorInclude = AuthorInclude
  { posts :: Bool
  }
  deriving (Show, Eq)

data AuthorWithPosts = AuthorWithPosts
  { author :: AuthorRow,
    posts :: [PostRow]
  }
  deriving (Show, Eq)

type family ResolveInclude preset :: Type
type instance ResolveInclude NoInclude = AuthorRow
type instance ResolveInclude AuthorInclude = AuthorWithPosts

newtype NoInclude = NoInclude AuthorInclude
  deriving (Show, Eq)

unwrapNoInclude :: NoInclude -> AuthorInclude
unwrapNoInclude (NoInclude include) = include

instance IncludesJoin AuthorInclude where
  includesJoin include = include.posts

instance NestInclude AuthorInclude AuthorWithPosts where
  type RootRow AuthorInclude = AuthorRow
  wrapRoot _ author = AuthorWithPosts {author, posts = []}

loadAuthorInclude :: AuthorInclude -> [AuthorRow] -> Db [AuthorWithPosts]
loadAuthorInclude include roots = do
  postsMap <-
    if include.posts
      then indexHasMany (.authorId) <$> findByIn @PostTable @PostRow Post.postAuthorId (map (.id) roots)
      else pure emptyGroups
  pure
    [
      AuthorWithPosts {
        author = root,
        posts = lookupGroups root.id postsMap
      }
    | root <- roots
    ]

instance {-# OVERLAPPING #-} ExecuteInclude AuthorTable AuthorInclude AuthorWithPosts where
  executeInclude include modifier = do
    roots <- findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable modifier)
    loadAuthorInclude include roots

