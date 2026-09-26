{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.PostInclude
  ( PostInclude (..),
    NoInclude (..),
    ResolveInclude,
    unwrapNoInclude,
    PostWithAuthor (..)
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
    emptyByPk,
    indexByPk,
    lookupByPk
  )
import Schema.Post (PostTable, PostRow (..))
import Schema.Author (AuthorTable, AuthorRow (..))
import qualified Schema.Author as Author

newtype PostInclude = PostInclude
  { author :: Bool
  }
  deriving (Show, Eq)

data PostWithAuthor = PostWithAuthor
  { post :: PostRow,
    author :: Maybe AuthorRow
  }
  deriving (Show, Eq)

type family ResolveInclude preset :: Type
type instance ResolveInclude NoInclude = PostRow
type instance ResolveInclude PostInclude = PostWithAuthor

newtype NoInclude = NoInclude PostInclude
  deriving (Show, Eq)

unwrapNoInclude :: NoInclude -> PostInclude
unwrapNoInclude (NoInclude include) = include

instance IncludesJoin PostInclude where
  includesJoin include = include.author

instance NestInclude PostInclude PostWithAuthor where
  type RootRow PostInclude = PostRow
  wrapRoot _ post = PostWithAuthor {post, author = Nothing}

loadPostInclude :: PostInclude -> [PostRow] -> Db [PostWithAuthor]
loadPostInclude include roots = do
  authorMap <-
    if include.author
      then indexByPk (.id) <$> findByIn @AuthorTable @AuthorRow Author.authorId (map (.authorId) roots)
      else pure emptyByPk
  pure
    [
      PostWithAuthor {
        post = root,
        author = lookupByPk root.authorId authorMap
      }
    | root <- roots
    ]

instance {-# OVERLAPPING #-} ExecuteInclude PostTable PostInclude PostWithAuthor where
  executeInclude include modifier = do
    roots <- findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable modifier)
    loadPostInclude include roots

