{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.EditorInclude
  ( EditorInclude (..),
    NoInclude (..),
    ResolveInclude,
    unwrapNoInclude,
    EditorWithWrittenPostsEditedPosts (..)
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
import Schema.Editor (EditorTable, EditorRow (..))
import Schema.Article (ArticleTable, ArticleRow (..))
import qualified Schema.Article as Article

data EditorInclude = EditorInclude
  { writtenPosts :: Bool,
    editedPosts :: Bool
  }
  deriving (Show, Eq)

data EditorWithWrittenPostsEditedPosts = EditorWithWrittenPostsEditedPosts
  { editor :: EditorRow,
    writtenPosts :: [ArticleRow],
    editedPosts :: [ArticleRow]
  }
  deriving (Show, Eq)

type family ResolveInclude preset :: Type
type instance ResolveInclude NoInclude = EditorRow
type instance ResolveInclude EditorInclude = EditorWithWrittenPostsEditedPosts

newtype NoInclude = NoInclude EditorInclude
  deriving (Show, Eq)

unwrapNoInclude :: NoInclude -> EditorInclude
unwrapNoInclude (NoInclude include) = include

instance IncludesJoin EditorInclude where
  includesJoin include = include.writtenPosts || include.editedPosts

instance NestInclude EditorInclude EditorWithWrittenPostsEditedPosts where
  type RootRow EditorInclude = EditorRow
  wrapRoot _ editor = EditorWithWrittenPostsEditedPosts {editor, writtenPosts = [], editedPosts = []}

loadEditorInclude :: EditorInclude -> [EditorRow] -> Db [EditorWithWrittenPostsEditedPosts]
loadEditorInclude include roots = do
  writtenPostsMap <-
    if include.writtenPosts
      then indexHasMany (.authorId) <$> findByIn @ArticleTable @ArticleRow Article.articleAuthorId (map (.id) roots)
      else pure emptyGroups
  editedPostsMap <-
    if include.editedPosts
      then indexHasMany (.authorId) <$> findByIn @ArticleTable @ArticleRow Article.articleAuthorId (map (.id) roots)
      else pure emptyGroups
  pure
    [
      EditorWithWrittenPostsEditedPosts {
        editor = root,
        writtenPosts = lookupGroups root.id writtenPostsMap,
        editedPosts = lookupGroups root.id editedPostsMap
      }
    | root <- roots
    ]

instance {-# OVERLAPPING #-} ExecuteInclude EditorTable EditorInclude EditorWithWrittenPostsEditedPosts where
  executeInclude include modifier = do
    roots <- findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable modifier)
    loadEditorInclude include roots

