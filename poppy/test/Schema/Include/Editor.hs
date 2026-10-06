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

module Schema.Include.Editor
  ( EditorInclude (..)
  , EditorWith (..)
  , EditorWithPicked (..)
  , EditorWrittenPosts
  , EditorEditedPosts
  , EditorResult
  , EditorRead
  , LoadEditor (..)
  , toEditorWithPicked
  )
where

import Poppy.Db (Db)
import Poppy.Include (IncludeFor, Load (..), Skip (..), Skipped, ValidEdge, skipped)
import Poppy.Select (OmitSelect (..))
import Poppy.SelectIn (findByIn, indexHasMany, lookupGroups)
import Schema.Editor (EditorPicked, EditorRow (..), EditorSelect, toEditorPicked)
import qualified Schema.Article as Article
import Schema.Article (ArticleRow (..), ArticleTable)

data EditorInclude writtenPosts editedPosts = EditorInclude
  { writtenPosts :: writtenPosts,
    editedPosts :: editedPosts
  }
  deriving (Show, Eq)

data EditorWith writtenPosts editedPosts = EditorWith
  { editor :: EditorRow,
    writtenPosts :: EditorWrittenPosts writtenPosts,
    editedPosts :: EditorEditedPosts editedPosts
  }

deriving instance (Eq EditorRow, Eq (EditorWrittenPosts writtenPosts), Eq (EditorEditedPosts editedPosts)) => Eq (EditorWith writtenPosts editedPosts)
deriving instance (Show EditorRow, Show (EditorWrittenPosts writtenPosts), Show (EditorEditedPosts editedPosts)) => Show (EditorWith writtenPosts editedPosts)

data EditorWithPicked writtenPosts editedPosts = EditorWithPicked
  { editor :: EditorPicked,
    writtenPosts :: EditorWrittenPosts writtenPosts,
    editedPosts :: EditorEditedPosts editedPosts
  }

deriving instance (Eq EditorPicked, Eq (EditorWrittenPosts writtenPosts), Eq (EditorEditedPosts editedPosts)) => Eq (EditorWithPicked writtenPosts editedPosts)
deriving instance (Show EditorPicked, Show (EditorWrittenPosts writtenPosts), Show (EditorEditedPosts editedPosts)) => Show (EditorWithPicked writtenPosts editedPosts)

toEditorWithPicked :: EditorSelect -> EditorWith writtenPosts editedPosts -> EditorWithPicked writtenPosts editedPosts
toEditorWithPicked select_ nested =
  EditorWithPicked
    { editor = toEditorPicked select_ nested.editor,
      writtenPosts = nested.writtenPosts,
      editedPosts = nested.editedPosts
    }

type family EditorWrittenPosts edge where
  EditorWrittenPosts Skip = Skipped "writtenPosts" [ArticleRow]
  EditorWrittenPosts (Load ()) = [ArticleRow]

type family EditorEditedPosts edge where
  EditorEditedPosts Skip = Skipped "editedPosts" [ArticleRow]
  EditorEditedPosts (Load ()) = [ArticleRow]

type family EditorResult include where
  EditorResult () = EditorRow
  EditorResult (EditorInclude writtenPosts editedPosts) = EditorWith writtenPosts editedPosts

type family EditorRead include select where
  EditorRead () OmitSelect = EditorRow
  EditorRead () EditorSelect = EditorPicked
  EditorRead (EditorInclude writtenPosts editedPosts) OmitSelect = EditorWith writtenPosts editedPosts
  EditorRead (EditorInclude writtenPosts editedPosts) EditorSelect = EditorWithPicked writtenPosts editedPosts

class LoadEditorWrittenPosts edge where
  loadEditorWrittenPosts :: edge -> [EditorRow] -> Db [EditorWrittenPosts edge]

instance LoadEditorWrittenPosts Skip where
  loadEditorWrittenPosts Skip roots = pure (map (const skipped) roots)

instance LoadEditorWrittenPosts (Load ()) where
  loadEditorWrittenPosts (Load ()) roots = do
    rows <- findByIn @ArticleTable @ArticleRow Article.articleAuthorId (map (.id) roots)
    let grouped = indexHasMany (.authorId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Article" edge) => LoadEditorWrittenPosts edge where
  loadEditorWrittenPosts _ roots = pure (map (const skipped) roots)

class LoadEditorEditedPosts edge where
  loadEditorEditedPosts :: edge -> [EditorRow] -> Db [EditorEditedPosts edge]

instance LoadEditorEditedPosts Skip where
  loadEditorEditedPosts Skip roots = pure (map (const skipped) roots)

instance LoadEditorEditedPosts (Load ()) where
  loadEditorEditedPosts (Load ()) roots = do
    rows <- findByIn @ArticleTable @ArticleRow Article.articleAuthorId (map (.id) roots)
    let grouped = indexHasMany (.authorId) rows
    pure [lookupGroups root.id grouped | root <- roots]

instance {-# OVERLAPPABLE #-} (ValidEdge "Article" edge) => LoadEditorEditedPosts edge where
  loadEditorEditedPosts _ roots = pure (map (const skipped) roots)

class LoadEditor writtenPosts editedPosts where
  loadEditor :: EditorInclude writtenPosts editedPosts -> [EditorRow] -> Db [EditorWith writtenPosts editedPosts]

instance (LoadEditorWrittenPosts writtenPosts, LoadEditorEditedPosts editedPosts, ValidEdge "Article" writtenPosts, ValidEdge "Article" editedPosts) => LoadEditor writtenPosts editedPosts where
  loadEditor include roots = do
    writtenPostsLoaded <- loadEditorWrittenPosts include.writtenPosts roots
    editedPostsLoaded <- loadEditorEditedPosts include.editedPosts roots
    pure
      [ EditorWith
          { editor = root,
            writtenPosts = writtenPostsLoaded !! n,
            editedPosts = editedPostsLoaded !! n
          }
      | (n, root) <- zip [0 :: Int ..] roots
      ]

instance (ValidEdge "Article" writtenPosts, ValidEdge "Article" editedPosts) => IncludeFor "Editor" (EditorInclude writtenPosts editedPosts)

