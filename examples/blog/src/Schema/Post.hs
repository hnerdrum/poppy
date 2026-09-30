{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Post
  ( PostTable (..),
    PostRow (..),
    PostSelect (..),
    PostPicked (..),
    postSelect,
    postSelectColumns,
    parsePostPicked,
    toPostPicked,
    PostCreate (..),
    PostUpdate (..),
    postId,
    postAuthorId,
    postTitle,
    postStatus
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.PG (FromRow (..), RowParser, field)
import Poppy.Core
import Poppy.Select (Picked (..), picked)
import Poppy.Insert (Insertable (..), emptyInsert, set, setMaybe)
import Poppy.Update (Updatable (..), emptyUpdate, setFieldMaybe)
import Schema.ArticleStatus (ArticleStatus)

data PostTable = PostTable

type instance PrimaryKeyType PostTable = UUID

instance Entity PostTable where
  tableName = "post"
  primaryKey = postId
  tableColumns = ["id", "author_id", "title", "status"]

instance Insertable PostTable where
  type CreateInput PostTable = PostCreate
  toInsertBuilder input =
    setMaybe postId input.id $
      set postAuthorId input.authorId $
      set postTitle input.title $
      set postStatus input.status $
      emptyInsert @PostTable


instance Updatable PostTable where
  type UpdateInput PostTable = PostUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe postAuthorId input.authorId $
      setFieldMaybe postTitle input.title $
      setFieldMaybe postStatus input.status $
      emptyUpdate @PostTable


data PostRow = PostRow
  { id :: UUID,
    authorId :: UUID,
    title :: Text,
    status :: ArticleStatus
  }
  deriving (Show, Eq)


data PostCreate = PostCreate
  { id :: Maybe UUID,
    authorId :: UUID,
    title :: Text,
    status :: ArticleStatus
  }
  deriving (Show, Eq)


data PostUpdate = PostUpdate
  { authorId :: Maybe UUID,
    title :: Maybe Text,
    status :: Maybe ArticleStatus
  }
  deriving (Show, Eq)


instance FromRow PostRow where
  fromRow = PostRow <$> field <*> field <*> field <*> field


data PostSelect = PostSelect
  { id :: Bool,
    authorId :: Bool,
    title :: Bool,
    status :: Bool
  }
  deriving (Show, Eq)
data PostPicked = PostPicked
  { id :: UUID,
    authorId :: Picked UUID,
    title :: Picked Text,
    status :: Picked ArticleStatus
  }
  deriving (Show, Eq)
postSelect :: PostSelect
postSelect =
  PostSelect
    { id = False,
      authorId = False,
      title = False,
      status = False
    }
postSelectColumns :: PostSelect -> [Text]
postSelectColumns select_ =
  fieldColumn postId
    : concat
      [ [fieldColumn postAuthorId | select_.authorId]
      , [fieldColumn postTitle | select_.title]
      , [fieldColumn postStatus | select_.status]
      ]
parsePostPicked :: PostSelect -> RowParser PostPicked
parsePostPicked select_ = do
  idVal <- field
  authorIdVal <- if select_.authorId then Picked <$> field else pure Skipped
  titleVal <- if select_.title then Picked <$> field else pure Skipped
  statusVal <- if select_.status then Picked <$> field else pure Skipped
  pure PostPicked { id = idVal, authorId = authorIdVal, title = titleVal, status = statusVal }
toPostPicked :: PostSelect -> PostRow -> PostPicked
toPostPicked select_ row =
  PostPicked
    { id = row.id,
      authorId = picked select_.authorId row.authorId,
      title = picked select_.title row.title,
      status = picked select_.status row.status
    }


postId :: Field PostTable UUID
postId = Field "id" "id"

postAuthorId :: Field PostTable UUID
postAuthorId = Field "authorId" "author_id"

postTitle :: Field PostTable Text
postTitle = Field "title" "title"

postStatus :: Field PostTable ArticleStatus
postStatus = Field "status" "status"

