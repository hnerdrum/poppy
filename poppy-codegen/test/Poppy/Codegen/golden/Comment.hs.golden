{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Comment
  ( CommentTable (..),
    CommentRow (..),
    CommentSelect (..),
    CommentPicked (..),
    commentSelect,
    commentSelectColumns,
    parseCommentPicked,
    toCommentPicked,
    CommentCreate (..),
    CommentUpdate (..),
    commentId,
    commentParentId,
    commentBody
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.PG (FromRow (..), RowParser, field)
import Poppy.Core
import Poppy.Include (ModelTable)
import Poppy.Select (Picked (..), picked)
import Poppy.Insert (Insertable (..), emptyInsert, set, setMaybe, setNullable)
import Poppy.Update (Updatable (..), emptyUpdate, setFieldMaybe, setFieldNullable)

data CommentTable = CommentTable

type instance PrimaryKeyType CommentTable = UUID

type instance ModelTable "Comment" = CommentTable

instance Entity CommentTable where
  tableName = "test_comment"
  primaryKey = commentId
  tableColumns = ["id", "parent_id", "body"]

instance Insertable CommentTable where
  type CreateInput CommentTable = CommentCreate
  toInsertBuilder input =
    setMaybe commentId input.id $
      setNullable commentParentId input.parentId $
      set commentBody input.body $
      emptyInsert @CommentTable


instance Updatable CommentTable where
  type UpdateInput CommentTable = CommentUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldNullable commentParentId input.parentId $
      setFieldMaybe commentBody input.body $
      emptyUpdate @CommentTable


data CommentRow = CommentRow
  { id :: UUID,
    parentId :: Maybe UUID,
    body :: Text
  }
  deriving (Show, Eq)


data CommentCreate = CommentCreate
  { id :: Maybe UUID,
    parentId :: NullableValue UUID,
    body :: Text
  }
  deriving (Show, Eq)


data CommentUpdate = CommentUpdate
  { parentId :: NullableValue UUID,
    body :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow CommentRow where
  fromRow = CommentRow <$> field <*> field <*> field


data CommentSelect = CommentSelect
  { id :: Bool,
    parentId :: Bool,
    body :: Bool
  }
  deriving (Show, Eq)
data CommentPicked = CommentPicked
  { id :: UUID,
    parentId :: Picked (Maybe UUID),
    body :: Picked Text
  }
  deriving (Show, Eq)
commentSelect :: CommentSelect
commentSelect =
  CommentSelect
    { id = False,
      parentId = False,
      body = False
    }
commentSelectColumns :: CommentSelect -> [Text]
commentSelectColumns select_ =
  fieldColumn commentId
    : concat
      [ [fieldColumn commentParentId | select_.parentId]
      , [fieldColumn commentBody | select_.body]
      ]
parseCommentPicked :: CommentSelect -> RowParser CommentPicked
parseCommentPicked select_ = do
  idVal <- field
  parentIdVal <- if select_.parentId then Picked <$> field else pure Skipped
  bodyVal <- if select_.body then Picked <$> field else pure Skipped
  pure CommentPicked { id = idVal, parentId = parentIdVal, body = bodyVal }
toCommentPicked :: CommentSelect -> CommentRow -> CommentPicked
toCommentPicked select_ row =
  CommentPicked
    { id = row.id,
      parentId = picked select_.parentId row.parentId,
      body = picked select_.body row.body
    }


commentId :: Field CommentTable UUID
commentId = Field "id" "id"

commentParentId :: Field CommentTable UUID
commentParentId = Field "parentId" "parent_id"

commentBody :: Field CommentTable Text
commentBody = Field "body" "body"

