{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Task
  ( TaskTable (..),
    TaskRow (..),
    TaskSelect (..),
    TaskPicked (..),
    taskSelect,
    taskSelectColumns,
    parseTaskPicked,
    toTaskPicked,
    TaskCreate (..),
    TaskUpdate (..),
    taskId,
    taskCreatedAt,
    taskUpdatedAt,
    taskTitle,
    taskDone
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Poppy.PG (FromRow (..), RowParser, field)
import Poppy.Core
import Poppy.Select (Picked (..), picked)
import Poppy.Insert (Insertable (..), emptyInsert, set, setMaybe)
import Poppy.Update (Updatable (..), emptyUpdate, setFieldMaybe)

data TaskTable = TaskTable

type instance PrimaryKeyType TaskTable = UUID

instance Entity TaskTable where
  tableName = "task"
  primaryKey = taskId
  tableColumns = ["id", "created_at", "updated_at", "title", "done"]

instance Insertable TaskTable where
  type CreateInput TaskTable = TaskCreate
  toInsertBuilder input =
    setMaybe taskId input.id $
      setMaybe taskCreatedAt input.createdAt $
      setMaybe taskUpdatedAt input.updatedAt $
      set taskTitle input.title $
      set taskDone input.done $
      emptyInsert @TaskTable


instance Updatable TaskTable where
  type UpdateInput TaskTable = TaskUpdate
  updatedAtField = Just taskUpdatedAt
  toUpdateBuilder input =
    setFieldMaybe taskCreatedAt input.createdAt $
      setFieldMaybe taskUpdatedAt input.updatedAt $
      setFieldMaybe taskTitle input.title $
      setFieldMaybe taskDone input.done $
      emptyUpdate @TaskTable


data TaskRow = TaskRow
  { id :: UUID,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    title :: Text,
    done :: Bool
  }
  deriving (Show, Eq)


data TaskCreate = TaskCreate
  { id :: Maybe UUID,
    createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime,
    title :: Text,
    done :: Bool
  }
  deriving (Show, Eq)


data TaskUpdate = TaskUpdate
  { createdAt :: Maybe UTCTime,
    updatedAt :: Maybe UTCTime,
    title :: Maybe Text,
    done :: Maybe Bool
  }
  deriving (Show, Eq)


instance FromRow TaskRow where
  fromRow = TaskRow <$> field <*> field <*> field <*> field <*> field


data TaskSelect = TaskSelect
  { id :: Bool,
    createdAt :: Bool,
    updatedAt :: Bool,
    title :: Bool,
    done :: Bool
  }
  deriving (Show, Eq)
data TaskPicked = TaskPicked
  { id :: UUID,
    createdAt :: Picked UTCTime,
    updatedAt :: Picked UTCTime,
    title :: Picked Text,
    done :: Picked Bool
  }
  deriving (Show, Eq)
taskSelect :: TaskSelect
taskSelect =
  TaskSelect
    { id = False,
      createdAt = False,
      updatedAt = False,
      title = False,
      done = False
    }
taskSelectColumns :: TaskSelect -> [Text]
taskSelectColumns select_ =
  fieldColumn taskId
    : concat
      [ [fieldColumn taskCreatedAt | select_.createdAt]
      , [fieldColumn taskUpdatedAt | select_.updatedAt]
      , [fieldColumn taskTitle | select_.title]
      , [fieldColumn taskDone | select_.done]
      ]
parseTaskPicked :: TaskSelect -> RowParser TaskPicked
parseTaskPicked select_ = do
  idVal <- field
  createdAtVal <- if select_.createdAt then Picked <$> field else pure Skipped
  updatedAtVal <- if select_.updatedAt then Picked <$> field else pure Skipped
  titleVal <- if select_.title then Picked <$> field else pure Skipped
  doneVal <- if select_.done then Picked <$> field else pure Skipped
  pure TaskPicked { id = idVal, createdAt = createdAtVal, updatedAt = updatedAtVal, title = titleVal, done = doneVal }
toTaskPicked :: TaskSelect -> TaskRow -> TaskPicked
toTaskPicked select_ row =
  TaskPicked
    { id = row.id,
      createdAt = picked select_.createdAt row.createdAt,
      updatedAt = picked select_.updatedAt row.updatedAt,
      title = picked select_.title row.title,
      done = picked select_.done row.done
    }


taskId :: Field TaskTable UUID
taskId = Field "id" "id"

taskCreatedAt :: Field TaskTable UTCTime
taskCreatedAt = Field "createdAt" "created_at"

taskUpdatedAt :: Field TaskTable UTCTime
taskUpdatedAt = Field "updatedAt" "updated_at"

taskTitle :: Field TaskTable Text
taskTitle = Field "title" "title"

taskDone :: Field TaskTable Bool
taskDone = Field "done" "done"

