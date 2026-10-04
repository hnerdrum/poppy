{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Editor
  ( EditorTable (..),
    EditorRow (..),
    EditorSelect (..),
    EditorPicked (..),
    editorSelect,
    editorSelectColumns,
    parseEditorPicked,
    toEditorPicked,
    EditorCreate (..),
    EditorUpdate (..),
    editorId,
    editorName
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.PG (FromRow (..), RowParser, field)
import Poppy.Core
import Poppy.Select (Picked (..), picked)
import Poppy.Insert (Insertable (..), emptyInsert, set, setMaybe)
import Poppy.Update (Updatable (..), emptyUpdate, setFieldMaybe)

data EditorTable = EditorTable

type instance PrimaryKeyType EditorTable = UUID

instance Entity EditorTable where
  tableName = "test_editor"
  primaryKey = editorId
  tableColumns = ["id", "name"]

instance Insertable EditorTable where
  type CreateInput EditorTable = EditorCreate
  toInsertBuilder input =
    setMaybe editorId input.id $
      set editorName input.name $
      emptyInsert @EditorTable


instance Updatable EditorTable where
  type UpdateInput EditorTable = EditorUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe editorName input.name $
      emptyUpdate @EditorTable


data EditorRow = EditorRow
  { id :: UUID,
    name :: Text
  }
  deriving (Show, Eq)


data EditorCreate = EditorCreate
  { id :: Maybe UUID,
    name :: Text
  }
  deriving (Show, Eq)


data EditorUpdate = EditorUpdate
  { name :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow EditorRow where
  fromRow = EditorRow <$> field <*> field


data EditorSelect = EditorSelect
  { id :: Bool,
    name :: Bool
  }
  deriving (Show, Eq)
data EditorPicked = EditorPicked
  { id :: UUID,
    name :: Picked Text
  }
  deriving (Show, Eq)
editorSelect :: EditorSelect
editorSelect =
  EditorSelect
    { id = False,
      name = False
    }
editorSelectColumns :: EditorSelect -> [Text]
editorSelectColumns select_ =
  fieldColumn editorId
    : concat
      [ [fieldColumn editorName | select_.name]
      ]
parseEditorPicked :: EditorSelect -> RowParser EditorPicked
parseEditorPicked select_ = do
  idVal <- field
  nameVal <- if select_.name then Picked <$> field else pure Skipped
  pure EditorPicked { id = idVal, name = nameVal }
toEditorPicked :: EditorSelect -> EditorRow -> EditorPicked
toEditorPicked select_ row =
  EditorPicked
    { id = row.id,
      name = picked select_.name row.name
    }


editorId :: Field EditorTable UUID
editorId = Field "id" "id"

editorName :: Field EditorTable Text
editorName = Field "name" "name"

