{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Shelf
  ( ShelfTable (..),
    ShelfRow (..),
    ShelfSelect (..),
    ShelfPicked (..),
    shelfSelect,
    shelfSelectColumns,
    parseShelfPicked,
    toShelfPicked,
    ShelfCreate (..),
    ShelfUpdate (..),
    shelfId,
    shelfName
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.Internal.Generated
  ( FromRow (..),
    RowParser,
    field,
    Entity (..),
    Field (..),
    PrimaryKeyType,
    ModelTable,
    Picked (..),
    picked,
    Insertable (..),
    emptyInsert,
    set,
    setMaybe,
    Updatable (..),
    emptyUpdate,
    setFieldMaybe
  )

data ShelfTable = ShelfTable

type instance PrimaryKeyType ShelfTable = UUID

type instance ModelTable "Shelf" = ShelfTable

instance Entity ShelfTable where
  tableName = "test_shelf"
  primaryKey = shelfId
  tableColumns = ["id", "name"]

instance Insertable ShelfTable where
  type CreateInput ShelfTable = ShelfCreate
  toInsertBuilder input =
    setMaybe shelfId input.id $
      set shelfName input.name $
      emptyInsert @ShelfTable


instance Updatable ShelfTable where
  type UpdateInput ShelfTable = ShelfUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe shelfName input.name $
      emptyUpdate @ShelfTable


data ShelfRow = ShelfRow
  { id :: UUID,
    name :: Text
  }
  deriving (Show, Eq)


data ShelfCreate = ShelfCreate
  { id :: Maybe UUID,
    name :: Text
  }
  deriving (Show, Eq)


data ShelfUpdate = ShelfUpdate
  { name :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow ShelfRow where
  fromRow = ShelfRow <$> field <*> field


data ShelfSelect = ShelfSelect
  { id :: Bool,
    name :: Bool
  }
  deriving (Show, Eq)
data ShelfPicked = ShelfPicked
  { id :: UUID,
    name :: Picked Text
  }
  deriving (Show, Eq)
shelfSelect :: ShelfSelect
shelfSelect =
  ShelfSelect
    { id = False,
      name = False
    }
shelfSelectColumns :: ShelfSelect -> [Text]
shelfSelectColumns select_ =
  fieldColumn shelfId
    : concat
      [ [fieldColumn shelfName | select_.name]
      ]
parseShelfPicked :: ShelfSelect -> RowParser ShelfPicked
parseShelfPicked select_ = do
  idVal <- field
  nameVal <- if select_.name then Picked <$> field else pure Skipped
  pure ShelfPicked { id = idVal, name = nameVal }
toShelfPicked :: ShelfSelect -> ShelfRow -> ShelfPicked
toShelfPicked select_ row =
  ShelfPicked
    { id = row.id,
      name = picked select_.name row.name
    }


shelfId :: Field ShelfTable UUID
shelfId = Field "id" "id"

shelfName :: Field ShelfTable Text
shelfName = Field "name" "name"

