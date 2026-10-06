{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Author
  ( AuthorTable (..),
    AuthorRow (..),
    AuthorSelect (..),
    AuthorPicked (..),
    authorSelect,
    authorSelectColumns,
    parseAuthorPicked,
    toAuthorPicked,
    AuthorCreate (..),
    AuthorUpdate (..),
    authorId,
    authorName
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.PG (FromRow (..), RowParser, field)
import Poppy.Core
import Poppy.Include (ModelTable)
import Poppy.Select (Picked (..), picked)
import Poppy.Insert (Insertable (..), emptyInsert, set, setMaybe)
import Poppy.Update (Updatable (..), emptyUpdate, setFieldMaybe)

data AuthorTable = AuthorTable

type instance PrimaryKeyType AuthorTable = UUID

type instance ModelTable "Author" = AuthorTable

instance Entity AuthorTable where
  tableName = "author"
  primaryKey = authorId
  tableColumns = ["id", "name"]

instance Insertable AuthorTable where
  type CreateInput AuthorTable = AuthorCreate
  toInsertBuilder input =
    setMaybe authorId input.id $
      set authorName input.name $
      emptyInsert @AuthorTable


instance Updatable AuthorTable where
  type UpdateInput AuthorTable = AuthorUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe authorName input.name $
      emptyUpdate @AuthorTable


data AuthorRow = AuthorRow
  { id :: UUID,
    name :: Text
  }
  deriving (Show, Eq)


data AuthorCreate = AuthorCreate
  { id :: Maybe UUID,
    name :: Text
  }
  deriving (Show, Eq)


data AuthorUpdate = AuthorUpdate
  { name :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow AuthorRow where
  fromRow = AuthorRow <$> field <*> field


data AuthorSelect = AuthorSelect
  { id :: Bool,
    name :: Bool
  }
  deriving (Show, Eq)
data AuthorPicked = AuthorPicked
  { id :: UUID,
    name :: Picked Text
  }
  deriving (Show, Eq)
authorSelect :: AuthorSelect
authorSelect =
  AuthorSelect
    { id = False,
      name = False
    }
authorSelectColumns :: AuthorSelect -> [Text]
authorSelectColumns select_ =
  fieldColumn authorId
    : concat
      [ [fieldColumn authorName | select_.name]
      ]
parseAuthorPicked :: AuthorSelect -> RowParser AuthorPicked
parseAuthorPicked select_ = do
  idVal <- field
  nameVal <- if select_.name then Picked <$> field else pure Skipped
  pure AuthorPicked { id = idVal, name = nameVal }
toAuthorPicked :: AuthorSelect -> AuthorRow -> AuthorPicked
toAuthorPicked select_ row =
  AuthorPicked
    { id = row.id,
      name = picked select_.name row.name
    }


authorId :: Field AuthorTable UUID
authorId = Field "id" "id"

authorName :: Field AuthorTable Text
authorName = Field "name" "name"

