{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Book
  ( BookTable (..),
    BookRow (..),
    BookSelect (..),
    BookPicked (..),
    bookSelect,
    bookSelectColumns,
    parseBookPicked,
    toBookPicked,
    BookCreate (..),
    BookUpdate (..),
    bookId,
    bookShelfId,
    bookTitle,
    bookChapters
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import ORM.PG (FromRow (..), RowParser, field)
import ORM.Core
import ORM.Select (Picked (..), picked)
import ORM.Insert (Insertable (..), emptyInsert, set, setMaybe)
import ORM.Update (Updatable (..), emptyUpdate, setFieldMaybe)
import ORM.Relation (HasMany (..), JoinType (..))
import Schema.Chapter (ChapterTable, chapterBookRef)

data BookTable = BookTable

type instance PrimaryKeyType BookTable = UUID

instance Entity BookTable where
  tableName = "test_book"
  primaryKey = bookId
  tableColumns = ["id", "shelf_id", "title"]

instance Insertable BookTable where
  type CreateInput BookTable = BookCreate
  toInsertBuilder input =
    setMaybe bookId input.id $
      set bookShelfId input.shelfId $
      set bookTitle input.title $
      emptyInsert @BookTable


instance Updatable BookTable where
  type UpdateInput BookTable = BookUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe bookShelfId input.shelfId $
      setFieldMaybe bookTitle input.title $
      emptyUpdate @BookTable


data BookRow = BookRow
  { id :: UUID,
    shelfId :: UUID,
    title :: Text
  }
  deriving (Show, Eq)


data BookCreate = BookCreate
  { id :: Maybe UUID,
    shelfId :: UUID,
    title :: Text
  }
  deriving (Show, Eq)


data BookUpdate = BookUpdate
  { shelfId :: Maybe UUID,
    title :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow BookRow where
  fromRow = BookRow <$> field <*> field <*> field


data BookSelect = BookSelect
  { id :: Bool,
    shelfId :: Bool,
    title :: Bool
  }
  deriving (Show, Eq)
data BookPicked = BookPicked
  { id :: UUID,
    shelfId :: Picked UUID,
    title :: Picked Text
  }
  deriving (Show, Eq)
bookSelect :: BookSelect
bookSelect =
  BookSelect
    { id = False,
      shelfId = False,
      title = False
    }
bookSelectColumns :: BookSelect -> [Text]
bookSelectColumns select_ =
  fieldColumn bookId
    : concat
      [ [fieldColumn bookShelfId | select_.shelfId]
      , [fieldColumn bookTitle | select_.title]
      ]
parseBookPicked :: BookSelect -> RowParser BookPicked
parseBookPicked select_ = do
  idVal <- field
  shelfIdVal <- if select_.shelfId then Picked <$> field else pure Skipped
  titleVal <- if select_.title then Picked <$> field else pure Skipped
  pure BookPicked { id = idVal, shelfId = shelfIdVal, title = titleVal }
toBookPicked :: BookSelect -> BookRow -> BookPicked
toBookPicked select_ row =
  BookPicked
    { id = row.id,
      shelfId = picked select_.shelfId row.shelfId,
      title = picked select_.title row.title
    }


bookId :: Field BookTable UUID
bookId = Field "id" "id"

bookShelfId :: Field BookTable UUID
bookShelfId = Field "shelfId" "shelf_id"

bookTitle :: Field BookTable Text
bookTitle = Field "title" "title"

bookChapters :: HasMany BookTable ChapterTable UUID
bookChapters =
  HasMany
    { localKey = bookId,
      foreignKey = chapterBookRef,
      joinType = LeftJoin
    }

