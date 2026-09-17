{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Chapter
  ( ChapterTable (..),
    ChapterRow (..),
    ChapterSelect (..),
    ChapterPicked (..),
    chapterSelect,
    chapterSelectColumns,
    parseChapterPicked,
    toChapterPicked,
    ChapterCreate (..),
    ChapterUpdate (..),
    chapterId,
    chapterBookRef,
    chapterHeading,
    chapterSections
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
import Schema.Section (SectionTable, sectionChapterRef)

data ChapterTable = ChapterTable

type instance PrimaryKeyType ChapterTable = UUID

instance Entity ChapterTable where
  tableName = "test_chapter"
  primaryKey = chapterId
  tableColumns = ["id", "book_id", "heading"]

instance Insertable ChapterTable where
  type CreateInput ChapterTable = ChapterCreate
  toInsertBuilder input =
    setMaybe chapterId input.id $
      set chapterBookRef input.bookRef $
      set chapterHeading input.heading $
      emptyInsert @ChapterTable


instance Updatable ChapterTable where
  type UpdateInput ChapterTable = ChapterUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe chapterBookRef input.bookRef $
      setFieldMaybe chapterHeading input.heading $
      emptyUpdate @ChapterTable


data ChapterRow = ChapterRow
  { id :: UUID,
    bookRef :: UUID,
    heading :: Text
  }
  deriving (Show, Eq)


data ChapterCreate = ChapterCreate
  { id :: Maybe UUID,
    bookRef :: UUID,
    heading :: Text
  }
  deriving (Show, Eq)


data ChapterUpdate = ChapterUpdate
  { bookRef :: Maybe UUID,
    heading :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow ChapterRow where
  fromRow = ChapterRow <$> field <*> field <*> field


data ChapterSelect = ChapterSelect
  { id :: Bool,
    bookRef :: Bool,
    heading :: Bool
  }
  deriving (Show, Eq)
data ChapterPicked = ChapterPicked
  { id :: UUID,
    bookRef :: Picked UUID,
    heading :: Picked Text
  }
  deriving (Show, Eq)
chapterSelect :: ChapterSelect
chapterSelect =
  ChapterSelect
    { id = False,
      bookRef = False,
      heading = False
    }
chapterSelectColumns :: ChapterSelect -> [Text]
chapterSelectColumns select_ =
  fieldColumn chapterId
    : concat
      [ [fieldColumn chapterBookRef | select_.bookRef]
      , [fieldColumn chapterHeading | select_.heading]
      ]
parseChapterPicked :: ChapterSelect -> RowParser ChapterPicked
parseChapterPicked select_ = do
  idVal <- field
  bookRefVal <- if select_.bookRef then Picked <$> field else pure Skipped
  headingVal <- if select_.heading then Picked <$> field else pure Skipped
  pure ChapterPicked { id = idVal, bookRef = bookRefVal, heading = headingVal }
toChapterPicked :: ChapterSelect -> ChapterRow -> ChapterPicked
toChapterPicked select_ row =
  ChapterPicked
    { id = row.id,
      bookRef = picked select_.bookRef row.bookRef,
      heading = picked select_.heading row.heading
    }


chapterId :: Field ChapterTable UUID
chapterId = Field "id" "id"

chapterBookRef :: Field ChapterTable UUID
chapterBookRef = Field "bookRef" "book_id"

chapterHeading :: Field ChapterTable Text
chapterHeading = Field "heading" "heading"

chapterSections :: HasMany ChapterTable SectionTable UUID
chapterSections =
  HasMany
    { localKey = chapterId,
      foreignKey = sectionChapterRef,
      joinType = LeftJoin
    }

