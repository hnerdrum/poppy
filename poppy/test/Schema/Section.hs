{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Section
  ( SectionTable (..),
    SectionRow (..),
    SectionSelect (..),
    SectionPicked (..),
    sectionSelect,
    sectionSelectColumns,
    parseSectionPicked,
    toSectionPicked,
    SectionCreate (..),
    SectionUpdate (..),
    sectionId,
    sectionChapterRef,
    sectionLabel
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import ORM.PG (FromRow (..), RowParser, field)
import ORM.Core
import ORM.Select (Picked (..), picked)
import ORM.Insert (Insertable (..), emptyInsert, set, setMaybe)
import ORM.Update (Updatable (..), emptyUpdate, setFieldMaybe)

data SectionTable = SectionTable

type instance PrimaryKeyType SectionTable = UUID

instance Entity SectionTable where
  tableName = "test_section"
  primaryKey = sectionId
  tableColumns = ["id", "chapter_id", "label"]

instance Insertable SectionTable where
  type CreateInput SectionTable = SectionCreate
  toInsertBuilder input =
    setMaybe sectionId input.id $
      set sectionChapterRef input.chapterRef $
      set sectionLabel input.label $
      emptyInsert @SectionTable


instance Updatable SectionTable where
  type UpdateInput SectionTable = SectionUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe sectionChapterRef input.chapterRef $
      setFieldMaybe sectionLabel input.label $
      emptyUpdate @SectionTable


data SectionRow = SectionRow
  { id :: UUID,
    chapterRef :: UUID,
    label :: Text
  }
  deriving (Show, Eq)


data SectionCreate = SectionCreate
  { id :: Maybe UUID,
    chapterRef :: UUID,
    label :: Text
  }
  deriving (Show, Eq)


data SectionUpdate = SectionUpdate
  { chapterRef :: Maybe UUID,
    label :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow SectionRow where
  fromRow = SectionRow <$> field <*> field <*> field


data SectionSelect = SectionSelect
  { id :: Bool,
    chapterRef :: Bool,
    label :: Bool
  }
  deriving (Show, Eq)
data SectionPicked = SectionPicked
  { id :: UUID,
    chapterRef :: Picked UUID,
    label :: Picked Text
  }
  deriving (Show, Eq)
sectionSelect :: SectionSelect
sectionSelect =
  SectionSelect
    { id = False,
      chapterRef = False,
      label = False
    }
sectionSelectColumns :: SectionSelect -> [Text]
sectionSelectColumns select_ =
  fieldColumn sectionId
    : concat
      [ [fieldColumn sectionChapterRef | select_.chapterRef]
      , [fieldColumn sectionLabel | select_.label]
      ]
parseSectionPicked :: SectionSelect -> RowParser SectionPicked
parseSectionPicked select_ = do
  idVal <- field
  chapterRefVal <- if select_.chapterRef then Picked <$> field else pure Skipped
  labelVal <- if select_.label then Picked <$> field else pure Skipped
  pure SectionPicked { id = idVal, chapterRef = chapterRefVal, label = labelVal }
toSectionPicked :: SectionSelect -> SectionRow -> SectionPicked
toSectionPicked select_ row =
  SectionPicked
    { id = row.id,
      chapterRef = picked select_.chapterRef row.chapterRef,
      label = picked select_.label row.label
    }


sectionId :: Field SectionTable UUID
sectionId = Field "id" "id"

sectionChapterRef :: Field SectionTable UUID
sectionChapterRef = Field "chapterRef" "chapter_id"

sectionLabel :: Field SectionTable Text
sectionLabel = Field "label" "label"

