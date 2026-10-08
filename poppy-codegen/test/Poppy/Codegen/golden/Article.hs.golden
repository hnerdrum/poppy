{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Schema.Article
  ( ArticleTable (..),
    ArticleRow (..),
    ArticleSelect (..),
    ArticlePicked (..),
    articleSelect,
    articleSelectColumns,
    parseArticlePicked,
    toArticlePicked,
    ArticleCreate (..),
    ArticleUpdate (..),
    articleId,
    articleAuthorId,
    articleTitle
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

data ArticleTable = ArticleTable

type instance PrimaryKeyType ArticleTable = UUID

type instance ModelTable "Article" = ArticleTable

instance Entity ArticleTable where
  tableName = "test_article"
  primaryKey = articleId
  tableColumns = ["id", "author_id", "title"]

instance Insertable ArticleTable where
  type CreateInput ArticleTable = ArticleCreate
  toInsertBuilder input =
    setMaybe articleId input.id $
      set articleAuthorId input.authorId $
      set articleTitle input.title $
      emptyInsert @ArticleTable


instance Updatable ArticleTable where
  type UpdateInput ArticleTable = ArticleUpdate
  updatedAtField = Nothing
  toUpdateBuilder input =
    setFieldMaybe articleAuthorId input.authorId $
      setFieldMaybe articleTitle input.title $
      emptyUpdate @ArticleTable


data ArticleRow = ArticleRow
  { id :: UUID,
    authorId :: UUID,
    title :: Text
  }
  deriving (Show, Eq)


data ArticleCreate = ArticleCreate
  { id :: Maybe UUID,
    authorId :: UUID,
    title :: Text
  }
  deriving (Show, Eq)


data ArticleUpdate = ArticleUpdate
  { authorId :: Maybe UUID,
    title :: Maybe Text
  }
  deriving (Show, Eq)


instance FromRow ArticleRow where
  fromRow = ArticleRow <$> field <*> field <*> field


data ArticleSelect = ArticleSelect
  { id :: Bool,
    authorId :: Bool,
    title :: Bool
  }
  deriving (Show, Eq)
data ArticlePicked = ArticlePicked
  { id :: UUID,
    authorId :: Picked UUID,
    title :: Picked Text
  }
  deriving (Show, Eq)
articleSelect :: ArticleSelect
articleSelect =
  ArticleSelect
    { id = False,
      authorId = False,
      title = False
    }
articleSelectColumns :: ArticleSelect -> [Text]
articleSelectColumns select_ =
  fieldColumn articleId
    : concat
      [ [fieldColumn articleAuthorId | select_.authorId]
      , [fieldColumn articleTitle | select_.title]
      ]
parseArticlePicked :: ArticleSelect -> RowParser ArticlePicked
parseArticlePicked select_ = do
  idVal <- field
  authorIdVal <- if select_.authorId then Picked <$> field else pure Skipped
  titleVal <- if select_.title then Picked <$> field else pure Skipped
  pure ArticlePicked { id = idVal, authorId = authorIdVal, title = titleVal }
toArticlePicked :: ArticleSelect -> ArticleRow -> ArticlePicked
toArticlePicked select_ row =
  ArticlePicked
    { id = row.id,
      authorId = picked select_.authorId row.authorId,
      title = picked select_.title row.title
    }


articleId :: Field ArticleTable UUID
articleId = Field "id" "id"

articleAuthorId :: Field ArticleTable UUID
articleAuthorId = Field "authorId" "author_id"

articleTitle :: Field ArticleTable Text
articleTitle = Field "title" "title"

