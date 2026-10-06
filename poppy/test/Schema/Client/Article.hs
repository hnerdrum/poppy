{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Article
  ( create,
    createMany,
    update,
    updateMany,
    upsert,
    findMany,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstOrFail,
    count,
    delete,
    deleteMany,
    ArticleCreate (..),
    ArticleRow (..),
    ArticleSelect (..),
    ArticlePicked (..),
    articleSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    ArticleUpdate (..),
    ArticleTable,
    ArticleQuery (..),
    ArticleUnique (..),
    ArticleUniqueKey (..),
    ArticleUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    articleId
  )

where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.Db (Db)
import Poppy.Errors (ORMError (..), fromUniqueRows, requireFound, uniqueOrFail)
import qualified Poppy.Delete as Delete
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, QueryBuilder, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.Where (Where, eq)
import Schema.Article (ArticleCreate (..), ArticleRow (..), ArticleSelect (..), ArticlePicked (..), articleSelect, articleSelectColumns, parseArticlePicked, ArticleTable, ArticleUpdate (..), articleId)
import qualified Poppy.Update as Update

data ArticleUnique
  = ById UUID
  deriving (Eq, Show)

data ArticleUniqueKey
  = OnId
  deriving (Eq, Show)

articleUniqueWhere :: ArticleUnique -> Where ArticleTable
articleUniqueWhere = \case
  ById v1 -> eq articleId v1

articleConflictCols :: ArticleUniqueKey -> [Text]
articleConflictCols = \case
  OnId -> ["id"]


create :: ArticleCreate -> Db (Either ORMError ArticleRow)
create = Insert.insert @ArticleTable @ArticleRow


createMany :: [ArticleCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ArticleTable


update :: ArticleUnique -> ArticleUpdate -> Db (Either ORMError ArticleRow)
update key input =
  Update.updateWhere @ArticleTable @ArticleRow (articleUniqueWhere key) input


updateMany :: Where ArticleTable -> ArticleUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @ArticleTable


upsert :: ArticleUniqueKey -> ArticleCreate -> ArticleUpdate -> Db (Either ORMError ArticleRow)
upsert key createInput updateInput =
  Insert.upsert @ArticleTable @ArticleRow (articleConflictCols key) createInput updateInput


data ArticleQuery select = ArticleQuery
  { select_ :: select
  , where_ :: Maybe (Where ArticleTable)
  , orderBy_ :: [OrderBy ArticleTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data ArticleUniqueQuery select = ArticleUniqueQuery
  { select_ :: select
  , where_ :: ArticleUnique
  }


emptyQuery :: ArticleQuery OmitSelect
emptyQuery =
  ArticleQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: ArticleUnique -> ArticleUniqueQuery OmitSelect
uniqueQuery key =
  ArticleUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = ArticleRow
type instance ResolveSelect ArticleSelect = ArticlePicked


class ReadArticle select where
  findMany :: ArticleQuery select -> Db [ResolveSelect select]
  findUnique :: ArticleUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: ArticleUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: ArticleQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: ArticleQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadArticle OmitSelect where
  findMany q =
    Ops.findMany @ArticleTable @ArticleRow (applyQuery q)
  findUnique ArticleUniqueQuery {where_} = do
    let w = articleUniqueWhere where_
    rows <- Ops.findMany @ArticleTable @ArticleRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q =
    Ops.findFirst @ArticleTable @ArticleRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadArticle ArticleSelect where
  findMany q@ArticleQuery {select_} =
    Ops.findManyWith
      (parseArticlePicked select_)
      (selectColumns (articleSelectColumns select_) . applyQuery q)
  findUnique ArticleUniqueQuery {where_, select_} = do
    let w = articleUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseArticlePicked select_)
        (selectColumns (articleSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q@ArticleQuery {select_} =
    Ops.findFirstWith
      (parseArticlePicked select_)
      (selectColumns (articleSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: ArticleQuery select -> QueryBuilder ArticleTable -> QueryBuilder ArticleTable
applyQuery ArticleQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: ArticleQuery select -> Db Int
count q = Ops.count @ArticleTable (applyQuery q)


delete :: ArticleUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @ArticleTable (articleUniqueWhere key)


deleteMany :: Where ArticleTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ArticleTable

