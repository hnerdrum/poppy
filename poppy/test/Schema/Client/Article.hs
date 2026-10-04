{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    emptyQuery,
    articleId
  )

where

import Data.UUID (UUID)
import Poppy.Db (Db)
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Delete as Delete
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, QueryBuilder, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.Where (Where)
import Schema.Article (ArticleCreate (..), ArticleRow (..), ArticleSelect (..), ArticlePicked (..), articleSelect, articleSelectColumns, parseArticlePicked, ArticleTable, ArticleUpdate (..), articleId)
import qualified Poppy.Update as Update

create :: ArticleCreate -> Db (Either ORMError ArticleRow)
create = Insert.insert @ArticleTable @ArticleRow


createMany :: [ArticleCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ArticleTable


update :: UUID -> ArticleUpdate -> Db (Either ORMError ArticleRow)
update = Update.update @ArticleTable @ArticleRow


updateMany :: Where ArticleTable -> ArticleUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @ArticleTable


upsert :: ArticleCreate -> ArticleUpdate -> Db (Either ORMError ArticleRow)
upsert = Insert.upsert @ArticleTable @ArticleRow ["id"]


data ArticleQuery select = ArticleQuery
  { select_ :: select
  , where_ :: Maybe (Where ArticleTable)
  , orderBy_ :: [OrderBy ArticleTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: ArticleQuery OmitSelect
emptyQuery =
  ArticleQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = ArticleRow
type instance ResolveSelect ArticleSelect = ArticlePicked


class ReadArticle select where
  findMany :: ArticleQuery select -> Db [ResolveSelect select]
  findUnique :: ArticleQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: ArticleQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: ArticleQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: ArticleQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadArticle OmitSelect where
  findMany q =
    Ops.findMany @ArticleTable @ArticleRow (applyQuery q)
  findUnique ArticleQuery {where_} =
    Ops.findUniqueWhere @ArticleTable @ArticleRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique ArticleQuery {select_, where_} =
    case Ops.requireUniqueWhere @ArticleTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseArticlePicked select_)
            (selectColumns (articleSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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


delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @ArticleTable


deleteMany :: Where ArticleTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ArticleTable

