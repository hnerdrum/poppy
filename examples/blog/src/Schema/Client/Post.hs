{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Post
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
    PostCreate (..),
    PostRow (..),
    PostSelect (..),
    PostPicked (..),
    postSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    PostUpdate (..),
    PostTable,
    PostQuery (..),
    emptyQuery,
    postId
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
import Schema.Post (PostCreate (..), PostRow (..), PostSelect (..), PostPicked (..), postSelect, postSelectColumns, parsePostPicked, PostTable, PostUpdate (..), postId)
import qualified Poppy.Update as Update

create :: PostCreate -> Db (Either ORMError PostRow)
create = Insert.insert @PostTable @PostRow


createMany :: [PostCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @PostTable


update :: UUID -> PostUpdate -> Db (Either ORMError PostRow)
update = Update.update @PostTable @PostRow


updateMany :: Where PostTable -> PostUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @PostTable


upsert :: PostCreate -> PostUpdate -> Db (Either ORMError PostRow)
upsert = Insert.upsert @PostTable @PostRow ["id"]


data PostQuery select = PostQuery
  { select_ :: select
  , where_ :: Maybe (Where PostTable)
  , orderBy_ :: [OrderBy PostTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: PostQuery OmitSelect
emptyQuery =
  PostQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = PostRow
type instance ResolveSelect PostSelect = PostPicked


class ReadPost select where
  findMany :: PostQuery select -> Db [ResolveSelect select]
  findUnique :: PostQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: PostQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: PostQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: PostQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadPost OmitSelect where
  findMany q =
    Ops.findMany @PostTable @PostRow (applyQuery q)
  findUnique PostQuery {where_} =
    Ops.findUniqueWhere @PostTable @PostRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q =
    Ops.findFirst @PostTable @PostRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadPost PostSelect where
  findMany q@PostQuery {select_} =
    Ops.findManyWith
      (parsePostPicked select_)
      (selectColumns (postSelectColumns select_) . applyQuery q)
  findUnique PostQuery {select_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parsePostPicked select_)
            (selectColumns (postSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q@PostQuery {select_} =
    Ops.findFirstWith
      (parsePostPicked select_)
      (selectColumns (postSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: PostQuery select -> QueryBuilder PostTable -> QueryBuilder PostTable
applyQuery PostQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: PostQuery select -> Db Int
count q = Ops.count @PostTable (applyQuery q)


delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @PostTable


deleteMany :: Where PostTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @PostTable

