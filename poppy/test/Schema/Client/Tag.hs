{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Tag
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
    TagCreate (..),
    TagRow (..),
    TagSelect (..),
    TagPicked (..),
    tagSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    TagUpdate (..),
    TagTable,
    TagQuery (..),
    emptyQuery,
    tagId
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
import Schema.Tag (TagCreate (..), TagRow (..), TagSelect (..), TagPicked (..), tagSelect, tagSelectColumns, parseTagPicked, TagTable, TagUpdate (..), tagId)
import qualified Poppy.Update as Update

create :: TagCreate -> Db (Either ORMError TagRow)
create = Insert.insert @TagTable @TagRow


createMany :: [TagCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @TagTable


update :: UUID -> TagUpdate -> Db (Either ORMError TagRow)
update = Update.update @TagTable @TagRow


updateMany :: Where TagTable -> TagUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @TagTable


upsert :: TagCreate -> TagUpdate -> Db (Either ORMError TagRow)
upsert = Insert.upsert @TagTable @TagRow ["id"]


data TagQuery select = TagQuery
  { select_ :: select
  , where_ :: Maybe (Where TagTable)
  , orderBy_ :: [OrderBy TagTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: TagQuery OmitSelect
emptyQuery =
  TagQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = TagRow
type instance ResolveSelect TagSelect = TagPicked


class ReadTag select where
  findMany :: TagQuery select -> Db [ResolveSelect select]
  findUnique :: TagQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: TagQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: TagQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: TagQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadTag OmitSelect where
  findMany q =
    Ops.findMany @TagTable @TagRow (applyQuery q)
  findUnique TagQuery {where_} =
    Ops.findUniqueWhere @TagTable @TagRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q =
    Ops.findFirst @TagTable @TagRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadTag TagSelect where
  findMany q@TagQuery {select_} =
    Ops.findManyWith
      (parseTagPicked select_)
      (selectColumns (tagSelectColumns select_) . applyQuery q)
  findUnique TagQuery {select_, where_} =
    case Ops.requireUniqueWhere @TagTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseTagPicked select_)
            (selectColumns (tagSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q@TagQuery {select_} =
    Ops.findFirstWith
      (parseTagPicked select_)
      (selectColumns (tagSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: TagQuery select -> QueryBuilder TagTable -> QueryBuilder TagTable
applyQuery TagQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: TagQuery select -> Db Int
count q = Ops.count @TagTable (applyQuery q)


delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @TagTable


deleteMany :: Where TagTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @TagTable

