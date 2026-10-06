{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    TagUnique (..),
    TagUniqueKey (..),
    TagUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    tagId
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
import Schema.Tag (TagCreate (..), TagRow (..), TagSelect (..), TagPicked (..), tagSelect, tagSelectColumns, parseTagPicked, TagTable, TagUpdate (..), tagId)
import qualified Poppy.Update as Update

data TagUnique
  = ById UUID
  deriving (Eq, Show)

data TagUniqueKey
  = OnId
  deriving (Eq, Show)

tagUniqueWhere :: TagUnique -> Where TagTable
tagUniqueWhere = \case
  ById v1 -> eq tagId v1

tagConflictCols :: TagUniqueKey -> [Text]
tagConflictCols = \case
  OnId -> ["id"]


create :: TagCreate -> Db (Either ORMError TagRow)
create = Insert.insert @TagTable @TagRow


createMany :: [TagCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @TagTable


update :: TagUnique -> TagUpdate -> Db (Either ORMError TagRow)
update key input =
  Update.updateWhere @TagTable @TagRow (tagUniqueWhere key) input


updateMany :: Where TagTable -> TagUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @TagTable


upsert :: TagUniqueKey -> TagCreate -> TagUpdate -> Db (Either ORMError TagRow)
upsert key createInput updateInput =
  Insert.upsert @TagTable @TagRow (tagConflictCols key) createInput updateInput


data TagQuery select = TagQuery
  { select_ :: select
  , where_ :: Maybe (Where TagTable)
  , orderBy_ :: [OrderBy TagTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data TagUniqueQuery select = TagUniqueQuery
  { select_ :: select
  , where_ :: TagUnique
  }


emptyQuery :: TagQuery OmitSelect
emptyQuery =
  TagQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: TagUnique -> TagUniqueQuery OmitSelect
uniqueQuery key =
  TagUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = TagRow
type instance ResolveSelect TagSelect = TagPicked


class ReadTag select where
  findMany :: TagQuery select -> Db [ResolveSelect select]
  findUnique :: TagUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: TagUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: TagQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: TagQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadTag OmitSelect where
  findMany q =
    Ops.findMany @TagTable @TagRow (applyQuery q)
  findUnique TagUniqueQuery {where_} = do
    let w = tagUniqueWhere where_
    rows <- Ops.findMany @TagTable @TagRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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
  findUnique TagUniqueQuery {where_, select_} = do
    let w = tagUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseTagPicked select_)
        (selectColumns (tagSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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


delete :: TagUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @TagTable (tagUniqueWhere key)


deleteMany :: Where TagTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @TagTable

