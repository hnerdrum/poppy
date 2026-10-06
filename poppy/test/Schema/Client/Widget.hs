{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Widget
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
    WidgetCreate (..),
    WidgetRow (..),
    WidgetSelect (..),
    WidgetPicked (..),
    widgetSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    WidgetUpdate (..),
    WidgetTable,
    WidgetQuery (..),
    WidgetUnique (..),
    WidgetUniqueKey (..),
    WidgetUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    widgetId
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
import Schema.Widget (WidgetCreate (..), WidgetRow (..), WidgetSelect (..), WidgetPicked (..), widgetSelect, widgetSelectColumns, parseWidgetPicked, WidgetTable, WidgetUpdate (..), widgetId, widgetName)
import qualified Poppy.Update as Update

data WidgetUnique
  = ById UUID
  | ByName Text
  deriving (Eq, Show)

data WidgetUniqueKey
  = OnId
  | OnName
  deriving (Eq, Show)

widgetUniqueWhere :: WidgetUnique -> Where WidgetTable
widgetUniqueWhere = \case
  ById v1 -> eq widgetId v1
  ByName v1 -> eq widgetName v1

widgetConflictCols :: WidgetUniqueKey -> [Text]
widgetConflictCols = \case
  OnId -> ["id"]
  OnName -> ["name"]


create :: WidgetCreate -> Db (Either ORMError WidgetRow)
create = Insert.insert @WidgetTable @WidgetRow


createMany :: [WidgetCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @WidgetTable


update :: WidgetUnique -> WidgetUpdate -> Db (Either ORMError WidgetRow)
update key input =
  Update.updateWhere @WidgetTable @WidgetRow (widgetUniqueWhere key) input


updateMany :: Where WidgetTable -> WidgetUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @WidgetTable


upsert :: WidgetUniqueKey -> WidgetCreate -> WidgetUpdate -> Db (Either ORMError WidgetRow)
upsert key createInput updateInput =
  Insert.upsert @WidgetTable @WidgetRow (widgetConflictCols key) createInput updateInput


data WidgetQuery select = WidgetQuery
  { select_ :: select
  , where_ :: Maybe (Where WidgetTable)
  , orderBy_ :: [OrderBy WidgetTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data WidgetUniqueQuery select = WidgetUniqueQuery
  { select_ :: select
  , where_ :: WidgetUnique
  }


emptyQuery :: WidgetQuery OmitSelect
emptyQuery =
  WidgetQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: WidgetUnique -> WidgetUniqueQuery OmitSelect
uniqueQuery key =
  WidgetUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = WidgetRow
type instance ResolveSelect WidgetSelect = WidgetPicked


class ReadWidget select where
  findMany :: WidgetQuery select -> Db [ResolveSelect select]
  findUnique :: WidgetUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: WidgetUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: WidgetQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: WidgetQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadWidget OmitSelect where
  findMany q =
    Ops.findMany @WidgetTable @WidgetRow (applyQuery q)
  findUnique WidgetUniqueQuery {where_} = do
    let w = widgetUniqueWhere where_
    rows <- Ops.findMany @WidgetTable @WidgetRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q =
    Ops.findFirst @WidgetTable @WidgetRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadWidget WidgetSelect where
  findMany q@WidgetQuery {select_} =
    Ops.findManyWith
      (parseWidgetPicked select_)
      (selectColumns (widgetSelectColumns select_) . applyQuery q)
  findUnique WidgetUniqueQuery {where_, select_} = do
    let w = widgetUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseWidgetPicked select_)
        (selectColumns (widgetSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q@WidgetQuery {select_} =
    Ops.findFirstWith
      (parseWidgetPicked select_)
      (selectColumns (widgetSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: WidgetQuery select -> QueryBuilder WidgetTable -> QueryBuilder WidgetTable
applyQuery WidgetQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: WidgetQuery select -> Db Int
count q = Ops.count @WidgetTable (applyQuery q)


delete :: WidgetUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @WidgetTable (widgetUniqueWhere key)


deleteMany :: Where WidgetTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @WidgetTable

