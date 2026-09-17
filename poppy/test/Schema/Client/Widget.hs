{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Widget
  ( create,
    update,
    findMany,
    findUnique,
    findUniqueOrFail,
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
    emptyQuery,
    widgetId,
  )
where

import Data.UUID (UUID)
import ORM.Db (Db)
import qualified ORM.Delete as Delete
import ORM.Errors (ORMError (..), requireFound)
import qualified ORM.Insert as Insert
import qualified ORM.Operations as Ops
import ORM.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)
import ORM.Select (OmitSelect (..), Picked (..))
import qualified ORM.Update as Update
import ORM.Where (Where)
import Schema.Widget
  ( WidgetCreate (..),
    WidgetPicked (..),
    WidgetRow (..),
    WidgetSelect (..),
    WidgetTable,
    WidgetUpdate (..),
    parseWidgetPicked,
    widgetId,
    widgetSelect,
    widgetSelectColumns,
  )

create :: WidgetCreate -> Db (Either ORMError WidgetRow)
create = Insert.insert @WidgetTable @WidgetRow

update :: UUID -> WidgetUpdate -> Db (Either ORMError WidgetRow)
update = Update.update @WidgetTable @WidgetRow

data WidgetQuery select = WidgetQuery
  { select_ :: select,
    where_ :: Maybe (Where WidgetTable),
    orderBy_ :: Maybe (QueryBuilder WidgetTable -> QueryBuilder WidgetTable),
    limit_ :: Maybe Int,
    offset_ :: Maybe Int
  }

emptyQuery :: WidgetQuery OmitSelect
emptyQuery =
  WidgetQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = Nothing, limit_ = Nothing, offset_ = Nothing}

type family ResolveSelect select

type instance ResolveSelect OmitSelect = WidgetRow

type instance ResolveSelect WidgetSelect = WidgetPicked

class ReadWidget select where
  findMany :: WidgetQuery select -> Db [ResolveSelect select]
  findUnique :: WidgetQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: WidgetQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadWidget OmitSelect where
  findMany q =
    Ops.findMany @WidgetTable @WidgetRow (applyQuery q)
  findUnique WidgetQuery {where_} =
    Ops.findUniqueWhere @WidgetTable @WidgetRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")

instance ReadWidget WidgetSelect where
  findMany q@WidgetQuery {select_} =
    Ops.findManyWith
      (parseWidgetPicked select_)
      (selectColumns (widgetSelectColumns select_) . applyQuery q)
  findUnique WidgetQuery {select_, where_} =
    case Ops.requireUniqueWhere @WidgetTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseWidgetPicked select_)
            (selectColumns (widgetSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")

applyQuery :: WidgetQuery select -> QueryBuilder WidgetTable -> QueryBuilder WidgetTable
applyQuery WidgetQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_

delete :: UUID -> Db Int
delete = Ops.delete @WidgetTable

deleteMany :: Where WidgetTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @WidgetTable
