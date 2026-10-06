{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Task
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
    TaskCreate (..),
    TaskRow (..),
    TaskSelect (..),
    TaskPicked (..),
    taskSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    TaskUpdate (..),
    TaskTable,
    TaskQuery (..),
    TaskUnique (..),
    TaskUniqueKey (..),
    TaskUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    taskId
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
import Schema.Task (TaskCreate (..), TaskRow (..), TaskSelect (..), TaskPicked (..), taskSelect, taskSelectColumns, parseTaskPicked, TaskTable, TaskUpdate (..), taskId)
import qualified Poppy.Update as Update

data TaskUnique
  = ById UUID
  deriving (Eq, Show)

data TaskUniqueKey
  = OnId
  deriving (Eq, Show)

taskUniqueWhere :: TaskUnique -> Where TaskTable
taskUniqueWhere = \case
  ById v1 -> eq taskId v1

taskConflictCols :: TaskUniqueKey -> [Text]
taskConflictCols = \case
  OnId -> ["id"]


create :: TaskCreate -> Db (Either ORMError TaskRow)
create = Insert.insert @TaskTable @TaskRow


createMany :: [TaskCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @TaskTable


update :: TaskUnique -> TaskUpdate -> Db (Either ORMError TaskRow)
update key input =
  Update.updateWhere @TaskTable @TaskRow (taskUniqueWhere key) input


updateMany :: Where TaskTable -> TaskUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @TaskTable


upsert :: TaskUniqueKey -> TaskCreate -> TaskUpdate -> Db (Either ORMError TaskRow)
upsert key createInput updateInput =
  Insert.upsert @TaskTable @TaskRow (taskConflictCols key) createInput updateInput


data TaskQuery select = TaskQuery
  { select_ :: select
  , where_ :: Maybe (Where TaskTable)
  , orderBy_ :: [OrderBy TaskTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data TaskUniqueQuery select = TaskUniqueQuery
  { select_ :: select
  , where_ :: TaskUnique
  }


emptyQuery :: TaskQuery OmitSelect
emptyQuery =
  TaskQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: TaskUnique -> TaskUniqueQuery OmitSelect
uniqueQuery key =
  TaskUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = TaskRow
type instance ResolveSelect TaskSelect = TaskPicked


class ReadTask select where
  findMany :: TaskQuery select -> Db [ResolveSelect select]
  findUnique :: TaskUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: TaskUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: TaskQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: TaskQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadTask OmitSelect where
  findMany q =
    Ops.findMany @TaskTable @TaskRow (applyQuery q)
  findUnique TaskUniqueQuery {where_} = do
    let w = taskUniqueWhere where_
    rows <- Ops.findMany @TaskTable @TaskRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q =
    Ops.findFirst @TaskTable @TaskRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadTask TaskSelect where
  findMany q@TaskQuery {select_} =
    Ops.findManyWith
      (parseTaskPicked select_)
      (selectColumns (taskSelectColumns select_) . applyQuery q)
  findUnique TaskUniqueQuery {where_, select_} = do
    let w = taskUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseTaskPicked select_)
        (selectColumns (taskSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q@TaskQuery {select_} =
    Ops.findFirstWith
      (parseTaskPicked select_)
      (selectColumns (taskSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: TaskQuery select -> QueryBuilder TaskTable -> QueryBuilder TaskTable
applyQuery TaskQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: TaskQuery select -> Db Int
count q = Ops.count @TaskTable (applyQuery q)


delete :: TaskUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @TaskTable (taskUniqueWhere key)


deleteMany :: Where TaskTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @TaskTable

