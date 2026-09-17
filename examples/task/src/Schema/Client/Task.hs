{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Task
  ( create,
    update,
    findMany,
    findUnique,
    findUniqueOrFail,
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
    emptyQuery,
    taskId
  )

where

import Data.UUID (UUID)
import Poppy.Db (Db)
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Delete as Delete
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.Where (Where)
import Schema.Task (TaskCreate (..), TaskRow (..), TaskSelect (..), TaskPicked (..), taskSelect, taskSelectColumns, parseTaskPicked, TaskTable, TaskUpdate (..), taskId)
import qualified Poppy.Update as Update

create :: TaskCreate -> Db (Either ORMError TaskRow)
create = Insert.insert @TaskTable @TaskRow


update :: UUID -> TaskUpdate -> Db (Either ORMError TaskRow)
update = Update.update @TaskTable @TaskRow


data TaskQuery select = TaskQuery
  { select_ :: select
  , where_ :: Maybe (Where TaskTable)
  , orderBy_ :: Maybe (QueryBuilder TaskTable -> QueryBuilder TaskTable)
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: TaskQuery OmitSelect
emptyQuery =
  TaskQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = Nothing, limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = TaskRow
type instance ResolveSelect TaskSelect = TaskPicked


class ReadTask select where
  findMany :: TaskQuery select -> Db [ResolveSelect select]
  findUnique :: TaskQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: TaskQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadTask OmitSelect where
  findMany q =
    Ops.findMany @TaskTable @TaskRow (applyQuery q)
  findUnique TaskQuery {where_} =
    Ops.findUniqueWhere @TaskTable @TaskRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")

instance ReadTask TaskSelect where
  findMany q@TaskQuery {select_} =
    Ops.findManyWith
      (parseTaskPicked select_)
      (selectColumns (taskSelectColumns select_) . applyQuery q)
  findUnique TaskQuery {select_, where_} =
    case Ops.requireUniqueWhere @TaskTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseTaskPicked select_)
            (selectColumns (taskSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")

applyQuery :: TaskQuery select -> QueryBuilder TaskTable -> QueryBuilder TaskTable
applyQuery TaskQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


delete :: UUID -> Db Int
delete = Ops.delete @TaskTable


deleteMany :: Where TaskTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @TaskTable

