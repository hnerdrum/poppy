{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Section
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
    SectionCreate (..),
    SectionRow (..),
    SectionSelect (..),
    SectionPicked (..),
    sectionSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    SectionUpdate (..),
    SectionTable,
    SectionQuery (..),
    SectionUnique (..),
    SectionUniqueKey (..),
    SectionUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    sectionUniqueWhere,
    sectionId
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
import Schema.Section (SectionCreate (..), SectionRow (..), SectionSelect (..), SectionPicked (..), sectionSelect, sectionSelectColumns, parseSectionPicked, SectionTable, SectionUpdate (..), sectionId)
import qualified Poppy.Update as Update

data SectionUnique
  = ById UUID
  deriving (Eq, Show)

data SectionUniqueKey
  = OnId
  deriving (Eq, Show)

sectionUniqueWhere :: SectionUnique -> Where SectionTable
sectionUniqueWhere = \case
  ById v1 -> eq sectionId v1

sectionConflictCols :: SectionUniqueKey -> [Text]
sectionConflictCols = \case
  OnId -> ["id"]


create :: SectionCreate -> Db (Either ORMError SectionRow)
create = Insert.insert @SectionTable @SectionRow


createMany :: [SectionCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @SectionTable


update :: SectionUnique -> SectionUpdate -> Db (Either ORMError SectionRow)
update key input =
  Update.updateWhere @SectionTable @SectionRow (sectionUniqueWhere key) input


updateMany :: Where SectionTable -> SectionUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @SectionTable


upsert :: SectionUniqueKey -> SectionCreate -> SectionUpdate -> Db (Either ORMError SectionRow)
upsert key createInput updateInput =
  Insert.upsert @SectionTable @SectionRow (sectionConflictCols key) createInput updateInput


data SectionQuery select = SectionQuery
  { select_ :: select
  , where_ :: Maybe (Where SectionTable)
  , orderBy_ :: [OrderBy SectionTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data SectionUniqueQuery select = SectionUniqueQuery
  { select_ :: select
  , where_ :: SectionUnique
  }


emptyQuery :: SectionQuery OmitSelect
emptyQuery =
  SectionQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: SectionUnique -> SectionUniqueQuery OmitSelect
uniqueQuery key =
  SectionUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = SectionRow
type instance ResolveSelect SectionSelect = SectionPicked


class ReadSection select where
  findMany :: SectionQuery select -> Db [ResolveSelect select]
  findUnique :: SectionUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: SectionUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: SectionQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: SectionQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadSection OmitSelect where
  findMany q =
    Ops.findMany @SectionTable @SectionRow (applyQuery q)
  findUnique SectionUniqueQuery {where_} = do
    let w = sectionUniqueWhere where_
    rows <- Ops.findMany @SectionTable @SectionRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q =
    Ops.findFirst @SectionTable @SectionRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadSection SectionSelect where
  findMany q@SectionQuery {select_} =
    Ops.findManyWith
      (parseSectionPicked select_)
      (selectColumns (sectionSelectColumns select_) . applyQuery q)
  findUnique SectionUniqueQuery {where_, select_} = do
    let w = sectionUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseSectionPicked select_)
        (selectColumns (sectionSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst q@SectionQuery {select_} =
    Ops.findFirstWith
      (parseSectionPicked select_)
      (selectColumns (sectionSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: SectionQuery select -> QueryBuilder SectionTable -> QueryBuilder SectionTable
applyQuery SectionQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: SectionQuery select -> Db Int
count q = Ops.count @SectionTable (applyQuery q)


delete :: SectionUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @SectionTable (sectionUniqueWhere key)


deleteMany :: Where SectionTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @SectionTable

