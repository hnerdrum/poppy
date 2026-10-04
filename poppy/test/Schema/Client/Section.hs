{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    emptyQuery,
    sectionId
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
import Schema.Section (SectionCreate (..), SectionRow (..), SectionSelect (..), SectionPicked (..), sectionSelect, sectionSelectColumns, parseSectionPicked, SectionTable, SectionUpdate (..), sectionId)
import qualified Poppy.Update as Update

create :: SectionCreate -> Db (Either ORMError SectionRow)
create = Insert.insert @SectionTable @SectionRow


createMany :: [SectionCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @SectionTable


update :: UUID -> SectionUpdate -> Db (Either ORMError SectionRow)
update = Update.update @SectionTable @SectionRow


updateMany :: Where SectionTable -> SectionUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @SectionTable


upsert :: SectionCreate -> SectionUpdate -> Db (Either ORMError SectionRow)
upsert = Insert.upsert @SectionTable @SectionRow ["id"]


data SectionQuery select = SectionQuery
  { select_ :: select
  , where_ :: Maybe (Where SectionTable)
  , orderBy_ :: [OrderBy SectionTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: SectionQuery OmitSelect
emptyQuery =
  SectionQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = SectionRow
type instance ResolveSelect SectionSelect = SectionPicked


class ReadSection select where
  findMany :: SectionQuery select -> Db [ResolveSelect select]
  findUnique :: SectionQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: SectionQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: SectionQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: SectionQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadSection OmitSelect where
  findMany q =
    Ops.findMany @SectionTable @SectionRow (applyQuery q)
  findUnique SectionQuery {where_} =
    Ops.findUniqueWhere @SectionTable @SectionRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique SectionQuery {select_, where_} =
    case Ops.requireUniqueWhere @SectionTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseSectionPicked select_)
            (selectColumns (sectionSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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


delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @SectionTable


deleteMany :: Where SectionTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @SectionTable

