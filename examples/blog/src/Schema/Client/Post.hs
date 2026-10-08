{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    PostUnique (..),
    PostUniqueKey (..),
    PostUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    postUniqueWhere,
    postId
  )

where

import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.Internal.Generated
  ( Db,
    ORMError (..),
    fromUniqueRows,
    requireFound,
    uniqueOrFail,
    OrderBy,
    QueryBuilder,
    applyQueryModifiers,
    matching,
    selectColumns,
    OmitSelect (..),
    Picked (..),
    Where,
    eq
  )
import qualified Poppy.Internal.Generated as Delete
  ( deleteMany
  )
import qualified Poppy.Internal.Generated as Insert
  ( insert,
    insertMany,
    upsert
  )
import qualified Poppy.Internal.Generated as Ops
  ( findMany,
    findManyWith,
    findFirst,
    findFirstWith,
    count
  )
import qualified Poppy.Internal.Generated as Update
  ( updateWhere,
    updateMany
  )

import Schema.Post (PostCreate (..), PostRow (..), PostSelect (..), PostPicked (..), postSelect, postSelectColumns, parsePostPicked, PostTable, PostUpdate (..), postId, postTitle)

data PostUnique
  = ById UUID
  | ByTitle Text
  deriving (Eq, Show)

data PostUniqueKey
  = OnId
  | OnTitle
  deriving (Eq, Show)

postUniqueWhere :: PostUnique -> Where PostTable
postUniqueWhere = \case
  ById v1 -> eq postId v1
  ByTitle v1 -> eq postTitle v1

postConflictCols :: PostUniqueKey -> [Text]
postConflictCols = \case
  OnId -> ["id"]
  OnTitle -> ["title"]


create :: PostCreate -> Db (Either ORMError PostRow)
create = Insert.insert @PostTable @PostRow


createMany :: [PostCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @PostTable


update :: PostUnique -> PostUpdate -> Db (Either ORMError PostRow)
update key input =
  Update.updateWhere @PostTable @PostRow (postUniqueWhere key) input


updateMany :: Where PostTable -> PostUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @PostTable


upsert :: PostUniqueKey -> PostCreate -> PostUpdate -> Db (Either ORMError PostRow)
upsert key createInput updateInput =
  Insert.upsert @PostTable @PostRow (postConflictCols key) createInput updateInput


data PostQuery select = PostQuery
  { select_ :: select
  , where_ :: Maybe (Where PostTable)
  , orderBy_ :: [OrderBy PostTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


data PostUniqueQuery select = PostUniqueQuery
  { select_ :: select
  , where_ :: PostUnique
  }


emptyQuery :: PostQuery OmitSelect
emptyQuery =
  PostQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


uniqueQuery :: PostUnique -> PostUniqueQuery OmitSelect
uniqueQuery key =
  PostUniqueQuery {select_ = OmitSelect, where_ = key}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = PostRow
type instance ResolveSelect PostSelect = PostPicked


class ReadPost select where
  findMany :: PostQuery select -> Db [ResolveSelect select]
  findUnique :: PostUniqueQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: PostUniqueQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: PostQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: PostQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadPost OmitSelect where
  findMany q =
    Ops.findMany @PostTable @PostRow (applyQuery q)
  findUnique PostUniqueQuery {where_} = do
    let w = postUniqueWhere where_
    rows <- Ops.findMany @PostTable @PostRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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
  findUnique PostUniqueQuery {where_, select_} = do
    let w = postUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parsePostPicked select_)
        (selectColumns (postSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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


delete :: PostUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @PostTable (postUniqueWhere key)


deleteMany :: Where PostTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @PostTable

