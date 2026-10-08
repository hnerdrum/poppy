{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Generated Client. Do not edit.
-}
module Schema.Client.Post
  ( findMany,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstOrFail,
    count,
    create,
    createMany,
    update,
    updateMany,
    upsert,
    delete,
    deleteMany,
    PostQuery (..),
    PostUnique (..),
    PostUniqueKey (..),
    PostUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    postUniqueWhere,
    OmitSelect (..),
    Picked (..),
    PostCreate (..),
    PostRow (..),
    PostSelect (..),
    PostPicked (..),
    postSelect,
    PostUpdate (..),
    PostTable,
    postId
  )
where

import Data.UUID (UUID)
import Poppy.Db (Db)
import qualified Poppy.Delete as Delete
import Poppy.Errors (ORMError (..), fromUniqueRows, requireFound, uniqueOrFail)
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.SelectIn (prepareIncludeRootQuery)
import qualified Poppy.Update as Update
import Poppy.Where (Where, eq)
import Schema.Post (PostCreate (..), PostUpdate (..), PostRow (..), PostSelect (..), PostPicked (..), postSelect, postSelectColumns, parsePostPicked, PostTable, postId)
import Schema.Include.Post (LoadPost (..), PostInclude (..), PostRead, toPostWithPicked)
import Data.Text (Text)

data PostUnique
  = ById UUID
  deriving (Eq, Show)

data PostUniqueKey
  = OnId
  deriving (Eq, Show)

postUniqueWhere :: PostUnique -> Where PostTable
postUniqueWhere = \case
  ById v1 -> eq postId v1

postConflictCols :: PostUniqueKey -> [Text]
postConflictCols = \case
  OnId -> ["id"]

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

data PostQuery include select = PostQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where PostTable)
  , orderBy_ :: [OrderBy PostTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data PostUniqueQuery include select = PostUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: PostUnique
  }

emptyQuery :: PostQuery () OmitSelect
emptyQuery =
  PostQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: PostUnique -> PostUniqueQuery () OmitSelect
uniqueQuery key =
  PostUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadPost include select where
  findMany :: PostQuery include select -> Db [PostRead include select]
  findUnique :: PostUniqueQuery include select -> Db (Either ORMError (Maybe (PostRead include select)))
  findUniqueOrFail :: PostUniqueQuery include select -> Db (Either ORMError (PostRead include select))
  findFirst :: PostQuery include select -> Db (Maybe (PostRead include select))
  findFirstOrFail :: PostQuery include select -> Db (Either ORMError (PostRead include select))

instance (LoadPost author) => ReadPost (PostInclude author) OmitSelect where
  findMany PostQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadPost include_ roots
  findUnique PostUniqueQuery {where_, include_} = do
    let w = postUniqueWhere where_
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (matching w))
    rows <- loadPost include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst PostQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadPost include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadPost () OmitSelect where
  findMany PostQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @PostTable @PostRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique PostUniqueQuery {where_} = do
    let w = postUniqueWhere where_
    rows <- Ops.findMany @PostTable @PostRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst PostQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @PostTable @PostRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadPost author) => ReadPost (PostInclude author) PostSelect where
  findMany PostQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadPost include_ roots
    pure $ map (toPostWithPicked select_) loaded
  findUnique PostUniqueQuery {where_, include_, select_} = do
    let w = postUniqueWhere where_
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (matching w))
    loaded <- loadPost include_ roots
    let rows = map (toPostWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst PostQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadPost include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toPostWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadPost () PostSelect where
  findMany PostQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parsePostPicked select_)
      (selectColumns (postSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique PostUniqueQuery {where_, select_} = do
    let w = postUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parsePostPicked select_)
        (selectColumns (postSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst PostQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parsePostPicked select_)
      (selectColumns (postSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: PostQuery include select -> Db Int
count PostQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @PostTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

delete :: PostUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @PostTable (postUniqueWhere key)

deleteMany :: Where PostTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @PostTable