{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    emptyQuery,
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
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.SelectIn (prepareIncludeRootQuery)
import qualified Poppy.Update as Update
import Poppy.Where (Where)
import Schema.Post (PostCreate (..), PostRow (..), PostSelect (..), PostPicked (..), postSelect, postSelectColumns, parsePostPicked, PostTable, PostUpdate (..), postId)
import Schema.Include.Post (LoadPost (..), PostInclude (..), PostRead, PostWith (..), toPostWithPicked)

create :: PostCreate -> Db (Either ORMError PostRow)
create = Insert.insert @PostTable @PostRow

createMany :: [PostCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @PostTable

update :: UUID -> PostUpdate -> Db (Either ORMError PostRow)
update = Update.update @PostTable @PostRow

updateMany :: Where PostTable -> PostUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @PostTable

upsert :: PostCreate -> PostUpdate -> Db (Either ORMError PostRow)
upsert = Insert.upsert @PostTable @PostRow ["id"]

data PostQuery include select = PostQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where PostTable)
  , orderBy_ :: [OrderBy PostTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: PostQuery () OmitSelect
emptyQuery =
  PostQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

class ReadPost include select where
  findMany :: PostQuery include select -> Db [PostRead include select]
  findUnique :: PostQuery include select -> Db (Either ORMError (Maybe (PostRead include select)))
  findUniqueOrFail :: PostQuery include select -> Db (Either ORMError (PostRead include select))
  findFirst :: PostQuery include select -> Db (Maybe (PostRead include select))
  findFirstOrFail :: PostQuery include select -> Db (Either ORMError (PostRead include select))

instance (LoadPost author) => ReadPost (PostInclude author) OmitSelect where
  findMany PostQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadPost include_ roots
  findUnique PostQuery {include_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (matching w))
        rows <- loadPost include_ roots
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique PostQuery {where_} =
    Ops.findUniqueWhere @PostTable @PostRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique PostQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @PostTable @PostRow (prepareIncludeRootQuery @PostTable (matching w))
        loaded <- loadPost include_ roots
        let rows = map (toPostWithPicked select_) loaded
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique PostQuery {select_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parsePostPicked select_)
            (selectColumns (postSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @PostTable

deleteMany :: Where PostTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @PostTable