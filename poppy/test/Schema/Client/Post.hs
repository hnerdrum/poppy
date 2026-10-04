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
    noInclude,
    PostInclude (..),
    NoInclude (..),
    ResolveInclude,
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
    postId,
    PostWithAuthorPicked (..)
  )
where

import Data.UUID (UUID)
import Poppy.Db (Db)
import qualified Poppy.Delete as Delete
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Include as Include
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import qualified Poppy.Update as Update
import Poppy.Where (Where)
import Schema.Post (PostCreate (..), PostRow (..), PostSelect (..), PostPicked (..), postSelect, postSelectColumns, parsePostPicked, toPostPicked, PostTable, PostUpdate (..), postId)
import Schema.PostInclude ( PostInclude (..), PostWithAuthor (..), NoInclude (..), ResolveInclude)
import Schema.Author (AuthorRow (..))

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

noInclude :: NoInclude
noInclude = NoInclude PostInclude {author = False}

data PostWithAuthorPicked = PostWithAuthorPicked
  { post :: PostPicked,
    author :: Maybe AuthorRow
  }
  deriving (Show, Eq)

toPostWithAuthorPicked :: PostSelect -> PostWithAuthor -> PostWithAuthorPicked
toPostWithAuthorPicked select_ PostWithAuthor {post, author} =
  PostWithAuthorPicked
    { post = toPostPicked select_ post
    , author = author
    }

data PostQuery include select = PostQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where PostTable)
  , orderBy_ :: [OrderBy PostTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: PostQuery NoInclude OmitSelect
emptyQuery =
  PostQuery {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

type instance ResolveInclude (PostQuery PostInclude OmitSelect) = PostWithAuthor
type instance ResolveInclude (PostQuery NoInclude OmitSelect) = PostRow
type instance ResolveInclude (PostQuery PostInclude PostSelect) = PostWithAuthorPicked
type instance ResolveInclude (PostQuery NoInclude PostSelect) = PostPicked

class ReadPost include select where
  findMany :: PostQuery include select -> Db [ResolveInclude (PostQuery include select)]
  findUnique :: PostQuery include select -> Db (Either ORMError (Maybe (ResolveInclude (PostQuery include select))))
  findUniqueOrFail :: PostQuery include select -> Db (Either ORMError (ResolveInclude (PostQuery include select)))
  findFirst :: PostQuery include select -> Db (Maybe (ResolveInclude (PostQuery include select)))
  findFirstOrFail :: PostQuery include select -> Db (Either ORMError (ResolveInclude (PostQuery include select)))

instance ReadPost PostInclude OmitSelect where
  findMany PostQuery {include_, where_, orderBy_, limit_, offset_} =
    Include.findMany @PostTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique PostQuery {include_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <- Include.findMany @PostTable include_ (matching w)
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
    rows <- Include.findMany @PostTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadPost NoInclude OmitSelect where
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

instance ReadPost PostInclude PostSelect where
  findMany PostQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    rows <- Include.findMany @PostTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
    pure $ map (toPostWithAuthorPicked select_) rows
  findUnique PostQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @PostTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        nested <- Include.findMany @PostTable include_ (matching w)
        let rows = map (toPostWithAuthorPicked select_) nested
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
    rows <- Include.findMany @PostTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just (toPostWithAuthorPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadPost NoInclude PostSelect where
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