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

Nested writes live on 'create' / 'update'.
  create-time relation fields are [CreateChild | ConnectChild unique]
  update-time relation fields are Maybe <Rel>Update (replaceWith, create, connect, delete, update, upsert;
  disconnect when the child foreign key is nullable)
-}
module Schema.Client.Comment
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
    CommentCreateScalars,
    CommentUpdateScalars,
    CommentNestedCreate (..),
    CommentNestedUpsert (..),
    RepliesUpdate (..),
    emptyRepliesUpdate,
    CommentQuery (..),
    CommentUnique (..),
    CommentUniqueKey (..),
    CommentUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    commentUniqueWhere,
    OmitSelect (..),
    Picked (..),
    CommentCreate (..),
    CommentRow (..),
    CommentSelect (..),
    CommentPicked (..),
    commentSelect,
    CommentUpdate (..),
    CommentTable,
    commentId
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.Core (NullableValue (..), fieldColumn)
import Poppy.PG (toField)
import Poppy.Db (Db, transactionEither)
import qualified Poppy.Delete as Delete
import Poppy.Errors (ORMError (..), fromUniqueRows, requireFound, uniqueOrFail)
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.SelectIn (prepareIncludeRootQuery)
import qualified Poppy.Update as Update
import Poppy.Where (Where, and_, eq)
import Schema.Comment (CommentRow (..), CommentSelect (..), CommentPicked (..), commentSelect, commentSelectColumns, parseCommentPicked, CommentTable, commentId, commentParentId)
import qualified Schema.Comment as CommentSchema (CommentCreate (..), CommentUpdate (..))

import Schema.Include.Comment (LoadComment (..), CommentInclude (..), CommentRead, toCommentWithPicked)

data CommentUnique
  = ById UUID
  deriving (Eq, Show)

data CommentUniqueKey
  = OnId
  deriving (Eq, Show)

commentUniqueWhere :: CommentUnique -> Where CommentTable
commentUniqueWhere = \case
  ById v1 -> eq commentId v1

commentConflictCols :: CommentUniqueKey -> [Text]
commentConflictCols = \case
  OnId -> ["id"]

data CommentNestedCreate
  = CreateComment
      { id :: Maybe UUID, body :: Text
      }
  | ConnectComment CommentUnique
  deriving (Show, Eq)
data CommentNestedUpsert = CommentNestedUpsert
  { where_ :: CommentUnique
  , create :: CommentNestedCreate
  , update :: CommentSchema.CommentUpdate
  }
  deriving (Show, Eq)
data RepliesUpdate = RepliesUpdate
  { replaceWith :: Maybe [CommentNestedCreate]
  , create :: [CommentNestedCreate]
  , createMany :: [CommentNestedCreate]
  , connect :: [CommentUnique]
  , delete :: [CommentUnique]
  , update :: [(CommentUnique, CommentSchema.CommentUpdate)]
  , upsert :: [CommentNestedUpsert]
  , disconnect :: [CommentUnique]
  }
  deriving (Show, Eq)

emptyRepliesUpdate :: RepliesUpdate
emptyRepliesUpdate =
  RepliesUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    , disconnect = []
    }
data CommentCreate = CommentCreate
  { id :: Maybe UUID,
    parentId :: NullableValue UUID,
    body :: Text,
    replies :: [CommentNestedCreate]
  }
  deriving (Show, Eq)

type CommentCreateScalars = CommentSchema.CommentCreate

toCommentCreateScalars :: CommentCreate -> CommentCreateScalars
toCommentCreateScalars input =
  CommentSchema.CommentCreate
    { id = input.id,
      parentId = input.parentId,
      body = input.body
    }
data CommentUpdate = CommentUpdate
  { parentId :: NullableValue UUID,
    body :: Maybe Text,
    replies :: Maybe RepliesUpdate
  }
  deriving (Show, Eq)

type CommentUpdateScalars = CommentSchema.CommentUpdate

toCommentUpdateScalars :: CommentUpdate -> CommentUpdateScalars
toCommentUpdateScalars input =
  CommentSchema.CommentUpdate
    { parentId = input.parentId,
      body = input.body
    }

create :: CommentCreate -> Db (Either ORMError CommentRow)
create input =
  if hasCommentNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @CommentTable @CommentRow (toCommentCreateScalars input)

hasCommentNestedCreate :: CommentCreate -> Bool
hasCommentNestedCreate input =
  not (null input.replies)

createWithNested :: CommentCreate -> Db (Either ORMError CommentRow)
createWithNested input = do
  rootResult <- Insert.insert @CommentTable @CommentRow (toCommentCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applyRepliesCreate row.id input.replies
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [CommentCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @CommentTable

update :: CommentUnique -> CommentUpdate -> Db (Either ORMError CommentRow)
update key input =
  if hasCommentNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @CommentTable @CommentRow (commentUniqueWhere key) (toCommentUpdateScalars input)

hasCommentNestedUpdate :: CommentUpdate -> Bool
hasCommentNestedUpdate input =
  isJust input.replies

updateWithNested :: CommentUnique -> CommentUpdate -> Db (Either ORMError CommentRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @CommentTable @CommentRow (commentUniqueWhere key) (toCommentUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applyRepliesUpdate row.id) input.replies
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where CommentTable -> CommentUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @CommentTable

upsert :: CommentUniqueKey -> CommentCreateScalars -> CommentUpdateScalars -> Db (Either ORMError CommentRow)
upsert key createInput updateInput =
  Insert.upsert @CommentTable @CommentRow (commentConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applyRepliesCreate :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
applyRepliesCreate = insertReplies
applyRepliesUpdate :: UUID -> RepliesUpdate -> Db (Either ORMError ())
applyRepliesUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceReplies parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteReplies parentId ops.delete
        , updateRepliesRows parentId ops.update
        , upsertReplies parentId ops.upsert
        , insertReplies parentId ops.create
        , insertReplies parentId ops.createMany
        , connectReplies parentId ops.connect
        , disconnectReplies parentId ops.disconnect
        ]
replaceReplies :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
replaceReplies parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn commentParentId <> " = ?") [toField parentId] (Delete.emptyDelete @CommentTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertReplies parentId items
insertReplies :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
insertReplies parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectComment key -> connectReplies parentId [key]
        CreateComment {id, body} -> do
          inserted <- Insert.insert @CommentTable @CommentRow CommentSchema.CommentCreate
            { id = id,
              body = body,
              parentId = Value parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteReplies :: UUID -> [CommentUnique] -> Db (Either ORMError ())
deleteReplies _ [] = pure (Right ())
deleteReplies parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @CommentTable
        (commentUniqueWhere key `and_` eq commentParentId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateRepliesRows :: UUID -> [(CommentUnique, CommentSchema.CommentUpdate)] -> Db (Either ORMError ())
updateRepliesRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = CommentSchema.CommentUpdate { parentId = Omit, body = nested.body }
      result <-
        Update.updateWhere @CommentTable @CommentRow
          (commentUniqueWhere key `and_` eq commentParentId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertReplies :: UUID -> [CommentNestedUpsert] -> Db (Either ORMError ())
upsertReplies parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @CommentTable @CommentRow (matching (commentUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertReplies parentId [item.create]
        Right (Just row) ->
          if row.parentId == Just parentId
            then do
              let patched = CommentSchema.CommentUpdate { parentId = Omit, body = item.update.body }
              updated <-
                Update.updateWhere @CommentTable @CommentRow
                  (commentUniqueWhere item.where_ `and_` eq commentParentId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectReplies :: UUID -> [CommentUnique] -> Db (Either ORMError ())
connectReplies _ [] = pure (Right ())
connectReplies parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @CommentTable @CommentRow
          (commentUniqueWhere key)
          (CommentSchema.CommentUpdate
            { parentId = Value parentId,
              body = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
disconnectReplies :: UUID -> [CommentUnique] -> Db (Either ORMError ())
disconnectReplies _ [] = pure (Right ())
disconnectReplies parentId keys = sequenceNested (map disconnectOne keys)
  where
    disconnectOne key = do
      result <-
        Update.updateWhere @CommentTable @CommentRow
          (commentUniqueWhere key `and_` eq commentParentId parentId)
          (CommentSchema.CommentUpdate
            { parentId = Null,
              body = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

data CommentQuery include select = CommentQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where CommentTable)
  , orderBy_ :: [OrderBy CommentTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data CommentUniqueQuery include select = CommentUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: CommentUnique
  }

emptyQuery :: CommentQuery () OmitSelect
emptyQuery =
  CommentQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: CommentUnique -> CommentUniqueQuery () OmitSelect
uniqueQuery key =
  CommentUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadComment include select where
  findMany :: CommentQuery include select -> Db [CommentRead include select]
  findUnique :: CommentUniqueQuery include select -> Db (Either ORMError (Maybe (CommentRead include select)))
  findUniqueOrFail :: CommentUniqueQuery include select -> Db (Either ORMError (CommentRead include select))
  findFirst :: CommentQuery include select -> Db (Maybe (CommentRead include select))
  findFirstOrFail :: CommentQuery include select -> Db (Either ORMError (CommentRead include select))

instance (LoadComment replies) => ReadComment (CommentInclude replies) OmitSelect where
  findMany CommentQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadComment include_ roots
  findUnique CommentUniqueQuery {where_, include_} = do
    let w = commentUniqueWhere where_
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (matching w))
    rows <- loadComment include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst CommentQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadComment include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadComment () OmitSelect where
  findMany CommentQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @CommentTable @CommentRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique CommentUniqueQuery {where_} = do
    let w = commentUniqueWhere where_
    rows <- Ops.findMany @CommentTable @CommentRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst CommentQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @CommentTable @CommentRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadComment replies) => ReadComment (CommentInclude replies) CommentSelect where
  findMany CommentQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadComment include_ roots
    pure $ map (toCommentWithPicked select_) loaded
  findUnique CommentUniqueQuery {where_, include_, select_} = do
    let w = commentUniqueWhere where_
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (matching w))
    loaded <- loadComment include_ roots
    let rows = map (toCommentWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst CommentQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadComment include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toCommentWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadComment () CommentSelect where
  findMany CommentQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseCommentPicked select_)
      (selectColumns (commentSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique CommentUniqueQuery {where_, select_} = do
    let w = commentUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseCommentPicked select_)
        (selectColumns (commentSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst CommentQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseCommentPicked select_)
      (selectColumns (commentSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: CommentQuery include select -> Db Int
count CommentQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @CommentTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

delete :: CommentUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @CommentTable (commentUniqueWhere key)

deleteMany :: Where CommentTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @CommentTable