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

Nested writes ('createNested' / 'updateNested'):
  Set xs  — replace all children with xs
  Ops o   — create, connect, disconnect, delete, update, upsert
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
    createNested,
    updateNested,
    CommentWriteCreate (..),
    CommentWriteUpdate (..),
    CommentNestedCreate (..),
    RepliesWrite (..),
    CommentNestedOps (..),
    emptyCommentNestedOps,
    CommentQuery (..),
    emptyQuery,
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

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import Poppy.Core (fieldColumn, NullableValue (Omit, Value))
import Poppy.PG (toField)
import Poppy.Db (Db, liftIO, transactionEither)
import qualified Poppy.Delete as Delete
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.SelectIn (prepareIncludeRootQuery)
import qualified Poppy.Update as Update
import Poppy.Where (Where, and_, eq, in_)
import Schema.Comment (CommentCreate (..), CommentRow (..), CommentSelect (..), CommentPicked (..), commentSelect, commentSelectColumns, parseCommentPicked, CommentTable, CommentUpdate (..), commentId)
import Schema.Include.Comment (LoadComment (..), CommentInclude (..), CommentRead, CommentWith (..), toCommentWithPicked)
import Schema.Comment (CommentCreate (..), CommentRow (..), CommentUpdate (..), CommentTable, commentId, commentParentId)

create :: CommentCreate -> Db (Either ORMError CommentRow)
create = Insert.insert @CommentTable @CommentRow

createMany :: [CommentCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @CommentTable

update :: UUID -> CommentUpdate -> Db (Either ORMError CommentRow)
update = Update.update @CommentTable @CommentRow

updateMany :: Where CommentTable -> CommentUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @CommentTable

upsert :: CommentCreate -> CommentUpdate -> Db (Either ORMError CommentRow)
upsert = Insert.upsert @CommentTable @CommentRow ["id"]

data CommentNestedCreate = CommentNestedCreate
  { id :: Maybe UUID, body :: Text
  }
  deriving (Show, Eq)

data CommentNestedOps = CommentNestedOps
  { create :: [CommentNestedCreate]
  , createMany :: [CommentNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, CommentNestedCreate)]
  , upsert :: [CommentNestedCreate]
  }
  deriving (Show, Eq)

data RepliesWrite = Set [CommentNestedCreate] | Ops CommentNestedOps
  deriving (Show, Eq)

emptyCommentNestedOps :: CommentNestedOps
emptyCommentNestedOps =
  CommentNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data CommentWriteCreate = CommentWriteCreate
  { root :: CommentCreate
  , replies :: RepliesWrite
  }
  deriving (Show, Eq)

data CommentWriteUpdate = CommentWriteUpdate
  { root :: CommentUpdate
  , replies :: Maybe RepliesWrite
  }
  deriving (Show, Eq)

toCommentCreate :: UUID -> CommentNestedCreate -> CommentCreate
toCommentCreate parentId nested =
  CommentCreate
    { id = nested.id,
      body = nested.body,
      parentId = Value parentId
    }

toCommentUpdate :: CommentNestedCreate -> CommentUpdate
toCommentUpdate nested =
  CommentUpdate
    { parentId = Omit,
      body = Just nested.body
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applyRepliesWrite :: UUID -> RepliesWrite -> Db (Either ORMError ())
applyRepliesWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn commentParentId <> " = ?") [toField parentId] (Delete.emptyDelete @CommentTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applyCommentNestedOps parentId ops

insertNestedCreates :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @CommentTable @CommentRow (toCommentCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applyCommentNestedOps :: UUID -> CommentNestedOps -> Db (Either ORMError ())
applyCommentNestedOps parentId ops =
  sequenceNested
    [ deleteNested parentId ops.delete
    , deleteNested parentId ops.disconnect
    , updateChildRows parentId ops.update
    , upsertNested parentId ops.upsert
    , insertNestedCreates parentId ops.create
    , insertNestedCreateMany parentId ops.createMany
    , connectNested parentId ops.connect
    ]

deleteNested :: UUID -> [UUID] -> Db (Either ORMError ())
deleteNested _ [] = pure (Right ())
deleteNested parentId ids = do
  result <- Delete.deleteMany @CommentTable (in_ commentId ids `and_` eq commentParentId parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, CommentNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @CommentTable @CommentRow $
          Update.whereUpdate (fieldColumn commentId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn commentParentId <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @CommentTable (toCommentUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @CommentTable @CommentRow $
          Prelude.id $
          Insert.toInsertBuilder @CommentTable (toCommentCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [CommentNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @CommentTable (toCommentCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @CommentTable @CommentRow $
          Update.setField commentParentId parentId $
            Update.whereUpdate (fieldColumn commentId <> " = ?") [toField childId] $
              Update.emptyUpdate @CommentTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (LoadComment replies) =>
  CommentInclude replies ->
  CommentWriteCreate ->
  Db (Either ORMError (CommentWith replies))
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        CommentCreate { id = Just rootId, parentId = input.root.parentId, body = input.root.body }
  rootResult <-
    Insert.insert @CommentTable @CommentRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applyRepliesWrite rootId input.replies
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

updateNested ::
  (LoadComment replies) =>
  CommentInclude replies ->
  UUID ->
  CommentWriteUpdate ->
  Db (Either ORMError (CommentWith replies))
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @CommentTable @CommentRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.replies of
        Nothing -> pure (Right ())
        Just write -> applyRepliesWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

data CommentQuery include select = CommentQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where CommentTable)
  , orderBy_ :: [OrderBy CommentTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: CommentQuery () OmitSelect
emptyQuery =
  CommentQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

class ReadComment include select where
  findMany :: CommentQuery include select -> Db [CommentRead include select]
  findUnique :: CommentQuery include select -> Db (Either ORMError (Maybe (CommentRead include select)))
  findUniqueOrFail :: CommentQuery include select -> Db (Either ORMError (CommentRead include select))
  findFirst :: CommentQuery include select -> Db (Maybe (CommentRead include select))
  findFirstOrFail :: CommentQuery include select -> Db (Either ORMError (CommentRead include select))

instance (LoadComment replies) => ReadComment (CommentInclude replies) OmitSelect where
  findMany CommentQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadComment include_ roots
  findUnique CommentQuery {include_, where_} =
    case Ops.requireUniqueWhere @CommentTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (matching w))
        rows <- loadComment include_ roots
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique CommentQuery {where_} =
    Ops.findUniqueWhere @CommentTable @CommentRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique CommentQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @CommentTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @CommentTable @CommentRow (prepareIncludeRootQuery @CommentTable (matching w))
        loaded <- loadComment include_ roots
        let rows = map (toCommentWithPicked select_) loaded
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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
  findUnique CommentQuery {select_, where_} =
    case Ops.requireUniqueWhere @CommentTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseCommentPicked select_)
            (selectColumns (commentSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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

reload ::
  (LoadComment replies) =>
  CommentInclude replies ->
  UUID ->
  Db (Either ORMError (CommentWith replies))
reload include rootId = do
  found <- Ops.findUnique @CommentTable @CommentRow rootId
  case found of
    Nothing -> pure (Left (RecordNotFound "Record not found with primary key"))
    Just row -> do
      loaded <- loadComment include [row]
      pure $ case loaded of
        (one : _) -> Right one
        [] -> Left (RecordNotFound "Record not found with primary key")

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @CommentTable

deleteMany :: Where CommentTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @CommentTable