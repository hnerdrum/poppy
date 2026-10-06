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
module Schema.Client.Author
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
    AuthorWriteCreate (..),
    AuthorWriteUpdate (..),
    PostNestedCreate (..),
    PostsWrite (..),
    PostNestedOps (..),
    emptyPostNestedOps,
    AuthorQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    AuthorCreate (..),
    AuthorRow (..),
    AuthorSelect (..),
    AuthorPicked (..),
    authorSelect,
    AuthorUpdate (..),
    AuthorTable,
    authorId
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import Poppy.Core (fieldColumn)
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
import Schema.Author (AuthorCreate (..), AuthorRow (..), AuthorSelect (..), AuthorPicked (..), authorSelect, authorSelectColumns, parseAuthorPicked, AuthorTable, AuthorUpdate (..), authorId)
import Schema.Include.Author (LoadAuthor (..), AuthorInclude (..), AuthorRead, AuthorWith (..), toAuthorWithPicked)
import Schema.Post (PostCreate (..), PostRow (..), PostUpdate (..), PostTable, postId, postAuthorId)
import Schema.ArticleStatus (ArticleStatus (..))

create :: AuthorCreate -> Db (Either ORMError AuthorRow)
create = Insert.insert @AuthorTable @AuthorRow

createMany :: [AuthorCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @AuthorTable

update :: UUID -> AuthorUpdate -> Db (Either ORMError AuthorRow)
update = Update.update @AuthorTable @AuthorRow

updateMany :: Where AuthorTable -> AuthorUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @AuthorTable

upsert :: AuthorCreate -> AuthorUpdate -> Db (Either ORMError AuthorRow)
upsert = Insert.upsert @AuthorTable @AuthorRow ["id"]

data PostNestedCreate = PostNestedCreate
  { id :: Maybe UUID, title :: Text, status :: ArticleStatus
  }
  deriving (Show, Eq)

data PostNestedOps = PostNestedOps
  { create :: [PostNestedCreate]
  , createMany :: [PostNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, PostNestedCreate)]
  , upsert :: [PostNestedCreate]
  }
  deriving (Show, Eq)

data PostsWrite = Set [PostNestedCreate] | Ops PostNestedOps
  deriving (Show, Eq)

emptyPostNestedOps :: PostNestedOps
emptyPostNestedOps =
  PostNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data AuthorWriteCreate = AuthorWriteCreate
  { root :: AuthorCreate
  , posts :: PostsWrite
  }
  deriving (Show, Eq)

data AuthorWriteUpdate = AuthorWriteUpdate
  { root :: AuthorUpdate
  , posts :: Maybe PostsWrite
  }
  deriving (Show, Eq)

toPostCreate :: UUID -> PostNestedCreate -> PostCreate
toPostCreate parentId nested =
  PostCreate
    { id = nested.id,
      title = nested.title,
      status = nested.status,
      authorId = parentId
    }

toPostUpdate :: PostNestedCreate -> PostUpdate
toPostUpdate nested =
  PostUpdate
    { authorId = Nothing,
      title = Just nested.title,
      status = Just nested.status
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applyPostsWrite :: UUID -> PostsWrite -> Db (Either ORMError ())
applyPostsWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn postAuthorId <> " = ?") [toField parentId] (Delete.emptyDelete @PostTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applyPostNestedOps parentId ops

insertNestedCreates :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @PostTable @PostRow (toPostCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applyPostNestedOps :: UUID -> PostNestedOps -> Db (Either ORMError ())
applyPostNestedOps parentId ops =
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
  result <- Delete.deleteMany @PostTable (in_ postId ids `and_` eq postAuthorId parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, PostNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @PostTable @PostRow $
          Update.whereUpdate (fieldColumn postId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn postAuthorId <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @PostTable (toPostUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @PostTable @PostRow $
          Prelude.id $
          Insert.toInsertBuilder @PostTable (toPostCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @PostTable (toPostCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @PostTable @PostRow $
          Update.setField postAuthorId parentId $
            Update.whereUpdate (fieldColumn postId <> " = ?") [toField childId] $
              Update.emptyUpdate @PostTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (LoadAuthor posts) =>
  AuthorInclude posts ->
  AuthorWriteCreate ->
  Db (Either ORMError (AuthorWith posts))
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        AuthorCreate { id = Just rootId, name = input.root.name }
  rootResult <-
    Insert.insert @AuthorTable @AuthorRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applyPostsWrite rootId input.posts
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

updateNested ::
  (LoadAuthor posts) =>
  AuthorInclude posts ->
  UUID ->
  AuthorWriteUpdate ->
  Db (Either ORMError (AuthorWith posts))
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @AuthorTable @AuthorRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.posts of
        Nothing -> pure (Right ())
        Just write -> applyPostsWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

data AuthorQuery include select = AuthorQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where AuthorTable)
  , orderBy_ :: [OrderBy AuthorTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: AuthorQuery () OmitSelect
emptyQuery =
  AuthorQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

class ReadAuthor include select where
  findMany :: AuthorQuery include select -> Db [AuthorRead include select]
  findUnique :: AuthorQuery include select -> Db (Either ORMError (Maybe (AuthorRead include select)))
  findUniqueOrFail :: AuthorQuery include select -> Db (Either ORMError (AuthorRead include select))
  findFirst :: AuthorQuery include select -> Db (Maybe (AuthorRead include select))
  findFirstOrFail :: AuthorQuery include select -> Db (Either ORMError (AuthorRead include select))

instance (LoadAuthor posts) => ReadAuthor (AuthorInclude posts) OmitSelect where
  findMany AuthorQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadAuthor include_ roots
  findUnique AuthorQuery {include_, where_} =
    case Ops.requireUniqueWhere @AuthorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (matching w))
        rows <- loadAuthor include_ roots
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst AuthorQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadAuthor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadAuthor () OmitSelect where
  findMany AuthorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @AuthorTable @AuthorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique AuthorQuery {where_} =
    Ops.findUniqueWhere @AuthorTable @AuthorRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst AuthorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @AuthorTable @AuthorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadAuthor posts) => ReadAuthor (AuthorInclude posts) AuthorSelect where
  findMany AuthorQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadAuthor include_ roots
    pure $ map (toAuthorWithPicked select_) loaded
  findUnique AuthorQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @AuthorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (matching w))
        loaded <- loadAuthor include_ roots
        let rows = map (toAuthorWithPicked select_) loaded
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst AuthorQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadAuthor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toAuthorWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadAuthor () AuthorSelect where
  findMany AuthorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique AuthorQuery {select_, where_} =
    case Ops.requireUniqueWhere @AuthorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseAuthorPicked select_)
            (selectColumns (authorSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst AuthorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: AuthorQuery include select -> Db Int
count AuthorQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

reload ::
  (LoadAuthor posts) =>
  AuthorInclude posts ->
  UUID ->
  Db (Either ORMError (AuthorWith posts))
reload include rootId = do
  found <- Ops.findUnique @AuthorTable @AuthorRow rootId
  case found of
    Nothing -> pure (Left (RecordNotFound "Record not found with primary key"))
    Just row -> do
      loaded <- loadAuthor include [row]
      pure $ case loaded of
        (one : _) -> Right one
        [] -> Left (RecordNotFound "Record not found with primary key")

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @AuthorTable

deleteMany :: Where AuthorTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @AuthorTable