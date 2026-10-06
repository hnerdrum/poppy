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
module Schema.Client.Editor
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
    EditorWriteCreate (..),
    EditorWriteUpdate (..),
    ArticleNestedCreate (..),
    WrittenPostsWrite (..),
    ArticleNestedOps (..),
    emptyArticleNestedOps,
    EditorQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    EditorCreate (..),
    EditorRow (..),
    EditorSelect (..),
    EditorPicked (..),
    editorSelect,
    EditorUpdate (..),
    EditorTable,
    editorId
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
import Schema.Editor (EditorCreate (..), EditorRow (..), EditorSelect (..), EditorPicked (..), editorSelect, editorSelectColumns, parseEditorPicked, EditorTable, EditorUpdate (..), editorId)
import Schema.Include.Editor (LoadEditor (..), EditorInclude (..), EditorRead, EditorWith (..), toEditorWithPicked)
import Schema.Article (ArticleCreate (..), ArticleRow (..), ArticleUpdate (..), ArticleTable, articleId, articleAuthorId)

create :: EditorCreate -> Db (Either ORMError EditorRow)
create = Insert.insert @EditorTable @EditorRow

createMany :: [EditorCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @EditorTable

update :: UUID -> EditorUpdate -> Db (Either ORMError EditorRow)
update = Update.update @EditorTable @EditorRow

updateMany :: Where EditorTable -> EditorUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @EditorTable

upsert :: EditorCreate -> EditorUpdate -> Db (Either ORMError EditorRow)
upsert = Insert.upsert @EditorTable @EditorRow ["id"]

data ArticleNestedCreate = ArticleNestedCreate
  { id :: Maybe UUID, title :: Text
  }
  deriving (Show, Eq)

data ArticleNestedOps = ArticleNestedOps
  { create :: [ArticleNestedCreate]
  , createMany :: [ArticleNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, ArticleNestedCreate)]
  , upsert :: [ArticleNestedCreate]
  }
  deriving (Show, Eq)

data WrittenPostsWrite = Set [ArticleNestedCreate] | Ops ArticleNestedOps
  deriving (Show, Eq)

emptyArticleNestedOps :: ArticleNestedOps
emptyArticleNestedOps =
  ArticleNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data EditorWriteCreate = EditorWriteCreate
  { root :: EditorCreate
  , writtenPosts :: WrittenPostsWrite
  }
  deriving (Show, Eq)

data EditorWriteUpdate = EditorWriteUpdate
  { root :: EditorUpdate
  , writtenPosts :: Maybe WrittenPostsWrite
  }
  deriving (Show, Eq)

toArticleCreate :: UUID -> ArticleNestedCreate -> ArticleCreate
toArticleCreate parentId nested =
  ArticleCreate
    { id = nested.id,
      title = nested.title,
      authorId = parentId
    }

toArticleUpdate :: ArticleNestedCreate -> ArticleUpdate
toArticleUpdate nested =
  ArticleUpdate
    { authorId = Nothing,
      title = Just nested.title
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applyWrittenPostsWrite :: UUID -> WrittenPostsWrite -> Db (Either ORMError ())
applyWrittenPostsWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn articleAuthorId <> " = ?") [toField parentId] (Delete.emptyDelete @ArticleTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applyArticleNestedOps parentId ops

insertNestedCreates :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @ArticleTable @ArticleRow (toArticleCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applyArticleNestedOps :: UUID -> ArticleNestedOps -> Db (Either ORMError ())
applyArticleNestedOps parentId ops =
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
  result <- Delete.deleteMany @ArticleTable (in_ articleId ids `and_` eq articleAuthorId parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, ArticleNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @ArticleTable @ArticleRow $
          Update.whereUpdate (fieldColumn articleId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn articleAuthorId <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @ArticleTable (toArticleUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @ArticleTable @ArticleRow $
          Prelude.id $
          Insert.toInsertBuilder @ArticleTable (toArticleCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @ArticleTable (toArticleCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @ArticleTable @ArticleRow $
          Update.setField articleAuthorId parentId $
            Update.whereUpdate (fieldColumn articleId <> " = ?") [toField childId] $
              Update.emptyUpdate @ArticleTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (LoadEditor writtenPosts editedPosts) =>
  EditorInclude writtenPosts editedPosts ->
  EditorWriteCreate ->
  Db (Either ORMError (EditorWith writtenPosts editedPosts))
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        EditorCreate { id = Just rootId, name = input.root.name }
  rootResult <-
    Insert.insert @EditorTable @EditorRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applyWrittenPostsWrite rootId input.writtenPosts
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

updateNested ::
  (LoadEditor writtenPosts editedPosts) =>
  EditorInclude writtenPosts editedPosts ->
  UUID ->
  EditorWriteUpdate ->
  Db (Either ORMError (EditorWith writtenPosts editedPosts))
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @EditorTable @EditorRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.writtenPosts of
        Nothing -> pure (Right ())
        Just write -> applyWrittenPostsWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

data EditorQuery include select = EditorQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where EditorTable)
  , orderBy_ :: [OrderBy EditorTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: EditorQuery () OmitSelect
emptyQuery =
  EditorQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

class ReadEditor include select where
  findMany :: EditorQuery include select -> Db [EditorRead include select]
  findUnique :: EditorQuery include select -> Db (Either ORMError (Maybe (EditorRead include select)))
  findUniqueOrFail :: EditorQuery include select -> Db (Either ORMError (EditorRead include select))
  findFirst :: EditorQuery include select -> Db (Maybe (EditorRead include select))
  findFirstOrFail :: EditorQuery include select -> Db (Either ORMError (EditorRead include select))

instance (LoadEditor writtenPosts editedPosts) => ReadEditor (EditorInclude writtenPosts editedPosts) OmitSelect where
  findMany EditorQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadEditor include_ roots
  findUnique EditorQuery {include_, where_} =
    case Ops.requireUniqueWhere @EditorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (matching w))
        rows <- loadEditor include_ roots
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst EditorQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadEditor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadEditor () OmitSelect where
  findMany EditorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @EditorTable @EditorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique EditorQuery {where_} =
    Ops.findUniqueWhere @EditorTable @EditorRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst EditorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @EditorTable @EditorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadEditor writtenPosts editedPosts) => ReadEditor (EditorInclude writtenPosts editedPosts) EditorSelect where
  findMany EditorQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadEditor include_ roots
    pure $ map (toEditorWithPicked select_) loaded
  findUnique EditorQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @EditorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (matching w))
        loaded <- loadEditor include_ roots
        let rows = map (toEditorWithPicked select_) loaded
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst EditorQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadEditor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toEditorWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadEditor () EditorSelect where
  findMany EditorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseEditorPicked select_)
      (selectColumns (editorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique EditorQuery {select_, where_} =
    case Ops.requireUniqueWhere @EditorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseEditorPicked select_)
            (selectColumns (editorSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst EditorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseEditorPicked select_)
      (selectColumns (editorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: EditorQuery include select -> Db Int
count EditorQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @EditorTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

reload ::
  (LoadEditor writtenPosts editedPosts) =>
  EditorInclude writtenPosts editedPosts ->
  UUID ->
  Db (Either ORMError (EditorWith writtenPosts editedPosts))
reload include rootId = do
  found <- Ops.findUnique @EditorTable @EditorRow rootId
  case found of
    Nothing -> pure (Left (RecordNotFound "Record not found with primary key"))
    Just row -> do
      loaded <- loadEditor include [row]
      pure $ case loaded of
        (one : _) -> Right one
        [] -> Left (RecordNotFound "Record not found with primary key")

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @EditorTable

deleteMany :: Where EditorTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @EditorTable