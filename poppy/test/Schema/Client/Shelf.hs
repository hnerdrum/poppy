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

Nested writes ('createNested' / 'updateNested'):
  Set xs  — replace all children with xs
  Ops o   — create, connect, disconnect, delete, update, upsert
-}
module Schema.Client.Shelf
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
    ShelfWriteCreate (..),
    ShelfWriteUpdate (..),
    BookNestedCreate (..),
    BooksWrite (..),
    BookNestedOps (..),
    emptyBookNestedOps,
    ShelfQuery (..),
    ShelfUnique (..),
    ShelfUniqueKey (..),
    ShelfUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    OmitSelect (..),
    Picked (..),
    ShelfCreate (..),
    ShelfRow (..),
    ShelfSelect (..),
    ShelfPicked (..),
    shelfSelect,
    ShelfUpdate (..),
    ShelfTable,
    shelfId
  )
where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import Poppy.Core (fieldColumn)
import Poppy.PG (toField)
import Poppy.Db (Db, liftIO, transactionEither)
import qualified Poppy.Delete as Delete
import Poppy.Errors (ORMError (..), fromUniqueRows, requireFound, uniqueOrFail)
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.SelectIn (prepareIncludeRootQuery)
import qualified Poppy.Update as Update
import Poppy.Where (Where, and_, eq, in_)
import Schema.Shelf (ShelfCreate (..), ShelfRow (..), ShelfSelect (..), ShelfPicked (..), shelfSelect, shelfSelectColumns, parseShelfPicked, ShelfTable, ShelfUpdate (..), shelfId)
import Schema.Include.Shelf (LoadShelf (..), ShelfInclude (..), ShelfRead, toShelfWithPicked, ShelfWith (..))
import Schema.Book (BookCreate (..), BookRow (..), BookUpdate (..), BookTable, bookId, bookShelfId)

data ShelfUnique
  = ById UUID
  deriving (Eq, Show)

data ShelfUniqueKey
  = OnId
  deriving (Eq, Show)

shelfUniqueWhere :: ShelfUnique -> Where ShelfTable
shelfUniqueWhere = \case
  ById v1 -> eq shelfId v1

shelfConflictCols :: ShelfUniqueKey -> [Text]
shelfConflictCols = \case
  OnId -> ["id"]

create :: ShelfCreate -> Db (Either ORMError ShelfRow)
create = Insert.insert @ShelfTable @ShelfRow

createMany :: [ShelfCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ShelfTable

update :: ShelfUnique -> ShelfUpdate -> Db (Either ORMError ShelfRow)
update key input =
  Update.updateWhere @ShelfTable @ShelfRow (shelfUniqueWhere key) input

updateMany :: Where ShelfTable -> ShelfUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @ShelfTable

upsert :: ShelfUniqueKey -> ShelfCreate -> ShelfUpdate -> Db (Either ORMError ShelfRow)
upsert key createInput updateInput =
  Insert.upsert @ShelfTable @ShelfRow (shelfConflictCols key) createInput updateInput

data BookNestedCreate = BookNestedCreate
  { id :: Maybe UUID, title :: Text
  }
  deriving (Show, Eq)

data BookNestedOps = BookNestedOps
  { create :: [BookNestedCreate]
  , createMany :: [BookNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, BookNestedCreate)]
  , upsert :: [BookNestedCreate]
  }
  deriving (Show, Eq)

data BooksWrite = Set [BookNestedCreate] | Ops BookNestedOps
  deriving (Show, Eq)

emptyBookNestedOps :: BookNestedOps
emptyBookNestedOps =
  BookNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data ShelfWriteCreate = ShelfWriteCreate
  { root :: ShelfCreate
  , books :: BooksWrite
  }
  deriving (Show, Eq)

data ShelfWriteUpdate = ShelfWriteUpdate
  { root :: ShelfUpdate
  , books :: Maybe BooksWrite
  }
  deriving (Show, Eq)

toBookCreate :: UUID -> BookNestedCreate -> BookCreate
toBookCreate parentId nested =
  BookCreate
    { id = nested.id,
      title = nested.title,
      shelfId = parentId
    }

toBookUpdate :: BookNestedCreate -> BookUpdate
toBookUpdate nested =
  BookUpdate
    { shelfId = Nothing,
      title = Just nested.title
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applyBooksWrite :: UUID -> BooksWrite -> Db (Either ORMError ())
applyBooksWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn bookShelfId <> " = ?") [toField parentId] (Delete.emptyDelete @BookTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applyBookNestedOps parentId ops

insertNestedCreates :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @BookTable @BookRow (toBookCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applyBookNestedOps :: UUID -> BookNestedOps -> Db (Either ORMError ())
applyBookNestedOps parentId ops =
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
  result <- Delete.deleteMany @BookTable (in_ bookId ids `and_` eq bookShelfId parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, BookNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @BookTable @BookRow $
          Update.whereUpdate (fieldColumn bookId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn bookShelfId <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @BookTable (toBookUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @BookTable @BookRow $
          Prelude.id $
          Insert.toInsertBuilder @BookTable (toBookCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @BookTable (toBookCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @BookTable @BookRow $
          Update.setField bookShelfId parentId $
            Update.whereUpdate (fieldColumn bookId <> " = ?") [toField childId] $
              Update.emptyUpdate @BookTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (LoadShelf books tags) =>
  ShelfInclude books tags ->
  ShelfWriteCreate ->
  Db (Either ORMError (ShelfWith books tags))
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        ShelfCreate { id = Just rootId, name = input.root.name }
  rootResult <-
    Insert.insert @ShelfTable @ShelfRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applyBooksWrite rootId input.books
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

updateNested ::
  (LoadShelf books tags) =>
  ShelfInclude books tags ->
  UUID ->
  ShelfWriteUpdate ->
  Db (Either ORMError (ShelfWith books tags))
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @ShelfTable @ShelfRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.books of
        Nothing -> pure (Right ())
        Just write -> applyBooksWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

data ShelfQuery include select = ShelfQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where ShelfTable)
  , orderBy_ :: [OrderBy ShelfTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data ShelfUniqueQuery include select = ShelfUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: ShelfUnique
  }

emptyQuery :: ShelfQuery () OmitSelect
emptyQuery =
  ShelfQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: ShelfUnique -> ShelfUniqueQuery () OmitSelect
uniqueQuery key =
  ShelfUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadShelf include select where
  findMany :: ShelfQuery include select -> Db [ShelfRead include select]
  findUnique :: ShelfUniqueQuery include select -> Db (Either ORMError (Maybe (ShelfRead include select)))
  findUniqueOrFail :: ShelfUniqueQuery include select -> Db (Either ORMError (ShelfRead include select))
  findFirst :: ShelfQuery include select -> Db (Maybe (ShelfRead include select))
  findFirstOrFail :: ShelfQuery include select -> Db (Either ORMError (ShelfRead include select))

instance (LoadShelf books tags) => ReadShelf (ShelfInclude books tags) OmitSelect where
  findMany ShelfQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadShelf include_ roots
  findUnique ShelfUniqueQuery {where_, include_} = do
    let w = shelfUniqueWhere where_
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (matching w))
    rows <- loadShelf include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ShelfQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadShelf include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadShelf () OmitSelect where
  findMany ShelfQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @ShelfTable @ShelfRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfUniqueQuery {where_} = do
    let w = shelfUniqueWhere where_
    rows <- Ops.findMany @ShelfTable @ShelfRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ShelfQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @ShelfTable @ShelfRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadShelf books tags) => ReadShelf (ShelfInclude books tags) ShelfSelect where
  findMany ShelfQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadShelf include_ roots
    pure $ map (toShelfWithPicked select_) loaded
  findUnique ShelfUniqueQuery {where_, include_, select_} = do
    let w = shelfUniqueWhere where_
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (matching w))
    loaded <- loadShelf include_ roots
    let rows = map (toShelfWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ShelfQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @ShelfTable @ShelfRow (prepareIncludeRootQuery @ShelfTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadShelf include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toShelfWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadShelf () ShelfSelect where
  findMany ShelfQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseShelfPicked select_)
      (selectColumns (shelfSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfUniqueQuery {where_, select_} = do
    let w = shelfUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseShelfPicked select_)
        (selectColumns (shelfSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ShelfQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseShelfPicked select_)
      (selectColumns (shelfSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: ShelfQuery include select -> Db Int
count ShelfQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @ShelfTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

reload ::
  (LoadShelf books tags) =>
  ShelfInclude books tags ->
  UUID ->
  Db (Either ORMError (ShelfWith books tags))
reload include rootId = do
  found <- Ops.findUnique @ShelfTable @ShelfRow rootId
  case found of
    Nothing -> pure (Left (RecordNotFound "Record not found with primary key"))
    Just row -> do
      loaded <- loadShelf include [row]
      pure $ case loaded of
        (one : _) -> Right one
        [] -> Left (RecordNotFound "Record not found with primary key")

delete :: ShelfUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @ShelfTable (shelfUniqueWhere key)

deleteMany :: Where ShelfTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ShelfTable