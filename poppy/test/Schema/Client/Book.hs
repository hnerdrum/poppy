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
module Schema.Client.Book
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
    BookWriteCreate (..),
    BookWriteUpdate (..),
    ChapterNestedCreate (..),
    ChaptersWrite (..),
    ChapterNestedOps (..),
    emptyChapterNestedOps,
    BookQuery (..),
    BookUnique (..),
    BookUniqueKey (..),
    BookUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    OmitSelect (..),
    Picked (..),
    BookCreate (..),
    BookRow (..),
    BookSelect (..),
    BookPicked (..),
    bookSelect,
    BookUpdate (..),
    BookTable,
    bookId
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
import Schema.Book (BookCreate (..), BookRow (..), BookSelect (..), BookPicked (..), bookSelect, bookSelectColumns, parseBookPicked, BookTable, BookUpdate (..), bookId)
import Schema.Include.Book (LoadBook (..), BookInclude (..), BookRead, toBookWithPicked, BookWith (..))
import Schema.Chapter (ChapterCreate (..), ChapterRow (..), ChapterUpdate (..), ChapterTable, chapterId, chapterBookRef)

data BookUnique
  = ById UUID
  deriving (Eq, Show)

data BookUniqueKey
  = OnId
  deriving (Eq, Show)

bookUniqueWhere :: BookUnique -> Where BookTable
bookUniqueWhere = \case
  ById v1 -> eq bookId v1

bookConflictCols :: BookUniqueKey -> [Text]
bookConflictCols = \case
  OnId -> ["id"]

create :: BookCreate -> Db (Either ORMError BookRow)
create = Insert.insert @BookTable @BookRow

createMany :: [BookCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @BookTable

update :: BookUnique -> BookUpdate -> Db (Either ORMError BookRow)
update key input =
  Update.updateWhere @BookTable @BookRow (bookUniqueWhere key) input

updateMany :: Where BookTable -> BookUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @BookTable

upsert :: BookUniqueKey -> BookCreate -> BookUpdate -> Db (Either ORMError BookRow)
upsert key createInput updateInput =
  Insert.upsert @BookTable @BookRow (bookConflictCols key) createInput updateInput

data ChapterNestedCreate = ChapterNestedCreate
  { id :: Maybe UUID, heading :: Text
  }
  deriving (Show, Eq)

data ChapterNestedOps = ChapterNestedOps
  { create :: [ChapterNestedCreate]
  , createMany :: [ChapterNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, ChapterNestedCreate)]
  , upsert :: [ChapterNestedCreate]
  }
  deriving (Show, Eq)

data ChaptersWrite = Set [ChapterNestedCreate] | Ops ChapterNestedOps
  deriving (Show, Eq)

emptyChapterNestedOps :: ChapterNestedOps
emptyChapterNestedOps =
  ChapterNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data BookWriteCreate = BookWriteCreate
  { root :: BookCreate
  , chapters :: ChaptersWrite
  }
  deriving (Show, Eq)

data BookWriteUpdate = BookWriteUpdate
  { root :: BookUpdate
  , chapters :: Maybe ChaptersWrite
  }
  deriving (Show, Eq)

toChapterCreate :: UUID -> ChapterNestedCreate -> ChapterCreate
toChapterCreate parentId nested =
  ChapterCreate
    { id = nested.id,
      heading = nested.heading,
      bookRef = parentId
    }

toChapterUpdate :: ChapterNestedCreate -> ChapterUpdate
toChapterUpdate nested =
  ChapterUpdate
    { bookRef = Nothing,
      heading = Just nested.heading
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applyChaptersWrite :: UUID -> ChaptersWrite -> Db (Either ORMError ())
applyChaptersWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn chapterBookRef <> " = ?") [toField parentId] (Delete.emptyDelete @ChapterTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applyChapterNestedOps parentId ops

insertNestedCreates :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @ChapterTable @ChapterRow (toChapterCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applyChapterNestedOps :: UUID -> ChapterNestedOps -> Db (Either ORMError ())
applyChapterNestedOps parentId ops =
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
  result <- Delete.deleteMany @ChapterTable (in_ chapterId ids `and_` eq chapterBookRef parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, ChapterNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @ChapterTable @ChapterRow $
          Update.whereUpdate (fieldColumn chapterId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn chapterBookRef <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @ChapterTable (toChapterUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @ChapterTable @ChapterRow $
          Prelude.id $
          Insert.toInsertBuilder @ChapterTable (toChapterCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @ChapterTable (toChapterCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @ChapterTable @ChapterRow $
          Update.setField chapterBookRef parentId $
            Update.whereUpdate (fieldColumn chapterId <> " = ?") [toField childId] $
              Update.emptyUpdate @ChapterTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (LoadBook chapters) =>
  BookInclude chapters ->
  BookWriteCreate ->
  Db (Either ORMError (BookWith chapters))
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        BookCreate { id = Just rootId, shelfId = input.root.shelfId, title = input.root.title }
  rootResult <-
    Insert.insert @BookTable @BookRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applyChaptersWrite rootId input.chapters
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

updateNested ::
  (LoadBook chapters) =>
  BookInclude chapters ->
  UUID ->
  BookWriteUpdate ->
  Db (Either ORMError (BookWith chapters))
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @BookTable @BookRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.chapters of
        Nothing -> pure (Right ())
        Just write -> applyChaptersWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> reload include rootId

data BookQuery include select = BookQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where BookTable)
  , orderBy_ :: [OrderBy BookTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data BookUniqueQuery include select = BookUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: BookUnique
  }

emptyQuery :: BookQuery () OmitSelect
emptyQuery =
  BookQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: BookUnique -> BookUniqueQuery () OmitSelect
uniqueQuery key =
  BookUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadBook include select where
  findMany :: BookQuery include select -> Db [BookRead include select]
  findUnique :: BookUniqueQuery include select -> Db (Either ORMError (Maybe (BookRead include select)))
  findUniqueOrFail :: BookUniqueQuery include select -> Db (Either ORMError (BookRead include select))
  findFirst :: BookQuery include select -> Db (Maybe (BookRead include select))
  findFirstOrFail :: BookQuery include select -> Db (Either ORMError (BookRead include select))

instance (LoadBook chapters) => ReadBook (BookInclude chapters) OmitSelect where
  findMany BookQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadBook include_ roots
  findUnique BookUniqueQuery {where_, include_} = do
    let w = bookUniqueWhere where_
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (matching w))
    rows <- loadBook include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst BookQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadBook include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadBook () OmitSelect where
  findMany BookQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @BookTable @BookRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique BookUniqueQuery {where_} = do
    let w = bookUniqueWhere where_
    rows <- Ops.findMany @BookTable @BookRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst BookQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @BookTable @BookRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadBook chapters) => ReadBook (BookInclude chapters) BookSelect where
  findMany BookQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadBook include_ roots
    pure $ map (toBookWithPicked select_) loaded
  findUnique BookUniqueQuery {where_, include_, select_} = do
    let w = bookUniqueWhere where_
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (matching w))
    loaded <- loadBook include_ roots
    let rows = map (toBookWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst BookQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @BookTable @BookRow (prepareIncludeRootQuery @BookTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadBook include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toBookWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadBook () BookSelect where
  findMany BookQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseBookPicked select_)
      (selectColumns (bookSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique BookUniqueQuery {where_, select_} = do
    let w = bookUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseBookPicked select_)
        (selectColumns (bookSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst BookQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseBookPicked select_)
      (selectColumns (bookSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: BookQuery include select -> Db Int
count BookQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @BookTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

reload ::
  (LoadBook chapters) =>
  BookInclude chapters ->
  UUID ->
  Db (Either ORMError (BookWith chapters))
reload include rootId = do
  found <- Ops.findUnique @BookTable @BookRow rootId
  case found of
    Nothing -> pure (Left (RecordNotFound "Record not found with primary key"))
    Just row -> do
      loaded <- loadBook include [row]
      pure $ case loaded of
        (one : _) -> Right one
        [] -> Left (RecordNotFound "Record not found with primary key")

delete :: BookUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @BookTable (bookUniqueWhere key)

deleteMany :: Where BookTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @BookTable