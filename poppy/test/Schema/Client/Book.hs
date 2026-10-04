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
    noInclude,
    BookInclude (..),
    ChapterInclude (..),
    NoInclude (..),
    ResolveInclude,
    BookQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    BookCreate (..),
    BookRow (..),
    BookSelect (..),
    BookPicked (..),
    bookSelect,
    BookUpdate (..),
    BookTable,
    bookId,
    BookWithChaptersPicked (..)
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
import qualified Poppy.Include as Include
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import qualified Poppy.Update as Update
import Poppy.Where (Where, and_, eq, in_)
import Schema.Book (BookCreate (..), BookRow (..), BookSelect (..), BookPicked (..), bookSelect, bookSelectColumns, parseBookPicked, toBookPicked, BookTable, BookUpdate (..), bookId)
import Schema.BookInclude ( BookInclude (..), ChapterInclude (..), BookWithChapters (..), ChapterWithSections (..), NoInclude (..), ResolveInclude)
import Schema.Chapter (ChapterCreate (..), ChapterRow (..), ChapterUpdate (..), ChapterTable, chapterId, chapterBookRef)

create :: BookCreate -> Db (Either ORMError BookRow)
create = Insert.insert @BookTable @BookRow

createMany :: [BookCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @BookTable

update :: UUID -> BookUpdate -> Db (Either ORMError BookRow)
update = Update.update @BookTable @BookRow

updateMany :: Where BookTable -> BookUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @BookTable

upsert :: BookCreate -> BookUpdate -> Db (Either ORMError BookRow)
upsert = Insert.upsert @BookTable @BookRow ["id"]

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
  (Include.ExecuteInclude BookTable include result) =>
  include ->
  BookWriteCreate ->
  Db (Either ORMError result)
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
        Right () -> Include.findUniqueOrFail @BookTable include rootId

updateNested ::
  (Include.ExecuteInclude BookTable include result) =>
  include ->
  UUID ->
  BookWriteUpdate ->
  Db (Either ORMError result)
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
        Right () -> Include.findUniqueOrFail @BookTable include rootId

noInclude :: NoInclude
noInclude = NoInclude BookInclude {chapters = Nothing}

data BookWithChaptersPicked = BookWithChaptersPicked
  { book :: BookPicked,
    chapters :: [ChapterWithSections]
  }
  deriving (Show, Eq)

toBookWithChaptersPicked :: BookSelect -> BookWithChapters -> BookWithChaptersPicked
toBookWithChaptersPicked select_ BookWithChapters {book, chapters} =
  BookWithChaptersPicked
    { book = toBookPicked select_ book
    , chapters = chapters
    }

data BookQuery include select = BookQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where BookTable)
  , orderBy_ :: [OrderBy BookTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: BookQuery NoInclude OmitSelect
emptyQuery =
  BookQuery {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

type instance ResolveInclude (BookQuery BookInclude OmitSelect) = BookWithChapters
type instance ResolveInclude (BookQuery NoInclude OmitSelect) = BookRow
type instance ResolveInclude (BookQuery BookInclude BookSelect) = BookWithChaptersPicked
type instance ResolveInclude (BookQuery NoInclude BookSelect) = BookPicked

class ReadBook include select where
  findMany :: BookQuery include select -> Db [ResolveInclude (BookQuery include select)]
  findUnique :: BookQuery include select -> Db (Either ORMError (Maybe (ResolveInclude (BookQuery include select))))
  findUniqueOrFail :: BookQuery include select -> Db (Either ORMError (ResolveInclude (BookQuery include select)))
  findFirst :: BookQuery include select -> Db (Maybe (ResolveInclude (BookQuery include select)))
  findFirstOrFail :: BookQuery include select -> Db (Either ORMError (ResolveInclude (BookQuery include select)))

instance ReadBook BookInclude OmitSelect where
  findMany BookQuery {include_, where_, orderBy_, limit_, offset_} =
    Include.findMany @BookTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique BookQuery {include_, where_} =
    case Ops.requireUniqueWhere @BookTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <- Include.findMany @BookTable include_ (matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst BookQuery {include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @BookTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadBook NoInclude OmitSelect where
  findMany BookQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @BookTable @BookRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique BookQuery {where_} =
    Ops.findUniqueWhere @BookTable @BookRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst BookQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @BookTable @BookRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadBook BookInclude BookSelect where
  findMany BookQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    rows <- Include.findMany @BookTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
    pure $ map (toBookWithChaptersPicked select_) rows
  findUnique BookQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @BookTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        nested <- Include.findMany @BookTable include_ (matching w)
        let rows = map (toBookWithChaptersPicked select_) nested
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst BookQuery {select_, include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @BookTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just (toBookWithChaptersPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadBook NoInclude BookSelect where
  findMany BookQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseBookPicked select_)
      (selectColumns (bookSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique BookQuery {select_, where_} =
    case Ops.requireUniqueWhere @BookTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseBookPicked select_)
            (selectColumns (bookSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @BookTable

deleteMany :: Where BookTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @BookTable