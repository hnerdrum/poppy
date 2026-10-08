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
    BookCreateScalars,
    BookUpdateScalars,
    ChapterNestedCreate (..),
    ChapterNestedUpsert (..),
    ChaptersUpdate (..),
    emptyChaptersUpdate,
    BookQuery (..),
    BookUnique (..),
    BookUniqueKey (..),
    BookUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    bookUniqueWhere,
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

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (UUID)
import Poppy.Internal.Generated
  ( Db,
    transactionEither,
    fieldColumn,
    toField,
    ORMError (..),
    fromUniqueRows,
    requireFound,
    uniqueOrFail,
    OrderBy,
    applyQueryModifiers,
    matching,
    selectColumns,
    OmitSelect (..),
    Picked (..),
    prepareIncludeRootQuery,
    Where,
    and_,
    eq
  )
import qualified Poppy.Internal.Generated as Delete
  ( deleteMany,
    deleteWhere,
    whereDelete,
    emptyDelete
  )
import qualified Poppy.Internal.Generated as Insert
  ( insert,
    insertMany,
    upsert
  )
import qualified Poppy.Internal.Generated as Ops
  ( findMany,
    findManyWith,
    findFirst,
    findFirstWith,
    count
  )
import qualified Poppy.Internal.Generated as Update
  ( updateWhere,
    updateMany
  )

import Schema.Book (BookRow (..), BookSelect (..), BookPicked (..), bookSelect, bookSelectColumns, parseBookPicked, BookTable, bookId)
import qualified Schema.Book as BookSchema (BookCreate (..), BookUpdate (..))

import Schema.Include.Book (LoadBook (..), BookInclude (..), BookRead, toBookWithPicked)
import Schema.Chapter (ChapterRow (..), ChapterTable, chapterId, chapterBookRef, ChapterCreate (..), ChapterUpdate (..))
import qualified Schema.Client.Chapter as Chapter (ChapterUnique (..), chapterUniqueWhere)

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

data ChapterNestedCreate
  = CreateChapter
      { id :: Maybe UUID, heading :: Text
      }
  | ConnectChapter Chapter.ChapterUnique
  deriving (Show, Eq)
data ChapterNestedUpsert = ChapterNestedUpsert
  { where_ :: Chapter.ChapterUnique
  , create :: ChapterNestedCreate
  , update :: ChapterUpdate
  }
  deriving (Show, Eq)
data ChaptersUpdate = ChaptersUpdate
  { replaceWith :: Maybe [ChapterNestedCreate]
  , create :: [ChapterNestedCreate]
  , createMany :: [ChapterNestedCreate]
  , connect :: [Chapter.ChapterUnique]
  , delete :: [Chapter.ChapterUnique]
  , update :: [(Chapter.ChapterUnique, ChapterUpdate)]
  , upsert :: [ChapterNestedUpsert]
  }
  deriving (Show, Eq)

emptyChaptersUpdate :: ChaptersUpdate
emptyChaptersUpdate =
  ChaptersUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data BookCreate = BookCreate
  { id :: Maybe UUID,
    shelfId :: UUID,
    title :: Text,
    chapters :: [ChapterNestedCreate]
  }
  deriving (Show, Eq)

type BookCreateScalars = BookSchema.BookCreate

toBookCreateScalars :: BookCreate -> BookCreateScalars
toBookCreateScalars input =
  BookSchema.BookCreate
    { id = input.id,
      shelfId = input.shelfId,
      title = input.title
    }
data BookUpdate = BookUpdate
  { shelfId :: Maybe UUID,
    title :: Maybe Text,
    chapters :: Maybe ChaptersUpdate
  }
  deriving (Show, Eq)

type BookUpdateScalars = BookSchema.BookUpdate

toBookUpdateScalars :: BookUpdate -> BookUpdateScalars
toBookUpdateScalars input =
  BookSchema.BookUpdate
    { shelfId = input.shelfId,
      title = input.title
    }

create :: BookCreate -> Db (Either ORMError BookRow)
create input =
  if hasBookNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @BookTable @BookRow (toBookCreateScalars input)

hasBookNestedCreate :: BookCreate -> Bool
hasBookNestedCreate input =
  not (null input.chapters)

createWithNested :: BookCreate -> Db (Either ORMError BookRow)
createWithNested input = do
  rootResult <- Insert.insert @BookTable @BookRow (toBookCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applyChaptersCreate row.id input.chapters
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [BookCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @BookTable

update :: BookUnique -> BookUpdate -> Db (Either ORMError BookRow)
update key input =
  if hasBookNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @BookTable @BookRow (bookUniqueWhere key) (toBookUpdateScalars input)

hasBookNestedUpdate :: BookUpdate -> Bool
hasBookNestedUpdate input =
  isJust input.chapters

updateWithNested :: BookUnique -> BookUpdate -> Db (Either ORMError BookRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @BookTable @BookRow (bookUniqueWhere key) (toBookUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applyChaptersUpdate row.id) input.chapters
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where BookTable -> BookUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @BookTable

upsert :: BookUniqueKey -> BookCreateScalars -> BookUpdateScalars -> Db (Either ORMError BookRow)
upsert key createInput updateInput =
  Insert.upsert @BookTable @BookRow (bookConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applyChaptersCreate :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
applyChaptersCreate = insertChapters
applyChaptersUpdate :: UUID -> ChaptersUpdate -> Db (Either ORMError ())
applyChaptersUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceChapters parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteChapters parentId ops.delete
        , updateChaptersRows parentId ops.update
        , upsertChapters parentId ops.upsert
        , insertChapters parentId ops.create
        , insertChapters parentId ops.createMany
        , connectChapters parentId ops.connect
        ]
replaceChapters :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
replaceChapters parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn chapterBookRef <> " = ?") [toField parentId] (Delete.emptyDelete @ChapterTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertChapters parentId items
insertChapters :: UUID -> [ChapterNestedCreate] -> Db (Either ORMError ())
insertChapters parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectChapter key -> connectChapters parentId [key]
        CreateChapter {id, heading} -> do
          inserted <- Insert.insert @ChapterTable @ChapterRow ChapterCreate
            { id = id,
              heading = heading,
              bookRef = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteChapters :: UUID -> [Chapter.ChapterUnique] -> Db (Either ORMError ())
deleteChapters _ [] = pure (Right ())
deleteChapters parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @ChapterTable
        (Chapter.chapterUniqueWhere key `and_` eq chapterBookRef parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateChaptersRows :: UUID -> [(Chapter.ChapterUnique, ChapterUpdate)] -> Db (Either ORMError ())
updateChaptersRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = ChapterUpdate { bookRef = Nothing, heading = nested.heading }
      result <-
        Update.updateWhere @ChapterTable @ChapterRow
          (Chapter.chapterUniqueWhere key `and_` eq chapterBookRef parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertChapters :: UUID -> [ChapterNestedUpsert] -> Db (Either ORMError ())
upsertChapters parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @ChapterTable @ChapterRow (matching (Chapter.chapterUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertChapters parentId [item.create]
        Right (Just row) ->
          if row.bookRef == parentId
            then do
              let patched = ChapterUpdate { bookRef = Nothing, heading = item.update.heading }
              updated <-
                Update.updateWhere @ChapterTable @ChapterRow
                  (Chapter.chapterUniqueWhere item.where_ `and_` eq chapterBookRef parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectChapters :: UUID -> [Chapter.ChapterUnique] -> Db (Either ORMError ())
connectChapters _ [] = pure (Right ())
connectChapters parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @ChapterTable @ChapterRow
          (Chapter.chapterUniqueWhere key)
          (ChapterUpdate
            { bookRef = Just parentId,
              heading = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

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

delete :: BookUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @BookTable (bookUniqueWhere key)

deleteMany :: Where BookTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @BookTable