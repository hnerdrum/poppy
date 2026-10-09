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
    ShelfCreateScalars,
    ShelfUpdateScalars,
    BookNestedCreate (..),
    BookNestedUpsert (..),
    BooksUpdate (..),
    emptyBooksUpdate,
    TagNestedCreate (..),
    TagNestedUpsert (..),
    TagsUpdate (..),
    emptyTagsUpdate,
    ShelfQuery (..),
    ShelfUnique (..),
    ShelfUniqueKey (..),
    ShelfUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    shelfUniqueWhere,
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

import Schema.Shelf (ShelfRow (..), ShelfSelect (..), ShelfPicked (..), shelfSelect, shelfSelectColumns, parseShelfPicked, ShelfTable, shelfId)
import qualified Schema.Shelf as ShelfSchema (ShelfCreate (..), ShelfUpdate (..))

import Schema.Include.Shelf (LoadShelf (..), ShelfInclude (..), ShelfRead, toShelfWithPicked)
import Schema.Book (BookRow (..), BookTable, bookId, bookShelfId, BookCreate (..), BookUpdate (..))
import Schema.Tag (TagRow (..), TagTable, tagId, tagShelfId, TagCreate (..), TagUpdate (..))
import qualified Schema.Client.Book as Book (BookUnique (..), bookUniqueWhere)
import qualified Schema.Client.Tag as Tag (TagUnique (..), tagUniqueWhere)

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

data BookNestedCreate
  = CreateBook
      { id :: Maybe UUID, title :: Text
      }
  | ConnectBook Book.BookUnique
  deriving (Show, Eq)
data TagNestedCreate
  = CreateTag
      { id :: Maybe UUID, label :: Text
      }
  | ConnectTag Tag.TagUnique
  deriving (Show, Eq)
data BookNestedUpsert = BookNestedUpsert
  { where_ :: Book.BookUnique
  , create :: BookNestedCreate
  , update :: BookUpdate
  }
  deriving (Show, Eq)
data TagNestedUpsert = TagNestedUpsert
  { where_ :: Tag.TagUnique
  , create :: TagNestedCreate
  , update :: TagUpdate
  }
  deriving (Show, Eq)
data BooksUpdate = BooksUpdate
  { replaceWith :: Maybe [BookNestedCreate]
  , create :: [BookNestedCreate]
  , createMany :: [BookNestedCreate]
  , connect :: [Book.BookUnique]
  , delete :: [Book.BookUnique]
  , update :: [(Book.BookUnique, BookUpdate)]
  , upsert :: [BookNestedUpsert]
  }
  deriving (Show, Eq)

emptyBooksUpdate :: BooksUpdate
emptyBooksUpdate =
  BooksUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data TagsUpdate = TagsUpdate
  { replaceWith :: Maybe [TagNestedCreate]
  , create :: [TagNestedCreate]
  , createMany :: [TagNestedCreate]
  , connect :: [Tag.TagUnique]
  , delete :: [Tag.TagUnique]
  , update :: [(Tag.TagUnique, TagUpdate)]
  , upsert :: [TagNestedUpsert]
  }
  deriving (Show, Eq)

emptyTagsUpdate :: TagsUpdate
emptyTagsUpdate =
  TagsUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data ShelfCreate = ShelfCreate
  { id :: Maybe UUID,
    name :: Text,
    books :: [BookNestedCreate],
    tags :: [TagNestedCreate]
  }
  deriving (Show, Eq)

type ShelfCreateScalars = ShelfSchema.ShelfCreate

toShelfCreateScalars :: ShelfCreate -> ShelfCreateScalars
toShelfCreateScalars input =
  ShelfSchema.ShelfCreate
    { id = input.id,
      name = input.name
    }
data ShelfUpdate = ShelfUpdate
  { name :: Maybe Text,
    books :: Maybe BooksUpdate,
    tags :: Maybe TagsUpdate
  }
  deriving (Show, Eq)

type ShelfUpdateScalars = ShelfSchema.ShelfUpdate

toShelfUpdateScalars :: ShelfUpdate -> ShelfUpdateScalars
toShelfUpdateScalars input =
  ShelfSchema.ShelfUpdate
    { name = input.name
    }

create :: ShelfCreate -> Db (Either ORMError ShelfRow)
create input =
  if hasShelfNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @ShelfTable @ShelfRow (toShelfCreateScalars input)

hasShelfNestedCreate :: ShelfCreate -> Bool
hasShelfNestedCreate input =
  not (null input.books) || not (null input.tags)

createWithNested :: ShelfCreate -> Db (Either ORMError ShelfRow)
createWithNested input = do
  rootResult <- Insert.insert @ShelfTable @ShelfRow (toShelfCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applyBooksCreate row.id input.books
          , applyTagsCreate row.id input.tags
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [ShelfCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ShelfTable

update :: ShelfUnique -> ShelfUpdate -> Db (Either ORMError ShelfRow)
update key input =
  if hasShelfNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @ShelfTable @ShelfRow (shelfUniqueWhere key) (toShelfUpdateScalars input)

hasShelfNestedUpdate :: ShelfUpdate -> Bool
hasShelfNestedUpdate input =
  isJust input.books || isJust input.tags

updateWithNested :: ShelfUnique -> ShelfUpdate -> Db (Either ORMError ShelfRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @ShelfTable @ShelfRow (shelfUniqueWhere key) (toShelfUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applyBooksUpdate row.id) input.books
          , maybe (pure (Right ())) (applyTagsUpdate row.id) input.tags
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where ShelfTable -> ShelfUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @ShelfTable

upsert :: ShelfUniqueKey -> ShelfCreateScalars -> ShelfUpdateScalars -> Db (Either ORMError ShelfRow)
upsert key createInput updateInput =
  Insert.upsert @ShelfTable @ShelfRow (shelfConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applyBooksCreate :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
applyBooksCreate = insertBooks
applyBooksUpdate :: UUID -> BooksUpdate -> Db (Either ORMError ())
applyBooksUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceBooks parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteBooks parentId ops.delete
        , updateBooksRows parentId ops.update
        , upsertBooks parentId ops.upsert
        , insertBooks parentId ops.create
        , insertBooks parentId ops.createMany
        , connectBooks parentId ops.connect
        ]
replaceBooks :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
replaceBooks parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn bookShelfId <> " = ?") [toField parentId] (Delete.emptyDelete @BookTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertBooks parentId items
insertBooks :: UUID -> [BookNestedCreate] -> Db (Either ORMError ())
insertBooks parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectBook key -> connectBooks parentId [key]
        CreateBook {id, title} -> do
          inserted <- Insert.insert @BookTable @BookRow BookCreate
            { id = id,
              title = title,
              shelfId = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteBooks :: UUID -> [Book.BookUnique] -> Db (Either ORMError ())
deleteBooks _ [] = pure (Right ())
deleteBooks parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @BookTable
        (Book.bookUniqueWhere key `and_` eq bookShelfId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateBooksRows :: UUID -> [(Book.BookUnique, BookUpdate)] -> Db (Either ORMError ())
updateBooksRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = BookUpdate { shelfId = Nothing, title = nested.title }
      result <-
        Update.updateWhere @BookTable @BookRow
          (Book.bookUniqueWhere key `and_` eq bookShelfId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertBooks :: UUID -> [BookNestedUpsert] -> Db (Either ORMError ())
upsertBooks parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @BookTable @BookRow (matching (Book.bookUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertBooks parentId [item.create]
        Right (Just row) ->
          if row.shelfId == parentId
            then do
              let patched = BookUpdate { shelfId = Nothing, title = item.update.title }
              updated <-
                Update.updateWhere @BookTable @BookRow
                  (Book.bookUniqueWhere item.where_ `and_` eq bookShelfId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectBooks :: UUID -> [Book.BookUnique] -> Db (Either ORMError ())
connectBooks _ [] = pure (Right ())
connectBooks parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @BookTable @BookRow
          (Book.bookUniqueWhere key)
          (BookUpdate
            { shelfId = Just parentId,
              title = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
applyTagsCreate :: UUID -> [TagNestedCreate] -> Db (Either ORMError ())
applyTagsCreate = insertTags
applyTagsUpdate :: UUID -> TagsUpdate -> Db (Either ORMError ())
applyTagsUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceTags parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteTags parentId ops.delete
        , updateTagsRows parentId ops.update
        , upsertTags parentId ops.upsert
        , insertTags parentId ops.create
        , insertTags parentId ops.createMany
        , connectTags parentId ops.connect
        ]
replaceTags :: UUID -> [TagNestedCreate] -> Db (Either ORMError ())
replaceTags parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn tagShelfId <> " = ?") [toField parentId] (Delete.emptyDelete @TagTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertTags parentId items
insertTags :: UUID -> [TagNestedCreate] -> Db (Either ORMError ())
insertTags parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectTag key -> connectTags parentId [key]
        CreateTag {id, label} -> do
          inserted <- Insert.insert @TagTable @TagRow TagCreate
            { id = id,
              label = label,
              shelfId = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteTags :: UUID -> [Tag.TagUnique] -> Db (Either ORMError ())
deleteTags _ [] = pure (Right ())
deleteTags parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @TagTable
        (Tag.tagUniqueWhere key `and_` eq tagShelfId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateTagsRows :: UUID -> [(Tag.TagUnique, TagUpdate)] -> Db (Either ORMError ())
updateTagsRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = TagUpdate { shelfId = Nothing, label = nested.label }
      result <-
        Update.updateWhere @TagTable @TagRow
          (Tag.tagUniqueWhere key `and_` eq tagShelfId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertTags :: UUID -> [TagNestedUpsert] -> Db (Either ORMError ())
upsertTags parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @TagTable @TagRow (matching (Tag.tagUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertTags parentId [item.create]
        Right (Just row) ->
          if row.shelfId == parentId
            then do
              let patched = TagUpdate { shelfId = Nothing, label = item.update.label }
              updated <-
                Update.updateWhere @TagTable @TagRow
                  (Tag.tagUniqueWhere item.where_ `and_` eq tagShelfId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectTags :: UUID -> [Tag.TagUnique] -> Db (Either ORMError ())
connectTags _ [] = pure (Right ())
connectTags parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @TagTable @TagRow
          (Tag.tagUniqueWhere key)
          (TagUpdate
            { shelfId = Just parentId,
              label = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

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

delete :: ShelfUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @ShelfTable (shelfUniqueWhere key)

deleteMany :: Where ShelfTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ShelfTable