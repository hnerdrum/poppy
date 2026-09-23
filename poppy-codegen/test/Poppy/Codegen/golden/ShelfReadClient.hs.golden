{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Shelf
  ( findMany,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstOrFail,
    count,
    withBooksTags,
    noInclude,
    ShelfInclude (..),
    BookInclude (..),
    ChapterInclude (..),
    NoInclude (..),
    ResolveInclude,
    ShelfQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    ShelfSelect (..),
    ShelfPicked (..),
    shelfSelect,
    ShelfWithBooksTagsPicked (..)
  )
where

import Poppy.Db (Db)
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Include as Include
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.Where (Where)
import Schema.Shelf (ShelfTable, ShelfRow, ShelfSelect (..), ShelfPicked (..), shelfSelect, shelfSelectColumns, parseShelfPicked, toShelfPicked)
import Schema.ShelfInclude ( ShelfInclude (..), BookInclude (..), ChapterInclude (..), ShelfWithBooksTags (..), BookWithChapters (..), NoInclude (..), ResolveInclude)
import Schema.Tag (TagRow (..))

withBooksTags :: ShelfInclude
withBooksTags =
  ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}), tags = True}

noInclude :: NoInclude
noInclude = NoInclude ShelfInclude {books = Nothing, tags = False}

data ShelfWithBooksTagsPicked = ShelfWithBooksTagsPicked
  { shelf :: ShelfPicked,
    books :: [BookWithChapters],
    tags :: [TagRow]
  }
  deriving (Show, Eq)

toShelfWithBooksTagsPicked :: ShelfSelect -> ShelfWithBooksTags -> ShelfWithBooksTagsPicked
toShelfWithBooksTagsPicked select_ ShelfWithBooksTags {shelf, books, tags} =
  ShelfWithBooksTagsPicked
    { shelf = toShelfPicked select_ shelf
    , books = books
    , tags = tags
    }

data ShelfQuery include select = ShelfQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where ShelfTable)
  , orderBy_ :: [OrderBy ShelfTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: ShelfQuery NoInclude OmitSelect
emptyQuery =
  ShelfQuery {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

type instance ResolveInclude (ShelfQuery ShelfInclude OmitSelect) = ShelfWithBooksTags
type instance ResolveInclude (ShelfQuery NoInclude OmitSelect) = ShelfRow
type instance ResolveInclude (ShelfQuery ShelfInclude ShelfSelect) = ShelfWithBooksTagsPicked
type instance ResolveInclude (ShelfQuery NoInclude ShelfSelect) = ShelfPicked

class ReadShelf include select where
  findMany :: ShelfQuery include select -> Db [ResolveInclude (ShelfQuery include select)]
  findUnique :: ShelfQuery include select -> Db (Either ORMError (Maybe (ResolveInclude (ShelfQuery include select))))
  findUniqueOrFail :: ShelfQuery include select -> Db (Either ORMError (ResolveInclude (ShelfQuery include select)))
  findFirst :: ShelfQuery include select -> Db (Maybe (ResolveInclude (ShelfQuery include select)))
  findFirstOrFail :: ShelfQuery include select -> Db (Either ORMError (ResolveInclude (ShelfQuery include select)))

instance ReadShelf ShelfInclude OmitSelect where
  findMany ShelfQuery {include_, where_, orderBy_, limit_, offset_} =
    Include.findMany @ShelfTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfQuery {include_, where_} =
    case Ops.requireUniqueWhere @ShelfTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <- Include.findMany @ShelfTable include_ (matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ShelfQuery {include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @ShelfTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadShelf NoInclude OmitSelect where
  findMany ShelfQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @ShelfTable @ShelfRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfQuery {where_} =
    Ops.findUniqueWhere @ShelfTable @ShelfRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ShelfQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @ShelfTable @ShelfRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadShelf ShelfInclude ShelfSelect where
  findMany ShelfQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    rows <- Include.findMany @ShelfTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
    pure $ map (toShelfWithBooksTagsPicked select_) rows
  findUnique ShelfQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @ShelfTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        nested <- Include.findMany @ShelfTable include_ (matching w)
        let rows = map (toShelfWithBooksTagsPicked select_) nested
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ShelfQuery {select_, include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @ShelfTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just (toShelfWithBooksTagsPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadShelf NoInclude ShelfSelect where
  findMany ShelfQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseShelfPicked select_)
      (selectColumns (shelfSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfQuery {select_, where_} =
    case Ops.requireUniqueWhere @ShelfTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseShelfPicked select_)
            (selectColumns (shelfSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
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