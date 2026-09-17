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
    withBooksTags,
    noInclude,
    WithBooksTags (..),
    NoInclude (..),
    ResolveInclude,
    ShelfQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    ShelfSelect (..),
    ShelfPicked (..),
    shelfSelect,
    ShelfWithBooksTagsPicked (..),
  )
where

import Data.UUID (UUID)
import ORM.Db (Db)
import ORM.Errors (ORMError (..), requireFound)
import qualified ORM.Include as Include
import qualified ORM.Operations as Ops
import ORM.Query (QueryBuilder, applyQueryModifiers, matching, selectColumns)
import ORM.Select (OmitSelect (..), Picked (..))
import ORM.Where (Where)
import Schema.Shelf (ShelfPicked (..), ShelfRow, ShelfSelect (..), ShelfTable, parseShelfPicked, shelfSelect, shelfSelectColumns, toShelfPicked)
import Schema.ShelfInclude (BookInclude (..), BookWithChapter (..), BookWithChapters (..), Books (..), BooksChapters (..), BooksChaptersSections (..), BooksChaptersTags (..), BooksTags (..), ChapterInclude (..), ChapterWithSections (..), Chapters (..), ChaptersSections (..), CombineInclude (..), IncludeBooks (..), IncludeChapters (..), IncludeTags (..), NoInclude (..), ResolveInclude, Sections (..), ShelfInclude (..), ShelfWithBook (..), ShelfWithBookTag (..), ShelfWithBooksChapter (..), ShelfWithBooksChapterTag (..), ShelfWithBooksChaptersSection (..), ShelfWithBooksTags (..), ShelfWithTag (..), Tags (..), WithBooksTags (..), unwrapBooks, unwrapBooksChapters, unwrapBooksChaptersSections, unwrapBooksChaptersTags, unwrapBooksTags, unwrapTags, unwrapWithBooksTags)
import Schema.Tag (TagRow (..))

withBooksTags :: WithBooksTags
withBooksTags =
  WithBooksTags ShelfInclude {books = Just (BookInclude {chapters = Just (ChapterInclude {sections = True})}), tags = True}

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
    { shelf = toShelfPicked select_ shelf,
      books = books,
      tags = tags
    }

data ShelfQuery include select = ShelfQuery
  { include_ :: include,
    select_ :: select,
    where_ :: Maybe (Where ShelfTable),
    orderBy_ :: Maybe (QueryBuilder ShelfTable -> QueryBuilder ShelfTable),
    limit_ :: Maybe Int,
    offset_ :: Maybe Int
  }

emptyQuery :: ShelfQuery NoInclude OmitSelect
emptyQuery =
  ShelfQuery {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = Nothing, limit_ = Nothing, offset_ = Nothing}

type instance ResolveInclude (ShelfQuery WithBooksTags OmitSelect) = ShelfWithBooksTags

type instance ResolveInclude (ShelfQuery NoInclude OmitSelect) = ShelfRow

type instance ResolveInclude (ShelfQuery WithBooksTags ShelfSelect) = ShelfWithBooksTagsPicked

type instance ResolveInclude (ShelfQuery NoInclude ShelfSelect) = ShelfPicked

class ReadShelf include select where
  findMany :: ShelfQuery include select -> Db [ResolveInclude (ShelfQuery include select)]
  findUnique :: ShelfQuery include select -> Db (Either ORMError (Maybe (ResolveInclude (ShelfQuery include select))))
  findUniqueOrFail :: ShelfQuery include select -> Db (Either ORMError (ResolveInclude (ShelfQuery include select)))

instance ReadShelf WithBooksTags OmitSelect where
  findMany ShelfQuery {include_, where_, orderBy_, limit_, offset_} =
    Include.findMany @ShelfTable (unwrapWithBooksTags include_) (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ShelfQuery {include_, where_} =
    case Ops.requireUniqueWhere @ShelfTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <- Include.findMany @ShelfTable (unwrapWithBooksTags include_) (matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")

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

instance ReadShelf WithBooksTags ShelfSelect where
  findMany ShelfQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    rows <- Include.findMany @ShelfTable (unwrapWithBooksTags include_) (applyQueryModifiers where_ orderBy_ limit_ offset_)
    pure $ map (toShelfWithBooksTagsPicked select_) rows
  findUnique ShelfQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @ShelfTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        nested <- Include.findMany @ShelfTable (unwrapWithBooksTags include_) (matching w)
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