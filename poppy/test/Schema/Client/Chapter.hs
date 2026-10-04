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
module Schema.Client.Chapter
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
    ChapterWriteCreate (..),
    ChapterWriteUpdate (..),
    SectionNestedCreate (..),
    SectionsWrite (..),
    SectionNestedOps (..),
    emptySectionNestedOps,
    noInclude,
    ChapterInclude (..),
    NoInclude (..),
    ResolveInclude,
    ChapterQuery (..),
    emptyQuery,
    OmitSelect (..),
    Picked (..),
    ChapterCreate (..),
    ChapterRow (..),
    ChapterSelect (..),
    ChapterPicked (..),
    chapterSelect,
    ChapterUpdate (..),
    ChapterTable,
    chapterId,
    ChapterWithSectionsPicked (..)
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
import Schema.Chapter (ChapterCreate (..), ChapterRow (..), ChapterSelect (..), ChapterPicked (..), chapterSelect, chapterSelectColumns, parseChapterPicked, toChapterPicked, ChapterTable, ChapterUpdate (..), chapterId)
import Schema.ChapterInclude ( ChapterInclude (..), ChapterWithSections (..), NoInclude (..), ResolveInclude)
import Schema.Section (SectionCreate (..), SectionRow (..), SectionUpdate (..), SectionTable, sectionId, sectionChapterRef)

create :: ChapterCreate -> Db (Either ORMError ChapterRow)
create = Insert.insert @ChapterTable @ChapterRow

createMany :: [ChapterCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ChapterTable

update :: UUID -> ChapterUpdate -> Db (Either ORMError ChapterRow)
update = Update.update @ChapterTable @ChapterRow

updateMany :: Where ChapterTable -> ChapterUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @ChapterTable

upsert :: ChapterCreate -> ChapterUpdate -> Db (Either ORMError ChapterRow)
upsert = Insert.upsert @ChapterTable @ChapterRow ["id"]

data SectionNestedCreate = SectionNestedCreate
  { id :: Maybe UUID, label :: Text
  }
  deriving (Show, Eq)

data SectionNestedOps = SectionNestedOps
  { create :: [SectionNestedCreate]
  , createMany :: [SectionNestedCreate]
  , connect :: [UUID]
  , disconnect :: [UUID]
  , delete :: [UUID]
  , update :: [(UUID, SectionNestedCreate)]
  , upsert :: [SectionNestedCreate]
  }
  deriving (Show, Eq)

data SectionsWrite = Set [SectionNestedCreate] | Ops SectionNestedOps
  deriving (Show, Eq)

emptySectionNestedOps :: SectionNestedOps
emptySectionNestedOps =
  SectionNestedOps
    { create = []
    , createMany = []
    , connect = []
    , disconnect = []
    , delete = []
    , update = []
    , upsert = []
    }

data ChapterWriteCreate = ChapterWriteCreate
  { root :: ChapterCreate
  , sections :: SectionsWrite
  }
  deriving (Show, Eq)

data ChapterWriteUpdate = ChapterWriteUpdate
  { root :: ChapterUpdate
  , sections :: Maybe SectionsWrite
  }
  deriving (Show, Eq)

toSectionCreate :: UUID -> SectionNestedCreate -> SectionCreate
toSectionCreate parentId nested =
  SectionCreate
    { id = nested.id,
      label = nested.label,
      chapterRef = parentId
    }

toSectionUpdate :: SectionNestedCreate -> SectionUpdate
toSectionUpdate nested =
  SectionUpdate
    { chapterRef = Nothing,
      label = Just nested.label
    }

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest

applySectionsWrite :: UUID -> SectionsWrite -> Db (Either ORMError ())
applySectionsWrite parentId write =
  case write of
    Set items -> do
      result <-
        Delete.deleteWhere $
          Delete.whereDelete (fieldColumn sectionChapterRef <> " = ?") [toField parentId] (Delete.emptyDelete @SectionTable)
      case result of
        Left err -> pure (Left err)
        Right _ -> insertNestedCreates parentId items
    Ops ops -> applySectionNestedOps parentId ops

insertNestedCreates :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
insertNestedCreates parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- Insert.insert @SectionTable @SectionRow (toSectionCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

applySectionNestedOps :: UUID -> SectionNestedOps -> Db (Either ORMError ())
applySectionNestedOps parentId ops =
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
  result <- Delete.deleteMany @SectionTable (in_ sectionId ids `and_` eq sectionChapterRef parentId)
  pure $ case result of
    Left err -> Left err
    Right _ -> Right ()

updateChildRows :: UUID -> [(UUID, SectionNestedCreate)] -> Db (Either ORMError ())
updateChildRows parentId = go
  where
    go [] = pure (Right ())
    go ((childId, nested) : rest) = do
      result <-
        Update.updateBuilder @SectionTable @SectionRow $
          Update.whereUpdate (fieldColumn sectionId <> " = ?") [toField childId] $
            Update.whereUpdate (fieldColumn sectionChapterRef <> " = ?") [toField parentId] $
              Update.toUpdateBuilder @SectionTable (toSectionUpdate nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

upsertNested :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
upsertNested parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.insertBuilder @SectionTable @SectionRow $
          Prelude.id $
          Insert.toInsertBuilder @SectionTable (toSectionCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

insertNestedCreateMany :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
insertNestedCreateMany parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <-
        Insert.tryExecuteInsert $
          Prelude.id $
          Insert.toInsertBuilder @SectionTable (toSectionCreate parentId nested)
      case result of
        Left err -> pure (Left err)
        Right () -> go rest

connectNested :: UUID -> [UUID] -> Db (Either ORMError ())
connectNested parentId = go
  where
    go [] = pure (Right ())
    go (childId : rest) = do
      result <-
        Update.updateBuilder @SectionTable @SectionRow $
          Update.setField sectionChapterRef parentId $
            Update.whereUpdate (fieldColumn sectionId <> " = ?") [toField childId] $
              Update.emptyUpdate @SectionTable
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest

createNested ::
  (Include.ExecuteInclude ChapterTable include result) =>
  include ->
  ChapterWriteCreate ->
  Db (Either ORMError result)
createNested include input = transactionEither $ do
  rootId <- liftIO $ maybe V4.nextRandom pure input.root.id
  let rootInput =
        ChapterCreate { id = Just rootId, bookRef = input.root.bookRef, heading = input.root.heading }
  rootResult <-
    Insert.insert @ChapterTable @ChapterRow rootInput
  case rootResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- applySectionsWrite rootId input.sections
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> Include.findUniqueOrFail @ChapterTable include rootId

updateNested ::
  (Include.ExecuteInclude ChapterTable include result) =>
  include ->
  UUID ->
  ChapterWriteUpdate ->
  Db (Either ORMError result)
updateNested include rootId input = transactionEither $ do
  updateResult <-
    Update.update @ChapterTable @ChapterRow rootId input.root
  case updateResult of
    Left err -> pure (Left err)
    Right _ -> do
      nestedResult <- case input.sections of
        Nothing -> pure (Right ())
        Just write -> applySectionsWrite rootId write
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> Include.findUniqueOrFail @ChapterTable include rootId

noInclude :: NoInclude
noInclude = NoInclude ChapterInclude {sections = False}

data ChapterWithSectionsPicked = ChapterWithSectionsPicked
  { chapter :: ChapterPicked,
    sections :: [SectionRow]
  }
  deriving (Show, Eq)

toChapterWithSectionsPicked :: ChapterSelect -> ChapterWithSections -> ChapterWithSectionsPicked
toChapterWithSectionsPicked select_ ChapterWithSections {chapter, sections} =
  ChapterWithSectionsPicked
    { chapter = toChapterPicked select_ chapter
    , sections = sections
    }

data ChapterQuery include select = ChapterQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where ChapterTable)
  , orderBy_ :: [OrderBy ChapterTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

emptyQuery :: ChapterQuery NoInclude OmitSelect
emptyQuery =
  ChapterQuery {include_ = noInclude, select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

type instance ResolveInclude (ChapterQuery ChapterInclude OmitSelect) = ChapterWithSections
type instance ResolveInclude (ChapterQuery NoInclude OmitSelect) = ChapterRow
type instance ResolveInclude (ChapterQuery ChapterInclude ChapterSelect) = ChapterWithSectionsPicked
type instance ResolveInclude (ChapterQuery NoInclude ChapterSelect) = ChapterPicked

class ReadChapter include select where
  findMany :: ChapterQuery include select -> Db [ResolveInclude (ChapterQuery include select)]
  findUnique :: ChapterQuery include select -> Db (Either ORMError (Maybe (ResolveInclude (ChapterQuery include select))))
  findUniqueOrFail :: ChapterQuery include select -> Db (Either ORMError (ResolveInclude (ChapterQuery include select)))
  findFirst :: ChapterQuery include select -> Db (Maybe (ResolveInclude (ChapterQuery include select)))
  findFirstOrFail :: ChapterQuery include select -> Db (Either ORMError (ResolveInclude (ChapterQuery include select)))

instance ReadChapter ChapterInclude OmitSelect where
  findMany ChapterQuery {include_, where_, orderBy_, limit_, offset_} =
    Include.findMany @ChapterTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ChapterQuery {include_, where_} =
    case Ops.requireUniqueWhere @ChapterTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <- Include.findMany @ChapterTable include_ (matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ChapterQuery {include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @ChapterTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadChapter NoInclude OmitSelect where
  findMany ChapterQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @ChapterTable @ChapterRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ChapterQuery {where_} =
    Ops.findUniqueWhere @ChapterTable @ChapterRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ChapterQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @ChapterTable @ChapterRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadChapter ChapterInclude ChapterSelect where
  findMany ChapterQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    rows <- Include.findMany @ChapterTable include_ (applyQueryModifiers where_ orderBy_ limit_ offset_)
    pure $ map (toChapterWithSectionsPicked select_) rows
  findUnique ChapterQuery {include_, select_, where_} =
    case Ops.requireUniqueWhere @ChapterTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        nested <- Include.findMany @ChapterTable include_ (matching w)
        let rows = map (toChapterWithSectionsPicked select_) nested
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ChapterQuery {select_, include_, where_, orderBy_, offset_} = do
    rows <- Include.findMany @ChapterTable include_ (applyQueryModifiers where_ orderBy_ (Just 1) offset_)
    pure $ case rows of
      [] -> Nothing
      (row : _) -> Just (toChapterWithSectionsPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadChapter NoInclude ChapterSelect where
  findMany ChapterQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseChapterPicked select_)
      (selectColumns (chapterSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ChapterQuery {select_, where_} =
    case Ops.requireUniqueWhere @ChapterTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseChapterPicked select_)
            (selectColumns (chapterSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst ChapterQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseChapterPicked select_)
      (selectColumns (chapterSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: ChapterQuery include select -> Db Int
count ChapterQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @ChapterTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @ChapterTable

deleteMany :: Where ChapterTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ChapterTable