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
    ChapterCreateScalars,
    ChapterUpdateScalars,
    SectionNestedCreate (..),
    SectionNestedUpsert (..),
    SectionsUpdate (..),
    emptySectionsUpdate,
    ChapterQuery (..),
    ChapterUnique (..),
    ChapterUniqueKey (..),
    ChapterUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    chapterUniqueWhere,
    OmitSelect (..),
    Picked (..),
    ChapterCreate (..),
    ChapterRow (..),
    ChapterSelect (..),
    ChapterPicked (..),
    chapterSelect,
    ChapterUpdate (..),
    ChapterTable,
    chapterId
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

import Schema.Chapter (ChapterRow (..), ChapterSelect (..), ChapterPicked (..), chapterSelect, chapterSelectColumns, parseChapterPicked, ChapterTable, chapterId)
import qualified Schema.Chapter as ChapterSchema (ChapterCreate (..), ChapterUpdate (..))

import Schema.Include.Chapter (LoadChapter (..), ChapterInclude (..), ChapterRead, toChapterWithPicked)
import Schema.Section (SectionRow (..), SectionTable, sectionId, sectionChapterRef, SectionCreate (..), SectionUpdate (..))
import qualified Schema.Client.Section as Section (SectionUnique (..), sectionUniqueWhere)

data ChapterUnique
  = ById UUID
  deriving (Eq, Show)

data ChapterUniqueKey
  = OnId
  deriving (Eq, Show)

chapterUniqueWhere :: ChapterUnique -> Where ChapterTable
chapterUniqueWhere = \case
  ById v1 -> eq chapterId v1

chapterConflictCols :: ChapterUniqueKey -> [Text]
chapterConflictCols = \case
  OnId -> ["id"]

data SectionNestedCreate
  = CreateSection
      { id :: Maybe UUID, label :: Text
      }
  | ConnectSection Section.SectionUnique
  deriving (Show, Eq)
data SectionNestedUpsert = SectionNestedUpsert
  { where_ :: Section.SectionUnique
  , create :: SectionNestedCreate
  , update :: SectionUpdate
  }
  deriving (Show, Eq)
data SectionsUpdate = SectionsUpdate
  { replaceWith :: Maybe [SectionNestedCreate]
  , create :: [SectionNestedCreate]
  , createMany :: [SectionNestedCreate]
  , connect :: [Section.SectionUnique]
  , delete :: [Section.SectionUnique]
  , update :: [(Section.SectionUnique, SectionUpdate)]
  , upsert :: [SectionNestedUpsert]
  }
  deriving (Show, Eq)

emptySectionsUpdate :: SectionsUpdate
emptySectionsUpdate =
  SectionsUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data ChapterCreate = ChapterCreate
  { id :: Maybe UUID,
    bookRef :: UUID,
    heading :: Text,
    sections :: [SectionNestedCreate]
  }
  deriving (Show, Eq)

type ChapterCreateScalars = ChapterSchema.ChapterCreate

toChapterCreateScalars :: ChapterCreate -> ChapterCreateScalars
toChapterCreateScalars input =
  ChapterSchema.ChapterCreate
    { id = input.id,
      bookRef = input.bookRef,
      heading = input.heading
    }
data ChapterUpdate = ChapterUpdate
  { bookRef :: Maybe UUID,
    heading :: Maybe Text,
    sections :: Maybe SectionsUpdate
  }
  deriving (Show, Eq)

type ChapterUpdateScalars = ChapterSchema.ChapterUpdate

toChapterUpdateScalars :: ChapterUpdate -> ChapterUpdateScalars
toChapterUpdateScalars input =
  ChapterSchema.ChapterUpdate
    { bookRef = input.bookRef,
      heading = input.heading
    }

create :: ChapterCreate -> Db (Either ORMError ChapterRow)
create input =
  if hasChapterNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @ChapterTable @ChapterRow (toChapterCreateScalars input)

hasChapterNestedCreate :: ChapterCreate -> Bool
hasChapterNestedCreate input =
  not (null input.sections)

createWithNested :: ChapterCreate -> Db (Either ORMError ChapterRow)
createWithNested input = do
  rootResult <- Insert.insert @ChapterTable @ChapterRow (toChapterCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applySectionsCreate row.id input.sections
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [ChapterCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @ChapterTable

update :: ChapterUnique -> ChapterUpdate -> Db (Either ORMError ChapterRow)
update key input =
  if hasChapterNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @ChapterTable @ChapterRow (chapterUniqueWhere key) (toChapterUpdateScalars input)

hasChapterNestedUpdate :: ChapterUpdate -> Bool
hasChapterNestedUpdate input =
  isJust input.sections

updateWithNested :: ChapterUnique -> ChapterUpdate -> Db (Either ORMError ChapterRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @ChapterTable @ChapterRow (chapterUniqueWhere key) (toChapterUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applySectionsUpdate row.id) input.sections
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where ChapterTable -> ChapterUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @ChapterTable

upsert :: ChapterUniqueKey -> ChapterCreateScalars -> ChapterUpdateScalars -> Db (Either ORMError ChapterRow)
upsert key createInput updateInput =
  Insert.upsert @ChapterTable @ChapterRow (chapterConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applySectionsCreate :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
applySectionsCreate = insertSections
applySectionsUpdate :: UUID -> SectionsUpdate -> Db (Either ORMError ())
applySectionsUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceSections parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteSections parentId ops.delete
        , updateSectionsRows parentId ops.update
        , upsertSections parentId ops.upsert
        , insertSections parentId ops.create
        , insertSections parentId ops.createMany
        , connectSections parentId ops.connect
        ]
replaceSections :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
replaceSections parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn sectionChapterRef <> " = ?") [toField parentId] (Delete.emptyDelete @SectionTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertSections parentId items
insertSections :: UUID -> [SectionNestedCreate] -> Db (Either ORMError ())
insertSections parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectSection key -> connectSections parentId [key]
        CreateSection {id, label} -> do
          inserted <- Insert.insert @SectionTable @SectionRow SectionCreate
            { id = id,
              label = label,
              chapterRef = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteSections :: UUID -> [Section.SectionUnique] -> Db (Either ORMError ())
deleteSections _ [] = pure (Right ())
deleteSections parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @SectionTable
        (Section.sectionUniqueWhere key `and_` eq sectionChapterRef parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateSectionsRows :: UUID -> [(Section.SectionUnique, SectionUpdate)] -> Db (Either ORMError ())
updateSectionsRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = SectionUpdate { chapterRef = Nothing, label = nested.label }
      result <-
        Update.updateWhere @SectionTable @SectionRow
          (Section.sectionUniqueWhere key `and_` eq sectionChapterRef parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertSections :: UUID -> [SectionNestedUpsert] -> Db (Either ORMError ())
upsertSections parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @SectionTable @SectionRow (matching (Section.sectionUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertSections parentId [item.create]
        Right (Just row) ->
          if row.chapterRef == parentId
            then do
              let patched = SectionUpdate { chapterRef = Nothing, label = item.update.label }
              updated <-
                Update.updateWhere @SectionTable @SectionRow
                  (Section.sectionUniqueWhere item.where_ `and_` eq sectionChapterRef parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectSections :: UUID -> [Section.SectionUnique] -> Db (Either ORMError ())
connectSections _ [] = pure (Right ())
connectSections parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @SectionTable @SectionRow
          (Section.sectionUniqueWhere key)
          (SectionUpdate
            { chapterRef = Just parentId,
              label = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

data ChapterQuery include select = ChapterQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where ChapterTable)
  , orderBy_ :: [OrderBy ChapterTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data ChapterUniqueQuery include select = ChapterUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: ChapterUnique
  }

emptyQuery :: ChapterQuery () OmitSelect
emptyQuery =
  ChapterQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: ChapterUnique -> ChapterUniqueQuery () OmitSelect
uniqueQuery key =
  ChapterUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadChapter include select where
  findMany :: ChapterQuery include select -> Db [ChapterRead include select]
  findUnique :: ChapterUniqueQuery include select -> Db (Either ORMError (Maybe (ChapterRead include select)))
  findUniqueOrFail :: ChapterUniqueQuery include select -> Db (Either ORMError (ChapterRead include select))
  findFirst :: ChapterQuery include select -> Db (Maybe (ChapterRead include select))
  findFirstOrFail :: ChapterQuery include select -> Db (Either ORMError (ChapterRead include select))

instance (LoadChapter sections) => ReadChapter (ChapterInclude sections) OmitSelect where
  findMany ChapterQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadChapter include_ roots
  findUnique ChapterUniqueQuery {where_, include_} = do
    let w = chapterUniqueWhere where_
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (matching w))
    rows <- loadChapter include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ChapterQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadChapter include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadChapter () OmitSelect where
  findMany ChapterQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @ChapterTable @ChapterRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ChapterUniqueQuery {where_} = do
    let w = chapterUniqueWhere where_
    rows <- Ops.findMany @ChapterTable @ChapterRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ChapterQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @ChapterTable @ChapterRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadChapter sections) => ReadChapter (ChapterInclude sections) ChapterSelect where
  findMany ChapterQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadChapter include_ roots
    pure $ map (toChapterWithPicked select_) loaded
  findUnique ChapterUniqueQuery {where_, include_, select_} = do
    let w = chapterUniqueWhere where_
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (matching w))
    loaded <- loadChapter include_ roots
    let rows = map (toChapterWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst ChapterQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @ChapterTable @ChapterRow (prepareIncludeRootQuery @ChapterTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadChapter include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toChapterWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadChapter () ChapterSelect where
  findMany ChapterQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseChapterPicked select_)
      (selectColumns (chapterSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique ChapterUniqueQuery {where_, select_} = do
    let w = chapterUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseChapterPicked select_)
        (selectColumns (chapterSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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

delete :: ChapterUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @ChapterTable (chapterUniqueWhere key)

deleteMany :: Where ChapterTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @ChapterTable