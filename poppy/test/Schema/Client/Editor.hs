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
    EditorCreateScalars,
    EditorUpdateScalars,
    ArticleNestedCreate (..),
    ArticleNestedUpsert (..),
    WrittenPostsUpdate (..),
    emptyWrittenPostsUpdate,
    EditedPostsUpdate (..),
    emptyEditedPostsUpdate,
    EditorQuery (..),
    EditorUnique (..),
    EditorUniqueKey (..),
    EditorUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    editorUniqueWhere,
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

import Schema.Editor (EditorRow (..), EditorSelect (..), EditorPicked (..), editorSelect, editorSelectColumns, parseEditorPicked, EditorTable, editorId)
import qualified Schema.Editor as EditorSchema (EditorCreate (..), EditorUpdate (..))

import Schema.Include.Editor (LoadEditor (..), EditorInclude (..), EditorRead, toEditorWithPicked)
import Schema.Article (ArticleRow (..), ArticleTable, articleId, articleAuthorId, ArticleCreate (..), ArticleUpdate (..))
import qualified Schema.Client.Article as Article (ArticleUnique (..), articleUniqueWhere)

data EditorUnique
  = ById UUID
  deriving (Eq, Show)

data EditorUniqueKey
  = OnId
  deriving (Eq, Show)

editorUniqueWhere :: EditorUnique -> Where EditorTable
editorUniqueWhere = \case
  ById v1 -> eq editorId v1

editorConflictCols :: EditorUniqueKey -> [Text]
editorConflictCols = \case
  OnId -> ["id"]

data ArticleNestedCreate
  = CreateArticle
      { id :: Maybe UUID, title :: Text
      }
  | ConnectArticle Article.ArticleUnique
  deriving (Show, Eq)
data ArticleNestedUpsert = ArticleNestedUpsert
  { where_ :: Article.ArticleUnique
  , create :: ArticleNestedCreate
  , update :: ArticleUpdate
  }
  deriving (Show, Eq)
data WrittenPostsUpdate = WrittenPostsUpdate
  { replaceWith :: Maybe [ArticleNestedCreate]
  , create :: [ArticleNestedCreate]
  , createMany :: [ArticleNestedCreate]
  , connect :: [Article.ArticleUnique]
  , delete :: [Article.ArticleUnique]
  , update :: [(Article.ArticleUnique, ArticleUpdate)]
  , upsert :: [ArticleNestedUpsert]
  }
  deriving (Show, Eq)

emptyWrittenPostsUpdate :: WrittenPostsUpdate
emptyWrittenPostsUpdate =
  WrittenPostsUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data EditedPostsUpdate = EditedPostsUpdate
  { replaceWith :: Maybe [ArticleNestedCreate]
  , create :: [ArticleNestedCreate]
  , createMany :: [ArticleNestedCreate]
  , connect :: [Article.ArticleUnique]
  , delete :: [Article.ArticleUnique]
  , update :: [(Article.ArticleUnique, ArticleUpdate)]
  , upsert :: [ArticleNestedUpsert]
  }
  deriving (Show, Eq)

emptyEditedPostsUpdate :: EditedPostsUpdate
emptyEditedPostsUpdate =
  EditedPostsUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data EditorCreate = EditorCreate
  { id :: Maybe UUID,
    name :: Text,
    writtenPosts :: [ArticleNestedCreate],
    editedPosts :: [ArticleNestedCreate]
  }
  deriving (Show, Eq)

type EditorCreateScalars = EditorSchema.EditorCreate

toEditorCreateScalars :: EditorCreate -> EditorCreateScalars
toEditorCreateScalars input =
  EditorSchema.EditorCreate
    { id = input.id,
      name = input.name
    }
data EditorUpdate = EditorUpdate
  { name :: Maybe Text,
    writtenPosts :: Maybe WrittenPostsUpdate,
    editedPosts :: Maybe EditedPostsUpdate
  }
  deriving (Show, Eq)

type EditorUpdateScalars = EditorSchema.EditorUpdate

toEditorUpdateScalars :: EditorUpdate -> EditorUpdateScalars
toEditorUpdateScalars input =
  EditorSchema.EditorUpdate
    { name = input.name
    }

create :: EditorCreate -> Db (Either ORMError EditorRow)
create input =
  if hasEditorNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @EditorTable @EditorRow (toEditorCreateScalars input)

hasEditorNestedCreate :: EditorCreate -> Bool
hasEditorNestedCreate input =
  not (null input.writtenPosts) || not (null input.editedPosts)

createWithNested :: EditorCreate -> Db (Either ORMError EditorRow)
createWithNested input = do
  rootResult <- Insert.insert @EditorTable @EditorRow (toEditorCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applyWrittenPostsCreate row.id input.writtenPosts
          , applyEditedPostsCreate row.id input.editedPosts
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [EditorCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @EditorTable

update :: EditorUnique -> EditorUpdate -> Db (Either ORMError EditorRow)
update key input =
  if hasEditorNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @EditorTable @EditorRow (editorUniqueWhere key) (toEditorUpdateScalars input)

hasEditorNestedUpdate :: EditorUpdate -> Bool
hasEditorNestedUpdate input =
  isJust input.writtenPosts || isJust input.editedPosts

updateWithNested :: EditorUnique -> EditorUpdate -> Db (Either ORMError EditorRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @EditorTable @EditorRow (editorUniqueWhere key) (toEditorUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applyWrittenPostsUpdate row.id) input.writtenPosts
          , maybe (pure (Right ())) (applyEditedPostsUpdate row.id) input.editedPosts
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where EditorTable -> EditorUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @EditorTable

upsert :: EditorUniqueKey -> EditorCreateScalars -> EditorUpdateScalars -> Db (Either ORMError EditorRow)
upsert key createInput updateInput =
  Insert.upsert @EditorTable @EditorRow (editorConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applyWrittenPostsCreate :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
applyWrittenPostsCreate = insertWrittenPosts
applyWrittenPostsUpdate :: UUID -> WrittenPostsUpdate -> Db (Either ORMError ())
applyWrittenPostsUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceWrittenPosts parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteWrittenPosts parentId ops.delete
        , updateWrittenPostsRows parentId ops.update
        , upsertWrittenPosts parentId ops.upsert
        , insertWrittenPosts parentId ops.create
        , insertWrittenPosts parentId ops.createMany
        , connectWrittenPosts parentId ops.connect
        ]
replaceWrittenPosts :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
replaceWrittenPosts parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn articleAuthorId <> " = ?") [toField parentId] (Delete.emptyDelete @ArticleTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertWrittenPosts parentId items
insertWrittenPosts :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
insertWrittenPosts parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectArticle key -> connectWrittenPosts parentId [key]
        CreateArticle {id, title} -> do
          inserted <- Insert.insert @ArticleTable @ArticleRow ArticleCreate
            { id = id,
              title = title,
              authorId = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteWrittenPosts :: UUID -> [Article.ArticleUnique] -> Db (Either ORMError ())
deleteWrittenPosts _ [] = pure (Right ())
deleteWrittenPosts parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @ArticleTable
        (Article.articleUniqueWhere key `and_` eq articleAuthorId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateWrittenPostsRows :: UUID -> [(Article.ArticleUnique, ArticleUpdate)] -> Db (Either ORMError ())
updateWrittenPostsRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = ArticleUpdate { authorId = Nothing, title = nested.title }
      result <-
        Update.updateWhere @ArticleTable @ArticleRow
          (Article.articleUniqueWhere key `and_` eq articleAuthorId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertWrittenPosts :: UUID -> [ArticleNestedUpsert] -> Db (Either ORMError ())
upsertWrittenPosts parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @ArticleTable @ArticleRow (matching (Article.articleUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertWrittenPosts parentId [item.create]
        Right (Just row) ->
          if row.authorId == parentId
            then do
              let patched = ArticleUpdate { authorId = Nothing, title = item.update.title }
              updated <-
                Update.updateWhere @ArticleTable @ArticleRow
                  (Article.articleUniqueWhere item.where_ `and_` eq articleAuthorId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectWrittenPosts :: UUID -> [Article.ArticleUnique] -> Db (Either ORMError ())
connectWrittenPosts _ [] = pure (Right ())
connectWrittenPosts parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @ArticleTable @ArticleRow
          (Article.articleUniqueWhere key)
          (ArticleUpdate
            { authorId = Just parentId,
              title = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
applyEditedPostsCreate :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
applyEditedPostsCreate = insertEditedPosts
applyEditedPostsUpdate :: UUID -> EditedPostsUpdate -> Db (Either ORMError ())
applyEditedPostsUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replaceEditedPosts parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deleteEditedPosts parentId ops.delete
        , updateEditedPostsRows parentId ops.update
        , upsertEditedPosts parentId ops.upsert
        , insertEditedPosts parentId ops.create
        , insertEditedPosts parentId ops.createMany
        , connectEditedPosts parentId ops.connect
        ]
replaceEditedPosts :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
replaceEditedPosts parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn articleAuthorId <> " = ?") [toField parentId] (Delete.emptyDelete @ArticleTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertEditedPosts parentId items
insertEditedPosts :: UUID -> [ArticleNestedCreate] -> Db (Either ORMError ())
insertEditedPosts parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectArticle key -> connectEditedPosts parentId [key]
        CreateArticle {id, title} -> do
          inserted <- Insert.insert @ArticleTable @ArticleRow ArticleCreate
            { id = id,
              title = title,
              authorId = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deleteEditedPosts :: UUID -> [Article.ArticleUnique] -> Db (Either ORMError ())
deleteEditedPosts _ [] = pure (Right ())
deleteEditedPosts parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @ArticleTable
        (Article.articleUniqueWhere key `and_` eq articleAuthorId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updateEditedPostsRows :: UUID -> [(Article.ArticleUnique, ArticleUpdate)] -> Db (Either ORMError ())
updateEditedPostsRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = ArticleUpdate { authorId = Nothing, title = nested.title }
      result <-
        Update.updateWhere @ArticleTable @ArticleRow
          (Article.articleUniqueWhere key `and_` eq articleAuthorId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertEditedPosts :: UUID -> [ArticleNestedUpsert] -> Db (Either ORMError ())
upsertEditedPosts parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @ArticleTable @ArticleRow (matching (Article.articleUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertEditedPosts parentId [item.create]
        Right (Just row) ->
          if row.authorId == parentId
            then do
              let patched = ArticleUpdate { authorId = Nothing, title = item.update.title }
              updated <-
                Update.updateWhere @ArticleTable @ArticleRow
                  (Article.articleUniqueWhere item.where_ `and_` eq articleAuthorId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectEditedPosts :: UUID -> [Article.ArticleUnique] -> Db (Either ORMError ())
connectEditedPosts _ [] = pure (Right ())
connectEditedPosts parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @ArticleTable @ArticleRow
          (Article.articleUniqueWhere key)
          (ArticleUpdate
            { authorId = Just parentId,
              title = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

data EditorQuery include select = EditorQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where EditorTable)
  , orderBy_ :: [OrderBy EditorTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data EditorUniqueQuery include select = EditorUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: EditorUnique
  }

emptyQuery :: EditorQuery () OmitSelect
emptyQuery =
  EditorQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: EditorUnique -> EditorUniqueQuery () OmitSelect
uniqueQuery key =
  EditorUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadEditor include select where
  findMany :: EditorQuery include select -> Db [EditorRead include select]
  findUnique :: EditorUniqueQuery include select -> Db (Either ORMError (Maybe (EditorRead include select)))
  findUniqueOrFail :: EditorUniqueQuery include select -> Db (Either ORMError (EditorRead include select))
  findFirst :: EditorQuery include select -> Db (Maybe (EditorRead include select))
  findFirstOrFail :: EditorQuery include select -> Db (Either ORMError (EditorRead include select))

instance (LoadEditor writtenPosts editedPosts) => ReadEditor (EditorInclude writtenPosts editedPosts) OmitSelect where
  findMany EditorQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadEditor include_ roots
  findUnique EditorUniqueQuery {where_, include_} = do
    let w = editorUniqueWhere where_
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (matching w))
    rows <- loadEditor include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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
  findUnique EditorUniqueQuery {where_} = do
    let w = editorUniqueWhere where_
    rows <- Ops.findMany @EditorTable @EditorRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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
  findUnique EditorUniqueQuery {where_, include_, select_} = do
    let w = editorUniqueWhere where_
    roots <- Ops.findMany @EditorTable @EditorRow (prepareIncludeRootQuery @EditorTable (matching w))
    loaded <- loadEditor include_ roots
    let rows = map (toEditorWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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
  findUnique EditorUniqueQuery {where_, select_} = do
    let w = editorUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseEditorPicked select_)
        (selectColumns (editorSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
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

delete :: EditorUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @EditorTable (editorUniqueWhere key)

deleteMany :: Where EditorTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @EditorTable