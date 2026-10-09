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
module Schema.Client.Author
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
    AuthorCreateScalars,
    AuthorUpdateScalars,
    PostNestedCreate (..),
    PostNestedUpsert (..),
    PostsUpdate (..),
    emptyPostsUpdate,
    AuthorQuery (..),
    AuthorUnique (..),
    AuthorUniqueKey (..),
    AuthorUniqueQuery (..),
    emptyQuery,
    uniqueQuery,
    authorUniqueWhere,
    OmitSelect (..),
    Picked (..),
    AuthorCreate (..),
    AuthorRow (..),
    AuthorSelect (..),
    AuthorPicked (..),
    authorSelect,
    AuthorUpdate (..),
    AuthorTable,
    authorId
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

import Schema.Author (AuthorRow (..), AuthorSelect (..), AuthorPicked (..), authorSelect, authorSelectColumns, parseAuthorPicked, AuthorTable, authorId)
import qualified Schema.Author as AuthorSchema (AuthorCreate (..), AuthorUpdate (..))

import Schema.Include.Author (LoadAuthor (..), AuthorInclude (..), AuthorRead, toAuthorWithPicked)
import Schema.Post (PostRow (..), PostTable, postId, postAuthorId, PostCreate (..), PostUpdate (..))
import qualified Schema.Client.Post as Post (PostUnique (..), postUniqueWhere)
import Schema.ArticleStatus (ArticleStatus (..))

data AuthorUnique
  = ById UUID
  deriving (Eq, Show)

data AuthorUniqueKey
  = OnId
  deriving (Eq, Show)

authorUniqueWhere :: AuthorUnique -> Where AuthorTable
authorUniqueWhere = \case
  ById v1 -> eq authorId v1

authorConflictCols :: AuthorUniqueKey -> [Text]
authorConflictCols = \case
  OnId -> ["id"]

data PostNestedCreate
  = CreatePost
      { id :: Maybe UUID, title :: Text, status :: ArticleStatus
      }
  | ConnectPost Post.PostUnique
  deriving (Show, Eq)
data PostNestedUpsert = PostNestedUpsert
  { where_ :: Post.PostUnique
  , create :: PostNestedCreate
  , update :: PostUpdate
  }
  deriving (Show, Eq)
data PostsUpdate = PostsUpdate
  { replaceWith :: Maybe [PostNestedCreate]
  , create :: [PostNestedCreate]
  , createMany :: [PostNestedCreate]
  , connect :: [Post.PostUnique]
  , delete :: [Post.PostUnique]
  , update :: [(Post.PostUnique, PostUpdate)]
  , upsert :: [PostNestedUpsert]
  }
  deriving (Show, Eq)

emptyPostsUpdate :: PostsUpdate
emptyPostsUpdate =
  PostsUpdate
    { replaceWith = Nothing
    , create = []
    , createMany = []
    , connect = []
    , delete = []
    , update = []
    , upsert = []
    }
data AuthorCreate = AuthorCreate
  { id :: Maybe UUID,
    name :: Text,
    posts :: [PostNestedCreate]
  }
  deriving (Show, Eq)

type AuthorCreateScalars = AuthorSchema.AuthorCreate

toAuthorCreateScalars :: AuthorCreate -> AuthorCreateScalars
toAuthorCreateScalars input =
  AuthorSchema.AuthorCreate
    { id = input.id,
      name = input.name
    }
data AuthorUpdate = AuthorUpdate
  { name :: Maybe Text,
    posts :: Maybe PostsUpdate
  }
  deriving (Show, Eq)

type AuthorUpdateScalars = AuthorSchema.AuthorUpdate

toAuthorUpdateScalars :: AuthorUpdate -> AuthorUpdateScalars
toAuthorUpdateScalars input =
  AuthorSchema.AuthorUpdate
    { name = input.name
    }

create :: AuthorCreate -> Db (Either ORMError AuthorRow)
create input =
  if hasAuthorNestedCreate input
    then transactionEither (createWithNested input)
    else Insert.insert @AuthorTable @AuthorRow (toAuthorCreateScalars input)

hasAuthorNestedCreate :: AuthorCreate -> Bool
hasAuthorNestedCreate input =
  not (null input.posts)

createWithNested :: AuthorCreate -> Db (Either ORMError AuthorRow)
createWithNested input = do
  rootResult <- Insert.insert @AuthorTable @AuthorRow (toAuthorCreateScalars input)
  case rootResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ applyPostsCreate row.id input.posts
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

createMany :: [AuthorCreateScalars] -> Db (Either ORMError Int)
createMany = Insert.insertMany @AuthorTable

update :: AuthorUnique -> AuthorUpdate -> Db (Either ORMError AuthorRow)
update key input =
  if hasAuthorNestedUpdate input
    then transactionEither (updateWithNested key input)
    else Update.updateWhere @AuthorTable @AuthorRow (authorUniqueWhere key) (toAuthorUpdateScalars input)

hasAuthorNestedUpdate :: AuthorUpdate -> Bool
hasAuthorNestedUpdate input =
  isJust input.posts

updateWithNested :: AuthorUnique -> AuthorUpdate -> Db (Either ORMError AuthorRow)
updateWithNested key input = do
  updateResult <- Update.updateWhere @AuthorTable @AuthorRow (authorUniqueWhere key) (toAuthorUpdateScalars input)
  case updateResult of
    Left err -> pure (Left err)
    Right row -> do
      nestedResult <-
        sequenceNested
          [ maybe (pure (Right ())) (applyPostsUpdate row.id) input.posts
          ]
      case nestedResult of
        Left err -> pure (Left err)
        Right () -> pure (Right row)

updateMany :: Where AuthorTable -> AuthorUpdateScalars -> Db (Either ORMError Int)
updateMany = Update.updateMany @AuthorTable

upsert :: AuthorUniqueKey -> AuthorCreateScalars -> AuthorUpdateScalars -> Db (Either ORMError AuthorRow)
upsert key createInput updateInput =
  Insert.upsert @AuthorTable @AuthorRow (authorConflictCols key) createInput updateInput

sequenceNested :: [Db (Either ORMError ())] -> Db (Either ORMError ())
sequenceNested [] = pure (Right ())
sequenceNested (action : rest) = do
  result <- action
  case result of
    Left err -> pure (Left err)
    Right () -> sequenceNested rest
applyPostsCreate :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
applyPostsCreate = insertPosts
applyPostsUpdate :: UUID -> PostsUpdate -> Db (Either ORMError ())
applyPostsUpdate parentId ops = do
  replaced <- case ops.replaceWith of
    Nothing -> pure (Right ())
    Just items -> replacePosts parentId items
  case replaced of
    Left err -> pure (Left err)
    Right () ->
      sequenceNested
        [ deletePosts parentId ops.delete
        , updatePostsRows parentId ops.update
        , upsertPosts parentId ops.upsert
        , insertPosts parentId ops.create
        , insertPosts parentId ops.createMany
        , connectPosts parentId ops.connect
        ]
replacePosts :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
replacePosts parentId items = do
  result <-
    Delete.deleteWhere $
      Delete.whereDelete (fieldColumn postAuthorId <> " = ?") [toField parentId] (Delete.emptyDelete @PostTable)
  case result of
    Left err -> pure (Left err)
    Right _ -> insertPosts parentId items
insertPosts :: UUID -> [PostNestedCreate] -> Db (Either ORMError ())
insertPosts parentId = go
  where
    go [] = pure (Right ())
    go (nested : rest) = do
      result <- case nested of
        ConnectPost key -> connectPosts parentId [key]
        CreatePost {id, title, status} -> do
          inserted <- Insert.insert @PostTable @PostRow PostCreate
            { id = id,
              title = title,
              status = status,
              authorId = parentId
            }
          pure $ case inserted of
            Left err -> Left err
            Right _ -> Right ()
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
deletePosts :: UUID -> [Post.PostUnique] -> Db (Either ORMError ())
deletePosts _ [] = pure (Right ())
deletePosts parentId keys = sequenceNested (map deleteOne keys)
  where
    deleteOne key = do
      result <- Delete.deleteMany @PostTable
        (Post.postUniqueWhere key `and_` eq postAuthorId parentId)
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()
updatePostsRows :: UUID -> [(Post.PostUnique, PostUpdate)] -> Db (Either ORMError ())
updatePostsRows parentId = go
  where
    go [] = pure (Right ())
    go ((key, nested) : rest) = do
      let patched = PostUpdate { authorId = Nothing, title = nested.title, status = nested.status }
      result <-
        Update.updateWhere @PostTable @PostRow
          (Post.postUniqueWhere key `and_` eq postAuthorId parentId)
          patched
      case result of
        Left err -> pure (Left err)
        Right _ -> go rest
upsertPosts :: UUID -> [PostNestedUpsert] -> Db (Either ORMError ())
upsertPosts parentId = go
  where
    go [] = pure (Right ())
    go (item : rest) = do
      existing <- Ops.findMany @PostTable @PostRow (matching (Post.postUniqueWhere item.where_))
      result <- case fromUniqueRows existing of
        Left err -> pure (Left err)
        Right Nothing -> insertPosts parentId [item.create]
        Right (Just row) ->
          if row.authorId == parentId
            then do
              let patched = PostUpdate { authorId = Nothing, title = item.update.title, status = item.update.status }
              updated <-
                Update.updateWhere @PostTable @PostRow
                  (Post.postUniqueWhere item.where_ `and_` eq postAuthorId parentId)
                  patched
              pure $ case updated of
                Left err -> Left err
                Right _ -> Right ()
            else pure (Left (UniqueViolation "nested upsert would reparent a row owned by another parent"))
      case result of
        Left err -> pure (Left err)
        Right () -> go rest
connectPosts :: UUID -> [Post.PostUnique] -> Db (Either ORMError ())
connectPosts _ [] = pure (Right ())
connectPosts parentId keys = sequenceNested (map connectOne keys)
  where
    connectOne key = do
      result <-
        Update.updateWhere @PostTable @PostRow
          (Post.postUniqueWhere key)
          (PostUpdate
            { authorId = Just parentId,
              title = Nothing,
              status = Nothing
            })
      pure $ case result of
        Left err -> Left err
        Right _ -> Right ()

data AuthorQuery include select = AuthorQuery
  { include_ :: include
  , select_ :: select
  , where_ :: Maybe (Where AuthorTable)
  , orderBy_ :: [OrderBy AuthorTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }

data AuthorUniqueQuery include select = AuthorUniqueQuery
  { include_ :: include
  , select_ :: select
  , where_ :: AuthorUnique
  }

emptyQuery :: AuthorQuery () OmitSelect
emptyQuery =
  AuthorQuery {include_ = (), select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}

uniqueQuery :: AuthorUnique -> AuthorUniqueQuery () OmitSelect
uniqueQuery key =
  AuthorUniqueQuery {include_ = (), select_ = OmitSelect, where_ = key}

class ReadAuthor include select where
  findMany :: AuthorQuery include select -> Db [AuthorRead include select]
  findUnique :: AuthorUniqueQuery include select -> Db (Either ORMError (Maybe (AuthorRead include select)))
  findUniqueOrFail :: AuthorUniqueQuery include select -> Db (Either ORMError (AuthorRead include select))
  findFirst :: AuthorQuery include select -> Db (Maybe (AuthorRead include select))
  findFirstOrFail :: AuthorQuery include select -> Db (Either ORMError (AuthorRead include select))

instance (LoadAuthor posts) => ReadAuthor (AuthorInclude posts) OmitSelect where
  findMany AuthorQuery {include_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loadAuthor include_ roots
  findUnique AuthorUniqueQuery {where_, include_} = do
    let w = authorUniqueWhere where_
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (matching w))
    rows <- loadAuthor include_ roots
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst AuthorQuery {include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadAuthor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just row
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadAuthor () OmitSelect where
  findMany AuthorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findMany @AuthorTable @AuthorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique AuthorUniqueQuery {where_} = do
    let w = authorUniqueWhere where_
    rows <- Ops.findMany @AuthorTable @AuthorRow (matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst AuthorQuery {where_, orderBy_, limit_, offset_} =
    Ops.findFirst @AuthorTable @AuthorRow (applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance (LoadAuthor posts) => ReadAuthor (AuthorInclude posts) AuthorSelect where
  findMany AuthorQuery {include_, select_, where_, orderBy_, limit_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_))
    loaded <- loadAuthor include_ roots
    pure $ map (toAuthorWithPicked select_) loaded
  findUnique AuthorUniqueQuery {where_, include_, select_} = do
    let w = authorUniqueWhere where_
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (matching w))
    loaded <- loadAuthor include_ roots
    let rows = map (toAuthorWithPicked select_) loaded
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst AuthorQuery {select_, include_, where_, orderBy_, offset_} = do
    roots <- Ops.findMany @AuthorTable @AuthorRow (prepareIncludeRootQuery @AuthorTable (applyQueryModifiers where_ orderBy_ (Just 1) offset_))
    loaded <- loadAuthor include_ roots
    pure $ case loaded of
      [] -> Nothing
      (row : _) -> Just (toAuthorWithPicked select_ row)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadAuthor () AuthorSelect where
  findMany AuthorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findManyWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findUnique AuthorUniqueQuery {where_, select_} = do
    let w = authorUniqueWhere where_
    rows <-
      Ops.findManyWith
        (parseAuthorPicked select_)
        (selectColumns (authorSelectColumns select_) . matching w)
    pure (fromUniqueRows rows)
  findUniqueOrFail q = uniqueOrFail <$> findUnique q
  findFirst AuthorQuery {select_, where_, orderBy_, limit_, offset_} =
    Ops.findFirstWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQueryModifiers where_ orderBy_ limit_ offset_)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

count :: AuthorQuery include select -> Db Int
count AuthorQuery {where_, orderBy_, limit_, offset_} =
  Ops.count @AuthorTable (applyQueryModifiers where_ orderBy_ limit_ offset_)

delete :: AuthorUnique -> Db (Either ORMError Int)
delete key =
  Delete.deleteMany @AuthorTable (authorUniqueWhere key)

deleteMany :: Where AuthorTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @AuthorTable