{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Schema.Client.Author
  ( create,
    createMany,
    update,
    updateMany,
    upsert,
    findMany,
    findUnique,
    findUniqueOrFail,
    findFirst,
    findFirstOrFail,
    count,
    delete,
    deleteMany,
    AuthorCreate (..),
    AuthorRow (..),
    AuthorSelect (..),
    AuthorPicked (..),
    authorSelect,
    OmitSelect (..),
    Picked (..),
    ResolveSelect,
    AuthorUpdate (..),
    AuthorTable,
    AuthorQuery (..),
    emptyQuery,
    authorId
  )

where

import Data.UUID (UUID)
import Poppy.Db (Db)
import Poppy.Errors (ORMError (..), requireFound)
import qualified Poppy.Delete as Delete
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import Poppy.Query (OrderBy, QueryBuilder, applyQueryModifiers, matching, selectColumns)
import Poppy.Select (OmitSelect (..), Picked (..))
import Poppy.Where (Where)
import Schema.Author (AuthorCreate (..), AuthorRow (..), AuthorSelect (..), AuthorPicked (..), authorSelect, authorSelectColumns, parseAuthorPicked, AuthorTable, AuthorUpdate (..), authorId)
import qualified Poppy.Update as Update

create :: AuthorCreate -> Db (Either ORMError AuthorRow)
create = Insert.insert @AuthorTable @AuthorRow


createMany :: [AuthorCreate] -> Db (Either ORMError Int)
createMany = Insert.insertMany @AuthorTable


update :: UUID -> AuthorUpdate -> Db (Either ORMError AuthorRow)
update = Update.update @AuthorTable @AuthorRow


updateMany :: Where AuthorTable -> AuthorUpdate -> Db (Either ORMError Int)
updateMany = Update.updateMany @AuthorTable


upsert :: AuthorCreate -> AuthorUpdate -> Db (Either ORMError AuthorRow)
upsert = Insert.upsert @AuthorTable @AuthorRow ["id"]


data AuthorQuery select = AuthorQuery
  { select_ :: select
  , where_ :: Maybe (Where AuthorTable)
  , orderBy_ :: [OrderBy AuthorTable]
  , limit_ :: Maybe Int
  , offset_ :: Maybe Int
  }


emptyQuery :: AuthorQuery OmitSelect
emptyQuery =
  AuthorQuery {select_ = OmitSelect, where_ = Nothing, orderBy_ = [], limit_ = Nothing, offset_ = Nothing}


type family ResolveSelect select
type instance ResolveSelect OmitSelect = AuthorRow
type instance ResolveSelect AuthorSelect = AuthorPicked


class ReadAuthor select where
  findMany :: AuthorQuery select -> Db [ResolveSelect select]
  findUnique :: AuthorQuery select -> Db (Either ORMError (Maybe (ResolveSelect select)))
  findUniqueOrFail :: AuthorQuery select -> Db (Either ORMError (ResolveSelect select))
  findFirst :: AuthorQuery select -> Db (Maybe (ResolveSelect select))
  findFirstOrFail :: AuthorQuery select -> Db (Either ORMError (ResolveSelect select))

instance ReadAuthor OmitSelect where
  findMany q =
    Ops.findMany @AuthorTable @AuthorRow (applyQuery q)
  findUnique AuthorQuery {where_} =
    Ops.findUniqueWhere @AuthorTable @AuthorRow where_
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q =
    Ops.findFirst @AuthorTable @AuthorRow (applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

instance ReadAuthor AuthorSelect where
  findMany q@AuthorQuery {select_} =
    Ops.findManyWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQuery q)
  findUnique AuthorQuery {select_, where_} =
    case Ops.requireUniqueWhere @AuthorTable where_ of
      Left err -> pure (Left err)
      Right w -> do
        rows <-
          Ops.findManyWith
            (parseAuthorPicked select_)
            (selectColumns (authorSelectColumns select_) . matching w)
        pure $ case rows of
          [] -> Right Nothing
          [row] -> Right (Just row)
          _ -> Left (MultipleRecordsFound "findUnique matched multiple rows")
  findUniqueOrFail q = do
    result <- findUnique q
    case result of
      Left err -> pure (Left err)
      Right found -> pure $ requireFound found (RecordNotFound "No record found matching query")
  findFirst q@AuthorQuery {select_} =
    Ops.findFirstWith
      (parseAuthorPicked select_)
      (selectColumns (authorSelectColumns select_) . applyQuery q)
  findFirstOrFail q = do
    result <- findFirst q
    pure $ requireFound result (RecordNotFound "No record found matching query")

applyQuery :: AuthorQuery select -> QueryBuilder AuthorTable -> QueryBuilder AuthorTable
applyQuery AuthorQuery {where_, orderBy_, limit_, offset_} =
  applyQueryModifiers where_ orderBy_ limit_ offset_


count :: AuthorQuery select -> Db Int
count q = Ops.count @AuthorTable (applyQuery q)


delete :: UUID -> Db (Either ORMError Int)
delete = Ops.delete @AuthorTable


deleteMany :: Where AuthorTable -> Db (Either ORMError Int)
deleteMany = Delete.deleteMany @AuthorTable

