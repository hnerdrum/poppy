{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Poppy.SelectSpec
  ( selectSpec,
  )
where

import Poppy (ORMError (..), Picked (..), asc, desc, loadWith, runDb, skip)
import qualified Poppy.Internal.Operations as Ops
import Poppy.Internal.Query (selectColumns)
import Poppy.Internal.Select (picked)
import qualified Poppy.ShelfFixtures as ShelfFixtures
import Poppy.Internal.Where (eq)
import qualified Poppy.WidgetFixtures as WidgetFixtures
import Schema.Book (BookRow (..))
import qualified Schema.Client.Shelf as Shelf
import qualified Schema.Client.Widget as Widget
import Schema.Include.Book (BookInclude (..), BookWith (..))
import Schema.Include.Shelf (ShelfInclude (..), ShelfWith (..), ShelfWithPicked (..))
import Schema.Shelf (ShelfPicked (..), ShelfRow (..), shelfId, shelfName)
import Schema.Widget
  ( WidgetPicked (..),
    WidgetRow (..),
    WidgetSelect (..),
    WidgetTable,
    parseWidgetPicked,
    widgetCreatedAt,
    widgetName,
    widgetSelectColumns,
  )
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

selectSpec :: SpecWith TestEnv
selectSpec = do
  describe "Poppy.Internal.Select columns" $ do
    it "always includes the primary key and only requested scalars" $ \_ ->
      widgetSelectColumns
        WidgetSelect
          { id = False,
            createdAt = False,
            updatedAt = False,
            name = True,
            description = True
          }
        `shouldBe` ["id", "name", "description"]

    it "maps False fields to Skipped" $ \_ -> do
      picked False ("secret" :: String) `shouldBe` Skipped
      picked True ("shown" :: String) `shouldBe` Picked "shown"

  describe "single-table select_" $ do
    it "loads only requested widget columns into WidgetPicked" $ \TestEnv {envPool = pool} -> do
      widget <- WidgetFixtures.insertWidget pool "alpha"
      let sel =
            WidgetSelect
              { id = False,
                createdAt = False,
                updatedAt = False,
                name = True,
                description = False
              }
      [row] <-
        runDb pool $
          Ops.findManyWith @WidgetTable
            (parseWidgetPicked sel)
            (selectColumns (widgetSelectColumns sel))
      let WidgetRow {id = widgetId} = widget
          WidgetPicked {id = rowId, name = rowName, description = rowDescription, createdAt = rowCreatedAt, updatedAt = rowUpdatedAt} = row
      rowId `shouldBe` widgetId
      rowName `shouldBe` Picked "alpha"
      rowDescription `shouldBe` Skipped
      rowCreatedAt `shouldBe` Skipped
      rowUpdatedAt `shouldBe` Skipped

    it "keeps WidgetRow when select_ is OmitSelect" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "salt"
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldBe` ["salt"]
      map (.id) rows `shouldBe` [created.id]

    it "findUnique looks up by primary key" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "thyme"
      found <-
        runDb
          pool
          (Widget.findUnique (Widget.uniqueQuery (Widget.ById created.id)))
      found `shouldBe` Right (Just created)

    it "findUnique looks up by a declared unique" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "sage"
      found <-
        runDb
          pool
          (Widget.findUnique (Widget.uniqueQuery (Widget.ByName "sage")))
      found `shouldBe` Right (Just created)

    it "findUnique projects columns when select_ is set" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "basil"
      let sel =
            WidgetSelect
              { id = False,
                createdAt = False,
                updatedAt = False,
                name = True,
                description = False
              }
      found <-
        runDb
          pool
          (Widget.findUnique ((Widget.uniqueQuery (Widget.ById created.id)) {Widget.select_ = sel}))
          >>= assertRight
      fmap (.name) found `shouldBe` Just (Picked "basil")

    it "returns WidgetPicked when select_ is set" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "pepper"
      let sel =
            WidgetSelect
              { id = False,
                createdAt = False,
                updatedAt = False,
                name = True,
                description = False
              }
          query :: Widget.WidgetQuery WidgetSelect
          query =
            Widget.WidgetQuery
              { select_ = sel,
                where_ = Nothing,
                orderBy_ = [],
                limit_ = Nothing,
                offset_ = Nothing
              }
      [row] <- runDb pool (Widget.findMany query)
      row.id `shouldBe` created.id
      row.name `shouldBe` Picked "pepper"
      row.createdAt `shouldBe` Skipped
      row.updatedAt `shouldBe` Skipped

    it "findMany orderBy_ sorts by listed fields" $ \TestEnv {envPool = pool} -> do
      _ <- WidgetFixtures.insertWidget pool "beta"
      _ <- WidgetFixtures.insertWidget pool "alpha"
      _ <- WidgetFixtures.insertWidget pool "gamma"
      ascending <-
        runDb
          pool
          (Widget.findMany Widget.emptyQuery {Widget.orderBy_ = [asc widgetName]})
      descending <-
        runDb
          pool
          (Widget.findMany Widget.emptyQuery {Widget.orderBy_ = [desc widgetName]})
      multi <-
        runDb
          pool
          ( Widget.findMany
              Widget.emptyQuery {Widget.orderBy_ = [asc widgetName, desc widgetCreatedAt]}
          )
      map (.name) ascending `shouldBe` ["alpha", "beta", "gamma"]
      map (.name) descending `shouldBe` ["gamma", "beta", "alpha"]
      map (.name) multi `shouldBe` ["alpha", "beta", "gamma"]

    it "count and findFirst use the generated Client" $ \TestEnv {envPool = pool} -> do
      _ <- WidgetFixtures.insertWidget pool "beta"
      alpha <- WidgetFixtures.insertWidget pool "alpha"
      n <- runDb pool (Widget.count Widget.emptyQuery {Widget.where_ = Just (eq widgetName "alpha")})
      n `shouldBe` 1
      firstAsc <-
        runDb
          pool
          ( Widget.findFirst
              Widget.emptyQuery {Widget.orderBy_ = [asc widgetName]}
          )
      firstAsc `shouldBe` Just alpha
      missing <-
        runDb
          pool
          ( Widget.findFirstOrFail
              Widget.emptyQuery {Widget.where_ = Just (eq widgetName "missing")}
          )
      missing
        `shouldSatisfy` ( \case
                            Left (RecordNotFound _) -> True
                            _ -> False
                        )

  describe "select_ + include" $ do
    it "projects shelf scalars and keeps nested books" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      let sel =
            Shelf.ShelfSelect
              { id = False,
                name = True
              }
          query =
            Shelf.ShelfQuery
              { include_ = shelfInclude,
                select_ = sel,
                where_ = Just (eq shelfName "fiction"),
                orderBy_ = [],
                limit_ = Nothing,
                offset_ = Nothing
              }
      [row] <- runDb pool (Shelf.findMany query)
      row.shelf.id `shouldBe` shelf.id
      row.shelf.name `shouldBe` Picked "fiction"
      length row.books `shouldBe` 1
      (head row.books).book.title `shouldBe` "Dune"

    it "findMany emptyQuery returns ShelfRow without include_" $ \TestEnv {envPool = pool} -> do
      created <- ShelfFixtures.insertShelf pool "Toast"
      rows <- runDb pool (Shelf.findMany Shelf.emptyQuery)
      map (.name) rows `shouldBe` ["Toast"]
      map (.id) rows `shouldBe` [created.id]
      found <-
        runDb
          pool
          (Shelf.findUnique (Shelf.uniqueQuery (Shelf.ById created.id)))
          >>= assertRight
      fmap (.name) found `shouldBe` Just "Toast"

    it "findUnique loads included relations" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "Omelette"
      _ <- ShelfFixtures.insertBook pool shelf.id "Eggs"
      found <-
        runDb
          pool
          (Shelf.findUnique ((Shelf.uniqueQuery (Shelf.ById shelf.id)) {Shelf.include_ = shelfInclude}))
          >>= assertRight
      fmap (.shelf.name) found `shouldBe` Just "Omelette"
      fmap (length . (.books)) found `shouldBe` Just 1

    it "create and findMany share Schema.Client.Shelf" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb pool (Shelf.create (Shelf.ShelfCreate {id = Nothing, name = "Pantry", books = [], tags = []}))
          >>= assertRight
      rows <-
        runDb
          pool
          ( Shelf.findMany
              Shelf.emptyQuery
                { Shelf.include_ = shelfInclude,
                  Shelf.where_ = Just (eq shelfId created.id)
                }
          )
      map ((.name) . (.shelf)) rows `shouldBe` ["Pantry"]
      map ((.id) . (.shelf)) rows `shouldBe` [created.id]

shelfInclude =
  ShelfInclude {books = loadWith (BookInclude {chapters = skip}), tags = skip}
