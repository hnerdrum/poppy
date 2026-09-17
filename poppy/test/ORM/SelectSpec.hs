{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ORM.SelectSpec
  ( selectSpec,
  )
where

import ORM (ORMError (..), Picked (..), runDb)
import qualified ORM.Operations as Ops
import ORM.Query (selectColumns)
import ORM.Select (picked)
import qualified ORM.ShelfFixtures as ShelfFixtures
import ORM.Where (eq)
import qualified ORM.WidgetFixtures as WidgetFixtures
import Schema.Book (BookRow (..))
import qualified Schema.Client.Shelf as Shelf
import qualified Schema.Client.Widget as Widget
import Schema.Shelf (ShelfRow (..), shelfId, shelfName)
import Schema.ShelfInclude (BookWithChapters (..), ShelfWithBooksTags (..))
import Schema.Widget
  ( WidgetPicked (..),
    WidgetRow (..),
    WidgetSelect (..),
    WidgetTable,
    parseWidgetPicked,
    widgetName,
    widgetSelectColumns,
  )
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

selectSpec :: SpecWith TestEnv
selectSpec = do
  describe "ORM.Select columns" $ do
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

    it "findUnique looks up by where_ on the query record" $ \TestEnv {envPool = pool} -> do
      created <- WidgetFixtures.insertWidget pool "thyme"
      found <-
        runDb
          pool
          ( Widget.findUnique
              Widget.emptyQuery {Widget.where_ = Just (eq Widget.widgetId created.id)}
          )
      found `shouldBe` Right (Just created)

    it "findUnique rejects a where_ that is not a unique key" $ \TestEnv {envPool = pool} -> do
      _ <- WidgetFixtures.insertWidget pool "sage"
      found <-
        runDb
          pool
          ( Widget.findUnique
              Widget.emptyQuery {Widget.where_ = Just (eq widgetName "sage")}
          )
      found
        `shouldSatisfy` ( \case
                            Left (InvalidUniqueInput _) -> True
                            _ -> False
                        )

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
                orderBy_ = Nothing,
                limit_ = Nothing,
                offset_ = Nothing
              }
      [row] <- runDb pool (Widget.findMany query)
      row.id `shouldBe` created.id
      row.name `shouldBe` Picked "pepper"
      row.createdAt `shouldBe` Skipped
      row.updatedAt `shouldBe` Skipped

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
              { include_ = Shelf.withBooksTags,
                select_ = sel,
                where_ = Just (eq shelfName "fiction"),
                orderBy_ = Nothing,
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
          ( Shelf.findUnique
              Shelf.emptyQuery {Shelf.where_ = Just (eq shelfId created.id)}
          )
          >>= assertRight
      fmap (.name) found `shouldBe` Just "Toast"

    it "findUnique applies include from the query record" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "Omelette"
      _ <- ShelfFixtures.insertBook pool shelf.id "Eggs"
      found <-
        runDb
          pool
          ( Shelf.findUnique
              Shelf.emptyQuery
                { Shelf.include_ = Shelf.withBooksTags,
                  Shelf.where_ = Just (eq shelfId shelf.id)
                }
          )
          >>= assertRight
      fmap (.shelf.name) found `shouldBe` Just "Omelette"
      fmap (length . (.books)) found `shouldBe` Just 1
