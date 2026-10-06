module Poppy.ClientWriteSpec
  ( clientWriteSpec,
  )
where

import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Poppy (NullableValue (Omit, Value), ORMError (..), eq, runDb)
import qualified Schema.Client.Widget as Widget
import Schema.Widget (WidgetCreate (..), WidgetRow (..), WidgetUpdate (..), widgetName)
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldMatchList, shouldSatisfy)

clientWriteSpec :: SpecWith TestEnv
clientWriteSpec =
  describe "generated Client batch writes" $ do
    it "createMany inserts rows and counts them" $ \TestEnv {envPool = pool} -> do
      n <-
        runDb pool (Widget.createMany [widgetCreate "batch-a", widgetCreate "batch-b"])
          >>= assertRight
      n `shouldBe` 2
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldMatchList` ["batch-a", "batch-b"]

    it "createMany rolls back when one insert fails" $ \TestEnv {envPool = pool} -> do
      uid <- nextRandom
      result <-
        runDb
          pool
          ( Widget.createMany
              [ (widgetCreate "keep-me") {id = Just uid},
                (widgetCreate "also-same-id") {id = Just uid}
              ]
          )
      result
        `shouldSatisfy` ( \case
                            Left (UniqueViolation _) -> True
                            _ -> False
                        )
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      rows `shouldBe` []

    it "updateMany updates matching rows" $ \TestEnv {envPool = pool} -> do
      _ <- runDb pool (Widget.create (widgetCreate "before")) >>= assertRight
      _ <- runDb pool (Widget.create (widgetCreate "other")) >>= assertRight
      n <-
        runDb
          pool
          ( Widget.updateMany
              (eq widgetName "before")
              widgetUpdate {name = Just "after"}
          )
          >>= assertRight
      n `shouldBe` 1
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldMatchList` ["after", "other"]

    it "deleteMany deletes matching rows" $ \TestEnv {envPool = pool} -> do
      _ <- runDb pool (Widget.create (widgetCreate "keep")) >>= assertRight
      _ <- runDb pool (Widget.create (widgetCreate "drop")) >>= assertRight
      n <- runDb pool (Widget.deleteMany (eq widgetName "drop")) >>= assertRight
      n `shouldBe` 1
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldBe` ["keep"]

    it "upsert OnName updates on conflict" $ \TestEnv {envPool = pool} -> do
      inserted <-
        runDb
          pool
          (Widget.upsert Widget.OnName (widgetCreate "first") widgetUpdate)
          >>= assertRight
      inserted.name `shouldBe` "first"
      updated <-
        runDb
          pool
          ( Widget.upsert
              Widget.OnName
              (widgetCreate "first")
              widgetUpdate {name = Just "second"}
          )
          >>= assertRight
      updated.id `shouldBe` inserted.id
      updated.name `shouldBe` "second"
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldBe` ["second"]

    it "update and delete take a unique key" $ \TestEnv {envPool = pool} -> do
      created <- runDb pool (Widget.create (widgetCreate "sage")) >>= assertRight
      same <-
        runDb pool (Widget.update (Widget.ByName "sage") widgetUpdate)
          >>= assertRight
      same.id `shouldBe` created.id
      updated <-
        runDb
          pool
          ( Widget.update
              (Widget.ByName "sage")
              widgetUpdate {description = Value "fresh"}
          )
          >>= assertRight
      updated.description `shouldBe` Just "fresh"
      n <-
        runDb pool (Widget.delete (Widget.ById created.id))
          >>= assertRight
      n `shouldBe` 1

widgetCreate :: Text -> WidgetCreate
widgetCreate name =
  WidgetCreate
    { id = Nothing,
      createdAt = Nothing,
      updatedAt = Nothing,
      name,
      description = Omit
    }

widgetUpdate :: WidgetUpdate
widgetUpdate =
  WidgetUpdate
    { createdAt = Nothing,
      updatedAt = Nothing,
      name = Nothing,
      description = Omit
    }
