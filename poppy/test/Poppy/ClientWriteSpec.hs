module Poppy.ClientWriteSpec
  ( clientWriteSpec,
  )
where

import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Poppy (NullableValue (Omit), ORMError (..), eq, runDb)
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

    it "upsert inserts then updates on the unique key" $ \TestEnv {envPool = pool} -> do
      uid <- nextRandom
      inserted <-
        runDb
          pool
          ( Widget.upsert
              (widgetCreate "first") {id = Just uid}
              widgetUpdate
          )
          >>= assertRight
      inserted.name `shouldBe` "first"
      updated <-
        runDb
          pool
          ( Widget.upsert
              (widgetCreate "ignored") {id = Just uid}
              widgetUpdate {name = Just "second"}
          )
          >>= assertRight
      updated.id `shouldBe` uid
      updated.name `shouldBe` "second"
      rows <- runDb pool (Widget.findMany Widget.emptyQuery)
      map (.name) rows `shouldBe` ["second"]

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
