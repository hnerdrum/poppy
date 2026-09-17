{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ORM.RawSpec
  ( rawSpec,
  )
where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.Types (Query (..))
import ORM (NullableValue (Omit), ORMError (..), runDb, transaction, withTransaction)
import ORM.Db (liftIO)
import qualified ORM.Insert as Insert
import qualified ORM.Operations as Ops
import ORM.Sql (catchDb, executeRaw, param, queryRaw)
import qualified ORM.WidgetFixtures as WidgetFixtures
import Schema.Widget (WidgetCreate (..), WidgetRow (..), WidgetTable)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

data CountRow = CountRow {cnt :: Int64}
  deriving (Eq, Show)

instance FromRow CountRow where
  fromRow = CountRow <$> field

rawSpec :: SpecWith TestEnv
rawSpec =
  describe "ORM.Sql raw queries" $ do
    it "queryRaw returns typed rows" $ \TestEnv {envPool = pool} -> do
      _ <- WidgetFixtures.insertWidget pool "alpha"
      _ <- WidgetFixtures.insertWidget pool "beta"
      rows <-
        runDb pool $
          queryRaw
            (Query "SELECT COUNT(*) AS cnt FROM test_widget")
            []
      rows `shouldBe` [CountRow 2]

    it "queryRaw binds parameters" $ \TestEnv {envPool = pool} -> do
      widget <- WidgetFixtures.insertWidget pool "needle"
      _ <- WidgetFixtures.insertWidget pool "hay"
      rows <-
        runDb pool $
          queryRaw
            (Query "SELECT id, created_at, updated_at, name, description FROM test_widget WHERE name = ?")
            [param ("needle" :: Text)]
      rows `shouldBe` [widget]

    it "executeRaw updates rows" $ \TestEnv {envPool = pool} -> do
      widget <- WidgetFixtures.insertWidget pool "before"
      affected <-
        runDb pool $
          executeRaw
            (Query "UPDATE test_widget SET name = ? WHERE id = ?")
            [param ("after" :: Text), param widget.id]
      affected `shouldBe` 1
      updated <- runDb pool (Ops.findUnique @WidgetTable @WidgetRow widget.id)
      fmap (.name) updated `shouldBe` Just "after"

    it "executeRaw changes are visible to ORM reads within a transaction" $ \TestEnv {envPool = pool} -> do
      widget <- WidgetFixtures.insertWidget pool "tx-before"
      updated <-
        withTransaction pool $ do
          _ <-
            executeRaw
              (Query "UPDATE test_widget SET name = ? WHERE id = ?")
              [param ("tx-after" :: Text), param widget.id]
          Ops.findUnique @WidgetTable @WidgetRow widget.id
      fmap (.name) updated `shouldBe` Just "tx-after"
      persisted <- runDb pool (Ops.findUnique @WidgetTable @WidgetRow widget.id)
      fmap (.name) persisted `shouldBe` Just "tx-after"

    it "queryRaw participates in withTransaction" $ \TestEnv {envPool = pool} -> do
      widget <- WidgetFixtures.insertWidget pool "shared-connection"
      (count, found) <-
        withTransaction pool $ do
          results <-
            queryRaw
              (Query "SELECT COUNT(*) AS cnt FROM test_widget WHERE id = ?")
              [param widget.id]
          row <- Ops.findUnique @WidgetTable @WidgetRow widget.id
          pure (results, row)
      count `shouldBe` [CountRow 1]
      found `shouldBe` Just widget

    it "catchDb maps SQL errors to ORMError" $ \TestEnv {envPool = pool} -> do
      result <-
        runDb pool $
          catchDb $
            queryRaw @WidgetRow
              (Query "SELECT * FROM definitely_not_a_real_table")
              []
      result
        `shouldSatisfy` ( \case
                            Left (DatabaseError _) -> True
                            _ -> False
                        )

    it "transaction commits on success" $ \TestEnv {envPool = pool} -> do
      inserted <-
        runDb pool $
          transaction $
            Insert.insert @WidgetTable @WidgetRow
              WidgetCreate
                { id = Nothing,
                  createdAt = Nothing,
                  updatedAt = Nothing,
                  name = "tx-commit",
                  description = Omit
                }
      inserted
        `shouldSatisfy` ( \case
                            Right row -> row.name == "tx-commit"
                            Left _ -> False
                        )
      rows <- runDb pool (Ops.findMany @WidgetTable @WidgetRow id)
      length rows `shouldBe` 1

    it "transaction rolls back when the action throws" $ \TestEnv {envPool = pool} -> do
      _ <-
        try @ErrorCall $
          runDb pool $
            transaction $ do
              _ <-
                Insert.insert @WidgetTable @WidgetRow
                  WidgetCreate
                    { id = Nothing,
                      createdAt = Nothing,
                      updatedAt = Nothing,
                      name = "tx-rollback",
                      description = Omit
                    }
              liftIO $ throwIO (ErrorCall "boom")
      rows <- runDb pool (Ops.findMany @WidgetTable @WidgetRow id)
      rows `shouldBe` []
