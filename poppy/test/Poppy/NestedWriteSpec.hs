{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.NestedWriteSpec
  ( nestedWriteSpec,
  )
where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.List (sort)
import Poppy (runDb, transaction)
import Poppy.Db (liftIO)
import qualified Poppy.Delete as Delete
import qualified Poppy.Insert as Insert
import qualified Poppy.Operations as Ops
import qualified Poppy.ShelfFixtures as ShelfFixtures
import Poppy.Where (eq)
import Schema.Book (BookCreate (..), BookRow (..), BookTable, bookShelfId)
import Schema.Shelf (ShelfCreate (..), ShelfRow (..), ShelfTable)
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

nestedWriteSpec :: SpecWith TestEnv
nestedWriteSpec =
  describe "nested writes" $ do
    it "create inserts a parent and children in one transaction" $ \TestEnv {envPool = pool} -> do
      (shelf, books) <-
        runDb pool $
          transaction $ do
            created <-
              Insert.insert @ShelfTable @ShelfRow (ShelfCreate {id = Nothing, name = "fiction"})
                >>= either (liftIO . fail . show) pure
            dune <-
              Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId = created.id, title = "Dune"})
                >>= either (liftIO . fail . show) pure
            neuro <-
              Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId = created.id, title = "Neuromancer"})
                >>= either (liftIO . fail . show) pure
            pure (created, [dune, neuro])
      shelf.name `shouldBe` "fiction"
      sort (map (.title) books) `shouldBe` ["Dune", "Neuromancer"]

    it "replace-all children wipes then reinserts" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      runDb pool $ do
        _ <- Delete.deleteMany @BookTable (eq bookShelfId shelf.id)
        _ <- Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId = shelf.id, title = "Hyperion"}) >>= either (liftIO . fail . show) pure
        pure ()
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.title) remaining `shouldBe` ["Hyperion"]

    it "create adds a child without wiping existing ones" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      _ <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <-
        runDb pool (Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId = shelf.id, title = "Neuromancer"}))
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      sort (map (.title) remaining) `shouldBe` ["Dune", "Neuromancer"]

    it "delete removes a child by id" $ \TestEnv {envPool = pool} -> do
      shelf <- ShelfFixtures.insertShelf pool "fiction"
      dune <- ShelfFixtures.insertBook pool shelf.id "Dune"
      _ <- ShelfFixtures.insertBook pool shelf.id "Neuromancer"
      deleted <- runDb pool (Ops.delete @BookTable dune.id)
      deleted `shouldBe` 1
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.title) remaining `shouldBe` ["Neuromancer"]

    it "transaction rolls back nested inserts when the action throws" $ \TestEnv {envPool = pool} -> do
      _ <-
        try @ErrorCall $
          runDb pool $
            transaction $ do
              created <-
                Insert.insert @ShelfTable @ShelfRow (ShelfCreate {id = Nothing, name = "rollback"})
                  >>= either (liftIO . fail . show) pure
              _ <-
                Insert.insert @BookTable @BookRow (BookCreate {id = Nothing, shelfId = created.id, title = "Lost"})
                  >>= either (liftIO . fail . show) pure
              liftIO $ throwIO (ErrorCall "boom")
      shelves <- runDb pool (Ops.findMany @ShelfTable @ShelfRow id)
      books <- runDb pool (Ops.findMany @BookTable @BookRow id)
      shelves `shouldBe` []
      books `shouldBe` []
