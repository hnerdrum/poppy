{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.NestedWriteSpec
  ( nestedWriteSpec,
  )
where

import Data.List (sort)
import qualified Data.UUID.V4 as V4
import Poppy (ORMError (..), runDb)
import qualified Poppy.Operations as Ops
import Schema.Book (BookRow (..), BookTable)
import qualified Schema.Client.Shelf as Shelf
import Schema.Shelf (ShelfRow (..), ShelfTable)
import Schema.ShelfInclude (BookWithChapters (..), ShelfWithBooksTags (..))
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, expectationFailure, it, shouldBe)

nestedWriteSpec :: SpecWith TestEnv
nestedWriteSpec =
  describe "nested writes" $ do
    it "createNested inserts a parent and children in one transaction" $ \TestEnv {envPool = pool} -> do
      result <-
        runDb
          pool
          ( Shelf.createNested
              Shelf.withBooksTags
              Shelf.ShelfWriteCreate
                { root = Shelf.ShelfCreate {id = Nothing, name = "fiction"},
                  books =
                    Shelf.Set
                      [ Shelf.BookNestedCreate {id = Nothing, title = "Dune"},
                        Shelf.BookNestedCreate {id = Nothing, title = "Neuromancer"}
                      ]
                }
          )
          >>= assertRight
      let nested = result :: ShelfWithBooksTags
      nested.shelf.name `shouldBe` "fiction"
      sort (map ((.title) . (.book)) nested.books) `shouldBe` ["Dune", "Neuromancer"]

    it "Set replaces all children" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.createNested
              Shelf.withBooksTags
              Shelf.ShelfWriteCreate
                { root = Shelf.ShelfCreate {id = Nothing, name = "fiction"},
                  books =
                    Shelf.Set
                      [ Shelf.BookNestedCreate {id = Nothing, title = "Dune"},
                        Shelf.BookNestedCreate {id = Nothing, title = "Neuromancer"}
                      ]
                }
          )
          >>= assertRight
      let createdNested = created :: ShelfWithBooksTags
      updated <-
        runDb
          pool
          ( Shelf.updateNested
              Shelf.withBooksTags
              createdNested.shelf.id
              Shelf.ShelfWriteUpdate
                { root = Shelf.ShelfUpdate {name = Nothing},
                  books = Just (Shelf.Set [Shelf.BookNestedCreate {id = Nothing, title = "Hyperion"}])
                }
          )
          >>= assertRight
      let updatedNested = updated :: ShelfWithBooksTags
      map ((.title) . (.book)) updatedNested.books `shouldBe` ["Hyperion"]

    it "Ops create adds a child without wiping existing ones" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.createNested
              Shelf.withBooksTags
              Shelf.ShelfWriteCreate
                { root = Shelf.ShelfCreate {id = Nothing, name = "fiction"},
                  books = Shelf.Set [Shelf.BookNestedCreate {id = Nothing, title = "Dune"}]
                }
          )
          >>= assertRight
      let createdNested = created :: ShelfWithBooksTags
      _ <-
        runDb
          pool
          ( Shelf.updateNested
              Shelf.withBooksTags
              createdNested.shelf.id
              Shelf.ShelfWriteUpdate
                { root = Shelf.ShelfUpdate {name = Nothing},
                  books =
                    Just
                      ( Shelf.Ops
                          Shelf.BookNestedOps
                            { create = [Shelf.BookNestedCreate {id = Nothing, title = "Neuromancer"}],
                              createMany = [],
                              connect = [],
                              disconnect = [],
                              delete = [],
                              update = [],
                              upsert = []
                            }
                      )
                }
          )
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      sort (map (.title) remaining) `shouldBe` ["Dune", "Neuromancer"]

    it "Ops delete removes a child by id" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.createNested
              Shelf.withBooksTags
              Shelf.ShelfWriteCreate
                { root = Shelf.ShelfCreate {id = Nothing, name = "fiction"},
                  books =
                    Shelf.Set
                      [ Shelf.BookNestedCreate {id = Nothing, title = "Dune"},
                        Shelf.BookNestedCreate {id = Nothing, title = "Neuromancer"}
                      ]
                }
          )
          >>= assertRight
      let createdNested = created :: ShelfWithBooksTags
          duneId = head [b.book.id | b <- createdNested.books, b.book.title == "Dune"]
      _ <-
        runDb
          pool
          ( Shelf.updateNested
              Shelf.withBooksTags
              createdNested.shelf.id
              Shelf.ShelfWriteUpdate
                { root = Shelf.ShelfUpdate {name = Nothing},
                  books =
                    Just
                      ( Shelf.Ops
                          Shelf.BookNestedOps
                            { create = [],
                              createMany = [],
                              connect = [],
                              disconnect = [],
                              delete = [duneId],
                              update = [],
                              upsert = []
                            }
                      )
                }
          )
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.title) remaining `shouldBe` ["Neuromancer"]

    it "createNested rolls back the parent when a child write fails" $ \TestEnv {envPool = pool} -> do
      fixed <- V4.nextRandom
      result <-
        runDb pool $
          Shelf.createNested
            Shelf.withBooksTags
            Shelf.ShelfWriteCreate
              { root = Shelf.ShelfCreate {id = Nothing, name = "rollback"},
                books =
                  Shelf.Set
                    [ Shelf.BookNestedCreate {id = Just fixed, title = "Lost"},
                      Shelf.BookNestedCreate {id = Just fixed, title = "Dup"}
                    ]
              }
      case result of
        Left (UniqueViolation _) -> pure ()
        other -> expectationFailure ("expected UniqueViolation, got " <> show other)
      shelves <- runDb pool (Ops.findMany @ShelfTable @ShelfRow id)
      books <- runDb pool (Ops.findMany @BookTable @BookRow id)
      shelves `shouldBe` []
      books `shouldBe` []
