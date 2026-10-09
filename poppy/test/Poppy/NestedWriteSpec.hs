{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.NestedWriteSpec
  ( nestedWriteSpec,
  )
where

import Data.List (sort)
import qualified Data.UUID.V4 as V4
import Poppy (NullableValue (Omit), ORMError (..), load, loadWith, runDb, skip)
import qualified Poppy.Internal.Operations as Ops
import Schema.Book (BookRow (..), BookTable, BookUpdate (..))
import qualified Schema.Client.Book as Book
import qualified Schema.Client.Comment as Comment
import qualified Schema.Client.Shelf as Shelf
import Schema.Comment (CommentRow (..), CommentTable)
import Schema.Include.Book (BookInclude (..), BookWith (..))
import Schema.Include.Shelf (ShelfInclude (..), ShelfWith (..))
import Schema.Shelf (ShelfRow (..), ShelfTable)
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, expectationFailure, it, shouldBe, shouldMatchList)

nestedWriteSpec :: SpecWith TestEnv
nestedWriteSpec =
  describe "nested writes" $ do
    it "create inserts a parent and children in one transaction" $ \TestEnv {envPool = pool} -> do
      result <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate
                { id = Nothing,
                  name = "fiction",
                  books =
                    [ Shelf.CreateBook {id = Nothing, title = "Dune"},
                      Shelf.CreateBook {id = Nothing, title = "Neuromancer"}
                    ],
                  tags = []
                }
          )
          >>= assertRight
      result.name `shouldBe` "fiction"
      books <- runDb pool (Ops.findMany @BookTable @BookRow id)
      sort (map (.title) books) `shouldBe` ["Dune", "Neuromancer"]

    it "replaceWith replaces all children" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate
                { id = Nothing,
                  name = "fiction",
                  books =
                    [ Shelf.CreateBook {id = Nothing, title = "Dune"},
                      Shelf.CreateBook {id = Nothing, title = "Neuromancer"}
                    ],
                  tags = []
                }
          )
          >>= assertRight
      _ <-
        runDb
          pool
          ( Shelf.update
              (Shelf.ById created.id)
              Shelf.ShelfUpdate
                { name = Nothing,
                  books = Just (booksReplace [Shelf.CreateBook {id = Nothing, title = "Hyperion"}]),
                  tags = Nothing
                }
          )
          >>= assertRight
      books <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.title) books `shouldBe` ["Hyperion"]

    it "create on update adds a child without wiping existing ones" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate
                { id = Nothing,
                  name = "fiction",
                  books = [Shelf.CreateBook {id = Nothing, title = "Dune"}],
                  tags = []
                }
          )
          >>= assertRight
      _ <-
        runDb
          pool
          ( Shelf.update
              (Shelf.ById created.id)
              Shelf.ShelfUpdate
                { name = Nothing,
                  books = Just (booksCreate [Shelf.CreateBook {id = Nothing, title = "Neuromancer"}]),
                  tags = Nothing
                }
          )
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      sort (map (.title) remaining) `shouldBe` ["Dune", "Neuromancer"]

    it "delete removes a child by unique key" $ \TestEnv {envPool = pool} -> do
      created <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate
                { id = Nothing,
                  name = "fiction",
                  books =
                    [ Shelf.CreateBook {id = Nothing, title = "Dune"},
                      Shelf.CreateBook {id = Nothing, title = "Neuromancer"}
                    ],
                  tags = []
                }
          )
          >>= assertRight
      loaded <-
        runDb
          pool
          (Shelf.findUnique ((Shelf.uniqueQuery (Shelf.ById created.id)) {Shelf.include_ = shelfInclude}))
          >>= assertRight
      let duneId =
            case loaded of
              Nothing -> error "expected shelf"
              Just shelf ->
                head [b.book.id | b <- shelf.books, b.book.title == "Dune"]
      _ <-
        runDb
          pool
          ( Shelf.update
              (Shelf.ById created.id)
              Shelf.ShelfUpdate
                { name = Nothing,
                  books = Just (booksDelete [Book.ById duneId]),
                  tags = Nothing
                }
          )
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.title) remaining `shouldBe` ["Neuromancer"]

    it "disconnect keeps the child and clears a nullable FK" $ \TestEnv {envPool = pool} -> do
      parent <-
        runDb
          pool
          ( Comment.create
              Comment.CommentCreate
                { id = Nothing,
                  parentId = Omit,
                  body = "parent",
                  replies = []
                }
          )
          >>= assertRight
      child <-
        runDb
          pool
          ( Comment.create
              Comment.CommentCreate
                { id = Nothing,
                  parentId = Omit,
                  body = "child",
                  replies = []
                }
          )
          >>= assertRight
      _ <-
        runDb
          pool
          ( Comment.update
              (Comment.ById parent.id)
              Comment.CommentUpdate
                { parentId = Omit,
                  body = Nothing,
                  replies = Just (repliesConnect [Comment.ById child.id])
                }
          )
          >>= assertRight
      _ <-
        runDb
          pool
          ( Comment.update
              (Comment.ById parent.id)
              Comment.CommentUpdate
                { parentId = Omit,
                  body = Nothing,
                  replies = Just (repliesDisconnect [Comment.ById child.id])
                }
          )
          >>= assertRight
      remaining <- runDb pool (Ops.findMany @CommentTable @CommentRow id)
      sort (map (.body) remaining) `shouldMatchList` ["child", "parent"]
      let childRow = head [c | c <- remaining, c.body == "child"]
      childRow.parentId `shouldBe` Nothing

    it "upsert does not steal another parent's child" $ \TestEnv {envPool = pool} -> do
      shelfA <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate
                { id = Nothing,
                  name = "a",
                  books = [Shelf.CreateBook {id = Nothing, title = "Owned"}],
                  tags = []
                }
          )
          >>= assertRight
      shelfB <-
        runDb
          pool
          ( Shelf.create
              Shelf.ShelfCreate {id = Nothing, name = "b", books = [], tags = []}
          )
          >>= assertRight
      owned <- runDb pool (Ops.findMany @BookTable @BookRow id)
      let bookId = head [b.id | b <- owned, b.title == "Owned"]
      result <-
        runDb
          pool
          ( Shelf.update
              (Shelf.ById shelfB.id)
              Shelf.ShelfUpdate
                { name = Nothing,
                  books =
                    Just
                      ( booksUpsert
                          [ Shelf.BookNestedUpsert
                              { where_ = Book.ById bookId,
                                create = Shelf.CreateBook {id = Nothing, title = "Stolen"},
                                update = BookUpdate {shelfId = Nothing, title = Just "Stolen"}
                              }
                          ]
                      ),
                  tags = Nothing
                }
          )
      case result of
        Left (UniqueViolation _) -> pure ()
        Left other -> expectationFailure ("expected UniqueViolation, got " <> show other)
        Right _ -> expectationFailure "expected UniqueViolation when upsert would reparent"
      remaining <- runDb pool (Ops.findMany @BookTable @BookRow id)
      map (.shelfId) remaining `shouldBe` [shelfA.id]
      map (.title) remaining `shouldBe` ["Owned"]

    it "create rolls back the parent when a child write fails" $ \TestEnv {envPool = pool} -> do
      fixed <- V4.nextRandom
      result <-
        runDb pool $
          Shelf.create
            Shelf.ShelfCreate
              { id = Nothing,
                name = "rollback",
                books =
                  [ Shelf.CreateBook {id = Just fixed, title = "Lost"},
                    Shelf.CreateBook {id = Just fixed, title = "Dup"}
                  ],
                tags = []
              }
      case result of
        Left (UniqueViolation _) -> pure ()
        Left other -> expectationFailure ("expected UniqueViolation, got " <> show other)
        Right _ -> expectationFailure "expected UniqueViolation, got a written shelf"
      shelves <- runDb pool (Ops.findMany @ShelfTable @ShelfRow id)
      books <- runDb pool (Ops.findMany @BookTable @BookRow id)
      shelves `shouldBe` []
      books `shouldBe` []

booksReplace xs =
  Shelf.BooksUpdate
    { replaceWith = Just xs,
      create = [],
      createMany = [],
      connect = [],
      delete = [],
      update = [],
      upsert = []
    }

booksCreate xs =
  Shelf.BooksUpdate
    { replaceWith = Nothing,
      create = xs,
      createMany = [],
      connect = [],
      delete = [],
      update = [],
      upsert = []
    }

booksDelete keys =
  Shelf.BooksUpdate
    { replaceWith = Nothing,
      create = [],
      createMany = [],
      connect = [],
      delete = keys,
      update = [],
      upsert = []
    }

booksUpsert items =
  Shelf.BooksUpdate
    { replaceWith = Nothing,
      create = [],
      createMany = [],
      connect = [],
      delete = [],
      update = [],
      upsert = items
    }

repliesConnect keys =
  Comment.RepliesUpdate
    { replaceWith = Nothing,
      create = [],
      createMany = [],
      connect = keys,
      delete = [],
      update = [],
      upsert = [],
      disconnect = []
    }

repliesDisconnect keys =
  Comment.RepliesUpdate
    { replaceWith = Nothing,
      create = [],
      createMany = [],
      connect = [],
      delete = [],
      update = [],
      upsert = [],
      disconnect = keys
    }

shelfInclude =
  ShelfInclude {books = loadWith (BookInclude {chapters = skip}), tags = skip}
