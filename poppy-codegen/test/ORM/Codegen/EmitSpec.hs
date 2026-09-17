{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.EmitSpec
  ( emitSpec,
  )
where

import Data.Function ((&))
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import ORM.Codegen.Emit.Schema (emitBelongsTo, emitHasMany, emitModelModule)
import ORM.Codegen.IR
  ( JoinKind (..),
    Model (..),
    RelationKind (..),
    RelationSpec (..),
    modelName,
    modelRelations,
    relJoin,
    relName,
    schemaModels,
  )
import qualified ORM.Codegen.Schema as Builder
import ORM.Codegen.Spec.Example (exampleSchema, taskModel)
import ORM.Codegen.Spec.Flag (flagModel, flagSchema)
import ORM.Codegen.Spec.Shelf (shelfSchema)
import ORM.Codegen.Spec.Widget (widgetModel, widgetSchema)
import Test.Hspec

emitSpec :: Spec
emitSpec = do
  describe "ORM.Codegen.Emit.Schema" $ do
    it "emits the canonical Example Task module" $ do
      let actual = emitModelModule "Schema.Task" exampleSchema taskModel
      actual `shouldSatisfy` T.isInfixOf "data TaskRow"
      actual `shouldSatisfy` T.isInfixOf "done :: Bool"
      actual `shouldSatisfy` T.isInfixOf "tableName = \"task\""

    it "emits Widget matching the golden file" $ do
      expected <- T.readFile "test/ORM/Codegen/golden/Widget.hs.golden"
      let actual = emitModelModule "Schema.Widget" widgetSchema widgetModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Flag matching the golden file" $ do
      expected <- T.readFile "test/ORM/Codegen/golden/Flag.hs.golden"
      let actual = emitModelModule "Schema.Flag" flagSchema flagModel
      T.strip actual `shouldBe` T.strip expected

    it "includes HasMany in a Shelf model module" $ do
      let actual = emitModelModule "Schema.Shelf" shelfSchema shelfModel
      actual `shouldSatisfy` T.isInfixOf "import ORM.Relation (HasMany (..), JoinType (..))"
      actual `shouldSatisfy` T.isInfixOf "import Schema.Book (BookTable, bookShelfId)"
      actual `shouldSatisfy` T.isInfixOf "    shelfBooks"
      T.strip (emitHasMany shelfSchema shelfBooksRel)
        `shouldSatisfy` (`T.isInfixOf` T.strip actual)

    it "includes BelongsTo in a Post model module" $ do
      let actual = emitModelModule "Schema.Post" authorPostSchema postModel
      actual
        `shouldSatisfy` T.isInfixOf "import ORM.Relation (BelongsTo (..), JoinType (..))"
      actual
        `shouldSatisfy` T.isInfixOf "import Schema.Author (AuthorTable)"
      actual `shouldSatisfy` T.isInfixOf "    author"
      T.strip (emitBelongsTo authorPostSchema authorRel)
        `shouldSatisfy` (`T.isInfixOf` T.strip actual)

    it "emits Shelf matching the golden file" $ do
      expected <- T.readFile "test/ORM/Codegen/golden/Shelf.hs.golden"
      let actual = emitModelModule "Schema.Shelf" shelfSchema shelfModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Book matching the golden file" $ do
      expected <- T.readFile "test/ORM/Codegen/golden/Book.hs.golden"
      let actual = emitModelModule "Schema.Book" shelfSchema bookModel
      T.strip actual `shouldBe` T.strip expected

    it "emits Chapter matching the golden file" $ do
      expected <- T.readFile "test/ORM/Codegen/golden/Chapter.hs.golden"
      let actual = emitModelModule "Schema.Chapter" shelfSchema chapterModel
      T.strip actual `shouldBe` T.strip expected

  describe "ORM.Codegen.Emit.Schema.emitHasMany" $ do
    it "emits shelfBooks" $ do
      T.strip (emitHasMany shelfSchema shelfBooksRel)
        `shouldBe` T.strip expectedShelfBooks
    it "honors InnerJoin" $ do
      let rel = shelfBooksRel {relJoin = JoinInner}
      T.strip (emitHasMany shelfSchema rel)
        `shouldBe` T.strip expectedShelfBooksInner

    it "emits bookChapters" $ do
      T.strip (emitHasMany shelfSchema bookChaptersRel)
        `shouldBe` T.strip expectedBookChapters

  describe "ORM.Codegen.Emit.Schema.emitBelongsTo" $ do
    it "emits author" $ do
      T.strip (emitBelongsTo authorPostSchema authorRel)
        `shouldBe` T.strip expectedAuthorRel

shelfModel :: Model
shelfModel =
  case find ((== "Shelf") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "shelfModel: Shelf missing"

shelfBooksRel :: RelationSpec
shelfBooksRel =
  case find ((== "shelfBooks") . relName) (modelRelations shelfModel) of
    Just rel | relKind rel == RelHasMany -> rel
    _ -> error "shelfBooksRel: shelfBooks HasMany missing"

expectedShelfBooks :: T.Text
expectedShelfBooks =
  T.unlines
    [ "shelfBooks :: HasMany ShelfTable BookTable UUID",
      "shelfBooks =",
      "  HasMany",
      "    { localKey = shelfId,",
      "      foreignKey = bookShelfId,",
      "      joinType = LeftJoin",
      "    }"
    ]

expectedShelfBooksInner :: T.Text
expectedShelfBooksInner =
  T.unlines
    [ "shelfBooks :: HasMany ShelfTable BookTable UUID",
      "shelfBooks =",
      "  HasMany",
      "    { localKey = shelfId,",
      "      foreignKey = bookShelfId,",
      "      joinType = InnerJoin",
      "    }"
    ]

bookModel :: Model
bookModel =
  case find ((== "Book") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "bookModel: Book missing"

chapterModel :: Model
chapterModel =
  case find ((== "Chapter") . modelName) (schemaModels shelfSchema) of
    Just m -> m
    Nothing -> error "chapterModel: Chapter missing"

bookChaptersRel :: RelationSpec
bookChaptersRel =
  case find ((== "bookChapters") . relName) (modelRelations bookModel) of
    Just rel | relKind rel == RelHasMany -> rel
    _ -> error "bookChaptersRel: bookChapters HasMany missing"

expectedBookChapters :: T.Text
expectedBookChapters =
  T.unlines
    [ "bookChapters :: HasMany BookTable ChapterTable UUID",
      "bookChapters =",
      "  HasMany",
      "    { localKey = bookId,",
      "      foreignKey = chapterBookRef,",
      "      joinType = LeftJoin",
      "    }"
    ]

authorPostSchema :: Builder.Schema
authorPostSchema =
  Builder.schema
    []
    [ Builder.model "Author" [Builder.uuid "id" & Builder.pk] [],
      Builder.model
        "Post"
        [ Builder.uuid "id" & Builder.pk,
          Builder.uuid "authorId",
          Builder.text "title"
        ]
        [Builder.belongsTo "author" "Author" "authorId"]
    ]
    []

postModel :: Model
postModel =
  case find ((== "Post") . modelName) (schemaModels authorPostSchema) of
    Just m -> m
    Nothing -> error "postModel: Post missing"

authorRel :: RelationSpec
authorRel =
  case find ((== "author") . relName) (modelRelations postModel) of
    Just rel | relKind rel == RelBelongsTo -> rel
    _ -> error "authorRel: author BelongsTo missing"

expectedAuthorRel :: T.Text
expectedAuthorRel =
  T.unlines
    [ "author :: BelongsTo PostTable AuthorTable UUID",
      "author =",
      "  BelongsTo",
      "    { foreignKey = postAuthorId,",
      "      references = Author.authorId,",
      "      joinType = LeftJoin",
      "    }"
    ]
