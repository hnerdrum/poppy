{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.DriftSpec
  ( driftSpec,
    driftDbSpec,
  )
where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Poppy.Codegen.Drift
import Poppy.Codegen.IR (FieldDefault (..), Schema (..), schemaUniques, unique_)
import Poppy.Codegen.Introspect (canonicalizeColumnType, introspectCatalog)
import qualified Poppy.Codegen.Schema as Builder
import Poppy.Codegen.Spec.Author (authorSchema)
import Poppy.Codegen.Spec.Flag (flagSchema)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.Widget (widgetSchema)
import Poppy.Db (withConn)
import Support.TestDb (TestEnv (..))
import Test.Hspec

driftSpec :: Spec
driftSpec =
  describe "Poppy.Codegen.Drift" $ do
    it "accepts an IR that matches the catalog" $
      checkSchema widgetSchema (widgetCatalog False) `shouldBe` []

    it "reports a missing table" $
      checkSchema widgetSchema emptyCatalog
        `shouldBe` [DriftMissingTable "Widget" "test_widget"]

    it "reports nullability drift" $
      checkSchema widgetSchema (widgetCatalog True)
        `shouldBe` [DriftNullability "test_widget" "name" False True]

    it "reports a missing unique from the IR" $
      checkSchema widgetSchemaWithNameUnique (widgetCatalog False)
        `shouldBe` [DriftMissingUnique "test_widget" ["name"]]

    it "reports a database unique that is not in the IR" $
      checkSchema widgetSchema widgetCatalogWithUnique
        `shouldBe` [DriftUnexpectedUnique "test_widget" ["name"]]

    it "reports enum label drift" $
      checkSchema colorSchema colorCatalogWrongLabels
        `shouldBe` [DriftEnumLabels "Color" ["blue", "green", "red"] ["blue", "red"]]

    it "accepts a boolean column that matches the catalog" $
      checkSchema flagSchema flagCatalog `shouldBe` []

    it "reports type drift on a boolean column" $
      checkSchema flagSchema flagCatalogAsText
        `shouldBe` [DriftType "flag" "active" "boolean" "text"]

    it "canonicalizes Postgres boolean to the IR boolean type" $
      canonicalizeColumnType "boolean" "bool" `shouldBe` "boolean"

    it "reports a missing column DEFAULT" $
      checkSchema widgetSchema (widgetCatalogWithoutIdDefault)
        `shouldBe` [DriftMissingDefault "test_widget" "id" DefaultUuidV4]

    it "reports a mismatched column DEFAULT" $
      checkSchema widgetSchema (widgetCatalogWithNowOnId)
        `shouldBe` [DriftDefaultMismatch "test_widget" "id" DefaultUuidV4 "now()"]

    it "reports a missing foreign key implied by belongsTo" $
      checkSchema authorSchema authorCatalogNoFk
        `shouldBe` [DriftMissingForeignKey "test_post" "author_id" "test_author" "id"]

    it "reports a missing foreign key implied by hasMany" $
      checkSchema shelfSchema (shelfCatalogNoFk)
        `shouldBe` [ DriftMissingForeignKey "test_book" "shelf_id" "test_shelf" "id",
                     DriftMissingForeignKey "test_tag" "shelf_id" "test_shelf" "id",
                     DriftMissingForeignKey "test_chapter" "book_id" "test_book" "id",
                     DriftMissingForeignKey "test_section" "chapter_id" "test_chapter" "id"
                   ]

driftDbSpec :: SpecWith TestEnv
driftDbSpec =
  describe "Poppy.Codegen.Drift against Postgres" $ do
    it "matches widget, shelf, and author IR to the test database" $ \TestEnv {envPool = pool} -> do
      catalog <- withConn pool introspectCatalog
      checkSchema widgetSchema catalog `shouldBe` []
      checkSchema shelfSchema catalog `shouldBe` []
      checkSchema authorSchema catalog `shouldBe` []

widgetSchemaWithNameUnique :: Schema
widgetSchemaWithNameUnique =
  widgetSchema {schemaUniques = [unique_ "Widget" ["name"]]}

widgetCatalog :: Bool -> DbCatalog
widgetCatalog nameNullable =
  emptyCatalog
    { dbTables =
        Map.singleton
          "test_widget"
          DbTable
            { dbTableName = "test_widget",
              dbColumns =
                Map.fromList
                  [ col "id" "uuid" False (Just "uuid_generate_v4()"),
                    col "created_at" "timestamptz" False (Just "CURRENT_TIMESTAMP"),
                    col "updated_at" "timestamptz" False (Just "CURRENT_TIMESTAMP"),
                    col "name" "text" nameNullable Nothing,
                    col "description" "text" True Nothing
                  ],
              dbPrimaryKey = ["id"],
              dbUniques = []
            }
    }

widgetCatalogWithUnique :: DbCatalog
widgetCatalogWithUnique =
  let base = widgetCatalog False
      table = dbTables base Map.! "test_widget"
   in base {dbTables = Map.singleton "test_widget" table {dbUniques = [Set.singleton "name"]}}

widgetCatalogWithoutIdDefault :: DbCatalog
widgetCatalogWithoutIdDefault =
  setWidgetIdDefault Nothing

widgetCatalogWithNowOnId :: DbCatalog
widgetCatalogWithNowOnId =
  setWidgetIdDefault (Just "now()")

setWidgetIdDefault :: Maybe Text -> DbCatalog
setWidgetIdDefault mDefault =
  let base = widgetCatalog False
      table = dbTables base Map.! "test_widget"
      idCol = dbColumns table Map.! "id"
   in base
        { dbTables =
            Map.singleton
              "test_widget"
              table {dbColumns = Map.insert "id" idCol {dbColDefault = mDefault} (dbColumns table)}
        }

colorSchema :: Schema
colorSchema =
  Builder.schema
    [Builder.enum_ "Color" [Builder.variant "red", Builder.variant "blue", Builder.variant "green"]]
    [ Builder.model
        "Swatch"
        [ Builder.uuid "id" & Builder.pk,
          Builder.enumField "color" "Color"
        ]
        []
    ]
    []

colorCatalogWrongLabels :: DbCatalog
colorCatalogWrongLabels =
  emptyCatalog
    { dbEnums = Map.singleton "color" ["red", "blue"],
      dbTables =
        Map.singleton
          "swatch"
          DbTable
            { dbTableName = "swatch",
              dbColumns =
                Map.fromList
                  [ col "id" "uuid" False Nothing,
                    col "color" "enum:color" False Nothing
                  ],
              dbPrimaryKey = ["id"],
              dbUniques = []
            }
    }

flagCatalog :: DbCatalog
flagCatalog =
  emptyCatalog
    { dbTables =
        Map.singleton
          "flag"
          DbTable
            { dbTableName = "flag",
              dbColumns =
                Map.fromList
                  [ col "id" "uuid" False (Just "uuid_generate_v4()"),
                    col "active" "boolean" False Nothing
                  ],
              dbPrimaryKey = ["id"],
              dbUniques = []
            }
    }

flagCatalogAsText :: DbCatalog
flagCatalogAsText =
  let table = dbTables flagCatalog Map.! "flag"
      active = dbColumns table Map.! "active"
   in flagCatalog
        { dbTables =
            Map.singleton
              "flag"
              table {dbColumns = Map.insert "active" active {dbColType = "text"} (dbColumns table)}
        }

col :: Text -> Text -> Bool -> Maybe Text -> (Text, DbColumn)
col name ty isNullable mDefault =
  ( name,
    DbColumn
      { dbColName = name,
        dbColType = ty,
        dbColNullable = isNullable,
        dbColDefault = mDefault
      }
  )

authorCatalogNoFk :: DbCatalog
authorCatalogNoFk =
  emptyCatalog
    { dbTables =
        Map.fromList
          [ ( "test_author",
              DbTable
                { dbTableName = "test_author",
                  dbColumns =
                    Map.fromList
                      [ col "id" "uuid" False (Just "uuid_generate_v4()"),
                        col "name" "text" False Nothing
                      ],
                  dbPrimaryKey = ["id"],
                  dbUniques = []
                }
            ),
            ( "test_post",
              DbTable
                { dbTableName = "test_post",
                  dbColumns =
                    Map.fromList
                      [ col "id" "uuid" False (Just "uuid_generate_v4()"),
                        col "author_id" "uuid" False Nothing,
                        col "title" "text" False Nothing,
                        col "status" "enum:poststatus" False Nothing
                      ],
                  dbPrimaryKey = ["id"],
                  dbUniques = []
                }
            )
          ],
      dbEnums = Map.singleton "poststatus" ["draft", "published"]
    }

shelfCatalogNoFk :: DbCatalog
shelfCatalogNoFk =
  emptyCatalog
    { dbTables =
        Map.fromList
          [ table "test_shelf" [col "id" "uuid" False (Just "uuid_generate_v4()"), col "name" "text" False Nothing],
            table "test_book" [col "id" "uuid" False (Just "uuid_generate_v4()"), col "shelf_id" "uuid" False Nothing, col "title" "text" False Nothing],
            table "test_chapter" [col "id" "uuid" False (Just "uuid_generate_v4()"), col "book_id" "uuid" False Nothing, col "heading" "text" False Nothing],
            table "test_section" [col "id" "uuid" False (Just "uuid_generate_v4()"), col "chapter_id" "uuid" False Nothing, col "label" "text" False Nothing],
            table "test_tag" [col "id" "uuid" False (Just "uuid_generate_v4()"), col "shelf_id" "uuid" False Nothing, col "label" "text" False Nothing]
          ]
    }
  where
    table name columns =
      ( name,
        DbTable
          { dbTableName = name,
            dbColumns = Map.fromList columns,
            dbPrimaryKey = ["id"],
            dbUniques = []
          }
      )
