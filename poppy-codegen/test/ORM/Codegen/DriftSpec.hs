{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.DriftSpec
  ( driftSpec,
    driftDbSpec,
  )
where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import ORM.Codegen.Drift
import ORM.Codegen.IR (Schema (..), schemaUniques, unique_)
import ORM.Codegen.Introspect (canonicalizeColumnType, introspectCatalog)
import qualified ORM.Codegen.Schema as Builder
import ORM.Codegen.Spec.Flag (flagSchema)
import ORM.Codegen.Spec.Shelf (shelfSchema)
import ORM.Codegen.Spec.Widget (widgetSchema)
import ORM.Db (withConn)
import Support.TestDb (TestEnv (..))
import Test.Hspec

driftSpec :: Spec
driftSpec =
  describe "ORM.Codegen.Drift" $ do
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

driftDbSpec :: SpecWith TestEnv
driftDbSpec =
  describe "ORM.Codegen.Drift against Postgres" $ do
    it "matches widget and shelf IR to the test database" $ \TestEnv {envPool = pool} -> do
      catalog <- withConn pool introspectCatalog
      checkSchema widgetSchema catalog `shouldBe` []
      checkSchema shelfSchema catalog `shouldBe` []

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
                  [ col "id" "uuid" False,
                    col "created_at" "timestamptz" False,
                    col "updated_at" "timestamptz" False,
                    col "name" "text" nameNullable,
                    col "description" "text" True
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
                  [ col "id" "uuid" False,
                    col "color" "enum:color" False
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
                  [ col "id" "uuid" False,
                    col "active" "boolean" False
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

col :: Text -> Text -> Bool -> (Text, DbColumn)
col name ty isNullable =
  (name, DbColumn {dbColName = name, dbColType = ty, dbColNullable = isNullable})
