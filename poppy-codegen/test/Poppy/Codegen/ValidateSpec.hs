{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.ValidateSpec
  ( validateSpec,
  )
where

import Poppy.Codegen.IR
import qualified Poppy.Codegen.Schema as Builder
import Poppy.Codegen.Spec.Example (exampleSchema)
import Poppy.Codegen.Spec.Flag (flagSchema)
import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Validate
import Test.Hspec

emptySchema :: [Model] -> [ModelInclude] -> Schema
emptySchema models includes =
  Schema {schemaEnums = [], schemaModels = models, schemaIncludes = includes, schemaUniques = []}

validateSpec :: Spec
validateSpec =
  describe "Poppy.Codegen.Validate" $ do
    describe "example specs" $ do
      it "accepts the Shelf schema" $
        validateSchema shelfSchema `shouldBe` []

      it "accepts the canonical Example schema" $
        validateSchema exampleSchema `shouldBe` []

      it "accepts a Schema with a boolean field" $
        validateSchema flagSchema `shouldBe` []

      it "rejects a builder model with no primary key" $
        validateSchema (Builder.schema [] [Builder.model "Widget" [text "name"] []] [])
          `shouldBe` [ModelMissingPrimaryKey "Widget"]

    describe "primary keys" $ do
      it "rejects a model with no primary key" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Widget",
                      modelTable = "widget",
                      modelFields = [text "name"],
                      modelRelations = []
                    }
                ]
                []
        validateSchema schema
          `shouldBe` [ModelMissingPrimaryKey "Widget"]

      it "rejects a model with multiple primary keys" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Widget",
                      modelTable = "widget",
                      modelFields = [pk (uuid "id"), pk (uuid "otherId")],
                      modelRelations = []
                    }
                ]
                []
        validateSchema schema
          `shouldBe` [ModelMultiplePrimaryKeys "Widget"]

    describe "enums" $ do
      it "rejects a field referencing an unknown enum" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Item",
                      modelTable = "item",
                      modelFields = [pk (uuid "id"), enumField "unit" "Unit"],
                      modelRelations = []
                    }
                ]
                []
        validateSchema schema
          `shouldBe` [UnknownEnumType "Item" "unit" "Unit"]

      it "rejects an enum with no variants" $ do
        let schema =
              Schema
                { schemaEnums = [enum_ "Unit" []],
                  schemaModels =
                    [ Model
                        { modelName = "Item",
                          modelTable = "item",
                          modelFields = [pk (uuid "id")],
                          modelRelations = []
                        }
                    ],
                  schemaIncludes = [],
                  schemaUniques = []
                }
        validateSchema schema
          `shouldBe` [EnumHasNoVariants "Unit"]

      it "rejects duplicate enum variant names" $ do
        let schema =
              Schema
                { schemaEnums = [enum_ "Unit" [variant "G", variant "G"]],
                  schemaModels =
                    [ Model
                        { modelName = "Item",
                          modelTable = "item",
                          modelFields = [pk (uuid "id")],
                          modelRelations = []
                        }
                    ],
                  schemaIncludes = [],
                  schemaUniques = []
                }
        validateSchema schema
          `shouldBe` [DuplicateEnumVariant "Unit" "G"]

    describe "relations" $ do
      it "rejects a relation pointing at an unknown model" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id")],
                      modelRelations =
                        [ hasMany "shelfBooks" "Shelf" "Missing" "id" "shelfId"
                        ]
                    }
                ]
                []
        validateSchema schema
          `shouldBe` [UnknownRelationModel "shelfBooks" "to" "Missing"]

      it "rejects a relation with an unknown field" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id")],
                      modelRelations =
                        [ hasMany "shelfBooks" "Shelf" "Book" "id" "shelfId"
                        ]
                    },
                  Model
                    { modelName = "Book",
                      modelTable = "test_book",
                      modelFields = [pk (uuid "id"), text "title"],
                      modelRelations = []
                    }
                ]
                []
        validateSchema schema
          `shouldBe` [UnknownRelationField "shelfBooks" "Book" "shelfId"]

    describe "includes" $ do
      it "rejects an include rooted at an unknown model" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id")],
                      modelRelations = []
                    }
                ]
                [ ModelInclude
                    { includeName = "BadInclude",
                      includeRootModel = "Nope",
                      includeTree = []
                    }
                ]
        validateSchema schema
          `shouldBe` [UnknownIncludeRoot "BadInclude" "Nope"]

      it "rejects an include that references an unknown relation" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id")],
                      modelRelations = []
                    }
                ]
                [ ModelInclude
                    { includeName = "ShelfInclude",
                      includeRootModel = "Shelf",
                      includeTree =
                        [ IncludeTree
                            { includeRelation = "shelfBooks",
                              includeChildren = []
                            }
                        ]
                    }
                ]
        validateSchema schema
          `shouldBe` [UnknownIncludeRelation "ShelfInclude" "Shelf" "shelfBooks"]

      it "rejects duplicate relations at the same include level" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id"), uuid "name"],
                      modelRelations =
                        [ hasMany "shelfBooks" "Shelf" "Book" "id" "shelfId"
                        ]
                    },
                  Model
                    { modelName = "Book",
                      modelTable = "test_book",
                      modelFields = [pk (uuid "id"), column "shelf_id" (uuid "shelfId")],
                      modelRelations = []
                    }
                ]
                [ ModelInclude
                    { includeName = "ShelfInclude",
                      includeRootModel = "Shelf",
                      includeTree =
                        [ IncludeTree {includeRelation = "shelfBooks", includeChildren = []},
                          IncludeTree {includeRelation = "shelfBooks", includeChildren = []}
                        ]
                    }
                ]
        validateSchema schema
          `shouldBe` [DuplicateIncludeRelation "ShelfInclude" "Shelf" "shelfBooks"]

      it "rejects leftover include declarations that are not the relation graph" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Shelf",
                      modelTable = "test_shelf",
                      modelFields = [pk (uuid "id"), uuid "name"],
                      modelRelations =
                        [ hasMany "shelfBooks" "Shelf" "Book" "id" "shelfId"
                        ]
                    },
                  Model
                    { modelName = "Book",
                      modelTable = "test_book",
                      modelFields = [pk (uuid "id"), column "shelf_id" (uuid "shelfId")],
                      modelRelations = []
                    }
                ]
                [ ModelInclude
                    { includeName = "ShelfBooksOnly",
                      includeRootModel = "Shelf",
                      includeTree =
                        [IncludeTree {includeRelation = "shelfBooks", includeChildren = []}]
                    }
                ]
        validateSchema schema
          `shouldBe` [LeftoverIncludeDeclaration "ShelfBooksOnly"]

      it "rejects a unique constraint on an unknown model" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Widget",
                      modelTable = "widget",
                      modelFields = [pk (uuid "id")],
                      modelRelations = []
                    }
                ]
                []
            drifted = schema {schemaUniques = [unique_ "Nope" ["id"]]}
        validateSchema drifted
          `shouldBe` [UnknownUniqueModel "Nope"]

      it "rejects a unique constraint that references an unknown field" $ do
        let schema =
              emptySchema
                [ Model
                    { modelName = "Widget",
                      modelTable = "widget",
                      modelFields = [pk (uuid "id")],
                      modelRelations = []
                    }
                ]
                []
            drifted = schema {schemaUniques = [unique_ "Widget" ["name"]]}
        validateSchema drifted
          `shouldBe` [UnknownUniqueField "Widget" "name"]
