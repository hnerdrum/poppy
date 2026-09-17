{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.JoinAliasSpec
  ( joinAliasSpec,
  )
where

import ORM.Codegen.IR
import ORM.Codegen.JoinAlias (IncludeAliases (..), assignIncludeAliases, assignModelAliases)
import ORM.Codegen.Spec.Shelf (shelfSchema)
import Test.Hspec

joinAliasSpec :: Spec
joinAliasSpec =
  describe "ORM.Codegen.JoinAlias" $ do
    it "assigns Shelf include root and child aliases s and b" $ do
      let incl = head (schemaIncludes shelfSchema)
          IncludeAliases {iaRoot, iaChild} = assignIncludeAliases shelfSchema incl
      iaRoot `shouldBe` "s"
      iaChild `shouldBe` "b"

    it "assigns deep Shelf include alias c for Chapter" $ do
      let incl = head (schemaIncludes shelfSchema)
          IncludeAliases {iaGrand} = assignIncludeAliases shelfSchema incl
      iaGrand `shouldBe` Just "c"

    it "disambiguates Alpha and Article with distinct aliases" $ do
      let assigned = assignModelAliases [alphaModel, articleModel]
      map snd assigned `shouldBe` ["a", "ar"]

    it "disambiguates Recipe, RecipeIngredient, and RecipeItem" $ do
      let assigned = assignModelAliases [recipeModel, recipeIngredientModel, recipeItemModel]
      map snd assigned `shouldBe` ["r", "ri", "re"]

alphaModel :: Model
alphaModel =
  Model
    { modelName = "Alpha",
      modelTable = "alpha",
      modelFields = [pk (uuid "id")],
      modelRelations =
        [ hasMany "alphaArticles" "Alpha" "Article" "id" "alphaId"
        ]
    }

articleModel :: Model
articleModel =
  Model
    { modelName = "Article",
      modelTable = "article",
      modelFields =
        [ pk (uuid "id"),
          column "alpha_id" (uuid "alphaId")
        ],
      modelRelations = []
    }

recipeModel :: Model
recipeModel =
  Model
    { modelName = "Recipe",
      modelTable = "recipe",
      modelFields = [pk (uuid "id")],
      modelRelations =
        [ hasMany "recipeIngredients" "Recipe" "RecipeIngredient" "id" "recipeId"
        ]
    }

recipeIngredientModel :: Model
recipeIngredientModel =
  Model
    { modelName = "RecipeIngredient",
      modelTable = "recipe_ingredient",
      modelFields =
        [ pk (column "recipe_id" (uuid "recipeId")),
          column "ingredient_id" (uuid "ingredientId")
        ],
      modelRelations =
        [ belongsTo "riItem" "RecipeIngredient" "RecipeItem" "ingredientId" "id"
        ]
    }

recipeItemModel :: Model
recipeItemModel =
  Model
    { modelName = "RecipeItem",
      modelTable = "recipe_item",
      modelFields = [pk (uuid "id")],
      modelRelations = []
    }
