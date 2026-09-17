{-# LANGUAGE OverloadedStrings #-}

module Poppy.WhereSpec
  ( whereSpec,
  )
where

import Data.Text (Text)
import Poppy.Core (Field (..))
import Poppy.Where
  ( Where,
    and_,
    compileWhere,
    contains,
    eq,
    gt,
    in_,
    isNull,
    not_,
    or_,
  )
import Test.Hspec

data UserTable

userName :: Field UserTable Text
userName = Field "name" "name"

userAge :: Field UserTable Int
userAge = Field "age" "age"

whereSql :: Where UserTable -> Text
whereSql = fst . compileWhere

whereSpec :: Spec
whereSpec =
  describe "Poppy.Where" $ do
    it "compiles eq" $
      whereSql (eq userName "Ada") `shouldBe` "\"name\" = ?"

    it "compiles gt" $
      whereSql (gt userAge 30) `shouldBe` "\"age\" > ?"

    it "compiles isNull" $
      whereSql (isNull userName) `shouldBe` "\"name\" IS NULL"

    it "compiles empty in_ as FALSE" $
      whereSql (in_ userAge []) `shouldBe` "FALSE"

    it "compiles in_" $
      whereSql (in_ userAge [1, 2]) `shouldBe` "\"age\" IN ?"

    it "compiles contains as case-insensitive substring" $
      whereSql (contains userName "salt")
        `shouldBe` "POSITION(LOWER(?) IN LOWER(\"name\")) > 0"

    it "compiles and_ / or_ / not_ with parentheses" $
      whereSql (eq userName "Ada" `and_` not_ (isNull userAge) `or_` gt userAge 40)
        `shouldBe` "((\"name\" = ?) AND (NOT (\"age\" IS NULL))) OR (\"age\" > ?)"
