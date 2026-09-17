module ORM.GroupSpec
  ( groupSpec,
  )
where

import ORM.Group (groupByKey)
import Test.Hspec (Spec, describe, it, shouldBe)

groupSpec :: Spec
groupSpec =
  describe "ORM.Group.groupByKey" $ do
    it "preserves first-seen key order rather than sorted key order" $ do
      groupByKey id ([3, 1, 3, 2, 1] :: [Int]) `shouldBe` [[3, 3], [1, 1], [2]]

    it "preserves row order within each group" $ do
      groupByKey fst ([('b', 1), ('a', 2), ('b', 3), ('a', 4)] :: [(Char, Int)])
        `shouldBe` [[('b', 1), ('b', 3)], [('a', 2), ('a', 4)]]

    it "returns an empty list for no rows" $ do
      groupByKey id ([] :: [Int]) `shouldBe` []
