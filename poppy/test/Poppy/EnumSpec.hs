{-# LANGUAGE TypeApplications #-}

module Poppy.EnumSpec
  ( enumSpec,
  )
where

import Poppy (runDb)
import qualified Poppy.AuthorFixtures as AuthorFixtures
import qualified Poppy.Internal.Operations as Ops
import Poppy.Internal.Query (matching)
import Poppy.Internal.Where (eq)
import Schema.Author (AuthorRow (..))
import Schema.Post (PostRow (..), PostTable, postStatus)
import Schema.PostStatus (PostStatus (..), postStatusToString)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldMatchList)

enumSpec :: SpecWith TestEnv
enumSpec =
  describe "schema enums" $ do
    it "maps constructors to Postgres labels" $ \_ -> do
      postStatusToString Draft `shouldBe` "draft"
      postStatusToString Published `shouldBe` "published"

    it "inserts and reads enum columns through FromField/ToField" $ \TestEnv {envPool = pool} -> do
      author <- AuthorFixtures.insertAuthor pool "Ada"
      draft <- AuthorFixtures.insertPost pool author.id "Notes" Draft
      published <- AuthorFixtures.insertPost pool author.id "Essay" Published
      draft.status `shouldBe` Draft
      published.status `shouldBe` Published
      rows <- runDb pool (Ops.findMany @PostTable @PostRow id)
      map (.status) rows `shouldMatchList` [Draft, Published]

    it "filters on an enum field" $ \TestEnv {envPool = pool} -> do
      author <- AuthorFixtures.insertAuthor pool "Ada"
      _ <- AuthorFixtures.insertPost pool author.id "Notes" Draft
      _ <- AuthorFixtures.insertPost pool author.id "Essay" Published
      rows <-
        runDb pool (Ops.findMany @PostTable @PostRow (matching (eq postStatus Published)))
      map (.title) rows `shouldBe` ["Essay"]
      map (.status) rows `shouldBe` [Published]
