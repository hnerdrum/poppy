{-# LANGUAGE TypeApplications #-}

module Poppy.BelongsToSpec
  ( belongsToSpec,
  )
where

import Poppy (runDb)
import qualified Poppy.AuthorFixtures as AuthorFixtures
import Poppy.Include (findMany)
import Schema.Author (AuthorRow (..))
import Schema.Post (PostRow (..), PostTable)
import Schema.PostInclude (PostInclude (..), PostWithAuthor (..))
import Schema.PostStatus (PostStatus (..))
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

includeAuthor :: PostInclude
includeAuthor = PostInclude {author = True}

skipAuthor :: PostInclude
skipAuthor = PostInclude {author = False}

belongsToSpec :: SpecWith TestEnv
belongsToSpec =
  describe "belongsTo includes" $ do
    it "nests the parent row on each child" $ \TestEnv {envPool = pool} -> do
      ada <- AuthorFixtures.insertAuthor pool "Ada"
      grace <- AuthorFixtures.insertAuthor pool "Grace"
      notes <- AuthorFixtures.insertPost pool ada.id "Notes" Draft
      compiler <- AuthorFixtures.insertPost pool grace.id "Compiler" Published
      results <- runDb pool (findMany @PostTable includeAuthor id)
      let adaPost = head [r | r <- results, r.post.id == notes.id]
          gracePost = head [r | r <- results, r.post.id == compiler.id]
      fmap (.name) adaPost.author `shouldBe` Just "Ada"
      fmap (.id) adaPost.author `shouldBe` Just ada.id
      fmap (.name) gracePost.author `shouldBe` Just "Grace"
      fmap (.id) gracePost.author `shouldBe` Just grace.id

    it "leaves the parent unset when the edge is skipped" $ \TestEnv {envPool = pool} -> do
      ada <- AuthorFixtures.insertAuthor pool "Ada"
      _ <- AuthorFixtures.insertPost pool ada.id "Notes" Draft
      results <- runDb pool (findMany @PostTable skipAuthor id)
      map (.author) results `shouldBe` [Nothing]
      map ((.title) . (.post)) results `shouldBe` ["Notes"]
