module Poppy.BelongsToSpec
  ( belongsToSpec,
  )
where

import Poppy (load, runDb, skip)
import qualified Poppy.AuthorFixtures as AuthorFixtures
import Schema.Author (AuthorRow (..))
import qualified Schema.Client.Post as Post
import Schema.Include.Post (PostInclude (..), PostWith (..))
import Schema.Post (PostRow (..))
import Schema.PostStatus (PostStatus (..))
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

belongsToSpec :: SpecWith TestEnv
belongsToSpec =
  describe "belongsTo includes" $ do
    it "nests the parent row on each child" $ \TestEnv {envPool = pool} -> do
      ada <- AuthorFixtures.insertAuthor pool "Ada"
      grace <- AuthorFixtures.insertAuthor pool "Grace"
      notes <- AuthorFixtures.insertPost pool ada.id "Notes" Draft
      compiler <- AuthorFixtures.insertPost pool grace.id "Compiler" Published
      results <-
        runDb
          pool
          (Post.findMany (Post.emptyQuery {Post.include_ = PostInclude {author = load}}))
      let adaPost = head [r | r <- results, r.post.id == notes.id]
          gracePost = head [r | r <- results, r.post.id == compiler.id]
      adaPost.author.name `shouldBe` "Ada"
      adaPost.author.id `shouldBe` ada.id
      gracePost.author.name `shouldBe` "Grace"
      gracePost.author.id `shouldBe` grace.id

    it "leaves a skipped parent unread" $ \TestEnv {envPool = pool} -> do
      ada <- AuthorFixtures.insertAuthor pool "Ada"
      _ <- AuthorFixtures.insertPost pool ada.id "Notes" Draft
      results <-
        runDb
          pool
          (Post.findMany (Post.emptyQuery {Post.include_ = PostInclude {author = skip}}))
      map ((.title) . (.post)) results `shouldBe` ["Notes"]
