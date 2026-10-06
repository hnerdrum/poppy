module Poppy.CommentSpec
  ( commentSpec,
  )
where

import Poppy (NullableValue (..), load, loadWith, runDb)
import qualified Poppy.Insert as Insert
import Poppy.Where (isNull)
import qualified Schema.Client.Comment as Comment
import Schema.Comment (CommentCreate (..), CommentRow (..), CommentTable, commentParentId)
import Schema.Include.Comment (CommentInclude (..), CommentWith (..))
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

commentSpec :: SpecWith TestEnv
commentSpec =
  describe "self-relation includes" $
    it "loads replies two levels deep" $ \TestEnv {envPool = pool} -> do
      root <- insert pool "root" Null
      child <- insert pool "child" (Value root.id)
      _ <- insert pool "grandchild" (Value child.id)
      [loaded] <-
        runDb
          pool
          ( Comment.findMany
              Comment.emptyQuery
                { Comment.include_ =
                    CommentInclude {replies = loadWith (CommentInclude {replies = load})},
                  Comment.where_ = Just (isNull commentParentId)
                }
          )
      [reply] <- pure loaded.replies
      reply.comment.body `shouldBe` "child"
      map (.body) reply.replies `shouldBe` ["grandchild"]
  where
    insert pool body parentId =
      runDb
        pool
        ( Insert.insert
            @CommentTable
            @CommentRow
            (CommentCreate {id = Nothing, parentId, body})
        )
        >>= assertRight
