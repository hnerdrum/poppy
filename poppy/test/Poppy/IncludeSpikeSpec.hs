module Poppy.IncludeSpikeSpec
  ( includeSpikeSpec,
  )
where

import IncludeSpike.Examples (renderedPosts)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

includeSpikeSpec :: Spec
includeSpikeSpec =
  describe "precise include results" $ do
    it "accepts two include shapes that both loaded posts" $
      renderedPosts `shouldBe` []

    it "says a skipped field is NotIncluded rather than the row list" $ do
      err <- expectFail "include-spike-fail/SkippedPosts.hs"
      err `shouldContain` "NotIncluded"
      err `shouldContain` "[PostRow]"
      hidden <- expectFail "include-spike-fail/SkippedPostsNoSelectors.hs"
      hidden `shouldContain` "NotIncluded"
      hidden `shouldContain` "[PostRow]"

    it "rejects a tag include nested under books" $ do
      err <- expectFail "include-spike-fail/WrongChild.hs"
      err `shouldContain` "IncludeFor"
      err `shouldContain` "\"Book\""
      err `shouldContain` "TagInclude"

    it "updates an include from another module when the field name is unique" $
      expectOk "include-spike-fail/RecordUpdateOther.hs"

    it "updates an include in its defining module when the field name is unique" $
      expectOk "include-spike-fail/RecordUpdateSame.hs"

    it "rejects a record update when another record in scope shares the field" $ do
      err <- expectFail "include-spike-fail/RecordUpdateAmbiguous.hs"
      err `shouldContain` "mbiguous"

expectOk :: FilePath -> IO ()
expectOk path = do
  (code, _, err) <- compile path
  case code of
    ExitSuccess -> pure ()
    _ -> expectationFailure err

expectFail :: FilePath -> IO String
expectFail path = do
  (code, _, err) <- compile path
  case code of
    ExitSuccess -> expectationFailure ("expected " <> path <> " to fail") >> pure err
    _ -> pure err

compile :: FilePath -> IO (ExitCode, String, String)
compile path =
  readProcessWithExitCode
    "ghc"
    ["-fno-code", "-w", "-itest", "-outputdir", "/tmp/poppy-include-spike", path]
    ""
