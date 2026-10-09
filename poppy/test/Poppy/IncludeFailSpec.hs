module Poppy.IncludeFailSpec
  ( includeFailSpec,
  )
where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

includeFailSpec :: Spec
includeFailSpec =
  describe "include type errors" $ do
    it "says a skipped field is NotIncluded rather than the row list" $ do
      err <- expectFail "include-fail/SkippedChapters.hs"
      err `shouldContain` "NotIncluded"
      err `shouldContain` "[ChapterRow]"
      hidden <- expectFail "include-fail/SkippedChaptersNoSelectors.hs"
      hidden `shouldContain` "NotIncluded"
      hidden `shouldContain` "[ChapterRow]"

    it "rejects a shelf include nested under books" $ do
      err <- expectFail "include-fail/WrongChild.hs"
      err `shouldContain` "IncludeFor"
      err `shouldContain` "\"Book\""
      err `shouldContain` "ShelfInclude"

    it "rejects a record update when another record in scope shares the field" $ do
      err <- expectFail "include-fail/RecordUpdateAmbiguous.hs"
      err `shouldContain` "mbiguous"

expectFail :: FilePath -> IO String
expectFail path = do
  (code, _, err) <-
    readProcessWithExitCode
      "cabal"
      ( ["exec", "--", "ghc", "-package", "poppy", "-fno-code", "-w", "-itest", "-outputdir", "/tmp/poppy-include-fail"]
          ++ extensions
          ++ [path]
      )
      ""
  case code of
    ExitSuccess -> expectationFailure ("expected " <> path <> " to fail") >> pure err
    _ -> pure err

extensions :: [String]
extensions =
  [ "-XAllowAmbiguousTypes",
    "-XDuplicateRecordFields",
    "-XLambdaCase",
    "-XNamedFieldPuns",
    "-XOverloadedRecordDot",
    "-XOverloadedStrings",
    "-XScopedTypeVariables",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]
