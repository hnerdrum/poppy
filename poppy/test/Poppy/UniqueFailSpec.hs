module Poppy.UniqueFailSpec
  ( uniqueFailSpec,
  )
where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

uniqueFailSpec :: Spec
uniqueFailSpec =
  describe "unique key type errors" $
    it "rejects a where_ passed to findUnique" $ do
      err <- expectFail "unique-fail/NonUniqueFilter.hs"
      err `shouldContain` "WidgetUniqueQuery"

expectFail :: FilePath -> IO String
expectFail path = do
  (code, _, err) <-
    readProcessWithExitCode
      "cabal"
      ( ["exec", "--", "ghc", "-package", "poppy", "-fno-code", "-w", "-itest", "-outputdir", "/tmp/poppy-unique-fail"]
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
