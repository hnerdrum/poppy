{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.CLI
  ( generate,
    mainWith,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Poppy.Codegen.Drift (checkSchema, formatDriftError)
import Poppy.Codegen.IR (Schema (..))
import Poppy.Codegen.Introspect (introspectCatalog)
import Poppy.Codegen.Run (GenOutput (..), allOutputs, checkOutputs, schemasForTargets, writeOutputs)
import Poppy.Codegen.Target
  ( CodegenTarget,
    simpleTarget,
    targetSchemas,
  )
import Poppy.Codegen.Validate (ValidationError (..), validateSchema)
import Poppy.Db (closePool, connect, withConn)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)

-- | Write table types and a Client under @dir@.
generate :: FilePath -> Schema -> IO ()
generate dir schema = mainWith [simpleTarget dir schema]

-- | Flags: none (write files), @--check@, @--check-schema@, @--list@.
mainWith :: [CodegenTarget] -> IO ()
mainWith targets = do
  root <- getCurrentDirectory
  args <- getArgs
  case args of
    ("--check" : _) -> runCheck root targets
    ("--check-schema" : _) -> runCheckSchema targets
    ("--list" : _) -> runList targets
    _ -> runWrite root targets

runWrite :: FilePath -> [CodegenTarget] -> IO ()
runWrite root targets = do
  validateOrExit (schemasForTargets targets)
  writeOutputs root targets
  TIO.putStrLn $ "Wrote " <> pack (show (length (allOutputs targets))) <> " generated files."

runCheck :: FilePath -> [CodegenTarget] -> IO ()
runCheck root targets = do
  validateOrExit (schemasForTargets targets)
  stale <- checkOutputs root targets
  case stale of
    [] -> exitSuccess
    paths -> do
      TIO.putStrLn "Generated files are out of date. Re-run codegen without --check."
      mapM_ (TIO.putStrLn . ("  " <>)) (pack <$> paths)
      exitFailure

runCheckSchema :: [CodegenTarget] -> IO ()
runCheckSchema targets = do
  let schemas = targetSchemas targets
  validateOrExit schemas
  url <- schemaDatabaseUrl
  pool <- connect url
  catalog <- withConn pool introspectCatalog
  closePool pool
  case concatMap (`checkSchema` catalog) schemas of
    [] -> exitSuccess
    errs -> do
      TIO.putStrLn "IR does not match the database. Haskell IR is the source of truth."
      mapM_ (TIO.putStrLn . ("  " <>) . formatDriftError) errs
      exitFailure

schemaDatabaseUrl :: IO String
schemaDatabaseUrl = do
  mTest <- lookupEnv "TEST_DATABASE_URL"
  mDb <- lookupEnv "DATABASE_URL"
  case mTest <|> mDb of
    Just url -> pure url
    Nothing -> do
      TIO.putStrLn "Set TEST_DATABASE_URL or DATABASE_URL to check IR against Postgres."
      exitFailure

runList :: [CodegenTarget] -> IO ()
runList targets = mapM_ (TIO.putStrLn . pack . outputPath) (allOutputs targets)

validateOrExit :: [Schema] -> IO ()
validateOrExit schemas =
  case concatMap validateSchema schemas of
    [] -> pure ()
    errs -> do
      TIO.putStrLn "Schema validation failed:"
      mapM_ (TIO.putStrLn . ("  " <>) . formatValidationError) errs
      exitFailure

formatValidationError :: ValidationError -> Text
formatValidationError err = pack (show err)
