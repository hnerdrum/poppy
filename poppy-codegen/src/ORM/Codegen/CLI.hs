{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.CLI
  ( mainWith,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import ORM.Codegen.Drift (checkSchema, formatDriftError)
import ORM.Codegen.IR (Schema (..))
import ORM.Codegen.Introspect (introspectCatalog)
import ORM.Codegen.Run (GenOutput (..), allOutputs, checkOutputs, schemasForTargets, writeOutputs)
import ORM.Codegen.Target (CodegenTarget, targetSchemas)
import ORM.Codegen.Validate (ValidationError (..), validateSchema)
import ORM.Db (closePool, connect, withConn)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)

-- | @targets@ are generated and drift-checked with @--check@.
--   @schemaTargets@ are the Schemas compared to live Postgres by @--check-schema@.
mainWith :: [CodegenTarget] -> [CodegenTarget] -> IO ()
mainWith targets schemaTargets = do
  root <- getCurrentDirectory
  args <- getArgs
  case args of
    ("--check" : _) -> runCheck root targets
    ("--check-schema" : _) -> runCheckSchema schemaTargets
    ("--check-migrations" : _) -> runCheckSchema schemaTargets
    ("check-migrations" : _) -> runCheckSchema schemaTargets
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
      TIO.putStrLn "Generated ORM files are out of date. Re-run codegen without --check."
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
