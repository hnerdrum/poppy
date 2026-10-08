{-# LANGUAGE OverloadedStrings #-}

-- | Codegen CLI: write files, @--check@ freshness, @--check-schema@ drift, @--list@ paths.
module Poppy.Codegen.CLI
  ( generate,
    mainWith,
    simpleTarget,
    CodegenTarget,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Poppy.Codegen.Drift (checkSchema, formatDriftError)
import Poppy.Codegen.IR (Schema)
import Poppy.Codegen.Introspect (introspectCatalog)
import Poppy.Codegen.Run (GenOutput (..), allOutputs, checkOutputs, schemasForTargets, writeOutputs)
import Poppy.Codegen.Target
  ( CodegenTarget,
    simpleTarget,
    targetSchemas,
  )
import Poppy.Codegen.Validate (ValidationError (..), validateSchema)
import Poppy.Internal.Db (closePool, connect, withConn)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)

-- | Write table types and a Client under @dir@ (@src/Schema@ → module prefix @Schema@).
--
-- Flags on the process argv: none (write), @--check@, @--check-schema@, @--list@.
-- @--check-schema@ reads @DATABASE_URL@, or the URL passed as @--database-url@.
generate :: FilePath -> Schema -> IO ()
generate dir schema = mainWith [simpleTarget dir schema]

-- | Flags: none (write files), @--check@, @--check-schema@, @--list@.
--
-- @--check-schema@ may be followed by @--database-url URL@, which overrides
-- @DATABASE_URL@. The two arguments can appear in either order.
mainWith :: [CodegenTarget] -> IO ()
mainWith targets = do
  root <- getCurrentDirectory
  args <- getArgs
  case args of
    ("--check" : _) -> runCheck root targets
    ("--list" : _) -> runList targets
    _ | "--check-schema" `elem` args -> do
      urlFlag <- checkSchemaDatabaseUrl args
      runCheckSchema urlFlag targets
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

runCheckSchema :: Maybe String -> [CodegenTarget] -> IO ()
runCheckSchema urlFlag targets = do
  let schemas = targetSchemas targets
  validateOrExit schemas
  url <- schemaDatabaseUrl urlFlag
  pool <- connect url
  catalog <- withConn pool introspectCatalog
  closePool pool
  case concatMap (`checkSchema` catalog) schemas of
    [] -> exitSuccess
    errs -> do
      TIO.putStrLn "IR does not match the database. Haskell IR is the source of truth."
      mapM_ (TIO.putStrLn . ("  " <>) . formatDriftError) errs
      exitFailure

-- | @--database-url@ overrides @DATABASE_URL@. Other arguments besides
-- @--check-schema@ are rejected.
checkSchemaDatabaseUrl :: [String] -> IO (Maybe String)
checkSchemaDatabaseUrl = go Nothing
  where
    go found [] = pure found
    go found ("--check-schema" : rest) = go found rest
    go found ("--database-url" : url : rest)
      | isOption url = missingUrl
      | otherwise =
          case found of
            Just _ -> do
              TIO.putStrLn "--database-url was given more than once."
              exitFailure
            Nothing -> go (Just url) rest
    go _ ("--database-url" : _) = missingUrl
    go _ (other : _) = do
      TIO.putStrLn $ "Unknown argument: " <> pack other <> "."
      TIO.putStrLn "Usage: --check-schema [--database-url URL]"
      exitFailure
    missingUrl = do
      TIO.putStrLn "--database-url requires a URL."
      exitFailure
    isOption ('-' : '-' : _) = True
    isOption _ = False

schemaDatabaseUrl :: Maybe String -> IO String
schemaDatabaseUrl (Just url) = pure url
schemaDatabaseUrl Nothing = do
  mDb <- lookupEnv "DATABASE_URL"
  case mDb of
    Just url -> pure url
    Nothing -> do
      TIO.putStrLn "Set DATABASE_URL, or pass --database-url URL, to check the Schema against Postgres."
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
