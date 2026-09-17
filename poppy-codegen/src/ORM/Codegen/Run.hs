module ORM.Codegen.Run
  ( GenOutput (..),
    allOutputs,
    checkOutputs,
    writeOutputs,
    schemasForTargets,
  )
where

import Data.Text (Text, strip)
import qualified Data.Text.IO as TIO
import ORM.Codegen.IR (Schema (..))
import ORM.Codegen.Target
  ( CodegenTarget (..),
    GenOutput (..),
    targetOutputs,
    targetSchemas,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

schemasForTargets :: [CodegenTarget] -> [Schema]
schemasForTargets = targetSchemas

allOutputs :: [CodegenTarget] -> [GenOutput]
allOutputs = concatMap targetOutputs

writeOutputs :: FilePath -> [CodegenTarget] -> IO ()
writeOutputs root targets = mapM_ (writeOutput root) (allOutputs targets)

checkOutputs :: FilePath -> [CodegenTarget] -> IO [FilePath]
checkOutputs root targets = filterMismatches root (allOutputs targets)

writeOutput :: FilePath -> GenOutput -> IO ()
writeOutput root GenOutput {outputPath = path, outputText = text} = do
  let fullPath = root </> path
  createDirectoryIfMissing True (takeDirectory fullPath)
  TIO.writeFile fullPath text

filterMismatches :: FilePath -> [GenOutput] -> IO [FilePath]
filterMismatches root = foldr go (pure [])
  where
    go GenOutput {outputPath = path, outputText = expected} acc = do
      mismatches <- acc
      let fullPath = root </> path
      exists <- doesFileExist fullPath
      if not exists
        then pure (path : mismatches)
        else do
          actual <- TIO.readFile fullPath
          pure (if stripText actual == stripText expected then mismatches else path : mismatches)

stripText :: Text -> Text
stripText = strip
