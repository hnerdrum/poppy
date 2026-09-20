{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Target
  ( simpleTarget,
    CodegenTarget,
    GenOutput (..),
    targetOutputs,
    targetSchemas,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.List (nub)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Poppy.Codegen.Emit.Client
  ( emitIncludeReadClientModule,
    emitSimpleClientModule,
    emitWriteClientModule,
    nestedWriteRelation,
  )
import Poppy.Codegen.Emit.Include (emitIncludeModule)
import Poppy.Codegen.Emit.Schema (emitEnumModule, emitModelModule)
import Poppy.Codegen.IR
  ( EnumSpec (..),
    Model,
    ModelInclude (..),
    Schema (..),
    enumImport,
    enumName,
    includeName,
    includeRootModel,
    modelName,
    schemaEnums,
    schemaIncludes,
    schemaModels,
  )
import Poppy.Codegen.Lookup (lookupModel)
import Poppy.Codegen.Schema (isFullGraphInclude)
import System.FilePath (dropTrailingPathSeparator, splitDirectories, (</>))

-- | Path is relative to the process working directory.
data GenOutput = GenOutput
  { outputPath :: FilePath,
    outputText :: Text
  }

data SchemaLayout = SchemaLayout
  { slModulePrefix :: Text,
    slOutputDir :: FilePath
  }

data ClientLayout = ClientLayout
  { clModulePrefix :: Text,
    clOutputDir :: FilePath
  }

data CodegenTarget = CodegenTarget
  { ctSchemas :: [Schema],
    ctLayout :: SchemaLayout,
    ctClientLayout :: ClientLayout
  }

-- | Table types at @Prefix.Model@, clients at @Prefix.Client.Model@.
--
-- @Prefix@ is the path with non-module segments dropped (@src/Schema@ →
-- @Schema@).
simpleTarget :: FilePath -> Schema -> CodegenTarget
simpleTarget dir schema =
  let prefix = prefixFromDir dir
   in CodegenTarget
        { ctSchemas = [schema],
          ctLayout = SchemaLayout (prefix <> ".") dir,
          ctClientLayout =
            ClientLayout
              { clModulePrefix = prefix <> ".Client.",
                clOutputDir = dir </> "Client"
              }
        }

prefixFromDir :: FilePath -> Text
prefixFromDir dir =
  case filter isModuleSegment (map T.pack (splitDirectories (dropTrailingPathSeparator dir))) of
    [] ->
      error $
        "simpleTarget: "
          <> dir
          <> " has no Haskell module segment (e.g. src/Schema)"
    parts -> T.intercalate "." parts

isModuleSegment :: Text -> Bool
isModuleSegment name =
  case T.uncons name of
    Just (c, rest) -> isUpper c && T.all isModuleChar rest
    Nothing -> False

isModuleChar :: Char -> Bool
isModuleChar c = isAlphaNum c || c == '_' || c == '\''

targetSchemas :: [CodegenTarget] -> [Schema]
targetSchemas = concatMap (.ctSchemas)

targetOutputs :: CodegenTarget -> [GenOutput]
targetOutputs target =
  concatMap (schemaOutputs target) target.ctSchemas

schemaOutputs :: CodegenTarget -> Schema -> [GenOutput]
schemaOutputs target schema =
  enumOutputs target schema
    ++ modelOutputs target schema
    ++ includeOutputs target schema
    ++ clientOutputs target schema

enumOutputs :: CodegenTarget -> Schema -> [GenOutput]
enumOutputs target schema =
  concat
    [ enumOutput target enum
      | enum <- schemaEnums schema,
        isNothing (enumImport enum)
    ]

enumOutput :: CodegenTarget -> EnumSpec -> [GenOutput]
enumOutput target enum =
  let layout = target.ctLayout
      name = enumName enum
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
   in [gen path (emitEnumModule moduleName enum)]

modelOutputs :: CodegenTarget -> Schema -> [GenOutput]
modelOutputs target schema =
  concatMap (modelOutput target schema) (schemaModels schema)

modelOutput :: CodegenTarget -> Schema -> Model -> [GenOutput]
modelOutput target schema model =
  let layout = target.ctLayout
      name = modelName model
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
   in [gen path (emitModelModule moduleName schema model)]

includeOutputs :: CodegenTarget -> Schema -> [GenOutput]
includeOutputs target schema =
  concatMap (includeOutput target schema) (schemaIncludes schema)

includeOutput :: CodegenTarget -> Schema -> ModelInclude -> [GenOutput]
includeOutput target schema incl =
  let layout = target.ctLayout
      name = includeName incl
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
   in [gen path (emitIncludeModule moduleName schema incl)]

clientOutputs :: CodegenTarget -> Schema -> [GenOutput]
clientOutputs target schema =
  simpleClientOutputs target.ctClientLayout schema
    ++ includeClientOutputs target.ctClientLayout schema

simpleClientOutputs :: ClientLayout -> Schema -> [GenOutput]
simpleClientOutputs layout schema =
  concatMap (simpleClientOutput layout) (simpleClientModels schema)

simpleClientOutput :: ClientLayout -> Model -> [GenOutput]
simpleClientOutput layout model =
  let name = modelName model
      moduleName = layout.clModulePrefix <> name
      path = layout.clOutputDir </> T.unpack name <> ".hs"
      text = emitSimpleClientModule moduleName model
   in [gen path text]

includeClientOutputs :: ClientLayout -> Schema -> [GenOutput]
includeClientOutputs layout schema =
  concatMap (includeClientOutput layout schema) (schemaIncludes schema)

includeClientOutput :: ClientLayout -> Schema -> ModelInclude -> [GenOutput]
includeClientOutput layout schema incl =
  let rootName = includeRootModel incl
      root = lookupModel schema rootName
      moduleName = layout.clModulePrefix <> rootName
      path = layout.clOutputDir </> T.unpack rootName <> ".hs"
   in case includeClientKind schema root incl of
        WriteIncludeClient ->
          [gen path (emitWriteClientModule moduleName schema incl)]
        ReadIncludeClient ->
          [gen path (emitIncludeReadClientModule moduleName schema incl)]
        NoIncludeClient ->
          []

-- Nested writes are inferred from hasMany + child FK. Auto full-graph
-- Includes without an owned child keep a simple Client.
-- Explicit partial Includes still get an include-read Client.
data IncludeClientKind = WriteIncludeClient | ReadIncludeClient | NoIncludeClient
  deriving (Eq)

includeClientKind :: Schema -> Model -> ModelInclude -> IncludeClientKind
includeClientKind schema root incl
  | isJust (nestedWriteRelation schema root incl) = WriteIncludeClient
  | isFullGraphInclude (schemaModels schema) incl = NoIncludeClient
  | otherwise = ReadIncludeClient

simpleClientModels :: Schema -> [Model]
simpleClientModels schema =
  let skip = includeClientRootModels schema
   in filter (\model -> modelName model `notElem` skip) (schemaModels schema)

includeClientRootModels :: Schema -> [Text]
includeClientRootModels schema =
  nub
    [ includeRootModel incl
      | incl <- schemaIncludes schema,
        let root = lookupModel schema (includeRootModel incl),
        includeClientKind schema root incl /= NoIncludeClient
    ]

gen :: FilePath -> Text -> GenOutput
gen path text = GenOutput {outputPath = path, outputText = text}
