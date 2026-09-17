{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Target
  ( GenOutput (..),
    CodegenTarget (..),
    TargetSchema (..),
    SchemaLayout (..),
    ClientConfig (..),
    ClientLayout (..),
    SchemaEmit (..),
    targetOutputs,
    targetSchemas,
  )
where

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
import System.FilePath ((</>))

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
    clOutputDir :: FilePath,
    clGolden :: Bool
  }

data ClientConfig
  = NoClients
  | DeriveClients ClientLayout

data TargetSchema = TargetSchema
  { tsSchema :: Schema,
    tsGolden :: Bool,
    tsEmit :: SchemaEmit
  }

data SchemaEmit
  = EmitAll
  | EmitIncludesOnly

data CodegenTarget = CodegenTarget
  { ctSchemas :: [TargetSchema],
    ctLayout :: SchemaLayout,
    ctClients :: ClientConfig
  }

targetSchemas :: [CodegenTarget] -> [Schema]
targetSchemas targets = map (.tsSchema) (concatMap (.ctSchemas) targets)

targetOutputs :: CodegenTarget -> [GenOutput]
targetOutputs target =
  concatMap (schemaOutputs target) target.ctSchemas

schemaOutputs :: CodegenTarget -> TargetSchema -> [GenOutput]
schemaOutputs target TargetSchema {tsSchema = schema, tsGolden = golden, tsEmit = emit} =
  enumOutputs target golden schema emit
    ++ modelOutputs target golden schema emit
    ++ includeOutputs target golden schema emit
    ++ clientOutputs target schema emit

enumOutputs :: CodegenTarget -> Bool -> Schema -> SchemaEmit -> [GenOutput]
enumOutputs _ _ _ EmitIncludesOnly = []
enumOutputs target golden schema EmitAll =
  concat
    [ enumOutput target golden schema enum
      | enum <- schemaEnums schema,
        isNothing (enumImport enum)
    ]

enumOutput :: CodegenTarget -> Bool -> Schema -> EnumSpec -> [GenOutput]
enumOutput target golden _schema enum =
  let layout = target.ctLayout
      name = enumName enum
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
      text = emitEnumModule moduleName enum
   in withGolden golden (T.unpack name <> ".hs") path text

modelOutputs :: CodegenTarget -> Bool -> Schema -> SchemaEmit -> [GenOutput]
modelOutputs _ _ _ EmitIncludesOnly = []
modelOutputs target golden schema EmitAll =
  concatMap (modelOutput target golden schema) (schemaModels schema)

modelOutput :: CodegenTarget -> Bool -> Schema -> Model -> [GenOutput]
modelOutput target golden schema model =
  let layout = target.ctLayout
      name = modelName model
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
      text = emitModelModule moduleName schema model
   in withGolden golden (T.unpack name <> ".hs") path text

includeOutputs :: CodegenTarget -> Bool -> Schema -> SchemaEmit -> [GenOutput]
includeOutputs target golden schema _ =
  concatMap (includeOutput target golden schema) (schemaIncludes schema)

includeOutput :: CodegenTarget -> Bool -> Schema -> ModelInclude -> [GenOutput]
includeOutput target golden schema incl =
  let layout = target.ctLayout
      name = includeName incl
      moduleName = layout.slModulePrefix <> name
      path = layout.slOutputDir </> T.unpack name <> ".hs"
      text = emitIncludeModule moduleName schema incl
   in withGolden golden (T.unpack name <> ".hs") path text

clientOutputs :: CodegenTarget -> Schema -> SchemaEmit -> [GenOutput]
clientOutputs _ _ EmitIncludesOnly = []
clientOutputs target schema EmitAll =
  case target.ctClients of
    NoClients -> []
    DeriveClients layout ->
      simpleClientOutputs layout schema
        ++ includeClientOutputs layout schema

simpleClientOutputs :: ClientLayout -> Schema -> [GenOutput]
simpleClientOutputs layout schema =
  concatMap (simpleClientOutput layout) (simpleClientModels schema)

simpleClientOutput :: ClientLayout -> Model -> [GenOutput]
simpleClientOutput layout model =
  let name = modelName model
      moduleName = layout.clModulePrefix <> name
      path = layout.clOutputDir </> T.unpack name <> ".hs"
      text = emitSimpleClientModule moduleName model
   in withGolden layout.clGolden (T.unpack name <> "Client.hs") path text

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
          withGolden layout.clGolden (T.unpack rootName <> "Client.hs") path (emitWriteClientModule moduleName schema incl)
        ReadIncludeClient ->
          withGolden layout.clGolden (T.unpack rootName <> "Client.hs") path (emitIncludeReadClientModule moduleName schema incl)
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

withGolden :: Bool -> FilePath -> FilePath -> Text -> [GenOutput]
withGolden golden goldenFile path text =
  gen path text
    : ([genGolden goldenFile text | golden])

gen :: FilePath -> Text -> GenOutput
gen path text = GenOutput {outputPath = path, outputText = text}

genGolden :: FilePath -> Text -> GenOutput
genGolden file text =
  GenOutput
    { outputPath = "test/Poppy/Codegen/golden/" <> file <> ".golden",
      outputText = text
    }
