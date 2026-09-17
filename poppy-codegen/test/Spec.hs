module Main (main) where

import ORM.Codegen.DriftSpec (driftDbSpec, driftSpec)
import ORM.Codegen.EmitClientSpec (emitClientSpec)
import ORM.Codegen.EmitFlatRowSpec (emitFlatRowSpec)
import ORM.Codegen.EmitIncludeSpec (emitIncludeSpec)
import ORM.Codegen.EmitSpec (emitSpec)
import ORM.Codegen.IncludePathSpec (fourLevelPathSpec, includePathSpec)
import ORM.Codegen.JoinAliasSpec (joinAliasSpec)
import ORM.Codegen.SchemaSpec (schemaSpec)
import ORM.Codegen.TargetSpec (targetSpec)
import ORM.Codegen.ValidateSpec (validateSpec)
import Support.TestDb (withTestDb)
import Test.Hspec

main :: IO ()
main = hspec $ do
  schemaSpec
  validateSpec
  joinAliasSpec
  includePathSpec
  fourLevelPathSpec
  emitSpec
  emitFlatRowSpec
  emitIncludeSpec
  emitClientSpec
  targetSpec
  driftSpec
  withTestDb driftDbSpec
