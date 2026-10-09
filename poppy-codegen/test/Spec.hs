module Main (main) where

import Poppy.Codegen.DriftSpec (driftDbSpec, driftSpec)
import Poppy.Codegen.EmitClientSpec (emitClientSpec)
import Poppy.Codegen.EmitIncludeSpec (emitIncludeSpec)
import Poppy.Codegen.EmitSpec (emitSpec)
import Poppy.Codegen.SchemaSpec (schemaSpec)
import Poppy.Codegen.TargetSpec (targetSpec)
import Poppy.Codegen.ValidateSpec (validateSpec)
import Support.TestDb (withTestDb)
import Test.Hspec

main :: IO ()
main = hspec $ do
  schemaSpec
  validateSpec
  emitSpec
  emitIncludeSpec
  emitClientSpec
  targetSpec
  driftSpec
  withTestDb driftDbSpec
