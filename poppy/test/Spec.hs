module Main (main) where

import Poppy.ErrorsSpec (errorsSpec)
import Poppy.GroupSpec (groupSpec)
import Poppy.IncludeSpec (includeSpec)
import Poppy.JoinSpec (joinSpec)
import Poppy.NestedWriteSpec (nestedWriteSpec)
import Poppy.OperationsSpec (operationsSpec)
import Poppy.RawSpec (rawSpec)
import Poppy.SelectSpec (selectSpec)
import Poppy.WhereSpec (whereSpec)
import Support.TestDb (withTestDb)
import Test.Hspec

main :: IO ()
main = hspec $ do
  errorsSpec
  groupSpec
  whereSpec
  withTestDb $
    operationsSpec
      >> joinSpec
      >> includeSpec
      >> rawSpec
      >> selectSpec
      >> nestedWriteSpec
