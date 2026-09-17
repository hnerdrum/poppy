module Main (main) where

import ORM.ErrorsSpec (errorsSpec)
import ORM.GroupSpec (groupSpec)
import ORM.IncludeSpec (includeSpec)
import ORM.JoinSpec (joinSpec)
import ORM.NestedWriteSpec (nestedWriteSpec)
import ORM.OperationsSpec (operationsSpec)
import ORM.RawSpec (rawSpec)
import ORM.SelectSpec (selectSpec)
import ORM.WhereSpec (whereSpec)
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
