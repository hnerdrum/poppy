module Main (main) where

import Poppy.BelongsToSpec (belongsToSpec)
import Poppy.ClientWriteSpec (clientWriteSpec)
import Poppy.DbSpec (dbSpec)
import Poppy.EnumSpec (enumSpec)
import Poppy.ErrorsSpec (errorsSpec)
import Poppy.GroupSpec (groupSpec)
import Poppy.IncludeSpec (includeSpec)
import Poppy.MigrateSpec (migrateSpec)
import Poppy.NestedWriteSpec (nestedWriteSpec)
import Poppy.OperationsSpec (operationsSpec)
import Poppy.RawSpec (rawSpec)
import Poppy.ScalarSpec (scalarSpec)
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
      >> includeSpec
      >> belongsToSpec
      >> enumSpec
      >> rawSpec
      >> selectSpec
      >> nestedWriteSpec
      >> dbSpec
      >> migrateSpec
      >> clientWriteSpec
      >> scalarSpec
