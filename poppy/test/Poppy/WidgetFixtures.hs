module Poppy.WidgetFixtures
  ( insertWidget,
  )
where

import Data.Text (Text)
import Poppy (NullableValue (Omit), runDb)
import Poppy.Db (DbPool)
import qualified Poppy.Insert as Insert
import Schema.Widget
import Support.Assert (assertRight)

insertWidget :: DbPool -> Text -> IO WidgetRow
insertWidget pool name =
  runDb
    pool
    ( Insert.insert @WidgetTable @WidgetRow
        WidgetCreate
          { id = Nothing,
            createdAt = Nothing,
            updatedAt = Nothing,
            name,
            description = Omit
          }
    )
    >>= assertRight
