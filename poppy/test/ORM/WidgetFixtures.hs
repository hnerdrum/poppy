module ORM.WidgetFixtures
  ( insertWidget,
  )
where

import Data.Text (Text)
import ORM (NullableValue (Omit), runDb)
import ORM.Db (DbPool)
import qualified ORM.Insert as Insert
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
