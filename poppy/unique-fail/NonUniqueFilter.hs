module NonUniqueFilter where

import Poppy.Where (eq)
import qualified Schema.Client.Widget as Widget
import Schema.Widget (widgetName)

bad =
  Widget.findUnique
    Widget.emptyQuery {Widget.where_ = Just (eq widgetName "sage")}
