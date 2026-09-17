{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.ShelfDeep
  ( shelfDeepSchema,
  )
where

import Poppy.Codegen.IR (Schema)
import Poppy.Codegen.Spec.Shelf (shelfSchema)

-- Same models as Shelf; includes are derived from relations only.
shelfDeepSchema :: Schema
shelfDeepSchema = shelfSchema
