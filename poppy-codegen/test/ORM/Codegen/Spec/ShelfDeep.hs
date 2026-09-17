{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Spec.ShelfDeep
  ( shelfDeepSchema,
  )
where

import ORM.Codegen.IR (Schema)
import ORM.Codegen.Spec.Shelf (shelfSchema)

-- Same models as Shelf; includes are derived from relations only.
shelfDeepSchema :: Schema
shelfDeepSchema = shelfSchema
