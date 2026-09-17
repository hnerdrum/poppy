{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.TestTarget
  ( testTarget,
  )
where

import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.Widget (widgetSchema)
import Poppy.Codegen.Target
  ( ClientConfig (..),
    CodegenTarget (..),
    SchemaEmit (..),
    SchemaLayout (..),
    TargetSchema (..),
  )

testTarget :: CodegenTarget
testTarget =
  CodegenTarget
    { ctSchemas =
        [ TargetSchema widgetSchema True EmitAll,
          TargetSchema shelfSchema True EmitAll
        ],
      ctLayout = SchemaLayout "Schema." "test/Schema/",
      ctClients = NoClients
    }
