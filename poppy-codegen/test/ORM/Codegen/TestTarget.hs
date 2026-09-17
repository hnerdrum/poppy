{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.TestTarget
  ( testTarget,
  )
where

import ORM.Codegen.Spec.Shelf (shelfSchema)
import ORM.Codegen.Spec.Widget (widgetSchema)
import ORM.Codegen.Target
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
