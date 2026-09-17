{-# LANGUAGE OverloadedStrings #-}

module TaskTarget
  ( taskTarget,
  )
where

import Poppy.Codegen.Target
  ( ClientConfig (..),
    ClientLayout (..),
    CodegenTarget (..),
    SchemaEmit (..),
    SchemaLayout (..),
    TargetSchema (..),
  )
import TaskSchema (taskSchema)

taskTarget :: CodegenTarget
taskTarget =
  CodegenTarget
    { ctSchemas = [TargetSchema taskSchema False EmitAll],
      ctLayout = SchemaLayout "Schema." "src/Schema/",
      ctClients =
        DeriveClients
          ClientLayout
            { clModulePrefix = "Schema.Client.",
              clOutputDir = "src/Schema/Client/",
              clGolden = False
            }
    }
