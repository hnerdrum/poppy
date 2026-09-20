{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.TestTarget
  ( testTargets,
  )
where

import Poppy.Codegen.Spec.Shelf (shelfSchema)
import Poppy.Codegen.Spec.Widget (widgetSchema)
import Poppy.Codegen.Target (CodegenTarget, simpleTarget)

testTargets :: [CodegenTarget]
testTargets =
  [ simpleTarget "test/Schema" widgetSchema,
    simpleTarget "test/Schema" shelfSchema
  ]
