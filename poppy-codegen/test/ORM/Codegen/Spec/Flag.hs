{-# LANGUAGE OverloadedStrings #-}

module ORM.Codegen.Spec.Flag
  ( flagModel,
    flagSchema,
  )
where

import ORM.Codegen.Schema

flagSchema :: Schema
flagSchema =
  schema [] [flagModel] []

flagModel :: Model
flagModel =
  model
    "Flag"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      bool "active"
    ]
    []
