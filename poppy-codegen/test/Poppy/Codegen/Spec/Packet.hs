{-# LANGUAGE OverloadedStrings #-}

module Poppy.Codegen.Spec.Packet
  ( packetModel,
    packetSchema,
  )
where

import Poppy.Codegen.Schema

packetSchema :: Schema
packetSchema =
  schema [] [packetModel] []

packetModel :: Model
packetModel =
  model
    "Packet"
    [ uuid "id" & pk & withDefault DefaultUuidV4,
      numeric "amount",
      jsonb "payload"
    ]
    []
    & table "test_packet"
