{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Poppy.ScalarSpec
  ( scalarSpec,
  )
where

import Data.Aeson (object, (.=))
import Data.Scientific (scientific)
import Data.Text (Text)
import Poppy (runDb)
import qualified Poppy.Internal.Insert as Insert
import qualified Poppy.Internal.Operations as Ops
import Schema.Packet
import Support.Assert (assertRight)
import Support.TestDb (TestEnv (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

scalarSpec :: SpecWith TestEnv
scalarSpec =
  describe "numeric and jsonb scalars" $ do
    it "round-trips Scientific and aeson Value through insert and select" $ \TestEnv {envPool = pool} -> do
      let amount = scientific 1999 (-2)
          payload = object ["kind" .= ("box" :: Text), "n" .= (2 :: Int)]
      inserted <-
        runDb
          pool
          ( Insert.insert @PacketTable @PacketRow
              PacketCreate
                { id = Nothing,
                  amount,
                  payload
                }
          )
          >>= assertRight
      inserted.amount `shouldBe` amount
      inserted.payload `shouldBe` payload
      found <- runDb pool (Ops.findUnique @PacketTable @PacketRow inserted.id)
      found `shouldBe` Just inserted
