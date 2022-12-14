{-# LANGUAGE OverloadedStrings #-}

module SidanAPI where

import Prelude (IO, ($))
import qualified SIDANPlutusServer      as SIDAN
import qualified GPTE.EscrowValidator   as Escrow
import qualified GPTE.TreasuryValidator as Treasury

main :: IO ()
main = SIDAN.createServer app

app :: SIDAN.Api
app = do
  SIDAN.createEndpoint "gpte-escrow" $ SIDAN.mkV2Validator Escrow.validator
  SIDAN.createEndpoint "gpte-treasury" $ SIDAN.mkV2Validator Treasury.validator
