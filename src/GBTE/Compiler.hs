{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GBTE.Compiler where

import              Prelude (FilePath, IO)
import              Cardano.Api
import              Cardano.Api.Shelley             ( PlutusScript (..), PlutusScriptV2 )
import              Codec.Serialise                 (serialise)
import              Data.Aeson
import qualified    Data.ByteString.Lazy            as LBS
import qualified    Data.ByteString.Short           as SBS
import qualified    Plutus.V1.Ledger.Scripts
import qualified    Plutus.V1.Ledger.Value
import qualified    Plutus.V2.Ledger.Api
import qualified    Plutus.V2.Ledger.Contexts
import qualified    PlutusTx
import              PlutusTx.Prelude


import qualified GBTE.TreasuryValidator as Treasury
import qualified GBTE.EscrowValidator as Escrow
import GBTE.Types

-- If we do not import Ledger, then
-- how to replace Ledger.Validator?

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/escrow-gbte-v2-fe.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName     = "tgimbal"
    , accessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/treasury-gbte-v2-fe.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = "0ffb3de6f0a5cd949052c99234bf9269e464e1c18979c1d6b9e036f7"
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tgimbal"
    , tTreasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }