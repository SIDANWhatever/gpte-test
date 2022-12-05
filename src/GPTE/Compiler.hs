{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GPTE.Compiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified GPTE.EscrowValidator as Escrow
import qualified GPTE.TreasuryValidator as Treasury
import GPTE.Types
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO)

-- If we do not import Ledger, then
-- how to replace Ledger.Validator?

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

escrowParam :: EscrowParam
escrowParam =
  EscrowParam
    { projectTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c",
      projectTokenName = "tGimbal",
      contribTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784",
      treasuryIssuerPolicyId = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    }

writeProjectEscrowScript :: IO (Either (FileError ()) ())
writeProjectEscrowScript = writeValidator "output/escrow-gpte-v2-with-project-hash2.plutus" $ Escrow.validator escrowParam

writeProjectTreasuryScript :: IO (Either (FileError ()) ())
writeProjectTreasuryScript =
  writeValidator "output/treasury-gpte-v2-with-project-hash2.plutus" $
    Treasury.validator $
      TreasuryParam
        { tContribTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784",
          escrowContractHash = Escrow.escrowValidatorHash escrowParam,
          tProjectTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c",
          tProjectTokenName = "tGimbal",
          tIssuerPolicyId = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
        }
