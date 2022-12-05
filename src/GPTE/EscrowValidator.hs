{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- PlutusV2
module GPTE.EscrowValidator (validator, escrowValidatorHash) where

import GPTE.Types
import qualified Ledger (contains)
import qualified Ledger.Ada as Ada
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, mkUntypedValidator, validatorHash, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINEABLE escrowValidator #-}
escrowValidator :: EscrowParam -> CommitmentEscrowDatum -> ProjectAction -> ScriptContext -> Bool
escrowValidator ep dat action ctx =
  case action of
    Cancel ->
      traceIfFalse "Only Issuer can Cancel Commitment UTxO" inputHasIssuerToken
        && traceIfFalse "Can only cancel Commitment after deadline" deadlineReached
    Update ->
      traceIfFalse "Only Issuer can Update Commitment UTxO" inputHasIssuerToken
        && traceIfFalse "Output UTXO value must be geq datum specs" outputFulfillsValue
        && traceIfFalse "only ada and gimbal amount can be changed" checkNewDatum
    Distribute ->
      traceIfFalse "Issuer must sign to distribute Commitment UTxO" inputHasIssuerToken
        && traceIfFalse "Contributor must receive full escrow values" outputFulfillsCommitment
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    bCursym :: CurrencySymbol
    bCursym = projectTokenPolicyId ep

    bTokenN :: TokenName
    bTokenN = projectTokenName ep

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that input has Issuer Token
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = treasuryIssuerPolicyId ep `elem` inVals

    deadlineReached :: Bool
    deadlineReached = Ledger.contains (from $ bedExpirationTime dat) $ txInfoValidRange info

    valueToContributor :: Value
    valueToContributor = valuePaidTo info $ bedContributorPkh dat

    -- contributor must get tokenAmount ep of gimbals and lovelaceAmount ep...
    outputFulfillsCommitment :: Bool
    outputFulfillsCommitment =
      valueOf valueToContributor bCursym bTokenN >= bedTokenAmount dat
        && Ada.getLovelace (Ada.fromValue valueToContributor) >= bedLovelaceAmount dat

    ownInputVal :: Value
    ownInputVal = case findOwnInput ctx of
      Just iv -> txOutValue $ txInInfoResolved iv
      Nothing -> error ()

    -- new
    -- now check for correct value in new datum
    outputFulfillsValue :: Bool
    outputFulfillsValue =
      valueOf (txOutValue getOutputToContract) bCursym bTokenN == bedTokenAmount getNewEscrowDatum
        && Ada.getLovelace (Ada.fromValue $ txOutValue getOutputToContract) == bedLovelaceAmount getNewEscrowDatum

    -- Update means that exactly one UTXO must be left at contract address
    getOutputToContract :: TxOut
    getOutputToContract = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "exactly one output expected"

    -- new datum should be inline and type CommitmentEscrowDatum
    getNewEscrowDatum :: CommitmentEscrowDatum
    getNewEscrowDatum = case txOutDatum getOutputToContract of
      OutputDatum ns -> case fromBuiltinData (getDatum ns) of
        Just bd -> bd
        Nothing -> traceError "datum has wrong type"
      _ -> traceError "not an inline datum"

    -- ada/gimbals/deadline greater or equal
    checkNewDatum :: Bool
    checkNewDatum =
      let bd = getNewEscrowDatum
       in bedProjectHash bd == bedProjectHash dat
            && bedContributorPkh bd == bedContributorPkh dat
            && bedLovelaceAmount bd >= bedLovelaceAmount dat
            && bedTokenAmount bd >= bedTokenAmount dat
            && bedExpirationTime bd >= bedExpirationTime dat

typedValidator :: EscrowParam -> TypedValidator EscrowTypes
typedValidator ep = go ep
  where
    go =
      mkTypedValidatorParam @EscrowTypes
        $$(PlutusTx.compile [||escrowValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: EscrowParam -> Validator
validator = validatorScript . typedValidator

escrowValidatorHash :: EscrowParam -> ValidatorHash
escrowValidatorHash = validatorHash . typedValidator
