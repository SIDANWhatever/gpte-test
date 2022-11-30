{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- PlutusV2
module GBTE.EscrowValidator (validator, escrowValidatorHash) where

import qualified    Ledger (contains)
import qualified    Ledger.Ada as Ada
import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator, validatorHash)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              GBTE.Types

{-# INLINEABLE escrowValidator #-}
escrowValidator :: BountyParam -> BountyEscrowDatum -> BountyAction -> ScriptContext -> Bool
escrowValidator bp dat action ctx =
  case action of
    Cancel      ->  traceIfFalse "Only Issuer can Cancel Bounty"                inputHasIssuerToken &&
                    traceIfFalse "Can only cancel bounty after deadline"        deadlineReached
    Update      ->  traceIfFalse "Only Issuer can Update Bounty"                inputHasIssuerToken &&
                    -- not needed anymore
                    -- traceIfFalse "Update must create one new Bounty UTXO"       createsContinuingBounty &&
                    traceIfFalse "Output UTXO value must be geq datum specs"    outputFulfillsValue &&
                    traceIfFalse "only ada and gimbal amount can be changed"    checkNewDatum 
    Distribute  ->  traceIfFalse "Issuer must sign to distribute bounty"        inputHasIssuerToken &&
                    traceIfFalse "Contributor must receive full bounty values"  outputFulfillsBounty
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    bCursym :: CurrencySymbol
    bCursym = bountyTokenPolicyId bp

    bTokenN :: TokenName
    bTokenN = bountyTokenName bp

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that input has Issuer Token
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = treasuryIssuerPolicyId bp `elem` inVals

    deadlineReached :: Bool
    deadlineReached = Ledger.contains (from $ bedExpirationTime dat) $ txInfoValidRange info

    valueToContributor :: Value
    valueToContributor = valuePaidTo info $ bedContributorPkh dat

    -- contributor must get tokenAmount bp of gimbals and lovelaceAmount bp...
    outputFulfillsBounty :: Bool
    outputFulfillsBounty = valueOf valueToContributor bCursym bTokenN >= bedTokenAmount dat &&
                           Ada.getLovelace (Ada.fromValue valueToContributor) >= bedLovelaceAmount dat

    ownInputVal :: Value
    ownInputVal = case findOwnInput ctx of
                Just iv -> txOutValue $ txInInfoResolved iv
                Nothing -> error ()
    
    {--not needed anymore 
    createsContinuingBounty :: Bool
    createsContinuingBounty = length outputsToContract == 1
    --}
        {-- old
    -- The value sent to Contributor must be at least the amount specified by bounty

    outputContainsValue :: [TxOut] -> Bool
    outputContainsValue [x]   = valueOf (txOutValue x) bCursym bTokenN >= bedTokenAmount dat &&
                                Ada.getLovelace (Ada.fromValue $ txOutValue x) >= bedLovelaceAmount dat
    outputContainsValue _     = False

    outputFulfillsValue :: Bool
    outputFulfillsValue = outputContainsValue outputsToContract
    --}
    
    -- new 
    -- now check for correct value in new datum 
    outputFulfillsValue :: Bool 
    outputFulfillsValue = valueOf (txOutValue getOutputToContract) bCursym bTokenN == bedTokenAmount getNewEscrowDatum &&
                          Ada.getLovelace (Ada.fromValue $ txOutValue getOutputToContract) == bedLovelaceAmount getNewEscrowDatum

    -- Update means that exactly one UTXO must be left at contract address
    getOutputToContract :: TxOut  
    getOutputToContract = case getContinuingOutputs ctx of 
                      [o] -> o 
                      _   -> traceError "exactly one output expected" 

    -- new datum should be inline and type BountyEscrowDatum
    getNewEscrowDatum :: BountyEscrowDatum
    getNewEscrowDatum = case txOutDatum getOutputToContract of
      OutputDatum ns -> case fromBuiltinData (getDatum ns) of 
                          Just bd -> bd 
                          Nothing -> traceError "datum has wrong type"
      _              -> traceError "not an inline datum"

    -- ada/gimbals/deadline greater or equal
    checkNewDatum :: Bool 
    checkNewDatum = let bd = getNewEscrowDatum in  
                    bedBountyHash bd    == bedBountyHash dat &&
                    bedContributorPkh bd == bedContributorPkh dat &&
                    bedLovelaceAmount bd >= bedLovelaceAmount dat &&
                    bedTokenAmount bd    >= bedTokenAmount dat && 
                    bedExpirationTime bd >= bedExpirationTime dat


typedValidator :: BountyParam -> TypedValidator EscrowTypes
typedValidator bp = go bp where
    go = mkTypedValidatorParam @EscrowTypes
        $$(PlutusTx.compile [|| escrowValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: BountyParam -> Validator
validator = validatorScript . typedValidator

escrowValidatorHash :: BountyParam -> ValidatorHash 
escrowValidatorHash = validatorHash . typedValidator 

