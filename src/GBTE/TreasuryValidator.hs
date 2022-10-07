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


module GBTE.TreasuryValidator (validator) where

-- are all of these necessary?
import qualified    Ledger.Ada as Ada

import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude                   as Pr

import GBTE.Types

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: TreasuryParam -> WithdrawalDatum -> BountyDetails -> ScriptContext -> Bool
treasuryValidator tp dat b ctx =    traceIfFalse "Only Issuer can change Treasury"              signedByIssuer ||
                                    traceIfFalse "Access token missing from input"              inputHasAuthToken &&
                                    traceIfFalse "Access token missing from contract output"    contractOutputHasAuthToken &&
                                    traceIfFalse "Output Value must match BountyDetails"        checkValueToBountyContract &&
                                    traceIfFalse "Treasury must keep remaining lovelace"        treasuryGetsLovelaceBack &&
                                    traceIfFalse "Treasury must keep remaining tokens"          treasuryGetsTokensBack
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByIssuer :: Bool
    signedByIssuer = txSignedBy info $ tTreasuryIssuerPkh tp

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that list of CurrencySymbols includes Auth CurrencySymbol
    inputHasAuthToken :: Bool
    inputHasAuthToken = tAccessTokenPolicyId tp `elem` inVals

    -- The Value to be included in Bounty Contract UTXO
    toBountyContract :: Value
    toBountyContract = valueLockedBy info (bountyContractHash tp)

    -- Check that the Auth Token is sent to Bounty Contract UTXO
    contractOutputHasAuthToken :: Bool
    contractOutputHasAuthToken = tAccessTokenPolicyId tp `elem` symbols toBountyContract

    -- Check that the Value sent to Contract UTXO matches what is specified in the Redeemer
    -- Note: For now, we can just remember to match Treasury Redeemer to Bounty Datum
    -- when we build transactions

    -- change GEQ to EQ in next compilation:
    checkValueToBountyContract :: Bool
    checkValueToBountyContract =  Ada.getLovelace (Ada.fromValue toBountyContract) >= lovelaceAmount b &&
                                  valueOf toBountyContract (tBountyTokenPolicyId tp) (tBountyTokenName tp) >= tokenAmount b

    -- The UTXO input from Treasury
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "treasury input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Treasury
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- There must be exactly ONE output UTXO
        _   -> traceError "expected exactly one treasury output"

    -- Values of each
    treasuryInputValue :: Value
    treasuryInputValue = txOutValue ownInput

    treasuryOutputValue :: Value
    treasuryOutputValue = txOutValue ownOutput

    -- Compare Values from and to Treasury to make sure that Treasury gets the right value back.
    treasuryGetsLovelaceBack :: Bool
    treasuryGetsLovelaceBack = Ada.getLovelace ( Ada.fromValue treasuryInputValue) - Ada.getLovelace ( Ada.fromValue treasuryOutputValue) <= Ada.getLovelace ( Ada.fromValue toBountyContract)

    treasuryGetsTokensBack :: Bool
    treasuryGetsTokensBack = valueOf treasuryInputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp) - valueOf treasuryOutputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp) <= valueOf toBountyContract (tBountyTokenPolicyId tp) (tBountyTokenName tp)


typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp = go tp where
    go = mkTypedValidatorParam @TreasuryTypes
        $$(PlutusTx.compile [|| treasuryValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator