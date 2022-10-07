{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module GBTE.Types
    ( TreasuryParam (..)
    , WithdrawalDatum (..)
    , BountyDetails (..)
    , BountyAction (..)
    , BountyParam (..)
    , BountyEscrowDatum (..)
    , TreasuryTypes
    , EscrowTypes
    ) where

import              GHC.Generics                (Generic)
import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import  qualified   PlutusTx
import              PlutusTx.Prelude    hiding  (Semigroup (..), unless)
import              Prelude                     (Show (..))
import  qualified   Prelude                 as  Pr

data TreasuryParam = TreasuryParam
    { tAccessTokenPolicyId   :: !CurrencySymbol
    , bountyContractHash     :: !ValidatorHash
    , tBountyTokenPolicyId   :: !CurrencySymbol
    , tBountyTokenName       :: !TokenName
    , tTreasuryIssuerPkh     :: !PubKeyHash
    } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''TreasuryParam

data WithdrawalDatum = WithdrawalDatum
  { bountyCount     :: !Integer
  , treasuryKey     :: !PubKeyHash
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''WithdrawalDatum

data TreasuryTypes
instance ValidatorTypes TreasuryTypes where
    type DatumType TreasuryTypes = WithdrawalDatum
    type RedeemerType TreasuryTypes = BountyDetails

data BountyDetails = BountyDetails
  { issuerPkh           :: !PubKeyHash
  , contributorPkh      :: !PubKeyHash
  , lovelaceAmount      :: !Integer
  , tokenAmount         :: !Integer
  , expirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq BountyDetails where
  {-# INLINABLE (==) #-}
  BountyDetails iP cP lA tA eT == BountyDetails iP' cP' lA' tA' eT' =
    (iP == iP') && (cP == cP') && (lA == lA') && (tA == tA') && (eT == eT')

    -- Alternative way of comparisons
    -- a == b = (issuerPkh       a == issuerPkh      b) &&
    --          (contributorPkh  a == contributorPkh b) &&
    --          (lovelaceAmount  a == lovelaceAmount b) &&
    --          (expirationTime  a == expirationTime b)

PlutusTx.unstableMakeIsData ''BountyDetails
PlutusTx.makeLift ''BountyDetails

data BountyEscrowDatum = BountyEscrowDatum
  { bedIssuerPkh           :: !PubKeyHash
  , bedContributorPkh      :: !PubKeyHash
  , bedLovelaceAmount      :: !Integer
  , bedTokenAmount         :: !Integer
  , bedExpirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq BountyEscrowDatum where
  {-# INLINABLE (==) #-}
  BountyEscrowDatum bIP bCP bLA bTA bET == BountyEscrowDatum bIP' bCP' bLA' bTA' bET' =
    (bIP == bIP') && (bCP == bCP') && (bLA == bLA') && (bTA == bTA') && (bET == bET')

    -- Alternative way of comparisons
    -- a == b = (bedIssuerPkh       a == bedIssuerPkh      b) &&
    --          (bedContributorPkh  a == bedContributorPkh b) &&
    --          (bedLovelaceAmount  a == bedLovelaceAmount b) &&
    --          (bedTokenAmount     a == bedTokenAmount    b) &&
    --          (bedExpirationTime  a == bedExpirationTime b)

PlutusTx.unstableMakeIsData ''BountyEscrowDatum
PlutusTx.makeLift ''BountyEscrowDatum

data BountyParam = BountyParam
    { bountyTokenPolicyId     :: !CurrencySymbol
    , bountyTokenName         :: !TokenName
    , accessTokenPolicyId     :: !CurrencySymbol
    , treasuryIssuerPkh       :: !PubKeyHash
    } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''BountyParam

data BountyAction = Cancel | Update | Distribute
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Update, 1), ('Distribute, 2)]
PlutusTx.makeLift ''BountyAction

data EscrowTypes
instance ValidatorTypes EscrowTypes where
    type DatumType EscrowTypes = BountyEscrowDatum
    type RedeemerType EscrowTypes = BountyAction
