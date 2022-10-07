# Gimbal Bounty Treasury and Escrow
## PlutusV2

## Quick Reference: Instance Parameters for GBTE PlutusV2 Alpha
```
TREASURY_ADDR=addr_test1wrq9uzqnh8987dczx9krcr4f80aaescehf47dy4ksvlqs2stg5hrf
ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
REFERENCE_UTXO_TREASURY_SCRIPT="023f32a67b5e12a777454adc0afa796f16e393782730247eff6bdaa0161bbbb8#1"
REFERENCE_UTXO_TREASURY_DATUM="023f32a67b5e12a777454adc0afa796f16e393782730247eff6bdaa0161bbbb8#2"
REFERENCE_UTXO_ESCROW_SCRIPT="960c1d9681763a127c4b7614ab0c154179fd70e49cf6c68221ecf23961f7a8a9#1"

```
## Table of Contents
This readme covers the following:
1. Compile Plutus Scripts (Escrow and Treasury)
2. Build Addresses
3. Create the Treasury Script Reference UTxO and Treasury Datum Reference UTxO
4. Create the Escrow Script Reference UTxO - but not yet Datum.
5. Create a Bounty Commitment with Appropriate use of Datum -- was is that use?
6. Distribute an Escrow UTxO.
7. Review All Error Messages
8. Looking Ahead: When is a reference UTxO helpful for Datum?
9. Work to be done: Bounty Listings

## 1. Compile Plutus Scripts
- As with the previous version of GBTE, we will need to define some Parameters for our Treasury and Escrow Validators.
- Remember that the hash of the Escrow script is a parameter in the Treasury Validator, so we'll need to compile Escrow first.
- We will use the same parameters that are used in GBTE for PlutusV1 on Pre-Production, but the Escrow contract will still have a new address. That's because it's being compiled as a V2 script!
- It follows that the Escrow validator hash will be different this time.

```
writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/escrow-gbte-v2.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName     = "tgimbal"
    , accessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/treasury-gbte-v2.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = "0ffb3de6f0a5cd949052c99234bf9269e464e1c18979c1d6b9e036f7"
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tgimbal"
    , tTreasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
```

### When you complete Mastery Assignment 303.4:
- At minimum, change the values of `treasuryIssuerPkh` in `writeBountyEscrowScript`; and `bountyContractHash` and `tTreasuryIssuerPkh` in `writeBountyTreasuryScript`.
- You can also choose to test the contracts with different tokens, or keep `tgimbals` and the `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784` policyId for Contributor tokens.


## 2. Build Addresses
Use `cardano-cli address build` as usual, to create addresses for each Contract. For example:
```
TREASURY_ADDR=addr_test1wrq9uzqnh8987dczx9krcr4f80aaescehf47dy4ksvlqs2stg5hrf
ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
```

## 3. Create the Treasury Script Reference UTxO and Treasury Datum Reference UTxO
```
TX_IN_GIMBAL=""
TX_IN_LOVELACE=""
REFERENCE_ADDRESS=
TREASURY_ADDR=
PLUTUS_SCRIPT="<YOUR PATH TO>/gbte-plutus-v2/output/treasury-gbte-v2.plutus"
GBTE_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
LOVELACE_TO_LOCK=
GIMBALS_TO_LOCK=
GIMBALS_BACK_TO_ISSUER=
DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury.json"
```

### Note:
- In the [example from IOHK](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/plutus/babbage-script-example.md), separate UTxOs are created for the Reference Scripts (in the example, sent to `$dummyaddress`), for Reference Datum (in the example, sent to `$readonlyaddress`).
- In this example, we will create two unique UTxOs, but we will send them to the same address.
- TODO / DISCUSSION: When is there a good reason to place reference Datum and reference Script at the same UTxO?

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TX_IN_GIMBAL \
--tx-in $TX_IN_LOVELACE \
--tx-out $REFERENCE_ADDRESS+18679540 \
--tx-out-reference-script-file $PLUTUS_SCRIPT \
--tx-out $REFERENCE_ADDRESS+2000000 \
--tx-out-inline-datum-file $DATUM_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_LOCK + $GIMBALS_TO_LOCK $GBTE_ASSET" \
--tx-out-inline-datum-file $DATUM_FILE \
--tx-out $SENDERPREPROD+"1500000 + $GIMBALS_BACK_TO_ISSUER $GBTE_ASSET" \
--change-address $SENDERPREPROD \
--protocol-params-file protocol-preprod.json \
--out-file issuer-locks-funds-in-treasury.draft

cardano-cli transaction sign \
--tx-body-file issuer-locks-funds-in-treasury.draft \
--testnet-magic 1 \
--signing-key-file $SENDERKEYPREPROD \
--out-file issuer-locks-funds-in-treasury.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file issuer-locks-funds-in-treasury.signed
```

## 4. Create the Escrow Script Reference UTxO - but not yet Datum.
This is actually simpler than the transaction above. We just want to create a reference script for the escrow contract. Really, this step could have been included in #3 above - just build another reference. Oh well - next time!

```
ESCROW_PLUTUS_SCRIPT="<YOUR PATH TO>/gbte-plutus-v2/output/escrow-gbte-v2.plutus"
```

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-out $REFERENCE_ADDRESS+20653520 \
--tx-out-reference-script-file $ESCROW_PLUTUS_SCRIPT \
--change-address $SENDERPREPROD \
--protocol-params-file protocol-preprod.json \
--out-file create-escrow-reference-script.draft

cardano-cli transaction sign \
--tx-body-file create-escrow-reference-script.draft \
--testnet-magic 1 \
--signing-key-file $SENDERKEYPREPROD \
--out-file create-escrow-reference-script.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file create-escrow-reference-script.signed
```

## 5. Create a Bounty Commitment with Appropriate use of Datum

### 5a. First, let's skip using inline datum for the commitment, just to make sure things work.
```
BOUNTY_DETAILS_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/bounty-datum-example-01.json"
TREASURY_DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury.json"
BOUNTY_LOVELACE=
BOUNTY_GIMBALS=
LOVELACE_TO_TREASURY=
GIMBALS_TO_TREASURY=
GBTE_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
CONTRIBUTOR_TOKEN=""
CONTRIBUTOR_TOKEN_TXIN=""
LOVELACE_TXIN=""
COLLATERAL=""
TREASURY_UTXO=""
```

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $CONTRIBUTOR_TOKEN_TXIN \
--tx-in $LOVELACE_TXIN \
--tx-in-collateral $COLLATERAL \
--tx-in $TREASURY_UTXO \
--spending-tx-in-reference $REFERENCE_UTXO_TREASURY_SCRIPT \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $BOUNTY_DETAILS_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GBTE_ASSET" \
--tx-out-inline-datum-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDR+"$BOUNTY_LOVELACE + $BOUNTY_GIMBALS $GBTE_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-datum-hash-file $BOUNTY_DETAILS_FILE \
--change-address $SENDERPREPROD \
--out-file try-5a3.draft \
--protocol-params-file protocol-preprod.json

cardano-cli transaction sign \
--tx-body-file try-5a3.draft \
--testnet-magic 1 \
--signing-key-file $CONTRIBUTOR_KEY \
--out-file try-5a3.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file try-5a3.signed
```

### 5a. works with a fee of 487151 lovelace. Before moving to 5b, a few notes:
It appears that inline datum is incompatible with hashing. What are the cost differences? The 5a example uses a datum hash file in the Escrow UTxO. Let's make one that uses inline datum, and compare (a) the Commitment costs, and (b) the Distribute costs.


### 5b. Then, let's create a commitment utxo and use inline datum for the Commitment UTxO.

```
BOUNTY_DETAILS_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/bounty-datum-example-01.json"
TREASURY_DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury.json"
BOUNTY_LOVELACE=4000000
BOUNTY_GIMBALS=200
LOVELACE_TO_TREASURY=1867000000
GIMBALS_TO_TREASURY=74600
GBTE_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
CONTRIBUTOR_TOKEN=""
CONTRIBUTOR_TOKEN_TXIN=""
LOVELACE_TXIN=""
COLLATERAL=""
TREASURY_UTXO=""
CONTRIBUTOR_ADDR=""
CONTRIBUTOR_KEY=""
```

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $CONTRIBUTOR_TOKEN_TXIN \
--tx-in $LOVELACE_TXIN \
--tx-in-collateral $COLLATERAL \
--tx-in $TREASURY_UTXO \
--spending-tx-in-reference $REFERENCE_UTXO_TREASURY_SCRIPT \
--spending-plutus-script-v2 \
--spending-reference-tx-in-datum-file $TREASURY_DATUM_FILE \
--spending-reference-tx-in-redeemer-file $BOUNTY_DETAILS_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GBTE_ASSET" \
--tx-out-datum-hash-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDR+"$BOUNTY_LOVELACE + $BOUNTY_GIMBALS $GBTE_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-inline-datum-file $BOUNTY_DETAILS_FILE \
--change-address $CONTRIBUTOR_ADDR \
--out-file try-5b.draft \
--protocol-params-file protocol-preprod.json

cardano-cli transaction sign \
--tx-body-file try-5b.draft \
--testnet-magic 1 \
--signing-key-file $CONTRIBUTOR_KEY \
--out-file try-5b.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file try-5b.signed
```
### 5b. works with a fee of 491484 lovelace. (~ .002 ADA more than with just a datum hash)

---

### If 5a and 5b work --> Compare Costs. (+ Continue with both approaches on Step 6)
- 5a: 489382
- 5b: 491484

### Potential Conclusion: Inline Datum is best for Treasury, but not actually helpful for Commitments. T or F???
- If this is true, can change datum in forward output to Treasury!
- And then compare costs again.

### 5c. Todo: Create another bounty, and add inline datum for treasury.
- Coming soon!

## 6. Unlock Escrow UTXO to Distribute:
Two Scripts are provided
1. `03a-distribute-escrow-utxo-with-datum-hash.sh` (cost 359208)
2. `03b-distribute-escrow-utxo-with-inline-datum.sh` (cost 354963)