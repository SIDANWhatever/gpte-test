# Gimbal Bounty Treasury and Escrow
## PlutusV2

Updated 2022-10-10. See [changelog](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/projects/gbte/gbte-plutus-v2/-/blob/master/changelog.md).

## Quick Reference: Instance Parameters for GBTE PlutusV2 Alpha
```
TREASURY_ADDR=addr_test1wphcpc9rnyyrgzgfvlmzurfcd998gz8lek4d3026xtf66dqck7vdf
ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
REFERENCE_UTXO_TREASURY_SCRIPT="fd376248eb20e14c9785a49fe7617636d7f92c598486ec2e2f46857cd2fbf714#1"
REFERENCE_UTXO_ESCROW_SCRIPT="960c1d9681763a127c4b7614ab0c154179fd70e49cf6c68221ecf23961f7a8a9#1"

```
## Table of Contents
This readme covers the following:
1. Compile Plutus Scripts (Escrow and Treasury)
2. Build Addresses
3. Create the Treasury Script Reference UTxO and Treasury Datum Reference UTxO
4. Create the Escrow Script Reference UTxO - but not yet Datum.
5. Create a Bounty Commitment with Appropriate use of Datum
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
TREASURY_ADDR=addr_test1wphcpc9rnyyrgzgfvlmzurfcd998gz8lek4d3026xtf66dqck7vdf
ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
```

## 3. Create the Treasury Script Reference UTxO and Treasury Datum Reference UTxO
```
TX_IN_GIMBAL=
TX_IN_LOVELACE=
REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
TREASURY_ADDR=addr_test1wphcpc9rnyyrgzgfvlmzurfcd998gz8lek4d3026xtf66dqck7vdf
PLUTUS_SCRIPT="<YOUR PATH TO>/gbte-plutus-v2/output/treasury-gbte-v2-02.plutus"
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
--tx-out $REFERENCE_ADDRESS+2000000 \
--tx-out-reference-script-file $PLUTUS_SCRIPT \
--tx-out $REFERENCE_ADDRESS+2000000 \
--tx-out-inline-datum-file $DATUM_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_LOCK + $GIMBALS_TO_LOCK $GBTE_ASSET" \
--tx-out-inline-datum-file $DATUM_FILE \
--tx-out $SENDERPREPROD+"1500000 + $GIMBALS_BACK_TO_ISSUER $GBTE_ASSET" \
--change-address $SENDERPREPROD \
--protocol-params-file protocol-preprod.json \
--out-file issuer-locks-funds-in-treasury-02.draft

cardano-cli transaction sign \
--tx-body-file issuer-locks-funds-in-treasury-02.draft \
--testnet-magic 1 \
--signing-key-file $SENDERKEYPREPROD \
--out-file issuer-locks-funds-in-treasury-02.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file issuer-locks-funds-in-treasury-02.signed
```

### You will get an error the first time you run this transaction "correctly".
- If you do everything else right, you should get an error saying that 2000000 lovelace is not enough to satisfy the minUTxO. Change the amount of lovelace being sent to the reference address accordingly.

## 4. Create the Escrow Script Reference UTxO - but not yet Datum.
This is actually simpler than the transaction above. We just want to create a reference script for the escrow contract. Really, this step could have been included in #3 above - just adding another reference output.

```
ESCROW_PLUTUS_SCRIPT="<YOUR PATH TO>/gbte-plutus-v2/output/escrow-gbte-v2.plutus"
```

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-out $REFERENCE_ADDRESS+2000000 \
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
- Use the script [02-commit-to-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/projects/gbte/gbte-plutus-v2/-/blob/master/scripts/02-commit-to-bounty.sh)
- Error checking
- Cost comparisons

## 6. Unlock Escrow UTXO to Distribute:
- Use the script [03-distribute-escrow-utxo-with-inline-datum.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/projects/gbte/gbte-plutus-v2/-/blob/master/scripts/03-distribute-escrow-utxo-with-inline-datum.sh)


# Upcoming at Live Coding:
## 7. Review All Error Messages
## 8. Looking Ahead: When is a reference UTxO helpful for Datum?
## 9. Work to be done: Bounty Listings