#!/usr/bin/env bash

# Contributor Commits to Bounty - GBTE PlutusV2, with reference scripts
CONTRIBUTOR=$1
CONTRIBUTORKEY=$2

. 000-project-variables.sh

TREASURY_DATUM_FILE=$PROJECT_PATH"/datum-and-redeemers/datum-treasury-with-real-hashes-001.json"
TREASURY_ACTION_FILE=$PROJECT_PATH"/datum-and-redeemers/TreasuryAction-Commit-Example-01.json"
BOUNTY_DATUM_FILE=$PROJECT_PATH"/datum-and-redeemers/BountyEscrowDatum-Example-01.json"

# -------------------------------------------

# # Interaction for repeat use:
# cardano-cli query utxo --testnet-magic 1 --address $CONTRIBUTOR
# echo "Choose a UTxO that holds Contributor Token:"
# read CONTRIBUTOR_TOKEN_TXIN
# echo "What is the Asset ID of the Contributor Token?"
# read CONTRIBUTOR_TOKEN
# echo "Specify a Collateral UTxO:"
# read COLLATERAL
# echo "Specify a TXIN for fees:"
# read LOVELACE_TXIN

# cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR
# echo "Specify the Treasury UTxO:"
# read TREASURY_UTXO
# echo "How many lovelace are in the treasury?"
# read LOVELACE_IN_TREASURY
# echo "How many tgimbals are in the treasury?"
# read GIMBALS_IN_TREASURY

# echo ""
# echo "About the Bounty:"
# echo ""
# echo "Lovelace in Bounty:"
# read BOUNTY_LOVELACE
# echo "Gimbals in Bounty:"
# read BOUNTY_GIMBALS

# -------------------------------------------

# Hard Code for Quick Testing:
CONTRIBUTOR_TOKEN_TXIN=be3b13427b1012af414a9b205ef97f392bfe6a3b40127b18ff037c2063ad1b33#2
CONTRIBUTOR_TOKEN=738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784.6578616d706c65314742544570726570726f64
COLLATERAL=5083cd44d22f43cb2f18043763636371b6609adca1305e63da7536ae81471647#0
LOVELACE_TXIN=5083cd44d22f43cb2f18043763636371b6609adca1305e63da7536ae81471647#0
TREASURY_UTXO=cf4c79e3dfbd37009c4938095ad07d34c6289fdc84f04983875a04efed048e11#1
LOVELACE_IN_TREASURY=1500000000
GIMBALS_IN_TREASURY=28025
BOUNTY_LOVELACE=25000000
BOUNTY_GIMBALS=1000

# -------------------------------------------

LOVELACE_TO_TREASURY=$(expr $LOVELACE_IN_TREASURY - $BOUNTY_LOVELACE)
GIMBALS_TO_TREASURY=$(expr $GIMBALS_IN_TREASURY - $BOUNTY_GIMBALS)

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
--spending-reference-tx-in-redeemer-file $TREASURY_ACTION_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GIMBAL_ASSET" \
--tx-out-inline-datum-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDR+"$BOUNTY_LOVELACE + $BOUNTY_GIMBALS $GIMBAL_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-inline-datum-file $BOUNTY_DATUM_FILE \
--change-address $CONTRIBUTOR \
--out-file try-5b.draft \
--protocol-params-file protocol.json

# cardano-cli transaction sign \
# --tx-body-file try-5b.draft \
# --testnet-magic 1 \
# --signing-key-file $CONTRIBUTORKEY \
# --out-file try-5b.signed

# cardano-cli transaction submit \
# --testnet-magic 1 \
# --tx-file try-5b.signed