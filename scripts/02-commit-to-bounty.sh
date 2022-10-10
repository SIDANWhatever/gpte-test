#!/usr/bin/env bash

# Contributor Commits to Bounty - GBTE PlutusV2, with reference scripts

# Command Line Arguments:
CONTRIBUTOR=$1
CONTRIBUTORKEY=$2

# Be sure to review each of these files:
TREASURY_DATUM_FILE="/<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury.json"
BOUNTY_DATUM_FILE="/<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/bounty-datum-example-02.json"
TREASURY_ACTION_FILE="/<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/treasury-action-test-02.json"

export CARDANO_NODE_SOCKET_PATH=/<YOUR PATH TO>/testnet-pre-production/db/node.socket

TREASURY_ADDR=addr_test1wphcpc9rnyyrgzgfvlmzurfcd998gz8lek4d3026xtf66dqck7vdf
ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
REFERENCE_UTXO_TREASURY_SCRIPT=fd376248eb20e14c9785a49fe7617636d7f92c598486ec2e2f46857cd2fbf714#1
BOUNTY_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"

cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json



# -------------------------------------------
# # Usage Option 1: You can hard code these values for quick testing.
# # Recommended for making sure that all errors are working as expected.

# # Hard Code for Quick Testing:
# CONTRIBUTOR_TOKEN_TXIN=
# CONTRIBUTOR_TOKEN=
# COLLATERAL=
# LOVELACE_TXIN=
# TREASURY_UTXO=
# LOVELACE_IN_TREASURY=
# GIMBALS_IN_TREASURY=
# BOUNTY_LOVELACE=
# BOUNTY_GIMBALS=

# -------------------------------------------
# Usage Option 2: You can review utxos and input these values in the terminal
# Interaction for repeat use:
cardano-cli query utxo --testnet-magic 1 --address $CONTRIBUTOR
echo "Choose a UTxO that holds Contributor Token:"
read CONTRIBUTOR_TOKEN_TXIN
echo "What is the Asset ID of the Contributor Token?"
read CONTRIBUTOR_TOKEN
echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN for fees:"
read LOVELACE_TXIN

cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR
echo "Specify the Treasury UTxO:"
read TREASURY_UTXO
echo "How many lovelace are in the treasury?"
read LOVELACE_IN_TREASURY
echo "How many tgimbals are in the treasury?"
read GIMBALS_IN_TREASURY

echo ""
echo "About the Bounty:"
echo ""
echo "Lovelace in Bounty:"
read BOUNTY_LOVELACE
echo "Gimbals in Bounty:"
read BOUNTY_GIMBALS

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
--spending-reference-tx-in-datum-file $TREASURY_DATUM_FILE \
--spending-reference-tx-in-redeemer-file $TREASURY_ACTION_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GBTE_ASSET" \
--tx-out-datum-hash-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDR+"$BOUNTY_LOVELACE + $BOUNTY_GIMBALS $GBTE_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-inline-datum-file $BOUNTY_DATUM_FILE \
--change-address $CONTRIBUTOR \
--out-file try-002.draft \
--protocol-params-file protocol.json

cardano-cli transaction sign \
--tx-body-file try-002.draft \
--testnet-magic 1 \
--signing-key-file $CONTRIBUTORKEY \
--out-file try-002.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file try-002.signed