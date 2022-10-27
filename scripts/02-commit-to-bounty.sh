#!/usr/bin/env bash

# Contributor Commits to Bounty - GBTE PlutusV2, with reference scripts

CONTRIBUTOR=$1
CONTRIBUTORKEY=$2

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/cardano/testnet-pre-production/db/node.socket

TREASURY_ADDR=addr_test1wq6zj47drftsn0hmeckn279ws0p9h4fge8tz4n9z5yqnd9gzw7dgt
ESCROW_ADDR=addr_test1wzvd9dv2fmljn68fl8gklvhmptzpgmgtqae2g9764wzq5aq6646m0
REFERENCE_UTXO_TREASURY_SCRIPT="121d887abea6b7f17bce7ea4437dfbd78f6328be2412bad0a02974b18fe7259c#1"

cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

TREASURY_DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury-with-real-hashes.json"
TREASURY_ACTION_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/TreasuryAction-Commit-Example-01.json"
BOUNTY_DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/BountyEscrowDatum-Example-01.json"
GBTE_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"

# -------------------------------------------

# # Interaction for repeat use:
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

# Hard Code for Quick Testing:
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
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GBTE_ASSET" \
--tx-out-inline-datum-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDR+"$BOUNTY_LOVELACE + $BOUNTY_GIMBALS $GBTE_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-inline-datum-file $BOUNTY_DATUM_FILE \
--change-address $CONTRIBUTOR \
--out-file try-5b.draft \
--protocol-params-file protocol.json

cardano-cli transaction sign \
--tx-body-file try-5b.draft \
--testnet-magic 1 \
--signing-key-file $CONTRIBUTORKEY \
--out-file try-5b.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file try-5b.signed