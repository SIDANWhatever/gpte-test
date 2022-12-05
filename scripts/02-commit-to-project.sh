#!/usr/bin/env bash

# Contributor Commits to Project - GPTE PlutusV2, with reference scripts
CONTRIBUTOR=$1
CONTRIBUTORKEY=$2

. 000-project-variables.sh

TREASURY_DATUM_FILE=$PROJECT_PATH"/datum-and-redeemers/datum-treasury-with-project-hashes.json"
TREASURY_ACTION_FILE=$PROJECT_PATH"/datum-and-redeemers/TreasuryAction-Commit-Example-01.json"
COMMITMENT_DATUM_FILE=$PROJECT_PATH"/datum-and-redeemers/CommitmentEscrowDatum-Example-01.json"

# -------------------------------------------

# # Interactive Testing:
cardano-cli query utxo --testnet-magic 1 --address $CONTRIBUTOR
echo "Choose a UTxO that holds Contributor Token:"
read CONTRIBUTOR_TOKEN_TXIN
echo "What is the Asset ID of the Contributor Token?"
read CONTRIBUTOR_TOKEN
echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN for fees:"
read LOVELACE_TXIN

cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDRESS
echo "Specify the Treasury UTxO:"
read TREASURY_UTXO
echo "How many lovelace are in the treasury?"
read LOVELACE_IN_TREASURY
echo "How many tgimbals are in the treasury?"
read GIMBALS_IN_TREASURY

echo ""
echo "About the Project:"
echo ""
echo "Lovelace in Project:"
read PROJECT_LOVELACE
echo "Gimbals in Project:"
read PROJECT_GIMBALS

# -------------------------------------------

# For Quick Testing:
# CONTRIBUTOR_TOKEN_TXIN=
# CONTRIBUTOR_TOKEN=
# COLLATERAL=
# LOVELACE_TXIN=
# TREASURY_UTXO=
# LOVELACE_IN_TREASURY=
# GIMBALS_IN_TREASURY=
# PROJECT_LOVELACE=
# PROJECT_GIMBALS=

# -------------------------------------------

LOVELACE_TO_TREASURY=$(expr $LOVELACE_IN_TREASURY - $PROJECT_LOVELACE)
GIMBALS_TO_TREASURY=$(expr $GIMBALS_IN_TREASURY - $PROJECT_GIMBALS)

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
--tx-out $TREASURY_ADDRESS+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GIMBAL_ASSET" \
--tx-out-inline-datum-file $TREASURY_DATUM_FILE \
--tx-out $ESCROW_ADDRESS+"$PROJECT_LOVELACE + $PROJECT_GIMBALS $GIMBAL_ASSET + 1 $CONTRIBUTOR_TOKEN" \
--tx-out-inline-datum-file $COMMITMENT_DATUM_FILE \
--change-address $CONTRIBUTOR \
--out-file project-commitment-01.draft \
--protocol-params-file protocol.json

cardano-cli transaction sign \
--tx-body-file project-commitment-01.draft \
--testnet-magic 1 \
--signing-key-file $CONTRIBUTORKEY \
--out-file project-commitment-01.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file project-commitment-01.signed