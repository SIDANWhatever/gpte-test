#!/usr/bin/env bash

# Update Treasury Datum
. 000-project-variables.sh

export ISSUER=$1
export ISSUERKEY=$2
export REDEEMER_ACTION_FILE=$PROJECT_PATH"/datum-and-redeemers/Manage.json"
export NEW_DATUM_FILE=$PROJECT_PATH"/datum-and-redeemers/datum-treasury-with-real-hashes-002.json"

cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR
echo "Current Treasury UTxO:"
read CONTRACT_TXIN
echo "How many lovelace are in the utxo?"
read LOVELACE_IN_TREASURY
echo "How many tGimbals are in the utxo?"
read GIMBALS_IN_TREASURY

cardano-cli query utxo --testnet-magic 1 --address $ISSUER
echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN for fees:"
read TXIN1
echo "Specify a UTXO with IssuerToken"
read TX_IN_ISSUER
echo "What is the Asset ID of the IssuerToken?"
read ISSUER_TOKEN
# echo "Add tokens to Treasury? Specify UTxO:"
# read TX_IN_GIMBAL
# echo "Add lovelace to the treasury:"
# read ADDITIONAL_LOVELACE
# echo "tGimbals to the treasury:"
# read ADDTIONAL_GIMBALS
# echo "tGimbals to Issuer"
# read GIMBALS_BACK_TO_ISSUER

# LOVELACE_TO_LOCK=$(expr $LOVELACE_IN_TREASURY + $ADDITIONAL_LOVELACE)
# GIMBALS_TO_LOCK=$(expr $GIMBALS_IN_TREASURY + $ADDTIONAL_GIMBALS)

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN1 \
--tx-in $TX_IN_ISSUER \
--tx-in-collateral $TXIN1 \
--tx-in $CONTRACT_TXIN \
--spending-tx-in-reference $REFERENCE_UTXO_TREASURY_SCRIPT \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $REDEEMER_ACTION_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_IN_TREASURY + $GIMBALS_IN_TREASURY $GIMBAL_ASSET" \
--tx-out-inline-datum-file $NEW_DATUM_FILE \
--tx-out $SENDERPREPROD+"1500000 + 1 $ISSUER_TOKEN" \
--change-address $SENDERPREPROD \
--protocol-params-file protocol.json \
--out-file issuer-updates-treasury-datum.draft

cardano-cli transaction sign \
--tx-body-file issuer-updates-treasury-datum.draft \
--testnet-magic 1 \
--signing-key-file $SENDERKEYPREPROD \
--out-file issuer-updates-treasury-datum.signed

cardano-cli transaction submit \
--testnet-magic 1 \
--tx-file issuer-updates-treasury-datum.signed