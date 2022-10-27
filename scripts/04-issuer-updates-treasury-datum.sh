#!/usr/bin/env bash

# Update Treasury Datum

export ISSUER=$SENDERPREPROD
export ISSUERKEY=$SENDERKEYPREPROD
export REDEEMER_ACTION_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/Manage.json"
export TREASURY_ADDR=addr_test1wq6zj47drftsn0hmeckn279ws0p9h4fge8tz4n9z5yqnd9gzw7dgt
export TREASURY_REFERENCE_UTXO="121d887abea6b7f17bce7ea4437dfbd78f6328be2412bad0a02974b18fe7259c#1"
export BOUNTY_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"
export DATUM_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/datum-treasury-with-real-hashes.json"

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket

cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

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
echo "Add tokens to Treasury? Specify UTxO:"
read TX_IN_GIMBAL
echo "Add lovelace to the treasury:"
read ADDITIONAL_LOVELACE
echo "tGimbals to the treasury:"
read ADDTIONAL_GIMBALS
echo "tGimbals to Issuer"
read GIMBALS_BACK_TO_ISSUER

LOVELACE_TO_LOCK=$(expr $LOVELACE_IN_TREASURY + $ADDITIONAL_LOVELACE)
GIMBALS_TO_LOCK=$(expr $GIMBALS_IN_TREASURY + $ADDTIONAL_GIMBALS)

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TX_IN_GIMBAL \
--tx-in $TXIN1 \
--tx-in $TX_IN_ISSUER \
--tx-in-collateral $TXIN1 \
--tx-in $CONTRACT_TXIN \
--spending-tx-in-reference $TREASURY_REFERENCE_UTXO \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $REDEEMER_ACTION_FILE \
--tx-out $TREASURY_ADDR+"$LOVELACE_TO_LOCK + $GIMBALS_TO_LOCK $BOUNTY_ASSET" \
--tx-out-inline-datum-file $DATUM_FILE \
--tx-out $SENDERPREPROD+"1500000 + $GIMBALS_BACK_TO_ISSUER $BOUNTY_ASSET" \
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