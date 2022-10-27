#!/usr/bin/env bash

# Distribute Escrow UTxO
export CONTRIBUTOR=$1

export ISSUER=
export ISSUERKEY=
export REDEEMER_ACTION_FILE="<YOUR PATH TO>/gbte-plutus-v2/datum-and-redeemers/Distribute.json"
export ESCROW_ADDR=addr_test1wzvd9dv2fmljn68fl8gklvhmptzpgmgtqae2g9764wzq5aq6646m0
export REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
export ESCROW_REFERENCE_UTXO="f063fbc9a6542eb1cee7667fd1c6fe62ce562659b3dce4f6b8ee1230a0382bfa#1"
export BOUNTY_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/cardano/testnet-pre-production/db/node.socket

cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json


cardano-cli query utxo --testnet-magic 1 --address $ESCROW_ADDR
echo "Which bounty utxo will you distribute?"
read CONTRACT_TXIN
echo "How many lovelace are in this bounty?"
read LOVELACE_IN_BOUNTY
echo "How many tgimbals are in this bounty?"
read BOUNTY_TOKENS_IN_BOUNTY
echo "What is the Asset ID of the Contributor Token in this bounty?"
read CONTRIBUTOR_ASSET


cardano-cli query utxo --testnet-magic 1 --address $ISSUER
echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN for fees:"
read TXIN1
echo "Specify a UTXO with IssuerToken"
read ISSUER_TXIN
echo "What is the Asset ID of the IssuerToken?"
read ISSUER_ASSET


cardano-cli transaction build \
--babbage-era \
--tx-in $CONTRACT_TXIN \
--spending-tx-in-reference $ESCROW_REFERENCE_UTXO \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $REDEEMER_ACTION_FILE \
--tx-in $TXIN1 \
--tx-in $ISSUER_TXIN \
--tx-in-collateral $COLLATERAL \
--tx-out $CONTRIBUTOR+"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET" \
--tx-out $CONTRIBUTOR+"1500000 + 1 $CONTRIBUTOR_ASSET" \
--tx-out $ISSUER+"1500000 + 1 $ISSUER_ASSET" \
--change-address $ISSUER \
--protocol-params-file protocol.json \
--testnet-magic 1 \
--out-file distribute-bounty-tx.draft

cardano-cli transaction sign \
--signing-key-file $ISSUERKEY \
--testnet-magic 1 \
--tx-body-file distribute-bounty-tx.draft \
--out-file distribute-bounty-tx.signed

cardano-cli transaction submit \
--tx-file distribute-bounty-tx.signed \
--testnet-magic 1