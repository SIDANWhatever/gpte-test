#!/usr/bin/env bash

# Distribute Escrow UTxO
export CONTRIBUTOR=$1
export ISSUER=$SENDERPREPROD
export ISSUERKEY=$SENDERKEYPREPROD

. 000-project-variables.sh

export REDEEMER_ACTION_FILE=$PROJECT_PATH"/datum-and-redeemers/Distribute.json"

# Interactive Testing:
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

# For Quick Testing:
# CONTRACT_TXIN=
# LOVELACE_IN_BOUNTY=
# BOUNTY_TOKENS_IN_BOUNTY=
# CONTRIBUTOR_ASSET=
# COLLATERAL=
# TXIN1=
# ISSUER_TXIN=
# ISSUER_ASSET=

cardano-cli transaction build \
--babbage-era \
--tx-in $CONTRACT_TXIN \
--spending-tx-in-reference $REFERENCE_UTXO_ESCROW_SCRIPT \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $REDEEMER_ACTION_FILE \
--tx-in $TXIN1 \
--tx-in $ISSUER_TXIN \
--tx-in-collateral $COLLATERAL \
--tx-out $CONTRIBUTOR+"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $GIMBAL_ASSET" \
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