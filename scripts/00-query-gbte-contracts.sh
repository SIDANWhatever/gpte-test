#!/usr/bin/env bash

# Inspect relevant wallets

. 000-project-variables.sh

echo "-------- GBTE PlutusV2 ---------------------------------------------------------------"
echo ""
echo "-------- Treasury Contract: ----------------------------------------------------------"
echo ""
echo -e "\e[1;32m  address: $TREASURY_ADDR"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR --out-file treasury.json
echo ""
echo ""
echo ""
echo ""
echo "-------- Escrow Contract: ------------------------------------------------------------"
echo ""
echo -e "\e[1;33m  address: $ESCROW_ADDR"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $ESCROW_ADDR
echo ""
echo ""
echo ""
echo "-------- Reference Address: ----------------------------------------------------------"
echo ""
echo -e "\e[1;34m  address: $REFERENCE_ADDR"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $REFERENCE_ADDR
echo ""
echo ""
echo ""
