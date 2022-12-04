#!/usr/bin/env bash

# Inspect relevant wallets

. 000-project-variables.sh

echo "-------- GBTE PlutusV2 ---------------------------------------------------------------"
echo ""
echo "-------- Treasury Contract: ----------------------------------------------------------"
echo ""
echo -e "\e[1;32m  address: $TREASURY_ADDRESS"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDRESS --out-file treasury.json
cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDRESS
echo ""
echo ""
echo ""
echo ""
echo "-------- Escrow Contract: ------------------------------------------------------------"
echo ""
echo -e "\e[1;33m  address: $ESCROW_ADDRESS"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $ESCROW_ADDRESS
echo ""
echo ""
echo ""
echo "-------- Reference Address: ----------------------------------------------------------"
echo ""
echo -e "\e[1;34m  address: $REFERENCE_ADDRESS"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $REFERENCE_ADDRESS
echo ""
echo ""
echo ""
