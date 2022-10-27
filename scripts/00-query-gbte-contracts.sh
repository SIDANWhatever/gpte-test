#!/usr/bin/env bash

# Inspect relevant wallets

export TREASURY_ADDR=addr_test1wq6zj47drftsn0hmeckn279ws0p9h4fge8tz4n9z5yqnd9gzw7dgt
export ESCROW_ADDR=addr_test1wzvd9dv2fmljn68fl8gklvhmptzpgmgtqae2g9764wzq5aq6646m0
export REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket

cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

echo "-------- GBTE PlutusV2 ---------------------------------------------------------------"
echo ""
echo "-------- Treasury Contract: ----------------------------------------------------------"
echo ""
echo -e "\e[1;32m  address: $TREASURY_ADDR"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR
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
echo -e "\e[1;34m  address: $REFERENCE_ADDRESS"
echo -e "\e[m"
cardano-cli query utxo --testnet-magic 1 --address $REFERENCE_ADDRESS
echo ""
echo ""
echo ""
