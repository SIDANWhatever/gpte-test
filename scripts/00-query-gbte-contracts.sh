#!/usr/bin/env bash

# Inspect relevant wallets

export TREASURY_ADDR=addr_test1wphcpc9rnyyrgzgfvlmzurfcd998gz8lek4d3026xtf66dqck7vdf
export ESCROW_ADDR=addr_test1wq8lk00x7zjum9ys2tyeyd9ljf57ge8pcxyhnswkh8srdac4rsjss
export REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8

export CARDANO_NODE_SOCKET_PATH=/<YOUR PATH TO>/pre-prod/db/node.socket

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
