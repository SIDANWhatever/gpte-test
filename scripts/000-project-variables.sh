# Updated 2022-12-05

export CARDANO_NODE_SOCKET_PATH=/home/james/hd2/cardano/testnet-pre-production/db/node.socket
export PROJECT_PATH="/home/james/hd2/gpte/gpte-plutus-v2"

# Current Preprod 2022-12-05 (add naming convention)
export TREASURY_ADDRESS=addr_test1wp360epgpgpa5f9s987s2fnu7xz30qn62xm3n0z6rk6najckqrsh3
export ESCROW_ADDRESS=addr_test1wqrkvx32zyjyrdmszf7wtn904sl93fnuw4rvvnl4v5pcznq0jdj83
export REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
export REFERENCE_UTXO_TREASURY_SCRIPT="0235eebe85460cc9b2db9353410e18aabaa52482c4757faa9e848fdb8071e321#1"
export REFERENCE_UTXO_ESCROW_SCRIPT="0235eebe85460cc9b2db9353410e18aabaa52482c4757faa9e848fdb8071e321#2"
export GIMBAL_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"


cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json