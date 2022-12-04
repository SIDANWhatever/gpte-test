#sample: ./lockTreasury walletName adaAmount gimbalsAmount

source getTxFunc.sh 

WALLET=$1
ADA_AMOUNT=$2
GIMBALS_AMOUNT=$3

# first utxo ada only
getInputTx $1
ADA_UTXO=${SELECTED_UTXO}

# second utxo with gimbals
getInputTx $1
GIMBALS_UTXO=${SELECTED_UTXO}
GIMBAL_AMOUNT_UTXO=${SELECTED_UTXO_TOKENS}

GIMBALS_BACK_TO_WALLET=$(expr $GIMBAL_AMOUNT_UTXO - $GIMBALS_AMOUNT)

cardano-cli transaction build \
    $TESTNET \
    --babbage-era \
    --tx-in $ADA_UTXO \
    --tx-in $GIMBALS_UTXO \
    --tx-out $(cat ../output/treasury-gbte-v2-with-bounty-hash2.addr)+$ADA_AMOUNT"+ ${GIMBALS_AMOUNT} ${GIMBAL_ASSET}" \
    --tx-out-inline-datum-file ../datum-and-redeemers/datum-treasury-new.json \
    --tx-out $(cat $CLIWALLET/$WALLET.addr)+2000000"+ ${GIMBALS_BACK_TO_WALLET} ${GIMBAL_ASSET}" \
    --change-address $(cat $CLIWALLET/$WALLET.addr) \
    --out-file tx.tx 

cardano-cli transaction sign \
    --tx-body-file tx.tx \
    $TESTNET \
    --signing-key-file $CLIWALLET/$WALLET.skey \
    --out-file tx.sign 

cardano-cli transaction submit \
    --tx-file tx.sign \
    $TESTNET
