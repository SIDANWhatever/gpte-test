source getTxFunc.sh 

WALLET=$1

getInputTx $WALLET 
ADA_UTXO=${SELECTED_UTXO}

getInputTx $WALLET
ISSUER_TOKEN_UTXO=${SELECTED_UTXO}
ISSUER_TOKEN_UTXO_TOKEN=${SELECTED_UTXO_TOKENS}

getContractInputTx treasury 
TCONTRACT_UTXO=${SELECTED_UTXO_CONTRACT}
TCONTRACT_ADA_AMOUNT=${SELECTED_UTXO_LOVELACE}
TCONTRACT_GIMBAL_AMOUNT=${SELECTED_UTXO_TOKENS}

ISSUER_ASSET="a3072d3493dd41f23d4b22f5df6382dd75eb7d656e85d02b3b616eca.697373756572"

cardano-cli transaction build \
    $TESTNET \
    --babbage-era \
    --tx-in-collateral $ADA_UTXO \
    --tx-in $ISSUER_TOKEN_UTXO \
    --tx-in $TCONTRACT_UTXO \
    --tx-in-script-file ../output/treasury-gbte-v2-with-bounty-hash2.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file ../datum-and-redeemers/Manage.json \
    --tx-out $(cat $CLIWALLET/$WALLET.addr)+2000000"+ 1 $ISSUER_ASSET" \
    --tx-out $(cat $CLIWALLET/$WALLET.addr)+2000000"+ $TCONTRACT_GIMBAL_AMOUNT $GIMBAL_ASSET" \
    --change-address $(cat $CLIWALLET/$WALLET.addr) \
    --protocol-params-file protocol.json \
    --out-file tx.tx \

cardano-cli transaction sign --tx-body-file tx.tx --signing-key-file $CLIWALLET/$WALLET.skey $TESTNET --out-file tx.sign 

cardano-cli transaction submit $TESTNET --tx-file tx.sign

