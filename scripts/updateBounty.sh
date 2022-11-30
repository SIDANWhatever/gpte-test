source getTxFunc.sh 

WALLET=$1
GIMBAL_AMOUNT=$2

getInputTx $WALLET
ADA_UTXO=${SELECTED_UTXO}

getInputTx $WALLET 
ISSUER_TOKEN_UTXO=${SELECTED_UTXO}

getContractInputTx escrow 
ECONTRACT_UTXO=${SELECTED_UTXO_CONTRACT}
ECONTRACT_ADA_AMOUNT=${SELECTED_UTXO_LOVELACE}

myPKH="f979d7c07bfd878ecfc7da7ab3a1201209e2a5895feac1935e873c1a"
echo $myPKH
GIMBALS_COMMIT_AMOUNT=1
ADA_COMMIT_AMOUNT=4000000

bhash="5472"

ISSUER_ASSET="a3072d3493dd41f23d4b22f5df6382dd75eb7d656e85d02b3b616eca.697373756572"
AUTH_TOKEN="46aa828503b87500a691e3b6d44ae1563e16e11016c7e6863917335f.616363657373"

cat ../datum-and-redeemers/commitBountyEscrowDatum_$bhash.json | jq --argjson gAmount "$GIMBALS_COMMIT_AMOUNT" --argjson aAmount "$ADA_COMMIT_AMOUNT" '.fields[1].int = $aAmount | .fields[2].int = $gAmount '  > ../datum-and-redeemers/updateBountyEscrowDatum_$bhash.json

cardano-cli transaction build \
    $TESTNET \
    --babbage-era \
    --tx-in $ADA_UTXO \
    --tx-in $ISSUER_TOKEN_UTXO \
    --tx-in-collateral $ADA_UTXO \
    --tx-in $ECONTRACT_UTXO \
    --tx-in-script-file ../output/escrow-gbte-v2-with-bounty-hash2.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file ../datum-and-redeemers/Update.json \
    --tx-out $(cat ../output/escrow-gbte-v2-with-bounty-hash2.addr)+$ADA_COMMIT_AMOUNT+" 1 $AUTH_TOKEN + $GIMBALS_COMMIT_AMOUNT $GIMBAL_ASSET" \
    --tx-out-inline-datum-file ../datum-and-redeemers/updateBountyEscrowDatum_$bhash.json \
    --tx-out $(cat $CLIWALLET/$WALLET.addr)+2000000"+ 1 $ISSUER_ASSET" \
    --change-address $(cat $CLIWALLET/$WALLET.addr) \
    --protocol-params-file protocol.json \
    --out-file tx.tx 

cardano-cli transaction sign --tx-body-file tx.tx --signing-key-file $CLIWALLET/$WALLET.skey $TESTNET --out-file tx.sign 

cardano-cli transaction submit --tx-file tx.sign $TESTNET
