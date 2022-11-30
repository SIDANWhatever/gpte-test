source getTxFunc.sh 

WALLET=$1
ADA_COMMIT_AMOUNT=$2
GIMBALS_COMMIT_AMOUNT=$3

getInputTx $WALLET
ADA_UTXO=${SELECTED_UTXO}

getInputTx $WALLET 
AUTH_UTXO=${SELECTED_UTXO}
AUTH_TOKEN="46aa828503b87500a691e3b6d44ae1563e16e11016c7e6863917335f.616363657373"

getContractInputTx treasury
TCONTRACT_UTXO=${SELECTED_UTXO_CONTRACT}
TCONTRACT_ADA_AMOUNT=${SELECTED_UTXO_LOVELACE}
TCONTRACT_GIMBALS_AMOUNT=${SELECTED_UTXO_TOKENS}

LOVELACE_TO_TREASURY=$(expr $TCONTRACT_ADA_AMOUNT - $ADA_COMMIT_AMOUNT)
GIMBALS_TO_TREASURY=$(expr $TCONTRACT_GIMBALS_AMOUNT - $GIMBALS_COMMIT_AMOUNT)

myPKH=`cardano-cli address key-hash --payment-verification-key-file $CLIWALLET/$WALLET.vkey`
deadlineSlot=`cardano-cli query tip $TESTNET | jq '.slot'`

bhash="5472"

cat ../datum-and-redeemers/TreasuryAction-Commit-Example-01.json | jq --arg myPKH "$myPKH" --argjson gAmount "$GIMBALS_COMMIT_AMOUNT" --argjson aAmount "$ADA_COMMIT_AMOUNT" --argjson slot "$deadlineSlot" --arg bhash "$bhash"  '.fields[].fields[0].bytes = $myPKH | .fields[].fields[1].int = $aAmount | .fields[].fields[2].int = $gAmount | .fields[].fields[3].int = $slot | .fields[].fields[4].bytes = $bhash' > ../datum-and-redeemers/commitBountyTreasuryRedeemer_$bhash.json

cat ../datum-and-redeemers/BountyEscrowDatum-Example-01.json | jq --arg myPKH "$myPKH" --argjson gAmount "$GIMBALS_COMMIT_AMOUNT" --argjson aAmount "$ADA_COMMIT_AMOUNT" --argjson slot "$deadlineSlot" --arg bhash "$bhash" '.fields[0].bytes = $myPKH | .fields[1].int = $aAmount | .fields[2].int = $gAmount | .fields[3].int = $slot | .fields[4].bytes = $bhash' > ../datum-and-redeemers/commitBountyEscrowDatum_$bhash.json

cardano-cli transaction build \
--babbage-era \
$TESTNET \
--tx-in $AUTH_UTXO \
--tx-in $ADA_UTXO \
--tx-in-collateral $ADA_UTXO \
--tx-in $TCONTRACT_UTXO \
--tx-in-script-file ../output/treasury-gbte-v2-with-bounty-hash2.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file ../datum-and-redeemers/commitBountyTreasuryRedeemer_$bhash.json \
--tx-out $(cat ../output/treasury-gbte-v2-with-bounty-hash2.addr)+"$LOVELACE_TO_TREASURY + $GIMBALS_TO_TREASURY $GIMBAL_ASSET" \
--tx-out-inline-datum-file ../datum-and-redeemers/datum-treasury-new.json \
--tx-out $(cat ../output/escrow-gbte-v2-with-bounty-hash2.addr)+"$ADA_COMMIT_AMOUNT + $GIMBALS_COMMIT_AMOUNT $GIMBAL_ASSET + 1 $AUTH_TOKEN" \
--tx-out-inline-datum-file ../datum-and-redeemers/commitBountyEscrowDatum_$bhash.json \
--change-address $(cat $CLIWALLET/$WALLET.addr) \
--protocol-params-file protocol.json \
--out-file try-5b.draft \

cardano-cli transaction sign --tx-body-file try-5b.draft --signing-key-file $CLIWALLET/$WALLET.skey $TESTNET --out-file try-5b.sign 

cardano-cli transaction submit --tx-file try-5b.sign $TESTNET



