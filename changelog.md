## 2022-12-05
- Update Type names in project

## 2022-11-30
#### In `GBTE.TreasuryValidator`:
- Add check bounty details and output datum to escrow are the same
#### In `GBTE.EscrowValidator`:
- Add `escrowValidatorHash` function
- Add check for update datum ada/gimbal/deadline is greater or equal
- Change `outputFulFillsValue` is same as in new datum
#### In `GBTE.Compiler`:
- Change bountyContractHash from string to function (get rid of cardano-cli transaction policyid)
#### In `Script`
- Add minimal bash scripts for testing datum-logic
  - no reference scripts
  - no copy paste utxos
  - lockTreasury.sh/commitBounty.sh/updateBounty.sh/unlockAllTreasury.sh/balance.sh/cbalance.sh/getTxFunc.sh

## 2022-11-04
- Change project name to GPTE
#### In `GBTE.TreasuryValidator`:
- Add simple check that incoming and outgoing datum are equal in Commitment Tx
#### Scripts
- Add `000-project-variables.sh` for easier portability

## 2022-10-10
#### In `GBTE.TreasuryValidator`:
- change the condition in `checkValueToBountyContract` so that bounty amounts must match what is specified in redeemer
- change `treasuryGets` functions to reference redeemer directly
- contract is more strict, using equality instead of GEQ
- implement basic parameterized redeemer
