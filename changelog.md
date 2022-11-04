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
