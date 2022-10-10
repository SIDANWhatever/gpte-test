## 2022-10-10
In `GBTE.TreasuryValidator`:
- change the condition in `checkValueToBountyContract` so that bounty amounts must match what is specified in redeemer
- change `treasuryGets` functions to reference redeemer directly
- contract is more strict, using equality instead of GEQ
- implement basic parameterized redeemer
