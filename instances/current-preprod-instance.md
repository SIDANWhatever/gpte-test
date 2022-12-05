# Current Preprod Instance of GPTE
Updated 2022-12-05

## Project Branches:
- `gpte-plutus-v2` on branch `current-preprod`
- `gpte-front-end` on branch `current-preprod`

## Tokens:
- Issuer PolicyID is: `94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3`
- Contributor PolicyID is: `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`
- Project Token is `tGimbal` with AssetID `fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c`

## Project Variables
In [/scripts/000-project-variables.sh](../scripts/000-project-variables.sh)

```bash
export TREASURY_ADDRESS=addr_test1wpr838k666akr3p5k8tfcdfenrlzpueq2j87tp7zkx6mh8qm8maf8
export ESCROW_ADDRESS=addr_test1wrlh2k4wqjhyjxvg4hnhtq8uqpzp99v97c9nm6075rjyhkqtjphn5
export REFERENCE_ADDRESS=addr_test1qqe5wnjzkhrgfvntj3dndzml7003h0n5ezen924qjrrglv6648u33jzvq2msza6gyqdcnau0njhav2sv46adkc9c8wdqx5aas8
export REFERENCE_UTXO_TREASURY_SCRIPT=8502fe3617b70d12fc65c8c17dab8483f5a7f847e0fba0376fe7abacf4773262#1
export REFERENCE_UTXO_ESCROW_SCRIPT=8502fe3617b70d12fc65c8c17dab8483f5a7f847e0fba0376fe7abacf4773262#2
export GIMBAL_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"
```

## Instance Parameters
In [/src/GPTE/Compiler.hs](../src/GPTE/Compiler.hs)

```haskell
EscrowParam {
    projectTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c",
    projectTokenName = "tGimbal",
    contribTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784",
    treasuryIssuerPolicyId = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
}

TreasuryParam {
    tContribTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784",
    escrowContractHash = Escrow.escrowValidatorHash escrowParam,
    tProjectTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c",
    tProjectTokenName = "tGimbal",
    tIssuerPolicyId = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
}
```
