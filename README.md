# P2E

## Requeriments

[Nix](https://nixos.org)

## Minting the token

First create wallet files: 01.skey, 01.vkey, 01.vkeyhash and 01.addr in scripts/testnet (or mainnet), or use the given wallet.

:warn: Remember to have enough ADA funds in the wallet address. You can use [faucet to add fake funds in testnet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

[Tutorial](https://youtu.be/ABtffZPoUqU?list=PLNEK_Ejlx3x2zxcfoVGARFExzOHwXFCCL&t=248)

Then

```bash
# Run nix-shell
nix-shell

# Load environment
source scripts/env.sh testnet

# Run cardano-node
cardano-node.sh

query-key1.sh
# Get the data from last command and replace the string below
mint-token-cli.sh "TxHash#TxIx"

# Query again to see the token transaction
query-key1.sh
```

## Tokenomics

```
TokenName: APEXAVERSE
TokenSymbol: $AXV
TotalSupply: 10,000,000,000

Basic functions:
Mint, Burn
```
