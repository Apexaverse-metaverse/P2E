# P2E

## Requisites

[Nix](https://nixos.org)

## Minting the token

First create wallet files (skey and addr) or use the given wallet in scripts/testnet folder.

> Remember to have enough ADA funds in the wallet address. You can use [faucet to add fake funds in testnet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

[Tutorial how to create wallets](https://youtu.be/ABtffZPoUqU?list=PLNEK_Ejlx3x2zxcfoVGARFExzOHwXFCCL&t=248)

Then

```bash
# Run nix-shell
nix-shell

# Load environment
source scripts/env.sh testnet

# Run cardano-node
cardano-node.sh

query-key.sh scripts/testnet/wallet.addr
# Get the data from last command and replace the string below
mint-token-cli.sh "TxHash#TxIx" scripts/testnet/wallet.addr scripts/testnet/wallet.skey

# Query again to see the token transaction
query-key.sh wallet.addr
```

## Tokenomics

```
TokenName: APEXAVERSE
TokenSymbol: $AXV
TotalSupply: 10,000,000,000

Basic functions:
Mint, Burn
```
