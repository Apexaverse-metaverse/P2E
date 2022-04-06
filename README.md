# P2E

## Requeriments

[Nix](https://nixos.org)

## Minting the token

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
TotalSupply: 10000,000,000

Basic functions:
Mint, Burn
```
