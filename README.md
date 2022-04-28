# P2E

## Requisites

- [Nix](https://nixos.org)
- [IOHK Cache](https://github.com/input-output-hk/plutus/blob/master/README.adoc#nix-advice)

## Minting the token

```bash
# Run nix-shell
nix-shell

# Load environment (testnet or mainnet)
source scripts/env.sh testnet

# Run cardano-node in background (or other terminal)
cardano-node.sh &

# Define path to wallet
export WALLET=path/to/wallet/files

# Create the wallet
create-wallet.sh 
# Transfer covering funds, according given instructions
query-key.sh
# Get the data from last command and replace the string below
mint-token-cli.sh "TxHash#TxIx"

# Submit the transaction
submit.sh

# Query again to see the token transaction
query-key.sh
```

## Tokenomics

```
Ticker and asset name: AXV
Name: Apexaverse
Description: The intersection of the Metaverse, P2E, Web 3.0 and NFTs on Cardano Blockchain.
Decimals: 6
URL: https://apexaverse.com
Total Supply and Max Supply: 10,000,000,000.000000

Basic functions:
Mint, Burn
```

Testnet: [cb1229abf5925361df967077c6e51901061b17d9581ce25fd66f213f](https://testnet.cardanoscan.io/tokenPolicy/cb1229abf5925361df967077c6e51901061b17d9581ce25fd66f213f)
Mainnet: [bfe8c533adabcb6fbc75da0c9fd9777c81f4706b02af69ce6aea880d](https://cardanoscan.io/tokenPolicy/bfe8c533adabcb6fbc75da0c9fd9777c81f4706b02af69ce6aea880d)
