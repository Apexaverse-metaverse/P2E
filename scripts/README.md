# Minting token

```bash
# Run cardano-node
# Change paths on env.sh
source env.sh
./query-key1.sh
# Get the data from last command and replace the string below
./mint-token-cli.sh "TxHash#TxIx"
# Query again to see the token transaction
./query-key1.sh
