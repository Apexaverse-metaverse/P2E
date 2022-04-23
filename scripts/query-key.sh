#!/bin/bash
ADDR_FILE=$(realpath $1)
cardano-cli query utxo $MAGIC --address $(cat $ADDR_FILE)
