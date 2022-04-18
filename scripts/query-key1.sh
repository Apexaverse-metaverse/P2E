#!/bin/bash
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cardano-cli query utxo $MAGIC --address $(cat $scriptDir/testnet/01.addr)
