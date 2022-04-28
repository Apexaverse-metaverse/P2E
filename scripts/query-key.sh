#!/bin/bash
addrFile=$(realpath "${WALLET}/wallet.addr")
cardano-cli query utxo $MAGIC --address $(cat $addrFile)
