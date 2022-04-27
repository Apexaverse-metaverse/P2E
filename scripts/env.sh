#!/bin/bash
export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

NODE_DIR_TESTNET="testnet"
MAGIC_TESTNET="--testnet-magic 1097911063"
NODE_DIR_MAINNET="mainnet"
MAGIC_MAINNET="--mainnet"

if [[ $1 == "testnet" ]]; then
	export MAGIC="${MAGIC:-$MAGIC_TESTNET}" 
	export NODE_DIR=${NODE_DIR:-$NODE_DIR_TESTNET}
elif [[ $1 == "mainnet" ]]; then
	export MAGIC="${MAGIC:-$MAGIC_MAINNET}" 
	export NODE_DIR=${NODE_DIR:-$NODE_DIR_MAINNET}
fi

export PATH=$PATH:$SCRIPT_DIR
export CARDANO_NODE_SOCKET_PATH=$SCRIPT_DIR/$NODE_DIR/node.socket
export CARDANO_ENV=$1
