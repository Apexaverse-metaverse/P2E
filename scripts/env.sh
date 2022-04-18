#!/bin/bash
export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

NODE_DIR_TESTNET="testnet"
MAGIC_TESTNET="--testnet-magic 1097911063"

if [[ $1 == "testnet" ]]; then
	export MAGIC="${MAGIC:-$MAGIC_TESTNET}" 
	export NODE_DIR=${NODE_DIR:-$NODE_DIR_TESTNET}
fi

export PATH=$PATH:$SCRIPT_DIR
export CARDANO_NODE_SOCKET_PATH=$SCRIPT_DIR/$NODE_DIR/node.socket
