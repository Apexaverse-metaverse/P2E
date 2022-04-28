#!/bin/bash
set -e
addrFile=$(realpath "${WALLET}/wallet.addr")
skeyFile=$(realpath "${WALLET}/wallet.skey")
vkeyFile=$(realpath "${WALLET}/wallet.vkey")
faucetLink="https://testnets.cardano.org/en/testnets/cardano/tools/faucet"
cardano-cli address key-gen --verification-key-file $vkeyFile --signing-key-file $skeyFile
cardano-cli address build --payment-verification-key-file $vkeyFile $MAGIC --out-file $addrFile

echo "Please send now funds to cover the mint fees to this address:"
echo $(cat $addrFile)
echo "You can also use Faucet to add fake funds in testnet: $faucetLink"
