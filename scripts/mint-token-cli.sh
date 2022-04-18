#!/bin/bash
set -e
scriptDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
addrFile="$scriptDir/testnet/01.addr"
skeyFile="$scriptDir/testnet/01.skey"
policyFile="$scriptDir/../policy.plutus"
oref=$1

echo "address file: $addrFile"
echo "signing key file: $skeyFile"

ppFile=testnet/protocol-parameters.json
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

cd $scriptDir/..
cabal repl -v0 <<< 'writePolicyFile' 
echo "policy file: $policyFile"
tnHex=$(cabal repl -v0 <<< 'unsafeTokenNameHex')
tnHex=$(echo $tnHex | xargs echo)
cd $scriptDir

unsignedFile="$scriptDir/testnet/tx.unsigned"
signedFile="$scriptDir/testnet/tx.signed"
pid=$(cardano-cli transaction policyid --script-file $policyFile)
addr=$(cat $addrFile)
v="3000000000 $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

cardano-cli transaction build \
    $MAGIC \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file testnet/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile

cardano-cli transaction submit \
    $MAGIC \
    --tx-file $signedFile
