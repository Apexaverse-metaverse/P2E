#!/bin/bash
set -e
addrFile=$(realpath $2)
skeyFile=$(realpath $3)
ppFile="$SCRIPT_DIR/$NODE_DIR/protocol-parameters.json"
policyFile="$SCRIPT_DIR/../policy.plutus"
oref=$1

echo "address file: $addrFile"
echo "signing key file: $skeyFile"

cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

addr=$(cat $addrFile)

cd $SCRIPT_DIR/..
cabal repl -v0 <<EOF
    import Deploy (writePolicyFile)
    writePolicyFile "$addr" 
EOF
echo "policy file: $policyFile"
tnHex=$(
cabal repl -v0 <<EOF
    import Deploy (unsafeTokenNameHex)
    unsafeTokenNameHex
EOF
)
tnHex=$(echo $tnHex | xargs echo)
cd $SCRIPT_DIR

unsignedFile="$SCRIPT_DIR/$NODE_DIR/tx.unsigned"
signedFile="$SCRIPT_DIR/$NODE_DIR/tx.signed"
unitFile="$SCRIPT_DIR/$NODE_DIR/unit.json"
pid=$(cardano-cli transaction policyid --script-file $policyFile)
v="10000000000 $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minting value: $v"
echo "address: $addr"

cardano-cli transaction build \
    $MAGIC \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --required-signer $skeyFile \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file $unitFile \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile
