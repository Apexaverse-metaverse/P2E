#!/bin/bash
signedFile="$SCRIPT_DIR/$NODE_DIR/tx.signed"
cardano-cli transaction submit \
      $MAGIC \
      --tx-file $signedFile
