#!/bin/bash
if [[ $1 == "--rts" ]]; then
	RTS="-N -I0.1 -Iw7200 -A128m -n4m -F0.7 -H3500M -O4500M -T -S"
fi
cardano-node run \
	 +RTS $RTS -RTS \
	 --topology $SCRIPT_DIR/$NODE_DIR/topology.json \
	 --database-path $SCRIPT_DIR/$NODE_DIR/db \
	 --socket-path $SCRIPT_DIR/$NODE_DIR/node.socket \
	 --host-addr 127.0.0.1 \
	 --port 3001 \
	 --config $SCRIPT_DIR/$NODE_DIR/config.json
