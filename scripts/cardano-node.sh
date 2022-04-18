#!/bin/bash
cardano-node run \
	 --topology $SCRIPT_DIR/$NODE_DIR/topology.json \
	 --database-path $SCRIPT_DIR/$NODE_DIR/db \
	 --socket-path $SCRIPT_DIR/$NODE_DIR/node.socket \
	 --host-addr 127.0.0.1 \
	 --port 3001 \
	 --config $SCRIPT_DIR/$NODE_DIR/config.json
