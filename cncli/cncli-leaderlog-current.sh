#!/bin/bash

IP_NODE=$1
PORT_NODE=$2
POOL_ID=$3

export CARDANO_NODE_SOCKET_PATH=/opt/cardano/ipc/node.socket

/home/ubuntu/cncli/target/release/cncli sync --host $IP_NODE --port $PORT_NODE --db /home/ubuntu/db/cncli.db --no-service
echo "BCSH"
SNAPSHOT=$(/usr/bin/cardano-cli query stake-snapshot --stake-pool-id $POOL_ID --mainnet)
POOL_STAKE=$(echo "$SNAPSHOT" | grep -oP '(?<=    "poolStakeSet": )\d+(?=,?)')
ACTIVE_STAKE=$(echo "$SNAPSHOT" | grep -oP '(?<=    "activeStakeSet": )\d+(?=,?)')
BCSH=`/home/ubuntu/cncli/target/release/cncli leaderlog --db /home/ubuntu/db/cncli.db --pool-id $POOL_ID --pool-vrf-skey /home/ubuntu/config/keys/pool/*.vrf.skey --byron-genesis /home/ubuntu/config/mainnet-byron-genesis.json --shelley-genesis /home/ubuntu/config/mainnet-shelley-genesis.json --pool-stake $POOL_STAKE --active-stake $ACTIVE_STAKE --ledger-set current`
echo $BCSH | jq .

EPOCH=`echo $BCSH | jq .epoch`
echo "\`Epoch $EPOCH\` 🧙🔮:"

SLOTS=`echo $BCSH | jq .epochSlots`
IDEAL=`echo $BCSH | jq .epochSlotsIdeal`
PERFORMANCE=`echo $BCSH | jq .maxPerformance`
echo "\`BCSH  - $SLOTS \`🎰\`,  $PERFORMANCE% \`🍀max, \`$IDEAL\` 🧱ideal"
