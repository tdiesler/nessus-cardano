#!/bin/bash

# Optional ENV vars
#

if [ -v $CARDANO_CONFIG ]; then
  CARDANO_CONFIG="/opt/cardano/config/mainnet-config.json"
fi

if [ -v $CARDANO_TOPOLOGY ]; then
  CARDANO_TOPOLOGY="/opt/cardano/config/mainnet-topology.json"
fi

if [ -v $CARDANO_DATABASE_PATH ]; then
  CARDANO_DATABASE_PATH="/opt/cardano/data"
fi

if [ -v $CARDANO_SOCKET_PATH ]; then
  CARDANO_SOCKET_PATH="$CARDANO_DATABASE_PATH/socket"
fi

if [ -v $CARDANO_LOG_DIR ]; then
  CARDANO_LOG_DIR="/opt/cardano/logs"
fi

if [ -v $CARDANO_BIND_ADDR ]; then
  CARDANO_BIND_ADDR="0.0.0.0"
fi

if [ -v $CARDANO_PORT ]; then
  CARDANO_PORT=3001
fi

if [ -v $CARDANO_UPDATE_TOPOLOGY ]; then
  CARDANO_UPDATE_TOPOLOGY=false
fi

if [ -v $CARDANO_BLOCK_PRODUCER ]; then
  CARDANO_BLOCK_PRODUCER=false
fi

echo "CARDANO_BIND_ADDR=$CARDANO_BIND_ADDR"
echo "CARDANO_BLOCK_PRODUCER=$CARDANO_BLOCK_PRODUCER"
echo "CARDANO_CONFIG=$CARDANO_CONFIG"
echo "CARDANO_CUSTOM_PEERS=$CARDANO_CUSTOM_PEERS"
echo "CARDANO_DATABASE_PATH=$CARDANO_DATABASE_PATH"
echo "CARDANO_LOG_DIR=$CARDANO_LOG_DIR"
echo "CARDANO_PORT=$CARDANO_PORT"
echo "CARDANO_PUBLIC_IP=$CARDANO_PUBLIC_IP"
echo "CARDANO_SOCKET_PATH=$CARDANO_SOCKET_PATH"
echo "CARDANO_TOPOLOGY=$CARDANO_TOPOLOGY"
echo "CARDANO_UPDATE_TOPOLOGY=$CARDANO_UPDATE_TOPOLOGY"

# Block Producer
#

if [ "$CARDANO_BLOCK_PRODUCER" = true ]; then

  if [ -v $CARDANO_SHELLY_KES_KEY ]; then
    CARDANO_SHELLY_KES_KEY="$CARDANO_CONFIG/keys/kes.skey"
  fi
  
  if [ -v $CARDANO_SHELLY_VRF_KEY ]; then
    CARDANO_SHELLY_VRF_KEY="$CARDANO_CONFIG/keys/vrf.skey"
  fi
  
  if [ -v $CARDANO_SHELLY_OPERATIONAL_CERTIFICATE ]; then
    CARDANO_SHELLY_OPERATIONAL_CERTIFICATE="$CARDANO_CONFIG/keys/node.cert"
  fi

  echo "CARDANO_SHELLY_KES_KEY=$CARDANO_SHELLY_KES_KEY"
  echo "CARDANO_SHELLY_VRF_KEY=$CARDANO_SHELLY_VRF_KEY"
  echo "CARDANO_SHELLY_OPERATIONAL_CERTIFICATE=$CARDANO_SHELLY_OPERATIONAL_CERTIFICATE"
fi

if [ -z $1 ]; then
  echo "Nothing to do! Perhaps try [run]"
  exit 1
fi

cat << EOF > /root/env
#!/usr/bin/env bash

# Docker run ENV vars 
export CARDANO_BIND_ADDR="$CARDANO_BIND_ADDR"
export CARDANO_BLOCK_PRODUCER="$CARDANO_BLOCK_PRODUCER"
export CARDANO_CONFIG="$CARDANO_CONFIG"
export CARDANO_DATABASE_PATH="$CARDANO_DATABASE_PATH"
export CARDANO_LOG_DIR="$CARDANO_LOG_DIR"
export CARDANO_PORT="$CARDANO_PORT"
export CARDANO_SOCKET_PATH="$CARDANO_SOCKET_PATH"
export CARDANO_TOPOLOGY="$CARDANO_TOPOLOGY"
export CARDANO_UPDATE_TOPOLOGY="$CARDANO_UPDATE_TOPOLOGY"

# Mapping for gLiveView
export CCLI="/usr/local/bin/cardano-cli"
export CONFIG="$CARDANO_CONFIG"
export TOPOLOGY="$CARDANO_TOPOLOGY"
export SOCKET="$CARDANO_SOCKET_PATH"
export LOG_DIR="$CARDANO_LOG_DIR"
export CNODE_PORT="$CARDANO_PORT"

# Mapping for topologyUpdater
export CARDANO_PUBLIC_IP="$CARDANO_PUBLIC_IP"
export CARDANO_CUSTOM_PEERS="$CARDANO_CUSTOM_PEERS"
EOF

if [ "$1" == "run" ]; then

  if [ "$CARDANO_BLOCK_PRODUCER" = true ]; then
  
    cardano-node run \
      --config $CARDANO_CONFIG \
      --topology $CARDANO_TOPOLOGY \
      --database-path $CARDANO_DATABASE_PATH \
      --socket-path $CARDANO_SOCKET_PATH \
      --host-addr $CARDANO_BIND_ADDR \
      --port $CARDANO_PORT \
      --shelley-kes-key $CARDANO_SHELLY_KES_KEY \
      --shelley-vrf-key $CARDANO_SHELLY_VRF_KEY \
      --shelley-operational-certificate $CARDANO_SHELLY_OPERATIONAL_CERTIFICATE
      
  else
  
    # Install the topology updater
    
    TARGET="/etc/cron.d/topologyUpdater"
    if [ "$CARDANO_UPDATE_TOPOLOGY" = true ] && [ ! -f "$TARGET" ]; then
      
      MIN=`date +"%M"`
      MIN=$(($(($(($MIN + 15)) / 15)) * 15))
      if [ $MIN = 60 ]; then MIN=0; fi
      
      CRONJOB="$MIN * * * *  root  /usr/local/bin/topologyUpdater"
      echo "Installing $CRONJOB"
      
      echo "$CRONJOB" > $TARGET
      
      service cron start
      
    fi
    
    cardano-node run \
      --config $CARDANO_CONFIG \
      --topology $CARDANO_TOPOLOGY \
      --database-path $CARDANO_DATABASE_PATH \
      --socket-path $CARDANO_SOCKET_PATH \
      --host-addr $CARDANO_BIND_ADDR \
      --port $CARDANO_PORT
  fi
  
fi
