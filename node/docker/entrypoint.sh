#!/bin/bash

# Optional ENV vars
#

CARDANO_CONFIG_BASE="/opt/cardano/config"

if [ -v $CARDANO_CONFIG ]; then
  CARDANO_CONFIG="$CARDANO_CONFIG_BASE/mainnet-config.json"
fi

if [ -v $CARDANO_TOPOLOGY ]; then
  CARDANO_TOPOLOGY="$CARDANO_CONFIG_BASE/mainnet-topology.json"
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

# Trap the SIGTERM signal sent from `docker stop` and 
# signal the cardano-node with SIGINT for graceful shutdown
#
# The wait may return before the gracefull shutdown is done
# Wait a little longer before will kill everything

sigtermHandler () {
  echo "Signalling for shutdown ..."
  kill -SIGINT $1
  wait $1
  sleep 2
}

printRunEnv () {

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
  
  if [ "$CARDANO_BLOCK_PRODUCER" = true ]; then
  
    if [ -v $CARDANO_SHELLEY_KES_KEY ]; then
      CARDANO_SHELLEY_KES_KEY="$CARDANO_CONFIG_BASE/keys/kes.skey"
    fi
    
    if [ -v $CARDANO_SHELLEY_VRF_KEY ]; then
      CARDANO_SHELLEY_VRF_KEY="$CARDANO_CONFIG_BASE/keys/vrf.skey"
    fi
    
    if [ -v $CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE ]; then
      CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="$CARDANO_CONFIG_BASE/keys/node.cert"
    fi
  
    echo "CARDANO_SHELLEY_KES_KEY=$CARDANO_SHELLEY_KES_KEY"
    echo "CARDANO_SHELLEY_VRF_KEY=$CARDANO_SHELLEY_VRF_KEY"
    echo "CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE=$CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE"
  fi
}

writeRootEnv () {
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
}

topologyUpdaterCron () {

  # Install the topology updater cron job
  
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
}

runRelayNode () {

  # Run the child process in the background so that we can have its pid
   
  cardano-node run \
    --config $CARDANO_CONFIG \
    --topology $CARDANO_TOPOLOGY \
    --database-path $CARDANO_DATABASE_PATH \
    --socket-path $CARDANO_SOCKET_PATH \
    --host-addr $CARDANO_BIND_ADDR \
    --port $CARDANO_PORT &
}

runBlockProducerNode () {

  # Run the child process in the background so that we can have its pid
   
  cardano-node run \
    --config $CARDANO_CONFIG \
    --topology $CARDANO_TOPOLOGY \
    --database-path $CARDANO_DATABASE_PATH \
    --socket-path $CARDANO_SOCKET_PATH \
    --host-addr $CARDANO_BIND_ADDR \
    --port $CARDANO_PORT \
    --shelley-kes-key $CARDANO_SHELLEY_KES_KEY \
    --shelley-vrf-key $CARDANO_SHELLEY_VRF_KEY \
    --shelley-operational-certificate $CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE &
}

trapSIGTERM () {

  # Trap the SIGTERM signal
  PID="$!"
  trap "sigtermHandler $PID" SIGTERM

  # Wait for the child process to terminate
  wait $PID
}

# Command Switch ######################################################################################################

if [ "$1" == "run" ]; then

  printRunEnv
  writeRootEnv

  if [ "$CARDANO_BLOCK_PRODUCER" = true ]; then
      
    runBlockProducerNode
    
  else
  
    topologyUpdaterCron
    
    runRelayNode
  fi
  
  trapSIGTERM
  
elif [ "$1" == "cardano-cli" ]; then

  exec "$@"

elif [ "$1" == "ledger-state" ]; then

  echo "Generating $CARDANO_DATABASE_PATH/ledger-state.json"
  
  cardano-cli query ledger-state \
      --mainnet \
      --allegra-era \
      --out-file $CARDANO_DATABASE_PATH/ledger-state.json

elif [ "$1" == "sigma" ]; then

  CMD="/usr/bin/python3 /root/pooltool.io/leaderLogs/getSigma.py"
  ARGS=`echo $* | cut -d' ' -f2-`
  
  eval "$CMD $ARGS"

elif [ "$1" == "leader-logs" ]; then

  CMD="/usr/bin/python3 /root/pooltool.io/leaderLogs/leaderLogs.py"
  ARGS=`echo $* | cut -d' ' -f2-`
  
  eval "$CMD $ARGS"

else
  
  echo "Nothing to do! Perhaps try [run|cardano-cli|ledger-state|sigma|leader-logs]"
  exit 1
  
fi
