
## Run a Relay Node

```
VERSION=1.24.2-rev2

# Setup a custom subnet
docker network create --subnet=172.18.0.0/16 cardano

# Setup the Relay topology
# The Relay connects to the world + Producer
# Valency is a boolean - 0 disables the address

cat << EOF > cardano/config/mainnet-relay-topology.json
{
  "Producers": [
    {
      "addr": "relays-new.cardano-mainnet.iohk.io",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "172.18.0.20",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

# Setup the config volume

docker rm -f relay
docker volume rm -f cardano-relay-config
docker run --name=tmp -v cardano-relay-config:/var/cardano/config debian
docker cp cardano/config/mainnet-relay-topology.json tmp:/var/cardano/config/mainnet-topology.json
docker rm -f tmp

docker rm -f relay
docker run --detach \
    --name=relay \
    --restart=always \
    --ip 172.18.0.10 \
    --network cardano \
    --hostname="relay" \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -e CARDANO_CUSTOM_PEERS="172.18.0.20:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -v cardano-relay-config:/var/cardano/config  \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano:${VERSION} run

docker logs -f relay

docker exec -it relay gLiveView
docker exec -it relay tail -n 12 /opt/cardano/logs/topologyUpdater_lastresult.json
docker exec -it relay cat /opt/cardano/config/mainnet-topology.json

# Access the EKG metric
docker exec -it relay curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it relay curl 127.0.0.1:12798/metrics | sort
```

### Run a Producer Node

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > cardano/config/mainnet-prod-topology.json
{
  "Producers": [
    {
      "addr": "172.18.0.10",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

# Setup the config volume

docker rm -f prod
docker volume rm -f cardano-prod-config
docker run --name=tmp -v cardano-prod-config:/var/cardano/config debian
docker cp cardano/config/mainnet-prod-topology.json tmp:/var/cardano/config/mainnet-topology.json
docker cp cardano/config/keys tmp:/var/cardano/config/keys
docker run -it --rm -v cardano-prod-config:/var/cardano/config debian find /var/cardano/config -type f | sort
docker rm -f tmp

# Run the Producer node

docker rm -f prod
docker run --detach \
    --name=prod \
    --restart=always \
    --ip 172.18.0.20 \
    --network cardano \
    --hostname="prod" \
    -e CARDANO_BLOCK_PRODUCER=false \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -e CARDANO_SHELLY_KES_KEY="/var/cardano/config/keys/pool/kes.skey" \
    -e CARDANO_SHELLY_VRF_KEY="/var/cardano/config/keys/pool/vrf.skey" \
    -e CARDANO_SHELLY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/pool/node.cert" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \
    nessusio/cardano:${VERSION} run

docker logs -f prod

docker exec -it prod gLiveView

# Access the EKG metric
docker exec -it prod curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it prod curl 127.0.0.1:12798/metrics | sort
```

## Run a Relay on MacOS

```
VERSION=dev

# Run the Relay node

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3002:3002 \
    --hostname="relay" \
    -e CARDANO_PORT=3002 \
    -v ~/cardano/data:/opt/cardano/data \
    nessusio/cardano:${VERSION} run

docker logs -f relay

docker kill --signal=SIGINT relay

docker rm -f relay
docker run -it \
    --name=relay \
    --entrypoint=bash \
    -v ~/cardano/data:/opt/cardano/data \
    nessusio/cardano:${VERSION}
  
cardano-node run \
  --config /opt/cardano/config/mainnet-config.json \
  --topology /opt/cardano/config/mainnet-topology.json \
  --database-path /opt/cardano/data \
  --socket-path /opt/cardano/data/socket 
  --host-addr 0.0.0.0 --port 3002
```

