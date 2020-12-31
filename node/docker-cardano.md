
## Build the Image

```
# Build respective single arch image
./node/scripts/build-docker.sh

CARDANO=1.24.2
VERSION="$CARDANO-rev1"

docker manifest create nessusio/cardano:$VERSION \
  --amend nessusio/cardano:$VERSION-amd64 \
  --amend nessusio/cardano:$VERSION-arm64

docker manifest push nessusio/cardano:$VERSION

docker manifest create nessusio/cardano:$CARDANO \
  --amend nessusio/cardano:$CARDANO-amd64 \
  --amend nessusio/cardano:$CARDANO-arm64

docker manifest push nessusio/cardano:$CARDANO

docker manifest create nessusio/cardano \
  --amend nessusio/cardano:latest-amd64 \
  --amend nessusio/cardano:latest-arm64

docker manifest push nessusio/cardano
```


## Run the Relay Node

```
docker network create --subnet=172.18.0.0/16 cardano

RELAY_PUBLIC_IP=`curl -s ipinfo.io/ip`
echo "RELAY_PUBLIC_IP: ${RELAY_PUBLIC_IP}"

# Setup the Relay topology
# The Relay connects to the World + Block Producer
# Valency is a boolean - 0 disables the address

mkdir -p cardano/config
cat << EOF > cardano/config/mainnet-relay-topology.json
{
  "Producers": [
    {
      "addr": "relays-new.cardano-mainnet.iohk.io",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "172.18.0.11",
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

# Run the Relay node

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    --ip 172.18.0.10 \
    --network cardano \
    --hostname="cdrelay" \
    -e CARDANO_PUBLIC_IP="$RELAY_PUBLIC_IP" \
    -e CARDANO_CUSTOM_PEERS="172.18.0.11:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -v cardano-relay-config:/var/cardano/config  \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay

docker exec -it relay bash
docker exec -it relay gLiveView
docker exec -it relay topologyUpdater
docker exec -it relay tail -f /opt/cardano/logs/topologyUpdater_lastresult.json
docker exec -it relay cat /var/cardano/config/mainnet-topology.json

# Access the EKG metric
docker exec -it relay curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it relay curl 127.0.0.1:12798/metrics | sort
```

## Run the Producer Node

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
docker run -it --rm -v cardano-prod-config:/var/cardano/config centos find /var/cardano/config -type f | sort
docker rm -f tmp

docker rm -f prod
docker run --detach \
    --name=prod \
    --ip 172.18.0.11 \
    --network cardano \
    --hostname="cdprod" \
    -e CARDANO_BLOCK_PRODUCER="true" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -e CARDANO_SHELLY_KES_KEY="/var/cardano/config/keys/pool/kes.skey" \
    -e CARDANO_SHELLY_VRF_KEY="/var/cardano/config/keys/pool/vrf.skey" \
    -e CARDANO_SHELLY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/pool/node.cert" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \
    nessusio/cardano run

docker logs -f prod

docker exec -it prod bash
docker exec -it prod gLiveView

# Access the EKG metric
docker exec -it prod curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it prod curl 127.0.0.1:12798/metrics | sort
```

