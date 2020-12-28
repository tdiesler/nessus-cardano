
## Build the Image

```
CARDANO=1.24.2
ARCH=x86_64

./images/node/scripts/build-docker-$ARCH.sh

VERSION="$CARDANO-$ARCH-devel"
docker tag nessusio/cardano:$VERSION

docker run -it --rm nessusio/cardano
```

## Run the Relay Node

```
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
docker rm tmp

# Run the Relay node

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    --ip 172.18.0.10 \
    --network cardano \
    --hostname="cdrelay" \
    -e CARDANO_PUBLIC_IP="35.194.50.9" \
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
```


## Run the Producer Node

```
docker network create --subnet=172.18.0.0/16 cardano

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
docker rm tmp

docker rm -f prod
docker run --detach \
    --name=prod \
    --ip 172.18.0.11 \
    --network cardano \
    --hostname="cdprod" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \
    nessusio/cardano run

docker logs -f prod

docker exec -it prod bash
docker exec -it prod gLiveView
```

