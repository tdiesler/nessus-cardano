
## Fetch the config files

```
cd images/node
mkdir -p docker/config

curl -sLo docker/config/mainnet-config.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
curl -sLo docker/config/mainnet-topology.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-topology.json
curl -sLo docker/config/mainnet-shelley-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json
```

## Build the Cardano Image


```
VERSION=1.24.2

docker build -t nessusio/cardano docker
  
docker tag nessusio/cardano nessusio/cardano:$VERSION-dev
```

## Run the Cardano Node

```
# Create private network 
docker network create --subnet=172.18.0.0/16 cardano
    --network=cardano \
    --ip=172.18.0.10 \

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -v cardano-share:/var/cardano/share \
    -v cardano-data:/var/cardano/data \
    nessusio/cardano run \
        --topology /var/cardano/config/mainnet-topology.json \
        --config /var/cardano/config/mainnet-config.json \
        --socket-path /var/cardano/share/socket \
        --database-path /var/cardano/data \
        --host-addr 0.0.0.0 \
        --port 3001

docker logs -f relay

docker exec -it relay gLiveView
```
