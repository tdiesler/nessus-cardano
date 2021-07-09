## Running a Relay Node on the Testnet

```
docker rm -f alonzo-relay
docker run --detach \
    --name=alonzo-relay \
    --hostname=alonzo-relay
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-white \
    -v alonzo-data:/opt/cardano/data \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -f alonzo-relay

docker exec -it alonzo-relay gLiveView
```
