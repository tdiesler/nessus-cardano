
## Running a Cardano Node

To get up and running with [Cardano](https://cardano.org) is easy, just spin it up a node like this ...

```
docker rm -f cardano
docker run --detach \
    --name=cardano \
    -p 3001:3001 \
    --restart=always \
    -v cardano-share:/var/cardano/share \
    -v cardano-data:/var/cardano/data \
    nessusio/cardano run \
        --topology /var/cardano/config/mainnet-topology.json \
        --config /var/cardano/config/mainnet-config.json \
        --socket-path /var/cardano/share/socket \
        --database-path /var/cardano/data \
        --host-addr 0.0.0.0 \
        --port 3001

docker logs -f cardano
```

Within a few hours it will have synched with the mainnet.

The [image](https://hub.docker.com/r/nessusio/cardano) above is built from source in a multiple stages like [this](images/node/docker/Dockerfile) to optimize its size.

## Running the Cardano CLI

We can also use the image above to run Cardano CLI commands.

For better readability, you may want to define an alias like this ...

```
alias cardano-cli="docker run --rm \
  -v cardano-share:/var/cardano/share \
  -e CARDANO_NODE_SOCKET_PATH=/var/cardano/share/socket \
  --entrypoint=cardano-cli \
  nessusio/cardano"
```

Then, access the cardano CLI as you normally would.

```
cardano-cli query tip --mainnet

{
    "blockNo": 5102089,
    "headerHash": "e5984f27d1d3b5dcc296b33ccd919a28618ff2d77513971bd316cffd35afecda",
    "slotNo": 16910651
}
```

Enjoy!
