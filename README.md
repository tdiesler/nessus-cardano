
# Nessus Cardano 

With Nessus Cardano we explore, praise, comment, contribute to various technical aspects of [Cardano](https://cardano.org). 
This is our contribution to "Making The World Work Better For All".

Initially, we focus on a "container first" approach for the Cardano node.

## Running a Relay Node

To get up and running with [Cardano](https://cardano.org), you can spin up a node like this ...

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v shelly-data:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

This works on [x86_64 and arm64](https://hub.docker.com/r/nessusio/cardano).

The [image](https://hub.docker.com/r/nessusio/cardano) above is built from source in a multiple stages like [this](node/docker/Dockerfile).

## Accessing the build-in gLiveView

The image has [gLiveView](https://github.com/cardano-community/guild-operators/blob/alpha/scripts/cnode-helper-scripts/gLiveView.sh) monitoring built in.

For a running container, you can do ...

```
docker exec -it relay gLiveView
```

<img src="node/docs/img/relay-glview.png" width="400">

## Accessing the build-in topology updater

There is currently no P2P module activated in cardano-1.24.2. Your node may call out to well known relay nodes, but you may never have incoming connections.
According to [this](https://github.com/cardano-community/guild-operators/blob/alpha/docs/Scripts/topologyupdater.md) it is necessary to update your topology 
every hour. At the time of writing, cardano-node doesn't do this on its own.

This functionality has been built into nessus/cardano as well. On startup, you should see a log similar to this ...

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    --hostname="relay" \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v shelly-data:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay

CARDANO_BIND_ADDR=0.0.0.0
CARDANO_BLOCK_PRODUCER=false
CARDANO_CONFIG=/opt/cardano/config/mainnet-config.json
CARDANO_CUSTOM_PEERS=
CARDANO_DATABASE_PATH=/opt/cardano/data
CARDANO_LOG_DIR=/opt/cardano/logs
CARDANO_PORT=3001
CARDANO_PUBLIC_IP=
CARDANO_SOCKET_PATH=/opt/cardano/data/socket
CARDANO_TOPOLOGY=/opt/cardano/config/mainnet-topology.json
CARDANO_UPDATE_TOPOLOGY=true

Installing 45 * * * *  root  /usr/local/bin/topologyUpdater
 * Starting periodic command scheduler cron

Listening on http://0.0.0.0:12798
[relay:cardano.node.networkMagic:Notice:5] [2021-01-04 16:35:06.61 UTC] NetworkMagic 764824073
[relay:cardano.node.basicInfo.protocol:Notice:5] [2021-01-04 16:35:06.61 UTC] Byron; Shelley
[relay:cardano.node.basicInfo.version:Notice:5] [2021-01-04 16:35:06.61 UTC] 1.24.2
```

The topologyUpdater is triggered by `CARDANO_UPDATE_TOPOLOGY`. Without it, the cron job is not installed.

The set of supported config options is documented [here](https://hub.docker.com/repository/docker/nessusio/cardano);

## External storage for block data

In this configuration, we map the node's `--database-path` to a mounted directory.

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

## Using custom configurations

Here we define a config volume called `cardano-relay-config`. It holds the `mainnet-topology.json` file that we setup externally.
Please note, that our custom config lives in `/var/cardano/config` 

```
# The IP for our Block Producer node
# This can be on an internal network
PRODUCER_IP=35.18.0.11

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
      "addr": "$PRODUCER_IP",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

# Setup the config volume

docker volume rm -f cardano-relay-config
docker run --name=tmp -v cardano-relay-config:/var/cardano/config debian
docker cp cardano/config/mainnet-relay-topology.json tmp:/var/cardano/config/mainnet-topology.json
docker rm -f tmp

# Run the Relay node

docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -e CARDANO_CUSTOM_PEERS="$PRODUCER_IP:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -v cardano-relay-config:/var/cardano/config  \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

## Running a Block Producer Node

A producer node is configured in the same way as a Relay node, except that it does not have a `CARDANO_PUBLIC_IP` and hence no topology updater job.
Additionally, it needs to have its keys/certificates configured.

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > cardano/config/mainnet-prod-topology.json
{
  "Producers": [
    {
      "addr": "myrelay.org",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

# Setup the config volume

docker volume rm -f cardano-prod-config
docker run --name=tmp -v cardano-prod-config:/var/cardano/config debian
docker cp cardano/config/mainnet-prod-topology.json tmp:/var/cardano/config/mainnet-topology.json
docker cp cardano/config/keys tmp:/var/cardano/config/keys
docker run -it --rm -v cardano-prod-config:/var/cardano/config centos find /var/cardano/config -type f | sort
docker rm -f tmp

# Run the Producer node

docker run --detach \
    --name=prod \
    --hostname="prod" \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -e CARDANO_SHELLY_KES_KEY="/var/cardano/config/keys/pool/kes.skey" \
    -e CARDANO_SHELLY_VRF_KEY="/var/cardano/config/keys/pool/vrf.skey" \
    -e CARDANO_SHELLY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/pool/node.cert" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \
    nessusio/cardano run

docker logs -f prod

docker exec -it prod gLiveView
```

## Running the Cardano CLI

We can also use the image to run Cardano CLI commands.

```
# Define the cardano-cli alias

alias cardano-cli="docker run -it --rm \
  -v /mnt/disks/data00:/opt/cardano/data \
  nessusio/cardano cardano-cli"

cardano-cli query tip --mainnet
{
    "blockNo": 5102089,
    "headerHash": "e5984f27d1d3b5dcc296b33ccd919a28618ff2d77513971bd316cffd35afecda",
    "slotNo": 16910651
}
```

## Generate the Ledger State

```
docker run -it --rm \
  -v shelly-data01:/opt/cardano/data \
  nessusio/cardano ledger-state

Generating /opt/cardano/data/ledger-state.json
```

## Get Sigma

```
docker run -it --rm \
  -v shelly-data01:/opt/cardano/data \
  nessusio/cardano sigma \
    --pool-id 9e8009b249142d80144dfb681984e08d96d51c2085e8bb6d9d1831d2 \
    --ledger /opt/cardano/data/ledger-state.json

building active stake
Sigma: 0.000233
```

## Get Slot Leader Schedule

```
docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v shelly-data01:/opt/cardano/data \
  nessusio/cardano leader-logs \
    --vrf-skey /var/cardano/local/keys/pool/vrf.skey \
    --sigma 0.000233 \
    --d-param 0.32 \
    --epoch 240

Checking leadership log for Epoch 240 [ d Param: 0.32 ]
2021-01-06 06:36:13 ==> Leader for 60682, Cumulative epoch blocks: 1
2021-01-06 09:16:55 ==> Leader for 70324, Cumulative epoch blocks: 2
2021-01-08 00:15:49 ==> Leader for 210658, Cumulative epoch blocks: 3
2021-01-09 23:28:40 ==> Leader for 380629, Cumulative epoch blocks: 4
```

## Is there a pool that runs this tech?

Yes, we run [ASTOR Pool](http://astorpool.org). 

ASTOR charges the required minimum to provide a reliable stake pool.
Twenty-five percent of our margin goes to [charity](https://plant-for-the-planet.org).


Enjoy!
