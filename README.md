
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
    -v shelly-data:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

This works on [x86_64](https://hub.docker.com/r/nessusio/cardano/tags?name=amd64) and [arm64](https://hub.docker.com/r/nessusio/cardano/tags?name=arm64).

The [nessusio/cardano](https://hub.docker.com/r/nessusio/cardano) image is built from source in a multiple stages like [this](node/docker/Dockerfile).

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
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v shelly-data:/opt/cardano/data \
    nessusio/cardano run

$ docker exec -it relay tail /opt/cardano/logs/topologyUpdater_lastresult.json
{ "resultcode": "201", "datetime":"2021-01-10 18:30:06", "clientIp": "209.250.233.200", "iptype": 4, "msg": "nice to meet you" }
{ "resultcode": "203", "datetime":"2021-01-10 19:30:03", "clientIp": "209.250.233.200", "iptype": 4, "msg": "welcome to the topology" }
{ "resultcode": "204", "datetime":"2021-01-10 20:30:04", "clientIp": "209.250.233.200", "iptype": 4, "msg": "glad you're staying with us" }
```

The topologyUpdater is triggered by `CARDANO_UPDATE_TOPOLOGY`. Without it, the cron job is not installed.

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
Note, that our custom config lives in `/var/cardano/config` and not in the default location `/opt/cardano/config`.

```
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

A producer node is configured in the same way as a Relay node, except that it has some additional key/cert files configured and does not need to update its topology.

```
docker run --detach \
    --name=prod \
    -p 3001:3001 \
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

To determine the block producer's leader schedule (see below), we first need to obtain the current `ledger-state.json`

```
docker run -it --rm \
  -v shelly-data00:/opt/cardano/data \
  nessusio/cardano ledger-state

Generating /opt/cardano/data/ledger-state.json
```

## Getting the Sigma value

Sigma represents your pool's share of the active stake. 
It is the ratio of active stake for a given epoch divided by the total stake.

Details on how to get sigma are [here](https://github.com/papacarp/pooltool.io/tree/master/leaderLogs#getsigmapy-details)

```
docker run -it --rm \
  -v shelly-data00:/opt/cardano/data \
  nessusio/cardano sigma \
    --pool-id 9e8009b249142d80144dfb681984e08d96d51c2085e8bb6d9d1831d2 \
    --ledger /opt/cardano/data/ledger-state.json

building active stake
Sigma: 0.000233
```

## Getting Slot Leader Schedule

We can now obtain the leader schedule for our pool.

Details on how to do this are [here](https://github.com/papacarp/pooltool.io/tree/master/leaderLogs#leaderlogspy-details)

```
docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v shelly-data00:/opt/cardano/data \
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

Yes, we run [ASTOR Pool](http://astorpool.net). 

ASTOR charges the required minimum to provide a reliable stake pool.
Twenty-five percent of our margin goes to [charity](https://plant-for-the-planet.org).


Enjoy!
