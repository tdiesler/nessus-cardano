
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

<img src="docs/img/relay-glview.png" width="400">

## Accessing the build-in topology updater

There is currently no P2P module activated in cardano-1.24.2. Your node may call out to well known relay nodes, but you may never have incoming connections.
According to [this](https://github.com/cardano-community/guild-operators/blob/alpha/docs/Scripts/topologyupdater.md) it is necessary to update your topology 
every hour - the node does not do this on its own.

This functionality has been built into nessus/cardano as well. On startup, you should see a log similar to this ...

```
# The external IP for our Relay node
RELAY_PUBLIC_IP=`curl -s ipinfo.io/ip`
echo "RELAY_PUBLIC_IP: ${RELAY_PUBLIC_IP}"

docker run --detach \
    --name=relay \
    --hostname="cdrelay" \
    -p 3001:3001 \
    -e CARDANO_PUBLIC_IP="$RELAY_PUBLIC_IP" \
    nessusio/cardano run

docker logs -f relay

CARDANO_CONFIG=/opt/cardano/config/mainnet-config.json
CARDANO_TOPOLOGY=/opt/cardano/config/mainnet-topology.json
CARDANO_DATABASE_PATH=/opt/cardano/data
CARDANO_SOCKET_PATH=/opt/cardano/data/socket
CARDANO_LOG_DIR=/opt/cardano/logs
CARDANO_BIND_ADDR=0.0.0.0
CARDANO_PORT=3001
CARDANO_PUBLIC_IP=35.239.77.33
CARDANO_CUSTOM_PEERS=

Installing 30 * * * *  root  /usr/local/bin/topologyUpdater
Starting periodic command scheduler: cron.

Listening on http://0.0.0.0:12798
[cdrelay:cardano.node.networkMagic:Notice:5] [2020-12-28 18:16:09.88 UTC] NetworkMagic 764824073
[cdrelay:cardano.node.basicInfo.protocol:Notice:5] [2020-12-28 18:16:09.88 UTC] Byron; Shelley
[cdrelay:cardano.node.basicInfo.version:Notice:5] [2020-12-28 18:16:09.88 UTC] 1.24.2
```

The topologyUpdater is triggered by `CARDANO_PUBLIC_IP`. Without it, the cron job is not installed.

The set of supported config options is documented [here](https://hub.docker.com/repository/docker/nessusio/cardano);

## External storage for block data

In this configuration, we map the node's `--database-path` to a mounted directory.

```
# The external IP for our Relay node
RELAY_PUBLIC_IP=`curl -s ipinfo.io/ip`
echo "RELAY_PUBLIC_IP: ${RELAY_PUBLIC_IP}"

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_PUBLIC_IP="$RELAY_PUBLIC_IP" \
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
    -e CARDANO_PUBLIC_IP="$RELAY_PUBLIC_IP" \
    -e CARDANO_CUSTOM_PEERS="$PRODUCER_IP:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -v cardano-relay-config:/var/cardano/config  \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

## Running a Block Producer Node

A producer node is configured in the same way as a RElay node, except that it does not have a `CARDANO_PUBLIC_IP` and hence no topology updater job.
Additionally, it needs to have its keys/certificates configured.

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > cardano/config/mainnet-prod-topology.json
{
  "Producers": [
    {
      "addr": "$RELAY_PUBLIC_IP",
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
    --hostname="cdprod" \
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

We can also use the image above to run Cardano CLI commands.

```
docker exec -it relay \
  cardano-cli query tip --mainnet

{
    "blockNo": 5102089,
    "headerHash": "e5984f27d1d3b5dcc296b33ccd919a28618ff2d77513971bd316cffd35afecda",
    "slotNo": 16910651
}
```

## Is there a pool that runs this tech?

Yes, we run [ASTOR Pool](http://astorpool.org). 

ASTOR charges the required minimum to provide a reliable stake pool.
More than double of what we take for margin goes to [charity](https://plant-for-the-planet.org).


Enjoy!
