
The images are produced by the [Nessus Cardano](https://github.com/tdiesler/nessus-cardano) project.

The Cardano Node images generally follow [cardano-node](https://github.com/input-output-hk/cardano-node) releases.

There may be multiple revisions of this image based on the same cardano-node release. For example ...

|              |                                                                                                                           |
|:-------------|:--------------------------------------------------------------------------------------------------------------------------|
| 1.25.1-rev1  | Denotes the first revision for [Cardano Node 1.25.1](https://github.com/input-output-hk/cardano-node/releases/tag/1.25.1) |
| 1.25.1       | Is the latest revision for Cardano Node 1.25.1 |
| latest       | Is the latest revision for the latest Cardono Node release |
| 1.25.1-dev   | Is the latest development version for Cardano Node 1.25.1 |
| dev          | Is the latest development version |

Each image comes in multiple arch variant. Current we support `amd64` and `arm64`.

## Running a Relay Node

For these tools to work, we must have a running node, that publishes the prometheus port
and gives access to the socket path, for example through a shared volume.

For details, please have a look at the [cardano-node](https://hub.docker.com/repository/docker/nessusio/cardano-node) image.

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano-node run

docker logs -f relay
```

## Getting Slot Leader Schedule

Accessing the cncli through Docker can also be a little convoluted.
We again define an alias for it.

The details about this API are [here](https://github.com/AndrewWestberg/cncli/blob/develop/README.md#running).

```
alias cncli="docker run -it --rm \
  -v ~/cardano:/var/cardano \
  nessusio/cardano-tools cncli"

NODE_IP=10.128.0.31

# Pinging the node
cncli ping --host $NODE_IP
{
  "status": "ok",
  "host": "10.128.0.31",
  "port": 3001,
  "connectDurationMs": 181,
  "durationMs": 297
}
```

### Syncing the database

```
# Syncing the database
cncli sync --host $NODE_IP \
  --db /var/cardano/cncli/cncli.db \
  --no-service

...
2021-03-04T10:23:19.719Z INFO  cardano_ouroboros_network::protocols::chainsync   > block 5417518 of 5417518, 100.00% synced
2021-03-04T10:23:23.459Z INFO  cncli::nodeclient::sync                           > Exiting...
```

### Getting the leader log

```
cncli leaderlog \
  --pool-id 9e8009b249142d80144dfb681984e08d96d51c2085e8bb6d9d1831d2 \
  --shelley-genesis /var/cardano/config/mainnet-shelley-genesis.json \
  --byron-genesis /var/cardano/config/mainnet-byron-genesis.json \
  --pool-vrf-skey /var/cardano/keys/pool/vrf.skey \
  --db /var/cardano/cncli/cncli.db \
  --tz Europe/Berlin \
  --ledger-set current | tee leaderlog.json

cat leaderlog.json | jq -c ".assignedSlots[] | {no: .no, slot: .slotInEpoch, at: .at}"

{"no":1,"slot":165351,"at":"2021-02-26T20:40:42+01:00"}
{"no":2,"slot":312656,"at":"2021-02-28T13:35:47+01:00"}
{"no":3,"slot":330588,"at":"2021-02-28T18:34:39+01:00"}
{"no":4,"slot":401912,"at":"2021-03-01T14:23:23+01:00"}
```
