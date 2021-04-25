
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

## Environment variables


| ENV Variable                            | Used in cardano-node option       |                                                     |
|:----------------------------------------|:----------------------------------|:----------------------------------------------------|
| CARDANO_BIND_ADDR                       | --host-addr                       | Network bind address                                |
| CARDANO_BLOCK_PRODUCER                  |                                   | Run the node as block producer                      |
| CARDANO_CONFIG                          | --config                          | Path to the node configuration file                 |
| CARDANO_CUSTOM_PEERS                    |                                   | List of custom peers added by the topology updater  |
| CARDANO_DATABASE_PATH                   | --database-path                   | Directory where the state is stored                 |
| CARDANO_LOG_DIR                         |                                   | Path to the log directory                           |
| CARDANO_NETWORK                         |                                   | The cardano network (e.g. testnet)                  |
| CARDANO_PORT                            | --port                            | The port number                                     |
| CARDANO_PUBLIC_IP                       |                                   | Public IP used by the topology updater              |
| CARDANO_SHELLEY_KES_KEY                 | --shelley-kes-key                 | Path to the KES key file                            |
| CARDANO_SHELLEY_VRF_KEY                 | --shelley-vrf-key                 | Path to the VRF key file                            |
| CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE | --shelley-operational-certificate | Path to the operational certificate                 |
| CARDANO_SOCKET_PATH                     | --socket-path                     | Path to a cardano-node socket                       |
| CARDANO_TOPOLOGY                        | --topology                        | Path to a file describing the topology              |
| CARDANO_UPDATE_TOPOLOGY                 |                                   | Enable the built-in topology updater                |

## Running a Relay Node

The example below publishes the node's p2p port on the host.

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v node-data:/opt/cardano/data \
    nessusio/cardano-node run

docker logs -f relay
```

## Running a Relay Node on the Testnet

```
docker run --detach \
    --name=testrl \
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v test-data:/opt/cardano/data \
    nessusio/cardano-node run

docker logs -f testrl
```

## Reading topology updater results

```
docker exec -it relay tail /opt/cardano/logs/topologyUpdateResult

{ "resultcode": "201", "datetime":"2021-01-10 18:30:06", "clientIp": "209.250.233.200", "iptype": 4, "msg": "nice to meet you" }
{ "resultcode": "203", "datetime":"2021-01-10 19:30:03", "clientIp": "209.250.233.200", "iptype": 4, "msg": "welcome to the topology" }
{ "resultcode": "204", "datetime":"2021-01-10 20:30:04", "clientIp": "209.250.233.200", "iptype": 4, "msg": "glad you're staying with us" }
```

## Monitoring with gLiveView

An excellent bash based monitor that runs once every few seconds is provided for us by
[guild-operators](https://github.com/cardano-community/guild-operators/blob/alpha/scripts/cnode-helper-scripts/gLiveView.sh).

```
docker exec -it relay gLiveView
```

## Accessing the cardano-cli

We can also use the image to run Cardano CLI commands.

For this to work, the node must share its IPC socket location, which can then
be use in the alias definition.

```
alias cardano-cli="docker run -it --rm \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node cardano-cli"

cardano-cli query tip --mainnet
{
    "blockNo": 5416832,
    "headerHash": "9fef7757acab95f4133828070b5945e0b07476d5faacbe67b0182551b2dfbf10",
    "slotNo": 23273714
}
```

## Getting Slot Leader Schedule

The slot leader schedule is also provided by the
[cardano-tools](https://hub.docker.com/repository/docker/nessusio/cardano-tools) image.
