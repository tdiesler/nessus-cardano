
The images are produced by the [Nessus Cardano](https://github.com/tdiesler/nessus-cardano) project.

The Cardano Node images generally follow [cardano-node](https://github.com/input-output-hk/cardano-node) releases.

There may be multiple revisions of this image based on the same cardano-node release. For example ...

|              |                                                                                                                           |
|:-------------|:--------------------------------------------------------------------------------------------------------------------------|
| 1.24.2-rev1  | Denotes the first revision for [Cardano Node 1.24.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2) |
| 1.24.2       | Is the latest revision for Cardano Node 1.24.2 |
| latest       | Is the latest revision for the latest Cardono Node release |

Each image comes in multiple arch variant. Current we support `amd64` and `arm64`.

## Environment variables


| ENV Variable                           | Used in cardano-node option       |                                                     |
|:---------------------------------------|:----------------------------------|:----------------------------------------------------|
| CARDANO_BIND_ADDR                      | --host-addr                       | Network bind address                                |
| CARDANO_BLOCK_PRODUCER                 |                                   | Run the node as block producer                      |
| CARDANO_CONFIG                         | --config                          | Path to the node configuration file                 |
| CARDANO_CUSTOM_PEERS                   |                                   | List of custom peers added by the topology updater  |
| CARDANO_DATABASE_PATH                  | --database-path                   | Directory where the state is stored                 |
| CARDANO_LOG_DIR                        |                                   | Path to the log directory                           |
| CARDANO_PORT                           | --port                            | The port number                                     |
| CARDANO_PUBLIC_IP                      |                                   | Public IP used by the topology updater              |
| CARDANO_SHELLY_KES_KEY                 | --shelley-kes-key                 | Path to the KES key file                            |
| CARDANO_SHELLY_VRF_KEY                 | --shelley-vrf-key                 | Path to the VRF key file                            |
| CARDANO_SHELLY_OPERATIONAL_CERTIFICATE | --shelley-operational-certificate | Path to the operational certificate                 |
| CARDANO_SOCKET_PATH                    | --socket-path                     | Path to a cardano-node socket                       |
| CARDANO_TOPOLOGY                       | --topology                        | Path to a file describing the topology              |
| CARDANO_UPDATE_TOPOLOGY                |                                   | Enable the built-in topology updater                |

## Running a Relay Node

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

## Accessing the build-in topology updater

```
docker exec -it relay tail -n 12 /opt/cardano/logs/topologyUpdater_lastresult.json

{ "resultcode": "204", "datetime":"2020-12-29 10:15:04", "clientIp": "35.239.77.33", "iptype": 4, "msg": "glad you're staying with us" }
{ "resultcode": "204", "datetime":"2020-12-29 11:15:04", "clientIp": "35.239.77.33", "iptype": 4, "msg": "glad you're staying with us" }
{ "resultcode": "204", "datetime":"2020-12-29 12:15:04", "clientIp": "35.239.77.33", "iptype": 4, "msg": "glad you're staying with us" }
```

## Accessing the build-in gLiveView

```
docker exec -it relay gLiveView
```

## Accessing the build-in CLI

```
alias cardano-cli="docker exec -it relay cardano-cli"

cardano-cli query tip --mainnet
{
    "blockNo": 5102089,
    "headerHash": "e5984f27d1d3b5dcc296b33ccd919a28618ff2d77513971bd316cffd35afecda",
    "slotNo": 16910651
}
```
