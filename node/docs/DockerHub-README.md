
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
    -v shelly-data:/opt/cardano/data \
    nessusio/cardano run

docker logs -f relay
```

## Accessing the build-in topology updater

```
docker exec -it relay tail /opt/cardano/logs/topologyUpdater_lastresult.json

{ "resultcode": "201", "datetime":"2021-01-10 18:30:06", "clientIp": "209.250.233.200", "iptype": 4, "msg": "nice to meet you" }
{ "resultcode": "203", "datetime":"2021-01-10 19:30:03", "clientIp": "209.250.233.200", "iptype": 4, "msg": "welcome to the topology" }
{ "resultcode": "204", "datetime":"2021-01-10 20:30:04", "clientIp": "209.250.233.200", "iptype": 4, "msg": "glad you're staying with us" }
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

## Generate the Ledger State

To determine the block producer's leader schedule (see below), we first need to obtain the current `ledger-state.json`

```
docker run -it --rm \
  -v shelly-data:/opt/cardano/data \
  nessusio/cardano ledger-state

Generating /opt/cardano/data/ledger-state.json
```

## Getting the Sigma value

Sigma represents your pool's share of the active stake. 
It is the ratio of active stake for a given epoch divided by the total stake.

Details on how to get sigma are [here](https://github.com/papacarp/pooltool.io/tree/master/leaderLogs#getsigmapy-details)

```
docker run -it --rm \
  -v shelly-data:/opt/cardano/data \
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
  -v shelly-data:/opt/cardano/data \
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
