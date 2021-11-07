
# Nessus Cardano

With Nessus Cardano we explore, praise, comment, contribute to various technical aspects of [Cardano](https://cardano.org).
This is our contribution to "Making The World Work Better For All".

Initially, we focus on a "container first" approach for the Cardano node.

## Running a Node

To get up and running with [Cardano](https://cardano.org), you can spin up a node like this ...

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -v node-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node run

docker logs -f relay
```

This works on [x86_64](https://hub.docker.com/r/nessusio/cardano-node/tags?name=amd64) and [arm64](https://hub.docker.com/r/nessusio/cardano-node/tags?name=arm64).

The [nessusio/cardano-node](https://hub.docker.com/r/nessusio/cardano-node) image is built from source in multiple stages like [this](nix/cardano/Dockerfile) and then
with [Nix](https://nixos.org) like [this](nix/docker/node/default.nix).

## Running a Node on the Testnet

```
docker run --detach \
    --name=testrl \
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -v test-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node run

docker logs -f testrl
```

## Accessing the build-in gLiveView

The image has [gLiveView](https://github.com/cardano-community/guild-operators/blob/alpha/scripts/cnode-helper-scripts/gLiveView.sh) monitoring built in.

For a running container, you can do ...

```
docker exec -it relay gLiveView
```

<img src="docs/img/relay-glview.png" width="500">

## Accessing the build-in topology updater

There is currently no P2P module activated in cardano-1.26.1. Your node may call out to well known relay nodes, but you may never have incoming connections.
According to [this](https://github.com/cardano-community/guild-operators/blob/alpha/docs/Scripts/topologyupdater.md) it is necessary to update your topology
every hour. At the time of writing, cardano-node doesn't do this on its own.

This functionality has been built into nessus/cardano-node as well. On startup, you should see a log similar to this ...

```
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v node-data:/opt/cardano/data \
    nessusio/cardano-node run

$ docker exec -it relay tail /opt/cardano/logs/topologyUpdateResult
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
    nessusio/cardano-node run

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
    nessusio/cardano-node run

docker logs -f relay
```

## Running a Block Producer Node

A producer node is configured in the same way as a Relay node, except that it has some additional key/cert files configured and does not need to update its topology.

```
docker run --detach \
    --name=bprod \
    -p 3001:3001 \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -e CARDANO_SHELLEY_KES_KEY="/var/cardano/config/keys/pool/kes.skey" \
    -e CARDANO_SHELLEY_VRF_KEY="/var/cardano/config/keys/pool/vrf.skey" \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/pool/node.cert" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \ 
    nessusio/cardano-node run

docker logs -f bprod

docker exec -it bprod gLiveView
```

## Running the Cardano CLI

We can also use the image to run Cardano CLI commands.

For this to work, the node must share its IPC socket location, which can then
be use in the alias definition.

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node cardano-cli"

cardano-cli query tip --mainnet
{
    "blockNo": 5102089,
    "headerHash": "e5984f27d1d3b5dcc296b33ccd919a28618ff2d77513971bd316cffd35afecda",
    "slotNo": 16910651
}
```

## Docker Compose

We sometimes may prefer somm middle ground between manually spinning up individual docker containers and the full blown enterprise Kubernetes account.

Perhaps we'd like to use [Docker Compose](https://docs.docker.com/compose).

```
$ docker-compose -f nix/docker/compose/cardano-node-relay.yaml up --detach

Creating relay ... done
Creating monit ... done
Creating nginx ... done

$ docker-compose -f nix/docker/compose/cardano-node-bprod.yaml up --detach

Creating bprod ... done
Creating monit ... done
Creating nginx ... done
```

For details you may want to have a look at [nix/docker/compose/cardano-nodes.yaml](https://github.com/tdiesler/nessus-cardano/blob/master/nix/docker/compose/cardano-nodes.yaml).

## Kubernetes

This project has started as an incubator space for stuff that may eventually become available upstream. As part of this, we want to
"[provide high quality multiarch docker images and k8s support](https://forum.cardano.org/t/provide-high-quality-multiarch-docker-image-and-k8s-support/47906)"

With Kubernetes as the de-facto standard for container deployment, orchestration, monitoring, scaling , etc, it should be as easy as this to run Cardano nodes …


```
kubectl apply -f nix/docker/k8s/cardano-nodes.yaml

storageclass.storage.k8s.io/cardano-standard-rwo created
persistentvolumeclaim/relay-data created
pod/relay created
service/relay-np created
service/relay-clip created
persistentvolumeclaim/bprod-data created
pod/bprod created
service/bprod-clip created
```

This is documented in detail [over here](https://app.gitbook.com/@tdiesler/s/cardano/k8s/cardano-k8s).

For details you may want to have a look at [nix/docker/k8s/cardano-nodes.yaml](https://github.com/tdiesler/nessus-cardano/blob/master/nix/docker/k8s/cardano-nodes.yaml).

## Leader logs

For a Stake Pool Operator it is important to know when the node is scheduled to produce the next block. We definitely want to be online at that important moment and fullfil our block producing duties. There are better times to do node maintenance.

This important functionality has also been built into the [jterrier84/cncli_arm64](https://hub.docker.com/repository/docker/jterrier84/cncli_arm64) image.

First, lets create a docker volume in which to share the node.socket of the node container with the cncli container.

```
docker volume create --name nodesocket
```

Make sure running the block producer node with the following parameters (we add a docker volume and bind it with the node.socket folder in the node container)

```
docker run --detach \
    --name=bprod \
    -p 3001:3001 \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/mainnet-topology.json" \
    -e CARDANO_SHELLEY_KES_KEY="/var/cardano/config/keys/pool/kes.skey" \
    -e CARDANO_SHELLEY_VRF_KEY="/var/cardano/config/keys/pool/vrf.skey" \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/pool/node.cert" \
    -v cardano-prod-config:/var/cardano/config  \
    -v /mnt/disks/data01:/opt/cardano/data \
    -v nodesocket:/opt/cardano/ipc \
    nessusio/cardano-node run
```

Wait for the block producer node to sync and start. Now let's start the cncli docker container. 
Note: If running on an amd64 architecture, replace "cncli_arm64" with "cncli_amd64".

```
docker run -it -d \
    --name=cncli \
    --volumes-from bprod \
    -v /mnt/disks/data02:/home/ubuntu/db \
    -v cardano-prod-config:/home/ubuntu/config \
    -e CARDANO_NODE_SOCKET_PATH=/opt/cardano/ipc/node.socket \
    jterrier84/cncli_arm64:4.0.1
```

### Slot Leader Schedule

We can now obtain the leader schedule for our pool for the current, next or previous epoch. If executed the first time, the cncli database will be synced. This process might take some time (just be patient).


###### Current

```
docker exec -it cncli /home/ubuntu/cncli/scripts/cncli-leaderlog-current.sh [IP_RELAY] [PORT_RELAY] [POOL_ID]
```

###### Next

```
docker exec -it cncli /home/ubuntu/cncli/scripts/cncli-leaderlog-next.sh [IP_RELAY] [PORT_RELAY] [POOL_ID]
```

###### Previous   

```
docker exec -it cncli /home/ubuntu/cncli/scripts/cncli-leaderlog-previous.sh [IP_RELAY] [PORT_RELAY] [POOL_ID]
```

The output will show something like this.

```
2021-11-06T19:51:27.909Z WARN  cardano_ouroboros_network::protocols::chainsync > rollback to slot: 44660189
 2021-11-06T19:51:27.915Z INFO  cardano_ouroboros_network::protocols::chainsync > block 6467803 of 6467912, 100.00% synced
 2021-11-06T19:51:28.649Z INFO  cardano_ouroboros_network::protocols::chainsync > block 6467912 of 6467912, 100.00% synced
 2021-11-06T19:51:32.788Z INFO  cncli::nodeclient::sync                         > Exiting...
BCSH
{
  "status": "ok",
  "epoch": 301,
  "epochNonce": "c0e8aa015de7703c6fbec6c85a0aafb0974082e1eb4808061ad2e5ef23a2fd62",
  "epochSlots": 0,
  "epochSlotsIdeal": 0.09,
  "maxPerformance": 0,
  "poolId": "c3e7025ebae638e994c149e5703e82619b31897c9e1d64fc684f81c2",
  "sigma": 4.090600418054005e-06,
  "activeStake": 97591519270,
  "totalActiveStake": 23857504839454980,
  "d": 0,
  "f": 0.05,
  "assignedSlots": []
}
`Epoch 301` 🧙🔮:
`BCSH  - 0 `🎰`,  0% `🍀max, `0.09` 🧱ideal
```

If desired, the cncli container can now be stopped. 

```
docker stop cncli
```

# Build the Images

In line with the [upstream project](https://github.com/input-output-hk/cardano-node) we also us
a [Nix](https://nixos.org/guides/install-nix.html) based build. The build requires Nix and a
working Docker environment. This works on x86_64 and arm64.

Spinning up a build/test environment on GCE is documented [here](./vps/gce-centos8.md)

To build all images and their respective dependencies, run ...

```
./build.sh all
```

Enjoy!
