
### Install Nix

https://nixos.org/manual/nix/stable/#chap-installation

https://github.com/nmattia/niv

https://input-output-hk.github.io/haskell.nix/tutorials/getting-started

```
# Single user install
sh <(curl -L https://nixos.org/nix/install)
source ~/.nix-profile/etc/profile.d/nix.sh

# Install niv
nix-env -i niv

# Configure Nix to use the binary cache from IOHK
sudo mkdir /etc/nix
cat << EOF | sudo tee /etc/nix/nix.conf
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://cache.nixos.org

# Needed when runAsRoot is used by dockerTools.buildImage
# https://discourse.nixos.org/t/cannot-build-docker-image/7445
system-features = kvm
EOF
```

## Build + Run the Cardano Node

```
# Build the cardano node image
./build.sh

docker rm relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -v node-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n 100 -f relay

docker exec -it relay gLiveView

# Bash into the node to look around
docker run --rm -it --entrypoint=bash \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev}
```

## Running the Cardano CLI

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

cardano-cli query tip --mainnet
{
  "epoch": 258,
  "hash": "1df02b27a76b58f7ca12878e1384da9e55978182efa7dec3d1238b83e2fa34d9",
  "slot": 26299903,
  "block": 5563915
}
```

## Access Metrics

```
# Print env variables
docker exec relay cat /usr/local/bin/env

# EKG for topologyUpdater
docker exec relay curl -s -H 'Accept: application/json' 127.0.0.1:12788 | jq .

# Prometheus for gLiveView
docker exec relay curl -s 127.0.0.1:12798/metrics | sort
```

## Topology Updates

```
docker exec relay cat /opt/cardano/logs/topologyUpdateResult

docker exec relay cat /var/cardano/config/mainnet-topology.json
```

## Create Data Backup

```
BACKUP_FILE=mainnet-data-e270.tgz
DATA_VOLUME=/mnt/disks/data00
TMPDATA_DIR=$HOME/data

rm -rf $TMPDATA_DIR; mkdir -p $TMPDATA_DIR
docker run --name=tmp -v $DATA_VOLUME:/opt/cardano/data centos
docker cp tmp:/opt/cardano/data/protocolMagicId $TMPDATA_DIR
docker cp tmp:/opt/cardano/data/immutable $TMPDATA_DIR
docker cp tmp:/opt/cardano/data/ledger $TMPDATA_DIR
tar -C $TMPDATA_DIR -czvf $BACKUP_FILE protocolMagicId immutable ledger
rm -rf $TMPDATA_DIR
docker rm tmp
```

## Restore Data Backup

```
BACKUP_FILE=mainnet-data-e270.tgz
DATA_VOLUME=/mnt/disks/data01
TMPDATA_DIR=$HOME/data

scp -P 22 core@relay01.astorpool.net:$BACKUP_FILE .
rm -rf $TMPDATA_DIR; mkdir -p $TMPDATA_DIR
tar -C $TMPDATA_DIR -xzvf $BACKUP_FILE

docker run --name=tmp -v $DATA_VOLUME:/opt/cardano/data centos
docker cp $TMPDATA_DIR/protocolMagicId tmp:/opt/cardano/data
docker cp $TMPDATA_DIR/immutable tmp:/opt/cardano/data
docker cp $TMPDATA_DIR/ledger tmp:/opt/cardano/data
rm -rf $TMPDATA_DIR
docker rm tmp
```

## Run Monit

```
# Setup the Config Volume

MMONIT_PORT=8080
MMONIT_ADDR=astorpool.net
MMONIT_AUTH='username:changeit'

mkdir -p monit
cat << EOF > monit/monitrc-extras
set eventqueue basedir /var/monit/ slots 1000
set mmonit http://$MMONIT_AUTH@$MMONIT_ADDR:$MMONIT_PORT/collector
set httpd port 2812 and
    use address 0.0.0.0    # bind to all interfaces (i.e. not just to localhost)
    allow $MMONIT_ADDR     # allow the M/Monit host to connect to the server
    allow $MMONIT_AUTH     # monit authorization
EOF

docker rm -f monit
docker volume rm -f monit-config
docker run --name=tmp -v monit-config:/etc/monit.d/ centos
docker cp monit/monitrc-extras tmp:/etc/monit.d
docker rm -f tmp

# Run the Image

docker rm -f monit
docker run --detach \
  --name=monit \
  --restart=always \
  --memory=50m \
  --hostname=ada01rl \
  -v monit-config:/etc/monit.d \
  nessusio/monit:${CARDANO_NODE_VERSION:-dev} -Iv

docker logs -f monit
```

## Run M/Monit

Login: admin/swordfish

```
CONFDIR="/usr/local/var/mmonit/conf"
LICENSE="${CONFDIR}/license.xml"

docker rm -f mmonit
docker run --detach \
  --name=mmonit \
  -p 8080:8080 \
  --restart=always \
  -v ~/mmonit/conf:${CONFDIR} \
  nessusio/mmonit:${CARDANO_NODE_VERSION:-dev} -i

docker logs -f mmonit

docker exec -it mmonit cat ${LICENSE}
```

## Ledger State

```
alias cncli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v cncli:/var/cardano/cncli \
  nessusio/cardano-tools:${CARDANO_NODE_VERSION:-dev} cncli"

NODE_IP=34.68.137.181

cncli ping --host $NODE_IP
{
  "status": "ok",
  "host": "10.128.0.31",
  "port": 3001,
  "connectDurationMs": 0,
  "durationMs": 53
}
```

### Syncing the database

This command connects to a remote node and synchronizes blocks to a local sqlite database.

```
cncli sync --host $NODE_IP \
  --db /var/cardano/cncli/cncli.db \
  --no-service

...
2021-03-04T10:23:19.719Z INFO  cardano_ouroboros_network::protocols::chainsync   > block 5417518 of 5417518, 100.00% synced
2021-03-04T10:23:23.459Z INFO  cncli::nodeclient::sync                           > Exiting...
```

### Slot Leader Schedule

We can now obtain the leader schedule for our pool.

```
cardano-cli query ledger-state \
  --mary-era --mainnet > ~/cardano/scratch/ledger-state.json

# --ledger-state /var/cardano/local/scratch/ledger-state.json \

cncli leaderlog \
  --pool-id 9e8009b249142d80144dfb681984e08d96d51c2085e8bb6d9d1831d2 \
  --shelley-genesis /opt/cardano/config/mainnet-shelley-genesis.json \
  --byron-genesis /opt/cardano/config/mainnet-byron-genesis.json \
  --pool-vrf-skey /var/cardano/local/keys/pool/vrf.skey \
  --db /var/cardano/cncli/cncli.db \
  --tz Europe/Berlin \
  --ledger-set current | tee leaderlog.json

cat leaderlog.json | jq -c ".assignedSlots[] | {no: .no, slot: .slotInEpoch, at: .at}"

{"no":1,"slot":165351,"at":"2021-02-26T20:40:42+01:00"}
{"no":2,"slot":312656,"at":"2021-02-28T13:35:47+01:00"}
{"no":3,"slot":330588,"at":"2021-02-28T18:34:39+01:00"}
{"no":4,"slot":401912,"at":"2021-03-01T14:23:23+01:00"}
```

## Bare Metal Build

Debian 10 (Buster)

```
# Install system dependencies
sudo apt-get update
sudo apt-get install -y \
  autoconf \
  automake \
  build-essential \
  g++ \
  git \
  jq \
  libffi-dev \
  libgmp-dev \
  libncursesw5 \
  libnuma-dev \
  libssl-dev \
  libsystemd-dev \
  libtinfo-dev \
  libtool \
  llvm \
  make \
  pkg-config \
  tmux \
  wget \
  zlib1g-dev

# Run the bare metal build
./nix/bare-metal.sh
```
