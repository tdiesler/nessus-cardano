
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
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org

# Needed when runAsRoot is used by dockerTools.buildImage
# https://discourse.nixos.org/t/cannot-build-docker-image/7445
# system-features = kvm
EOF
```

## Build + Look around

```
./build.sh cardano

# Bash into the node to look around
docker run --rm -it --entrypoint=bash \
  -v node-data:/opt/cardano/data \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev}

cardano-node run \
  --config /opt/cardano/config/mainnet-config.json \
  --topology /opt/cardano/config/mainnet-topology.json \
  --socket-path /opt/cardano/ipc/socket \
  --database-path /opt/cardano/data \
  --host-addr 0.0.0.0 \
  --port 3001
```

## Run the Cardano Node

```
docker rm relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -v node-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n 100 -f relay

docker exec -it relay gLiveView

# Print env variables
docker exec relay cat /usr/local/bin/env

# EKG for topologyUpdater
docker exec relay curl -s -H 'Accept: application/json' 127.0.0.1:12788 | jq .

# Prometheus for gLiveView
docker exec relay curl -s 127.0.0.1:12798/metrics | sort

# Topology Updates
docker exec relay cat /opt/cardano/logs/topologyUpdateResult
docker exec relay cat /var/cardano/config/mainnet-topology.json
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

## Create Data Backup

```
BACKUP_FILE=mainnet-data-e288.tgz
TMPDATA_DIR=$HOME/data
DATA_VOLUME=node-data

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
BACKUP_FILE=mainnet-data-e288.tgz
TMPDATA_DIR=$HOME/data
DATA_VOLUME=node-data

rm -rf $TMPDATA_DIR; mkdir -p $TMPDATA_DIR
tar -C $TMPDATA_DIR -xzvf $BACKUP_FILE

docker run --name=tmp -v $DATA_VOLUME:/opt/cardano/data centos
docker cp $TMPDATA_DIR/protocolMagicId tmp:/opt/cardano/data
docker cp $TMPDATA_DIR/immutable tmp:/opt/cardano/data
docker cp $TMPDATA_DIR/ledger tmp:/opt/cardano/data
rm -rf $TMPDATA_DIR
docker rm tmp
```


## Run NGINX as Reverse Proxy

https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy

https://docs.nginx.com/nginx/admin-guide/security-controls/configuring-http-basic-authentication

```
mkdir nginx
cat << EOF > nginx/nginx.conf
events {}
http {
    server {
        # Unauthorized access to Liveness
        location /metrics/xyz.../liveness {
            proxy_pass http://relay:12798/metrics;
        }
    }
}
EOF

# Create nginx-config volume
docker rm -f nginx
docker volume rm -f nginx-config
docker run --name tmp -v nginx-config:/etc/nginx centos
docker cp ~/nginx tmp:/etc/
docker rm tmp

docker rm -f nginx
docker run --detach \
    --name=nginx \
    -p 12798:80 \
    --hostname=nginx \
    --network=cardano \
    --restart=always \
    --memory=50m \
    -v nginx-config:/etc/nginx:ro \
    nginx

docker logs -f nginx

curl http://localhost:12798/metrics/xyz.../liveness | sort
```

## Run Monit

```
# Add monit to the environment
nix-shell
> monit -V

# Setup the Config

MMONIT_PORT=8080
MMONIT_ADDR=astorpool.net
MMONIT_AUTH='username:changeit'

# Common config
cat << EOF > ~/.monitrc
set daemon 5 with start delay 10
set eventqueue basedir /var/monit/ slots 1000
set mmonit http://$MMONIT_AUTH@$MMONIT_ADDR:$MMONIT_PORT/collector
set httpd port 2812 and
    use address 0.0.0.0    # bind to all interfaces (i.e. not just to localhost)
    allow $MMONIT_ADDR     # allow the M/Monit host to connect to the server
    allow $MMONIT_AUTH     # monit authorization
EOF

# Relay specific
cat << EOF >> ~/.monitrc
check filesystem  system    path /dev/sda2
check system      $(hostname)
EOF

# Block Producer specific
cat << EOF >> ~/.monitrc
check filesystem  node-data path /dev/sdb
check system      $(hostname)
EOF

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
  -v ~/mmonit/conf/license.xml:${LICENSE} \
  nessusio/mmonit -i

docker logs -f mmonit

docker exec -it mmonit cat ${LICENSE}
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
