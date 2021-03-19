
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
# system-features = kvm
EOF
```

## Build + Run the Cardano Node

```
# Build the cardano node image
./build.sh

VERSION=dev

docker rm relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v node-data:/opt/cardano/data \
    nessusio/cardano-node:$VERSION run

docker logs -n 100 -f relay

docker exec -it relay gLiveView
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

docker exec relay cat /opt/cardano/config/mainnet-topology.json
```

## Populating the Data Volume

```
mkdir ~/data
scp core@relay01.astorpool.net:shelley-data-e251.tgz ~/data
cd ~/data; tar xzvf shelley-data-e251.tgz

docker run --name=tmp -v node-data:/data centos
docker cp ~/data/protocolMagicId tmp:/data
docker cp ~/data/immutable tmp:/data
docker cp ~/data/ledger tmp:/data
docker rm tmp
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
