
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

# Bash into the container to look around
docker run --rm -it --entrypoint=bash nessusio/cardano-node:dev

docker rm relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    -p 12798:12798 \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano-node:dev run

docker logs -n 100 -f relay
```

## Run the Cardano Tools

```
docker run --rm -it \
    -e PROM_HOST=10.128.0.31 \
    nessusio/cardano-tools:dev gLiveView
```

## Run the CNCLI Leader Log

```
# Bash into the container to look around
docker run --rm -it nessusio/cardano-tools:dev

docker run --rm -it \
    nessusio/cardano-tools:dev cncli

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
