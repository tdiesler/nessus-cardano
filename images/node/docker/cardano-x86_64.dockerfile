#
# A multistage approach to building a minumum size Cardano Node/CLI image
# 

ARG CARDANO=1.24.2
ARG CABAL=3.2.0.0
ARG GHC=8.10.2

## Install dependencies and libsodium #################################################################################

FROM debian:10 AS builderA

# Install dependencies
RUN apt-get update -y
RUN apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf

# Install Libsodium

WORKDIR /src
RUN git clone https://github.com/input-output-hk/libsodium
WORKDIR libsodium
RUN git checkout 66f017f1
RUN ./autogen.sh
RUN ./configure
RUN make install

ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

## Install Cabal + GHC ################################################################################################

FROM builderA as builderB

ARG CABAL
ARG GHC

# Download, unpack, install and update Cabal

WORKDIR /src
RUN wget -q https://downloads.haskell.org/~cabal/cabal-install-$CABAL/cabal-install-$CABAL-x86_64-unknown-linux.tar.xz
RUN tar -xf cabal-install-$CABAL-x86_64-unknown-linux.tar.xz
RUN mv cabal /usr/local/bin/
RUN cabal update

# Download and install GHC

WORKDIR /src
RUN wget -q https://downloads.haskell.org/ghc/$GHC/ghc-$GHC-x86_64-deb10-linux.tar.xz
RUN tar -xf ghc-$GHC-x86_64-deb10-linux.tar.xz
WORKDIR ghc-$GHC
RUN ./configure
RUN make install
RUN ghc --version

## Buid Cardano #######################################################################################################

FROM builderB as builderC

ARG CARDANO
ARG CABAL
ARG GHC

# Build and install cardano-node

WORKDIR /src
RUN git clone https://github.com/input-output-hk/cardano-node.git
WORKDIR cardano-node
RUN git checkout $CARDANO

RUN cabal configure

RUN echo "package cardano-crypto-praos" >> cabal.project.local
RUN echo "  flags: -external-libsodium-vrf" >> cabal.project.local

RUN cabal build all

RUN cp dist-newstyle/build/x86_64-linux/ghc-$GHC/cardano-node-$CARDANO/x/cardano-node/build/cardano-node/cardano-node /usr/local/bin/cardano-node
RUN cp dist-newstyle/build/x86_64-linux/ghc-$GHC/cardano-cli-$CARDANO/x/cardano-cli/build/cardano-cli/cardano-cli /usr/local/bin/cardano-cli

## Install Cardano ####################################################################################################

FROM builderA as builderD

WORKDIR /root

# Install a few diagnostic utils
# > nc -zvw100 nodeip 3001
RUN apt-get install -y curl cron netcat jq bc procps tcptraceroute

# Copy the executables from the previous build
COPY --from=builderC /usr/local/bin/cardano-node /usr/local/bin/cardano-node
COPY --from=builderC /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli

# Copy config files
COPY config/mainnet-config.json /opt/cardano/config/mainnet-config.json
COPY config/mainnet-topology.json /opt/cardano/config/mainnet-topology.json

# Fetch the Byron genesis data, which we don't want in SCM
RUN wget -qO /opt/cardano/config/mainnet-byron-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json
RUN wget -qO /opt/cardano/config/mainnet-shelley-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json

RUN mkdir -p /opt/cardano/data
RUN mkdir -p /opt/cardano/logs

## Install guild-operators  ###########################################################################################

FROM builderD as builderE

RUN git clone https://github.com/cardano-community/guild-operators.git
WORKDIR guild-operators

# Use env vars defined in the entrypoint to configure gLiveView

RUN sed -i "s|#CCLI|source /root/env\n\n#CCLI|" scripts/cnode-helper-scripts/env
RUN sed -i "s|CNODE_PORT=6000|#CNODE_PORT=6000|" scripts/cnode-helper-scripts/env

# Disable interactive script update
RUN git log -1 --format="%h" > /root/guild-operators/scripts/cnode-helper-scripts/.env_branch
RUN echo "#!/bin/bash" >> /usr/local/bin/gLiveView; \
    echo "guild-operators/scripts/cnode-helper-scripts/gLiveView.sh" >> /usr/local/bin/gLiveView; \
    chmod +x /usr/local/bin/gLiveView

# Configure the topologyUpdater
RUN sed -i 's|CNODE_HOSTNAME="CHANGE ME"|CNODE_HOSTNAME="$CARDANO_PUBLIC_IP"|' scripts/cnode-helper-scripts/topologyUpdater.sh
RUN sed -i 's|#CUSTOM_PEERS="None"|CUSTOM_PEERS="$CARDANO_CUSTOM_PEERS"|' scripts/cnode-helper-scripts/topologyUpdater.sh
RUN echo "#!/bin/bash" >> /usr/local/bin/topologyUpdater; \
    echo "guild-operators/scripts/cnode-helper-scripts/topologyUpdater.sh" >> /usr/local/bin/topologyUpdater; \
    chmod +x /usr/local/bin/topologyUpdater

WORKDIR /root
COPY entrypoint.sh /opt/cardano/entrypoint.sh

ENTRYPOINT ["/opt/cardano/entrypoint.sh"]
