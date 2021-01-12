#
# A multistage approach to building a Cardano Node/CLI image
# 

# Some build-arg parameters
ARG CARDANO_VER
ARG CABAL_VER
ARG GHC_VER

## Install dependencies and libsodium #################################################################################

FROM ubuntu:20.04 as builderA

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update \
    && apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev \
    zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf llvm libnuma-dev

# Install Libsodium

WORKDIR /src
RUN git clone https://github.com/input-output-hk/libsodium
WORKDIR libsodium
RUN git checkout 66f017f1
RUN ./autogen.sh \ 
  && ./configure \
  && make install

ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

## Build + Install GHC & Cabal ########################################################################################

FROM builderA as builderB

ARG CABAL_VER
ARG GHC_VER
ARG ARCH

# Download and install GHC

WORKDIR /src
RUN wget -q https://downloads.haskell.org/ghc/${GHC_VER}/ghc-${GHC_VER}-${ARCH}-deb10-linux.tar.xz \
  && tar -xf ghc-${GHC_VER}-${ARCH}-deb10-linux.tar.xz

WORKDIR ghc-${GHC_VER}
RUN ./configure \
  && make install

RUN ghc --version

# The Haskell Common Architecture for Building Applications and Libraries

ARG CABAL_TAG="${CABAL_VER}-rc4"

WORKDIR /src
RUN if [ "${ARCH}" = "aarch64" ]; then \
  wget -q https://oleg.fi/cabal-install-${CABAL_TAG}/cabal-install-${CABAL_VER}-${ARCH}-ubuntu-18.04.tar.xz \
  && tar -xf cabal-install-${CABAL_VER}-${ARCH}-ubuntu-18.04.tar.xz; \
elif [ "${ARCH}" = "x86_64" ]; then \
  wget -q https://oleg.fi/cabal-install-${CABAL_TAG}/cabal-install-${CABAL_VER}-${ARCH}-ubuntu-16.04.tar.xz \
  && tar -xf cabal-install-${CABAL_VER}-${ARCH}-ubuntu-16.04.tar.xz; \
fi

RUN mv cabal /usr/local/bin/

RUN cabal --version

## Buid Cardano #######################################################################################################

FROM builderB as builderC

ARG CARDANO_VER
ARG CABAL_VER
ARG GHC_VER
ARG ARCH

# Build and install cardano-node

WORKDIR /src
RUN git clone -b $CARDANO_VER --depth 1 https://github.com/input-output-hk/cardano-node.git
WORKDIR cardano-node

RUN cabal update \
  && cabal configure

RUN echo "package cardano-crypto-praos" >> cabal.project.local
RUN echo "  flags: -external-libsodium-vrf" >> cabal.project.local

RUN cabal build all

RUN cp dist-newstyle/build/${ARCH}-linux/ghc-${GHC_VER}/cardano-node-${CARDANO_VER}/x/cardano-node/build/cardano-node/cardano-node /usr/local/bin/cardano-node
RUN cp dist-newstyle/build/${ARCH}-linux/ghc-${GHC_VER}/cardano-cli-${CARDANO_VER}/x/cardano-cli/build/cardano-cli/cardano-cli /usr/local/bin/cardano-cli

## Install Cardano ####################################################################################################
#
# Note, this gets rid of the intermediary build steps B & C
# Instead, it builds on top of A (i.e libsodium)
#

FROM builderA as builderD

WORKDIR /root

# Install a few diagnostic utils
# > nc -zvw100 nodeip 3001
RUN apt-get install -y curl cron netcat jq bc procps tcptraceroute iproute2

# Copy the executables from the previous build
COPY --from=builderC /usr/local/bin/cardano-node /usr/local/bin/cardano-node
COPY --from=builderC /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli

# Copy config files
COPY config/mainnet-config.json /opt/cardano/config/mainnet-config.json
COPY config/mainnet-topology.json /opt/cardano/config/mainnet-topology.json

# Fetch the Byron/Shelley genesis data, which we don't want in SCM
# RUN wget -qO /opt/cardano/config/mainnet-config.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
# RUN wget -qO /opt/cardano/config/mainnet-topology.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-topology.json
RUN wget -qO /opt/cardano/config/mainnet-byron-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json
RUN wget -qO /opt/cardano/config/mainnet-shelley-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json

ENV CARDANO_NODE_SOCKET_PATH="/opt/cardano/data/socket"

RUN mkdir -p /opt/cardano/data
RUN mkdir -p /opt/cardano/logs

## Install gLiveView & topologyUpdater ################################################################################
#
# Note, that this currently does not use a specific version
# Instead it uses whatever happens to be in master at the build time of this image
# The auto update feature of gLiveView is disabled
#
# https://github.com/cardano-community/guild-operators

WORKDIR /root

RUN git clone --depth 1 https://github.com/cardano-community/guild-operators.git
WORKDIR guild-operators

# Use env vars defined in the entrypoint to configure gLiveView

RUN sed -i "s|#CCLI|source /root/env\n\n#CCLI|" scripts/cnode-helper-scripts/env
RUN sed -i "s|CNODE_PORT=6000|#CNODE_PORT=6000|" scripts/cnode-helper-scripts/env

# Disable interactive script update
# We do this by storing the SHA1 of the last commit in branch marker file 
RUN git log -1 --format="%h" > /root/guild-operators/scripts/cnode-helper-scripts/.env_branch

# Install gLiveView
COPY gLiveView.sh /usr/local/bin/gLiveView
RUN chmod +x /usr/local/bin/gLiveView

# Install topologyUpdater
COPY topologyUpdater.sh /usr/local/bin/topologyUpdater
RUN chmod +x /usr/local/bin/topologyUpdater

# Install traptest
# COPY traptest.sh /usr/local/bin/traptest
# RUN chmod +x /usr/local/bin/traptest

# Configure the topologyUpdater
RUN sed -i 's|CNODE_HOSTNAME="CHANGE ME"|CNODE_HOSTNAME="$CARDANO_PUBLIC_IP"|' scripts/cnode-helper-scripts/topologyUpdater.sh
RUN sed -i 's|#CUSTOM_PEERS="None"|CUSTOM_PEERS="$CARDANO_CUSTOM_PEERS"|' scripts/cnode-helper-scripts/topologyUpdater.sh

## Install leaderLogs  ###############################################################################################
#
# Note, that this currently does not use a specific version
# Instead it uses whatever happens to be in master at the build time of this image
#
# https://github.com/papacarp/pooltool.io

WORKDIR /root

# Install Phython with Timezone Extension
RUN apt-get install -y python3-pip \
  && python3 -m pip install pytz \
  && python3 --version

RUN git clone --depth 1 https://github.com/papacarp/pooltool.io.git

## Install the entrypoint  ############################################################################################

WORKDIR /root
COPY entrypoint.sh /opt/cardano/entrypoint.sh

ENTRYPOINT ["/opt/cardano/entrypoint.sh"]
