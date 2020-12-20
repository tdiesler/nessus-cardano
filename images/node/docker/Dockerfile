#
# A multistage approach to building a minumum size Cardano Node/CLI image
# 

ARG CARDANO=1.24.2
ARG CABAL=3.2.0.0
ARG GHC=8.10.2
ARG GUILD=1.23

## Install dependencies and libsodium #################################################################################

FROM centos:8 AS builderA

# Install dependencies
RUN yum install -y git gcc gcc-c++ tmux gmp-devel make tar xz wget zlib-devel libtool autoconf systemd-devel ncurses-devel ncurses-compat-libs
RUN yum install -y systemd-devel ncurses-devel ncurses-compat-libs
RUN yum install -y diffutils

RUN mkdir /src

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

# Copy the executables from the previous build
COPY --from=builderC /usr/local/bin/cardano-node /usr/local/bin/cardano-node
COPY --from=builderC /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli

# Copy config files
COPY config/mainnet-config.json /var/cardano/config/mainnet-config.json
COPY config/mainnet-topology.json /var/cardano/config/mainnet-topology.json
COPY config/mainnet-shelley-genesis.json /var/cardano/config/mainnet-shelley-genesis.json

# Fetch the Byron genesis data, which we don't want in SCM
RUN wget -qO /var/cardano/config/mainnet-byron-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json

## Install guild-operators  ###########################################################################################

FROM builderD as builderE

ARG GUILD

# Install dependencies
RUN yum install -y jq bc tcptraceroute 

WORKDIR /src
RUN git clone https://github.com/cardano-community/guild-operators.git
WORKDIR guild-operators
RUN git checkout node-$GUILD

ENV CNODE_PORT=3001
ENV CCLI="/usr/local/bin/cardano-cli"
ENV CONFIG="/var/cardano/config/mainnet-config.json"
ENV TOPOLOGY="/var/cardano/config/mainnet-topology.json"
ENV SOCKET="/var/cardano/share/socket"

RUN echo "node-$GUILD" > /usr/local/bin/.env_branch
RUN ln -s /src/guild-operators/scripts/cnode-helper-scripts/gLiveView.sh /usr/local/bin/gLiveView

WORKDIR /root

ENTRYPOINT ["cardano-node"]
