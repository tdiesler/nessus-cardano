#
# A multistage approach to building a Cardano Node/CLI image
#

# Some build-arg parameters
ARG CARDANO_VER
ARG ARCH

ARG CABAL_VER="3.4.0.0"
ARG GHC_VER="8.10.2"

## Install dependencies and libsodium #################################################################################

FROM debian:10-slim as builderA

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update \
  && apt-get install -y \
    autoconf automake build-essential g++ git jq \
    libffi-dev libgmp-dev libncursesw5 libnuma-dev libssl-dev libsystemd-dev libtinfo-dev libtool llvm \
    make pkg-config tmux wget zlib1g-dev

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

ARG CABAL_TAG="${CABAL_VER}"

WORKDIR /src
RUN if [ "${ARCH}" = "aarch64" ]; then \
  wget -q https://downloads.haskell.org/~cabal/cabal-install-${CABAL_TAG}/cabal-install-${CABAL_VER}-aarch64-ubuntu-18.04.tar.xz \
  && tar -xf cabal-install-${CABAL_VER}-aarch64-ubuntu-18.04.tar.xz; \
elif [ "${ARCH}" = "x86_64" ]; then \
  wget -q https://downloads.haskell.org/~cabal/cabal-install-${CABAL_TAG}/cabal-install-${CABAL_VER}-x86_64-ubuntu-16.04.tar.xz \
  && tar -xf cabal-install-${CABAL_VER}-x86_64-ubuntu-16.04.tar.xz; \
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

RUN cp dist-newstyle/build/${ARCH}-linux/ghc-${GHC_VER}/cardano-node-${CARDANO_VER}/x/cardano-node/build/cardano-node/cardano-node /usr/local/bin/
RUN cp dist-newstyle/build/${ARCH}-linux/ghc-${GHC_VER}/cardano-cli-${CARDANO_VER}/x/cardano-cli/build/cardano-cli/cardano-cli /usr/local/bin/

WORKDIR /root

## Buid Cardano #######################################################################################################

FROM builderA

# Copy the executables from the previous build
COPY --from=builderC /usr/local/bin/cardano-node /usr/local/bin/
COPY --from=builderC /usr/local/bin/cardano-cli /usr/local/bin/

WORKDIR /
