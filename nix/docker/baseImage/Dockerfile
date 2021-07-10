#
# A multistage approach to building an extended Debian
#
# - Log Navigator

# Required version args
ARG LNAV_VER

## Install required dependencies ######################################################################################

FROM debian:10-slim as builderA

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update \
  && apt-get install -y \
    bzip2 curl zlib1g-dev \
    libarchive-dev libcurl4-openssl-dev libncurses-dev libpcre++-dev libreadline-dev libsqlite3-dev

## Build lnav #########################################################################################################

FROM builderA as builderB

# Required version args
ARG LNAV_VER

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get install -y \
  autoconf automake build-essential git

RUN git clone --depth 1 --branch v${LNAV_VER} https://github.com/tstack/lnav gitsrc
WORKDIR gitsrc

RUN ./autogen.sh \
  && ./configure \
  && make install

## Buid Cardano #######################################################################################################

FROM builderA

# Copy the executables from the previous build
COPY --from=builderB /usr/bin/lnav /usr/local/bin/

WORKDIR /
