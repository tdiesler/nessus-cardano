#!/bin/bash
#
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

# Some build-arg parameters
cardanoVersion="1.26.0";
cabalVersion="3.4.0.0";
ghcVersion="8.10.4";

ARCH=`uname -m`

# Install dependencies

DEBIAN_FRONTEND="noninteractive"

# Install dependencies
# $ sudo apt-get update
# $ sudo apt-get install -y autoconf automake build-essential g++ git jq libffi-dev libgmp-dev libncursesw5 libnuma-dev libssl-dev \
#     libsystemd-dev libtinfo-dev libtool llvm make pkg-config tmux wget zlib1g-dev

workdir=`realpath ./target`

## Build + Install Libsodium ########################################################################################

if [[ "$LD_LIBRARY_PATH" != *"libsodium"* ]]; then

  echo "Libsodium not found in LD_LIBRARY_PATH ..."

  srcLibsod="$workdir/libsodium/src"
  outLibsod="$workdir/libsodium/out"

  if [ ! -d $outLibsod ]; then

    rm -rf $srcLibsod
    mkdir -p $srcLibsod

    git clone "https://github.com/input-output-hk/libsodium" $srcLibsod
    cd $srcLibsod; git checkout 66f017f1

    ./autogen.sh
    ./configure  --prefix=$outLibsod
    make install
  fi

  if [ ! -d $outLibsod/lib ]; then
    echo "[Error] Failed to build: $outLibsod"
    exit 1
  fi

  export LD_LIBRARY_PATH="$outLibsod/lib"
  export PKG_CONFIG_PATH="$outLibsod/lib/pkgconfig"
fi

echo "Checking libsodium ..."
if [[ "$LD_LIBRARY_PATH" != *"libsodium"* ]]; then
  echo "[Error] Cannot find libsodium in $LD_LIBRARY_PATH"
  exit 1
else
  echo "OK"
fi

## Build + Install GHC ########################################################################################

if [[ "$PATH" != *"ghc-${ghcVersion}"* ]]; then

  srcGHC="$workdir/ghc-${ghcVersion}/src"
  outGHC="$workdir/ghc-${ghcVersion}/out"

  if [ ! -d $outGHC ]; then

    rm -rf $srcGHC
    mkdir -p $srcGHC

    echo "Extracting ghc-${ghcVersion} sources ..."

    cd $srcGHC
    if [ "$ARCH" = "x86_64" ]; then
      wget -q "https://downloads.haskell.org/ghc/${ghcVersion}/ghc-${ghcVersion}-x86_64-deb10-linux.tar.xz"
      tar xf ghc-${ghcVersion}-x86_64-deb10-linux.tar.xz
    elif [ "$ARCH" = "aarch64" ]; then
      wget -q "https://downloads.haskell.org/ghc/${ghcVersion}/ghc-${ghcVersion}-aarch64-deb10-linux.tar.xz"
      tar xf ghc-${ghcVersion}-aarch64-deb10-linux.tar.xz
    fi

    mkdir -p $outGHC

    cd $srcGHC/ghc-${ghcVersion}
    ./configure --prefix=$outGHC
    make install
  fi

  if [ ! -d $outGHC/bin ]; then
    echo "[Error] Failed to build: $outGHC"
    exit 1
  fi

  export PATH="$PATH:$outGHC/bin"
fi


echo "Checking ghc ..."
if [[ "$PATH" != *"ghc-${ghcVersion}"* ]]; then
  echo "[Error] Cannot find ghc-${ghcVersion} in $PATH"
  exit 1
else
  echo "OK"
fi

## Build + Install Cabal ########################################################################################

if [[ "$PATH" != *"cabal-${cabalVersion}"* ]]; then

  srcCabal="$workdir/cabal-${cabalVersion}/src"
  outCabal="$workdir/cabal-${cabalVersion}/out"

  if [ ! -d $outCabal ]; then

    cabalTag="${cabalVersion}-rc4"

    rm -rf $srcCabal
    mkdir -p $srcCabal
    mkdir -p $outCabal/bin

    echo "Extracting cabal-${cabalVersion} binary ..."

    cd $srcCabal
    if [ "${ARCH}" = "x86_64" ]; then
      wget -q "https://oleg.fi/cabal-install-${cabalTag}/cabal-install-${cabalVersion}-x86_64-ubuntu-16.04.tar.xz"
      tar -xf cabal-install-${cabalVersion}-x86_64-ubuntu-16.04.tar.xz -C ${outCabal}/bin
    elif [ "${ARCH}" = "aarch64" ]; then
      wget -q "https://oleg.fi/cabal-install-${cabalTag}/cabal-install-${cabalVersion}-aarch64-ubuntu-18.04.tar.xz"
      tar -xf cabal-install-${cabalVersion}-aarch64-ubuntu-18.04.tar.xz -C ${outCabal}/bin
    fi
  fi

  if [ ! -d $outCabal/bin ]; then
    echo "[Error] Failed to build: $outCabal"
    exit 1
  fi

  export PATH="$PATH:$outCabal/bin"
fi


echo "Checking cabal ..."
if [[ "$PATH" != *"cabal-${cabalVersion}"* ]]; then
  echo "[Error] Cannot find cabal-${cabalVersion} in $PATH"
  exit 1
else
  echo "OK"
fi

## Buid + Install Cardano #######################################################################################################

srcCardano="$workdir/cardano-node-${cardanoVersion}/src"
outCardano="$workdir/cardano-node-${cardanoVersion}/out"

if [[ "$PATH" != *"cardano-node-${cardanoVersion}"* ]]; then

  if [ ! -d $outCardano ]; then

    echo "Cloning cardano-node-${cardanoVersion} ..."
    git clone -b ${cardanoVersion} https://github.com/input-output-hk/cardano-node.git $srcCardano/cardano-node
    cd $srcCardano/cardano-node

    echo "Update cabal ..."
    cabal update

    echo "Configure cabal ..."
    cabal --allow-newer=base configure

    echo "package cardano-crypto-praos" >> cabal.project.local
    echo "  flags: -external-libsodium-vrf" >> cabal.project.local

    echo "Build all ..."
    cabal build all

    echo "Copy binaries ..."
    mkdir -p $outCardano/bin
    cp ./dist-newstyle/build/$ARCH-linux/ghc-${ghcVersion}/cardano-node-${cardanoVersion}/x/cardano-node/build/cardano-node/cardano-node $outCardano/bin
    cp ./dist-newstyle/build/$ARCH-linux/ghc-${ghcVersion}/cardano-cli-${cardanoVersion}/x/cardano-cli/build/cardano-cli/cardano-cli $outCardano/bin
    cp ./dist-newstyle/build/$ARCH-linux/ghc-${ghcVersion}/cardano-submit-api-${cardanoVersion}/x/cardano-submit-api/build/cardano-submit-api/cardano-submit-api $outCardano/bin
  fi

  if [ ! -d $outCardano/bin ]; then
    echo "[Error] Failed to build: $outCardano"
    exit 1
  fi

  export PATH="$PATH:$outCardano/bin"
fi

echo "Checking cardano-node ..."
if [[ "$PATH" != *"cardano-node-${cardanoVersion}"* ]]; then
  echo "[Error] Cannot find cardano-node-${cardanoVersion} in $PATH"
  exit 1
else
  echo "OK"
fi

cardano-node run --help
