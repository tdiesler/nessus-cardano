#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

CARDANO_VER=1.25.1

NESSUS_REV=rev1
DOCKER_PUSH=true
 
CABAL_VER=3.4.0.0
GHC_VER=8.10.2

ARCH=`uname -m`

if [ "${ARCH}" == "aarch64" ]; then

  ARCH_TAG="arm64"

elif [ "${ARCH}" == "x86_64" ]; then

  ARCH_TAG="amd64"
  
fi

FULL_VERSION="${CARDANO_VER}-${NESSUS_REV}-${ARCH_TAG}"
ARCH_VERSION="${CARDANO_VER}-${ARCH_TAG}"
LATEST_VERSION="latest-${ARCH_TAG}"

echo "##########################################################"
echo "# Install Cardano Docker"
echo "#"
echo "# VERSION: ${FULL_VERSION}"
echo "#"
echo "# CARDANO: ${CARDANO_VER}"
echo "# CABAL:   ${CABAL_VER}"
echo "# GHC:     ${GHC_VER}"

if [ "${ARCH}" != "aarch64" ] && [ "${ARCH}" != "x86_64" ]; then
  echo "Unsupported architecture: ${ARCH}"
  exit 1
fi
 
PRG="$0"
HOMEDIR=`dirname $PRG`/..
cd ${HOMEDIR}

docker build \
  --build-arg CARDANO_VER=${CARDANO_VER} \
  --build-arg CABAL_VER=${CABAL_VER} \
  --build-arg GHC_VER=${GHC_VER} \
  --build-arg ARCH=${ARCH} \
  -t nessusio/cardano:${FULL_VERSION} docker


VERSION_SUFFIX="$(echo $NESSUS_REV | cut -d'-' -f 2)"
if [ "${VERSION_SUFFIX}" != "dev" ]; then

  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:${ARCH_VERSION}
  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:${LATEST_VERSION}
  
  echo "Successfully tagged nessusio/cardano:${ARCH_VERSION}"
  echo "Successfully tagged nessusio/cardano:${LATEST_VERSION}"
  
  docker push nessusio/cardano:${FULL_VERSION}
  docker push nessusio/cardano:${ARCH_VERSION}
  docker push nessusio/cardano:${LATEST_VERSION}

else

  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:${CARDANO_VER}-dev-${ARCH_TAG}
  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:dev-${ARCH_TAG}
  
  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:${CARDANO_VER}-dev
  docker tag nessusio/cardano:${FULL_VERSION} nessusio/cardano:dev
  
  echo "Successfully tagged nessusio/cardano:${CARDANO_VER}-dev-${ARCH_TAG}"
  echo "Successfully tagged nessusio/cardano:dev-${ARCH_TAG}"
  
  echo "Successfully tagged nessusio/cardano:${CARDANO_VER}-dev"
  echo "Successfully tagged nessusio/cardano:dev"
  
  if [ ${DOCKER_PUSH} ]; then
  
    docker push nessusio/cardano:${CARDANO_VER}-dev-${ARCH_TAG}
    docker push nessusio/cardano:dev-${ARCH_TAG}
  fi

fi
