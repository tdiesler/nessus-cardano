#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

ARCH=`uname -m`

MMONIT_VER=3.7.6

NESSUS_REV=rev1
 
if [ "$ARCH" == "aarch64" ]; then

  ARCH_TAG="arm64"

elif [ "$ARCH" == "x86_64" ]; then

  ARCH_TAG="amd64"
  
fi

FULL_VERSION="${MMONIT_VER}-${NESSUS_REV}-${ARCH_TAG}"
ARCH_VERSION="${MMONIT_VER}-${ARCH_TAG}"
LATEST_VERSION="latest-${ARCH_TAG}"

echo "##########################################################"
echo "# Install M/Monit Docker"
echo "#"
echo "# VERSION: ${FULL_VERSION}"
echo "#"
echo "# M/MONIT: ${MMONIT_VER}"

if [ "$ARCH" != "aarch64" ] && [ "$ARCH" != "x86_64" ]; then
  echo "Unsupported architecture: $ARCH"
  exit 1
fi
 
PRG="$0"
HOMEDIR=`dirname $PRG`/..
cd ${HOMEDIR}

docker build \
  --build-arg MMONIT_VER=${MMONIT_VER} \
  --build-arg ARCH=${ARCH} \
  -t nessusio/mmonit:${FULL_VERSION} docker

docker tag nessusio/mmonit:${FULL_VERSION} nessusio/mmonit:${ARCH_VERSION}
docker tag nessusio/mmonit:${FULL_VERSION} nessusio/mmonit:${LATEST_VERSION}

docker push nessusio/mmonit:${FULL_VERSION}
docker push nessusio/mmonit:${ARCH_VERSION}
docker push nessusio/mmonit:${LATEST_VERSION}
