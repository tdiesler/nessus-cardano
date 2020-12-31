#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

ARCH=`uname -m`

MONIT_VER=5.27.1

NESSUS_REV=rev1
 
if [ "$ARCH" == "aarch64" ]; then

  ARCH_TAG="arm64"

elif [ "$ARCH" == "x86_64" ]; then

  ARCH_TAG="amd64"
  
fi

FULL_VERSION="${MONIT_VER}-${NESSUS_REV}-${ARCH_TAG}"
ARCH_VERSION="${MONIT_VER}-${ARCH_TAG}"
LATEST_VERSION="latest-${ARCH_TAG}"

echo "##########################################################"
echo "# Install Monit Docker"
echo "#"
echo "# VERSION: ${FULL_VERSION}"
echo "#"
echo "# MONIT: ${MONIT_VER}"

if [ "$ARCH" != "aarch64" ] && [ "$ARCH" != "x86_64" ]; then
  echo "Unsupported architecture: $ARCH"
  exit 1
fi
 
PRG="$0"
HOMEDIR=`dirname $PRG`/..
cd ${HOMEDIR}

docker build \
  --build-arg MONIT_VER=${MONIT_VER} \
  --build-arg ARCH=${ARCH} \
  -t nessusio/monit:${FULL_VERSION} docker

docker tag nessusio/monit:${FULL_VERSION} nessusio/monit:${ARCH_VERSION}
docker tag nessusio/monit:${FULL_VERSION} nessusio/monit:${LATEST_VERSION}

docker push nessusio/monit:${FULL_VERSION}
docker push nessusio/monit:${ARCH_VERSION}
docker push nessusio/monit:${LATEST_VERSION}
