#!/bin/bash

CARDANO_VER="1.25.1"
NESSUS_REV="rev2"

CNCLI_VER="1.4.0"
ARCH=`uname -m`

# Checking supported architectures ====================================================================================

if [[ ${ARCH} == "x86_64" ]]; then

  ARCH_SUFFIX="amd64"

elif [[ ${ARCH} == "aarch64" ]]; then

  ARCH_SUFFIX="arm64"

else
  echo "[ERROR] Unsupported platform architecture: `uname -a`"
  exit 1
fi

FULL_ARCH_VERSION="${CARDANO_VER}-${NESSUS_REV}-${ARCH_SUFFIX}"

# Extracting binaries from arm64 Docker image =========================================================================
#
# Note, we currently cannot build the cardano derivation for arm64
# https://github.com/input-output-hk/haskell.nix/issues/1027

if [[ ${ARCH_SUFFIX} == "arm64" ]]; then

  dockerBuildOut="./nix/cardano/target/cardano-node-${FULL_ARCH_VERSION}"

  if [[ ! -d ${dockerBuildOut} ]]; then

    AUX_IMAGE_NAME="nessusio/cardano-aux:${FULL_ARCH_VERSION}"

    docker build \
      --build-arg CARDANO_VER=${CARDANO_VER} \
      --build-arg ARCH=${ARCH} \
      --tag ${AUX_IMAGE_NAME} \
      ./nix/cardano

    if [ $? -ne 0 ]; then
      echo "[ERROR] Unable to build image '${AUX_IMAGE_NAME}'"
      exit 1
    fi

    echo "Copy binaries to ${dockerBuildOut} ..."
    mkdir -p ${dockerBuildOut}/bin

    docker rm -f tmp &> /dev/null
    docker run --name tmp ${AUX_IMAGE_NAME} &> /dev/null
    docker cp tmp:/usr/local/bin/cardano-node ${dockerBuildOut}/bin/cardano-node
    docker cp tmp:/usr/local/bin/cardano-cli ${dockerBuildOut}/bin/cardano-cli
    docker rm -f tmp &> /dev/null

  else

    echo "Using binaries from ${dockerBuildOut} ..."

  fi
fi

function buildImage () {

  shortName=$1

  IMAGE_NAME="nessusio/${shortName}"
  FULL_IMAGE_NAME="${IMAGE_NAME}:${FULL_ARCH_VERSION}"

  echo "##########################################################"
  echo "# Building ${IMAGE_NAME} for ${ARCH}"
  echo "#"
  echo "# VERSION: ${FULL_ARCH_VERSION}"
  echo "#"

  IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix \
    --argstr cardanoVersion ${CARDANO_VER} \
    --argstr nessusRevision ${NESSUS_REV} \
    --argstr cncliVersion ${CNCLI_VER} \
    --argstr shortName ${shortName}`

  if [[ $? -ne 0 ]]; then
    echo "[ERROR] Unable to build image '${FULL_IMAGE_NAME}'"
    exit 1
  fi

  echo "Loading image ..."
  docker load -i ${IMAGEPATH}

  if [[ ${NESSUS_REV} != "dev" ]]; then

    CARDANO_ARCH_VERSION="${CARDANO_VER}-${ARCH_SUFFIX}"
    LATEST_ARCH_VERSION="latest-${ARCH_SUFFIX}"

    # Tag with arch suffix
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${CARDANO_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:latest"

    echo "Tagged image: ${IMAGE_NAME}:${CARDANO_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:latest"

    if [[ $PUSH == true ]]; then
      docker push "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${CARDANO_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    fi

  else

    DEV_ARCH_VERSION="dev-${ARCH_SUFFIX}"

    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${DEV_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:dev"

    echo "Tagged image: ${IMAGE_NAME}:${DEV_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:dev"

    if [[ $PUSH == true ]]; then
      docker push "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${DEV_ARCH_VERSION}"
    fi

  fi
}

if (( $# < 1 )); then
    echo "[Error] Illegal number of arguments."
    echo "Usage: $0 [all|[cardano-node|cardano-tools]] [push]"
    exit 1
fi

PUSH=$2

if [[ "$1" == "all" ]]; then
  buildImage "cardano-node"
  buildImage "cardano-tools"
else
  buildImage $1
fi
