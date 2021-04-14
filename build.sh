#!/bin/bash

CARDANO_VER="1.26.1"
NESSUS_REV="dev"

MMONIT_VER="3.7.7"
MMONIT_REV="rev1"

MONIT_VER="5.27.1"
MONIT_REV="rev1"

CNCLI_VER="1.5.1"
CABAL_VER="3.2.0.0"
GHC_VER="8.10.4"

ARCH=`uname -m`

# Checking supported architectures ====================================================================================

if [[ ${ARCH} == "x86_64" ]]; then ARCH_SUFFIX="amd64"
elif [[ ${ARCH} == "aarch64" ]]; then ARCH_SUFFIX="arm64"
else
  echo "[ERROR] Unsupported platform architecture: ${ARCH}"
  exit 1
fi

# Extracting binaries from arm64 Docker image =========================================================================
#
# Note, we currently cannot build the cardano derivation for arm64
# https://github.com/input-output-hk/haskell.nix/issues/1027

if [[ ${ARCH_SUFFIX} == "arm64" ]]; then

  AUX_IMAGE_VERSION="${CARDANO_VER}-${ARCH_SUFFIX}"

  dockerBuildOut="./nix/cardano/target/cardano-node-${AUX_IMAGE_VERSION}"

  if [[ ! -d ${dockerBuildOut} ]]; then

    AUX_IMAGE_NAME="nessusio/cardano-aux:${AUX_IMAGE_VERSION}"

    # Override cabal version for arm64
    CABAL_VER="3.4.0.0"

    docker build \
      --build-arg CARDANO_VER=${CARDANO_VER} \
      --build-arg CABAL_VER=${CABAL_VER} \
      --build-arg GHC_VER=${GHC_VER} \
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
  push=$2

  if [[ $shortName == "cardano-node" || $shortName == "cardano-tools" ]]; then
    VERSION_MAJOR=${CARDANO_VER}
    VERSION_MINOR=${NESSUS_REV}

  elif [[ $shortName == "mmonit" ]]; then
    VERSION_MAJOR=${MMONIT_VER}
    VERSION_MINOR=${MMONIT_REV}

  elif [[ $shortName == "monit" ]]; then
    VERSION_MAJOR=${MONIT_VER}
    VERSION_MINOR=${MONIT_REV}

  else
      echo "[Error] Illegal argument: $1"
      echo "Usage: $0 [cardano-node|cardano-tools|mmonit|monit] [push]"
      exit 1
  fi

  FULL_ARCH_VERSION="${VERSION_MAJOR}-${VERSION_MINOR}-${ARCH_SUFFIX}"

  IMAGE_NAME="nessusio/${shortName}"
  FULL_IMAGE_NAME="${IMAGE_NAME}:${FULL_ARCH_VERSION}"

  echo "##########################################################"
  echo "# Building ${IMAGE_NAME} for ${ARCH}"
  echo "#"
  echo "# VERSION: ${FULL_ARCH_VERSION}"
  echo "#"

  if [[ $shortName == "cardano-node" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/node \
      --argstr cardanoVersion ${CARDANO_VER} \
      --argstr nessusRevision ${NESSUS_REV} \
      --argstr cabalVersion ${CABAL_VER} \
      --argstr ghcVersion ${GHC_VER} \
      --argstr imageArch ${ARCH_SUFFIX}`

  elif [[ $shortName == "cardano-tools" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/tools \
      --argstr cardanoVersion ${CARDANO_VER} \
      --argstr nessusRevision ${NESSUS_REV} \
      --argstr cncliVersion ${CNCLI_VER} \
      --argstr cabalVersion ${CABAL_VER} \
      --argstr ghcVersion ${GHC_VER} \
      --argstr imageArch ${ARCH_SUFFIX}`

  elif [[ $shortName == "mmonit" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/mmonit \
      --argstr mmonitVersion ${MMONIT_VER} \
      --argstr mmonitRevision ${MMONIT_REV} \
      --argstr imageArch ${ARCH_SUFFIX}`

  elif [[ $shortName == "monit" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/monit \
      --argstr monitVersion ${MONIT_VER} \
      --argstr monitRevision ${MONIT_REV} \
      --argstr imageArch ${ARCH_SUFFIX}`
  fi

  if [[ $? -ne 0 ]]; then
    echo "[ERROR] Unable to build image '${FULL_IMAGE_NAME}'"
    exit 1
  fi

  echo "Loading image ..."
  docker load -i ${IMAGEPATH}

  if [[ ${VERSION_MINOR} != "dev" ]]; then

    MAJOR_ARCH_VERSION="${VERSION_MAJOR}-${ARCH_SUFFIX}"
    LATEST_ARCH_VERSION="latest-${ARCH_SUFFIX}"

    # Tag with arch suffix
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:latest"

    echo "Tagged image: ${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:latest"

    if [[ $push == true ]]; then
      docker push "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    fi

  else

    DEV_ARCH_VERSION="dev-${ARCH_SUFFIX}"

    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${DEV_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:dev"

    echo "Tagged image: ${IMAGE_NAME}:${DEV_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:dev"

    if [[ $push == true ]]; then
      docker push "${IMAGE_NAME}:${DEV_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:dev"
    fi

  fi
}

if (( $# < 1 )); then
    echo "[Error] Illegal number of arguments."
    echo "Usage: $0 [cardano-node|cardano-tools|mmonit|monit] [push]"
    exit 1
fi

shortName=$1
push=$2

if [[ $shortName == "all" ]]; then
  buildImage "cardano-node" $push
  buildImage "cardano-tools" $push
  buildImage "mmonit" $push
  buildImage "monit" $push

elif [[ $shortName == "cardano" ]]; then
  buildImage "cardano-node" $push
  buildImage "cardano-tools" $push

else
  buildImage $shortName $push
fi
