#!/bin/bash

source ./build-common.sh

# Extracting binaries from arm64 Docker images =================================
#
# Note, we currently cannot build the cardano derivation for arm64
# https://github.com/input-output-hk/haskell.nix/issues/1027

function buildCardanoNodeArm64 () {

  AUX_IMAGE_VERSION="${CARDANO_VER}-${ARCH_SUFFIX}"

  dockerBuildOut="./nix/cardano/target/cardano-node-${AUX_IMAGE_VERSION}"

  if [[ ! -d ${dockerBuildOut} ]]; then

    AUX_IMAGE_NAME="nessusio/cardano-aux:${AUX_IMAGE_VERSION}"

    # Override cabal version for arm64
    CABAL_VER="3.4.0.0"

    docker build \
      --build-arg CARDANO_VER="${CARDANO_VER}" \
      --build-arg CABAL_VER="${CABAL_VER}" \
      --build-arg GHC_VER="${GHC_VER}" \
      --build-arg ARCH="${ARCH}" \
      --tag "${AUX_IMAGE_NAME}" \
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

    echo "Using ${dockerBuildOut} ..."
  fi
}

function buildCncliArm64 () {

  AUX_IMAGE_VERSION="${CNCLI_VER}-${ARCH_SUFFIX}"

  dockerBuildOut="./nix/cncli/target/cncli-${AUX_IMAGE_VERSION}"

  if [[ ! -d ${dockerBuildOut} ]]; then

    AUX_IMAGE_NAME="nessusio/cncli-aux:${AUX_IMAGE_VERSION}"

    docker build \
      --build-arg CNCLI_VER="${CNCLI_VER}" \
      --build-arg ARCH="${ARCH}" \
      --tag "${AUX_IMAGE_NAME}" \
      ./nix/cncli

    if [ $? -ne 0 ]; then
      echo "[ERROR] Unable to build image '${AUX_IMAGE_NAME}'"
      exit 1
    fi

    echo "Copy binaries to ${dockerBuildOut} ..."
    mkdir -p ${dockerBuildOut}/bin

    docker rm -f tmp &> /dev/null
    docker run --name tmp ${AUX_IMAGE_NAME} &> /dev/null
    docker cp tmp:/usr/local/bin/cncli ${dockerBuildOut}/bin/cncli
    docker rm -f tmp &> /dev/null

  else

    echo "Using ${dockerBuildOut} ..."
  fi
}

# Bild Debian base image =======================================================
#
# Note, we currently cannot build the lnav derivation for arm64
# https://github.com/tstack/lnav/issues/882

function buildBaseImage () {

  AUX_IMAGE_VERSION="${DEBIAN_VER}-${ARCH_SUFFIX}"
  AUX_IMAGE_NAME="nessusio/debian:${AUX_IMAGE_VERSION}"

  dockerSaveDir="./nix/docker/baseImage/target"
  dockerSaveFile="${dockerSaveDir}/nessusio-debian-${AUX_IMAGE_VERSION}.tgz"

  if [[ ! -f ${dockerSaveFile} ]]; then

    docker build \
      --build-arg LNAV_VER="${LNAV_VER}" \
      --tag "${AUX_IMAGE_NAME}" \
      ./nix/docker/baseImage

    if [ $? -ne 0 ]; then
      echo "[ERROR] Unable to build image '${AUX_IMAGE_NAME}'"
      exit 1
    fi

    echo "Save image ${dockerSaveFile} ..."
    mkdir -p ${dockerSaveDir} && docker save -o ${dockerSaveFile} ${AUX_IMAGE_NAME}

  else

    echo "Using ${dockerSaveFile} ..."
  fi
}

function buildImage () {

  shortName=$1
  push=$2

  if [[ $shortName == "cardano-node" || $shortName == "cardano-tools" ]]; then
    VERSION_MAJOR="${CARDANO_VER}"
    VERSION_REV="${CARDANO_REV}"

  elif [[ $shortName == "mmonit" ]]; then
    VERSION_MAJOR="${MMONIT_VER}"
    VERSION_REV="${MMONIT_REV}"

  elif [[ $shortName == "monit" ]]; then
    VERSION_MAJOR="${MONIT_VER}"
    VERSION_REV="${MONIT_REV}"

  else
      echo "[Error] Illegal argument: $1"
      echo "Usage: $0 [all|cardano-node|cardano-tools|mmonit|monit] [push]"
      exit 1
  fi

  if [[ "${VERSION_REV}" == "" ]]; then
    FULL_VERSION="${VERSION_MAJOR}"
    LATEST_VERSION="latest"

  elif [[ "${VERSION_REV}" == "-dev" ]]; then
    FULL_VERSION="${VERSION_MAJOR}-dev"
    LATEST_VERSION="dev"

  else
    FULL_VERSION="${VERSION_MAJOR}${VERSION_REV}"
    LATEST_VERSION="latest"
  fi

  FULL_ARCH_VERSION="${FULL_VERSION}-${ARCH_SUFFIX}"
  LATEST_ARCH_VERSION="${LATEST_VERSION}-${ARCH_SUFFIX}"

  IMAGE_NAME="nessusio/${shortName}"
  FULL_IMAGE_NAME="${IMAGE_NAME}:${FULL_ARCH_VERSION}"

  echo "##########################################################"
  echo "# Building ${IMAGE_NAME} for ${ARCH}"
  echo "#"
  echo "# VERSION: ${FULL_ARCH_VERSION}"
  echo "#"

  if [[ $shortName == "cardano-node" ]]; then

    # Build the Debian base image
    buildBaseImage

    # Build the Cardano binaries for arm64
    if [[ ${ARCH_SUFFIX} == "arm64" ]]; then
      buildCardanoNodeArm64
      buildCncliArm64
    fi

    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/node \
      --argstr cardanoVersion "${CARDANO_VER}" \
      --argstr cardanoRev "${CARDANO_REV}" \
      --argstr debianVersion "${DEBIAN_VER}" \
      --argstr cabalVersion "${CABAL_VER}" \
      --argstr ghcVersion "${GHC_VER}" \
      --argstr glvVersion "${GLVIEW_VER}" \
      --argstr imageArch "${ARCH_SUFFIX}"`

  elif [[ $shortName == "cardano-tools" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/tools \
      --argstr cardanoVersion "${CARDANO_VER}" \
      --argstr cardanoRev "${CARDANO_REV}" \
      --argstr debianVersion "${DEBIAN_VER}" \
      --argstr cncliVersion "${CNCLI_VER}" \
      --argstr cabalVersion "${CABAL_VER}" \
      --argstr ghcVersion "${GHC_VER}" \
      --argstr imageArch "${ARCH_SUFFIX}"`

  elif [[ $shortName == "mmonit" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/mmonit \
      --argstr mmonitVersion "${MMONIT_VER}" \
      --argstr mmonitRevision "${MMONIT_REV}" \
      --argstr imageArch "${ARCH_SUFFIX}"`

  elif [[ $shortName == "monit" ]]; then
    IMAGEPATH=`nix-build --option sandbox false --show-trace ./nix/docker/monit \
      --argstr monitVersion "${MONIT_VER}" \
      --argstr monitRevision "${MONIT_REV}" \
      --argstr imageArch "${ARCH_SUFFIX}"`
  fi

  if [[ $? -ne 0 ]]; then
    echo "[ERROR] Unable to build image '${FULL_IMAGE_NAME}'"
    exit 1
  fi

  echo "Loading image ..."
  docker load -i ${IMAGEPATH}

  if [[ "${VERSION_REV}" == "" || "${VERSION_REV}" == "-dev" ]]; then

    # Tag images aliases
    # docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${FULL_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_VERSION}"

    echo "Tagged image: ${IMAGE_NAME}:${FULL_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_VERSION}"

    if [[ $push == true ]]; then
      docker push "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${FULL_VERSION}"
      docker push "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${LATEST_VERSION}"
    fi

  else

    MAJOR_ARCH_VERSION="${VERSION_MAJOR}-${ARCH_SUFFIX}"

    # Tag images aliases
    # docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${FULL_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${VERSION_MAJOR}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    docker tag ${FULL_IMAGE_NAME} "${IMAGE_NAME}:${LATEST_VERSION}"

    echo "Tagged image: ${IMAGE_NAME}:${FULL_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${VERSION_MAJOR}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
    echo "Tagged image: ${IMAGE_NAME}:${LATEST_VERSION}"

    if [[ $push == true ]]; then
      docker push "${IMAGE_NAME}:${FULL_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${FULL_VERSION}"
      docker push "${IMAGE_NAME}:${MAJOR_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${VERSION_MAJOR}"
      docker push "${IMAGE_NAME}:${LATEST_ARCH_VERSION}"
      docker push "${IMAGE_NAME}:${LATEST_VERSION}"
    fi
  fi
}

if (( $# < 1 )); then
    echo "[Error] Illegal number of arguments."
    echo "Usage: $0 [all|cardano-node|cardano-tools|mmonit|monit] [push]"
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
