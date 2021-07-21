#!/bin/bash

source ./build-common.sh

function buildManifest() {

  IMAGE_NAME="nessusio/$1"
  IMAGE_TAG="$2"

  echo "docker manifest rm ${IMAGE_NAME}:${IMAGE_TAG}"
  docker manifest rm "${IMAGE_NAME}:${IMAGE_TAG}"

  echo "docker manifest create ${IMAGE_NAME}:${IMAGE_TAG}"
  docker manifest create "${IMAGE_NAME}:${IMAGE_TAG}" \
    --amend "${IMAGE_NAME}:${IMAGE_TAG}-amd64" \
    --amend "${IMAGE_NAME}:${IMAGE_TAG}-arm64"

  echo "docker manifest annotate ${IMAGE_NAME}:${IMAGE_TAG}"
  docker manifest annotate "${IMAGE_NAME}:${IMAGE_TAG}" \
    "${IMAGE_NAME}:${IMAGE_TAG}-arm64" \
    --os=linux --arch=arm64 --variant=v8

  echo "docker manifest inspect ${IMAGE_NAME}:${IMAGE_TAG}"
  docker manifest inspect "${IMAGE_NAME}:${IMAGE_TAG}"
  if [[ $? != 0 ]]; then
      echo "[Error] No such manifest: ${IMAGE_NAME}:${IMAGE_TAG}"
      exit 1
  fi

  if [[ $PUSH == true ]]; then
    echo "docker manifest push ${IMAGE_NAME}:${IMAGE_TAG}"
    docker manifest push "${IMAGE_NAME}:${IMAGE_TAG}"
  fi
}

function buildImage () {

  shortName=$1

  if [[ $shortName == "cardano-node" || $shortName == "cardano-tools" ]]; then
    VERSION_MAJOR=${CARDANO_VER}
    VERSION_REV=${CARDANO_REV}

  elif [[ $shortName == "mmonit" ]]; then
    VERSION_MAJOR=${MMONIT_VER}
    VERSION_REV=${MMONIT_REV}

  elif [[ $shortName == "monit" ]]; then
    VERSION_MAJOR=${MONIT_VER}
    VERSION_REV=${MONIT_REV}

  else
      echo "[Error] Illegal argument: $1"
      echo "Usage: $0 [cardano-node|cardano-tools|mmonit|monit] [push]"
      exit 1
  fi

  FULL_VERSION="${VERSION_MAJOR}${VERSION_REV}"

  if [[ ${VERSION_MINOR} != "dev" ]]; then

    buildManifest ${shortName} ${FULL_VERSION}
    buildManifest ${shortName} ${VERSION_MAJOR}
    buildManifest ${shortName} "latest"

  else

    buildManifest ${shortName} "dev"

  fi
}

if (( $# < 1 )); then
    echo "[Error] Illegal number of arguments."
    echo "Usage: $0 [cardano-node|cardano-tools|mmonit|monit] [push]"
    exit 1
fi

shortName=$1
PUSH=$2

if [[ $shortName == "all" ]]; then
  buildImage "cardano-node"
  buildImage "cardano-tools"
  buildImage "mmonit"
  buildImage "monit"

elif [[ $shortName == "cardano" ]]; then
  buildImage "cardano-node"
  buildImage "cardano-tools"

else
  buildImage $shortName
fi
