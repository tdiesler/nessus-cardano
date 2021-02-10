#!/bin/bash

CARDANO_VER="1.25.1"
NESSUS_REV="dev"

FULL_VERSION="${CARDANO_VER}-${NESSUS_REV}"

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

  if [[ $PUSH == true ]]; then
    echo "docker manifest push ${IMAGE_NAME}:${IMAGE_TAG}"
    docker manifest push "${IMAGE_NAME}:${IMAGE_TAG}"
  fi
}

function buildImage () {

  shortName="$1"

  if [[ ${NESSUS_REV} != "dev" ]]; then

    buildManifest ${shortName} ${FULL_VERSION}
    buildManifest ${shortName} ${CARDANO_VER}
    buildManifest ${shortName} "latest"

  else

    buildManifest ${shortName} ${FULL_VERSION}
    buildManifest ${shortName} "dev"

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
