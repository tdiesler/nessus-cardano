#!/bin/bash

MMONIT=3.7.6
VERSION="${MMONIT}-rev2"

VERSION_SUFFIX="$(echo $NESSUS_REV | cut -d'-' -f 2)"
if [ "${VERSION_SUFFIX}" != "dev" ]; then

  docker manifest rm nessusio/mmonit:${VERSION}
  docker manifest rm nessusio/mmonit:${MMONIT}
  docker manifest rm nessusio/mmonit:latest

  docker manifest create nessusio/mmonit:${VERSION} \
    --amend nessusio/mmonit:${VERSION}-amd64 \
    --amend nessusio/mmonit:${VERSION}-arm64
  
  docker manifest inspect nessusio/mmonit:${VERSION}
  
  docker manifest push nessusio/mmonit:${VERSION}
  
  docker manifest create nessusio/mmonit:${MMONIT} \
    --amend nessusio/mmonit:${MMONIT}-amd64 \
    --amend nessusio/mmonit:${MMONIT}-arm64
  
  docker manifest inspect nessusio/mmonit:${MMONIT}
  
  docker manifest push nessusio/mmonit:${MMONIT}
  
  docker manifest create nessusio/mmonit \
    --amend nessusio/mmonit:latest-amd64 \
    --amend nessusio/mmonit:latest-arm64
  
  docker manifest inspect nessusio/mmonit
  
  docker manifest push nessusio/mmonit
  
else

  docker manifest rm nessusio/mmonit:dev
  
  docker manifest create nessusio/mmonit:dev \
    --amend nessusio/mmonit:dev-amd64 \
    --amend nessusio/mmonit:dev-arm64

  docker manifest inspect nessusio/mmonit:dev
  
  docker manifest push nessusio/mmonit:dev

fi
