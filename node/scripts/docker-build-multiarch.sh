#!/bin/bash

CARDANO=1.24.2

NESSUS_REV=rev6

VERSION="$CARDANO-$NESSUS_REV"

VERSION_SUFFIX="$(echo $NESSUS_REV | cut -d'-' -f 2)"
if [ "${VERSION_SUFFIX}" != "dev" ]; then

  docker manifest rm nessusio/cardano:$VERSION
  docker manifest rm nessusio/cardano:$CARDANO
  docker manifest rm nessusio/cardano:latest

  docker manifest create nessusio/cardano:$VERSION \
    --amend nessusio/cardano:$VERSION-amd64 \
    --amend nessusio/cardano:$VERSION-arm64
  
  docker manifest inspect nessusio/cardano:$VERSION

  docker manifest push nessusio/cardano:$VERSION
  
  docker manifest create nessusio/cardano:$CARDANO \
    --amend nessusio/cardano:$CARDANO-amd64 \
    --amend nessusio/cardano:$CARDANO-arm64
  
  docker manifest inspect nessusio/cardano:$CARDANO
  
  docker manifest push nessusio/cardano:$CARDANO
  
  docker manifest create nessusio/cardano:latest \
    --amend nessusio/cardano:latest-amd64 \
    --amend nessusio/cardano:latest-arm64
  
  docker manifest inspect nessusio/cardano:latest
  
  docker manifest push nessusio/cardano:latest
  
else

  docker manifest rm nessusio/cardano:dev
  
  docker manifest create nessusio/cardano:dev \
    --amend nessusio/cardano:dev-amd64 \
    --amend nessusio/cardano:dev-arm64

  docker manifest inspect nessusio/cardano:dev
  
  docker manifest push nessusio/cardano:dev

fi
