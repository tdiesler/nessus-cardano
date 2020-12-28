#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

CARDANO=1.24.2
CABAL=3.2.0.0
GHC=8.10.2
 
echo "##########################################################"
echo "# Install Cardano Docker"
echo "#"
echo "# CARDANO: $CARDANO"
echo "# CABAL:   $CABAL"
echo "# GHC:     $GHC"

cd ~/nessus-cardano/images/node

docker build \
  --build-arg CARDANO=$CARDANO \
  --build-arg CABAL=$CABAL \
  --build-arg GHC=$GHC \
  -f docker/cardano-x86_64.dockerfile \
  -t nessusio/cardano docker
  