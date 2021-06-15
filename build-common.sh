#!/bin/bash

# CARDANO_BUILD_VER="1.27.0"

CARDANO_VER="1.27.0"
NESSUS_REV="rev3"

MONIT_VER="5.28.0"
MONIT_REV="rev4"

MMONIT_VER="3.7.7"
MMONIT_REV="rev4"

CNCLI_VER="2.1.1"
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

# CARDANO_BUILD_VER is a possible stale source version
# CARDANO_VER is the source tag that we build
if [[ -z $CARDANO_BUILD_VER ]]; then
  CARDANO_BUILD_VER=${CARDANO_VER}
fi
