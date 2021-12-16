#!/bin/bash

# Also change HaskellNix in nix/sources.json
CARDANO_VER="1.32.1"
CARDANO_REV="-dev"

# https://mmonit.com/monit/#download
MONIT_VER="5.28.0"
MONIT_REV="-rev4"

# https://mmonit.com/download
MMONIT_VER="3.7.7"
MMONIT_REV="-rev4"

# https://github.com/tstack/lnav
DEBIAN_VER="10"
LNAV_VER="0.9.0"

# https://github.com/cardano-community/guild-operators/blob/alpha/scripts/cnode-helper-scripts/gLiveView.sh#L59
# Also change change in nix/gLiveView/default.nix
GLVIEW_VER="1.25.0"

# https://github.com/AndrewWestberg/cncli
CNCLI_VER="4.0.4"

CABAL_VER="3.4.0.0"
GHC_VER="8.10.4"

ARCH=`uname -m`

# Checking supported architectures ====================================================================================

if [[ ${ARCH} == "x86_64" ]]; then ARCH_SUFFIX="amd64"
elif [[ ${ARCH} == "aarch64" ]]; then ARCH_SUFFIX="arm64"
else
  echo "[ERROR] Unsupported platform architecture: ${ARCH}"
  exit 1
fi
