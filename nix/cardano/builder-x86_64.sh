source $stdenv/setup

# Set a few env vars

HOME="$TMPDIR/home"

CARDANO_VER="${cardanoVersion}"
NESSUS_REV="${nessusRevision}"

if [ "$system" = "x86_64-linux" ] || [ "$system" = "x86_64-darwin" ]; then
  ARCH="x86_64";
  ARCH_TAG="amd64"
elif [ "$system" = "aarch64-linux" ]; then
  ARCH="aarch64";
  ARCH_TAG="arm64"
else
  echo "Unsuported system: $system" >&2
  exit 1;
fi

FULL_VERSION="$CARDANO_VER-$NESSUS_REV-$ARCH_TAG"
ARCH_VERSION="$CARDANO_VER-$ARCH_TAG"
LATEST_VERSION="latest-$ARCH_TAG"

echo "##########################################################"
echo "# Build Cardano Node for ${ARCH}"
echo "#"
echo "# VERSION: ${FULL_VERSION}"
echo "# CABAL:   ${cabalVersion}"
echo "# GHC:     ${ghcVersion}"
echo ""

export LD_LIBRARY_PATH="${libsodium}/lib"
export PKG_CONFIG_PATH="${libsodium}/lib/pkgconfig"

echo "Checking libsodium ..."
if [[ "$LD_LIBRARY_PATH" != *"libsodium"* ]]; then
  echo "[Error] Cannot find libsodium in $LD_LIBRARY_PATH"
  exit 1
else
  echo "OK"
fi

echo "Checking ghc ..."
if [[ "$PATH" != *"ghc-${ghcVersion}"* ]]; then
  echo "[Error] Cannot find ghc-${ghcVersion} in $PATH"
  exit 1
else
  echo "OK"
fi

echo "Checking cabal ..."
if [[ "$PATH" != *"cabal-install-${cabalVersion}"* ]]; then
  echo "[Error] Cannot find cabal-${cabalVersion} in $PATH"
  exit 1
else
  echo "OK"
fi

echo ""

## Buid + Install Cardano #######################################################################################################

wrsrc="$TMPDIR/cardano-node"

git clone -b ${cardanoVersion} https://github.com/input-output-hk/cardano-node.git $wrsrc
cd $wrsrc

echo "Cabal update ..."
cabal update

echo "Cabal configure ..."
cabal configure

echo "package cardano-crypto-praos" >> cabal.project.local
echo "  flags: -external-libsodium-vrf" >> cabal.project.local

echo "Cabal build all ..."
cabal build all

echo "Copy configs ..."
mkdir -p $out/config
cp $src/configuration/cardano/mainnet-* $out/config/

echo "Copy binaries ..."
mkdir -p $out/bin
cp ./dist-newstyle/build/${ARCH}-linux/ghc-${ghcVersion}/cardano-node-${CARDANO_VER}/x/cardano-node/build/cardano-node/cardano-node $out/bin
cp ./dist-newstyle/build/${ARCH}-linux/ghc-${ghcVersion}/cardano-cli-${CARDANO_VER}/x/cardano-cli/build/cardano-cli/cardano-cli $out/bin
