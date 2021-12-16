source $stdenv/setup

# Set a few env vars

HOME="$TMPDIR/home"

CARDANO_VER="${cardanoVersion}"
CARDANO_REV="${cardanoRev}"

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

FULL_VERSION="$CARDANO_VER$CARDANO_REV-$ARCH_TAG"
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

CARDANO_NODE_WORKSPACE="$TMPDIR/cardano-node"

git clone -b ${cardanoVersion} --depth 1 https://github.com/input-output-hk/cardano-node.git $CARDANO_NODE_WORKSPACE
cd $CARDANO_NODE_WORKSPACE

CARDANO_NODE_GITREV=$(git rev-parse HEAD)
echo "CARDANO_NODE_GITREV=${CARDANO_NODE_GITREV}"

echo "Cabal update #############################################################"
cabal update

echo "Cabal configure ##########################################################"
cabal configure

echo "package cardano-crypto-praos" >> cabal.project.local
echo "  flags: -external-libsodium-vrf" >> cabal.project.local

echo "Cabal build ##############################################################"
git checkout -f ${CARDANO_NODE_GITREV}
cabal build cardano-node cardano-cli

echo "Copy configs ..."
mkdir -p $out/config
cp $src/configuration/cardano/mainnet-* $out/config/

echo "Copy binaries ..."
mkdir -p $out/bin
cp $(find ./dist-newstyle/build -type f -name "cardano-node") $out/bin
cp $(find ./dist-newstyle/build -type f -name "cardano-cli") $out/bin
