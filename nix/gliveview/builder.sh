source $stdenv/setup

# Make a writeable copy of the sources
# https://github.com/NixOS/nixpkgs/issues/14277

wrsrc=$TMPDIR

mkdir -p $wrsrc/cnode-helper-scripts
cp -r $src/scripts/cnode-helper-scripts/* $wrsrc/cnode-helper-scripts

echo "Scripts: `realpath $wrsrc/cnode-helper-scripts`"
touch $wrsrc/cnode-helper-scripts/foo

echo "Source /usr/local/bin/env"
sed -i "s|#CCLI|# Source generated env\nsource /usr/local/bin/env\n\n#CCLI|" $wrsrc/cnode-helper-scripts/env

echo "Disable CNODE_PORT"
sed -i "s/CNODE_PORT=6000/#CNODE_PORT=6000/" $wrsrc/cnode-helper-scripts/env

echo "Disable auto-update"
sed -i "s/NO_INTERNET_MODE=\"N\"/NO_INTERNET_MODE=\"Y\"/" $wrsrc/cnode-helper-scripts/gLiveView.sh

mkdir -p $out/cnode-helper-scripts
cp -r $wrsrc/cnode-helper-scripts/* $out/cnode-helper-scripts
