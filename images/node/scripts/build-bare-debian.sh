#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html


CARDANO=1.24.2
CABAL=3.2.0.0
GHC=8.10.2
 
echo "##########################################################"
echo "# Install Cardano Bare Metal"
echo "#"
echo "# CARDANO: $CARDANO"
echo "# CABAL:   $CABAL"
echo "# GHC:     $GHC"

echo "#========================================================="
echo "# Install dependencies"
echo "#"
 
TARGET=/usr/local/lib/libsodium.so
if [ ! -f "$TARGET" ]; then

  sudo apt-get update -y
  sudo apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf

else

  echo "Nothing to do!"
fi

echo "#========================================================="
echo "# Install Libsodium"
echo "#"

TARGET=/usr/local/lib/libsodium.so
if [ ! -f "$TARGET" ]; then

  mkdir $HOME/src
  cd $HOME/src
  
  git clone https://github.com/input-output-hk/libsodium
  cd libsodium; git checkout 66f017f1
  
  ./autogen.sh
  ./configure
  sudo make install

  echo "" >> $HOME/.profile
  echo "export LD_LIBRARY_PATH=\"/usr/local/lib\"" >> $HOME/.profile
  echo "export PKG_CONFIG_PATH=\"/usr/local/lib/pkgconfig\"" >> $HOME/.profile

else

  echo "Nothing to do!"
fi

echo "#========================================================="
echo "# Install GHC $GHC"
echo "#"

TARGET=/usr/local/bin/ghc
if [ ! -f "$TARGET" ]; then

  mkdir $HOME/src
  cd $HOME/src
  
  wget -q https://downloads.haskell.org/ghc/$GHC/ghc-$GHC-x86_64-deb10-linux.tar.xz
  tar -xf ghc-$GHC-x86_64-deb10-linux.tar.xz
  rm ghc-$GHC-x86_64-deb10-linux.tar.xz
  cd ghc-$GHC
  ./configure
  sudo make install

else

  echo "Nothing to do!"
fi

ghc --version
 
echo "#========================================================="
echo "# Install Cabal $CABAL"
echo "#"

TARGET=/usr/local/bin/cabal
if [ ! -f "$TARGET" ]; then

  mkdir $HOME/src
  cd $HOME/src
  
  wget -q https://downloads.haskell.org/~cabal/cabal-install-$CABAL/cabal-install-$CABAL-x86_64-unknown-linux.tar.xz
  tar -xf cabal-install-$CABAL-x86_64-unknown-linux.tar.xz
  rm cabal-install-$CABAL-x86_64-unknown-linux.tar.xz
  sudo mv cabal /usr/local/bin/
  cabal update
  
else

  echo "Nothing to do!"
fi

cabal --version

echo "#========================================================="
echo "# Build and Install Cardano $CARDANO"
echo "#"

TARGET=/usr/local/bin/cardano-node
if [ ! -f "$TARGET" ]; then

  mkdir $HOME/src
  cd $HOME/src
  
  git clone https://github.com/input-output-hk/cardano-node.git
  cd cardano-node; git checkout $CARDANO
  
  cabal configure
  
  echo "package cardano-crypto-praos" >> cabal.project.local
  echo "  flags: -external-libsodium-vrf" >> cabal.project.local
  
  cabal build all
  
  sudo cp dist-newstyle/build/x86_64-linux/ghc-$GHC/cardano-node-$CARDANO/x/cardano-node/build/cardano-node/cardano-node /usr/local/bin/cardano-node
  sudo cp dist-newstyle/build/x86_64-linux/ghc-$GHC/cardano-cli-$CARDANO/x/cardano-cli/build/cardano-cli/cardano-cli /usr/local/bin/cardano-cli
  
else

  echo "Nothing to do!"
fi

cardano-node --version

echo "#========================================================="
echo "# Get configuration files"
echo "#"

sudo chown core /opt
mkdir -p /opt/cardano/config

TARGET=/opt/cardano/config/mainnet-config.json
if [ ! -f "$TARGET" ]; then

  cd /opt/cardano/config
  wget -qO mainnet-config.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
  wget -qO mainnet-topology.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-topology.json
  wget -qO mainnet-byron-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json
  wget -qO mainnet-shelley-genesis.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json
  
else

  echo "Nothing to do!"
fi

echo "#========================================================="
echo "# Configure the Relay Node"
echo "#"

NODE_ADDR=`curl -s ipinfo.io/ip`
NODE_PORT=3001

CARDANO_RELAY=$NODE_ADDR:$NODE_PORT
echo "CARDANO_RELAY: $CARDANO_RELAY"

mkdir -p /opt/cardano/data
mkdir -p /opt/cardano/proc
cd $HOME

if [[ ! -v CARDANO_NODE_SOCKET_PATH ]]; then
  CARDANO_NODE_SOCKET_PATH=/opt/cardano/proc/socket
  echo "Exportting CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH"
  echo "export CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH" >> $HOME/.profile
fi

TARGET=/usr/lib/systemd/system/cardano.service
if [ ! -f "$TARGET" ]; then

cat << EOF | sudo tee /usr/lib/systemd/system/cardano.service
[Unit]
Description=Cardano Node
# https://www.freedesktop.org/software/systemd/man/systemd.unit.html#StartLimitIntervalSec=interval
StartLimitIntervalSec=0
After=network.target

[Service]

Environment="LD_LIBRARY_PATH=/usr/local/lib" "PKG_CONFIG_PATH=/usr/local/lib/pkgconfig"

ExecStart=/usr/local/bin/cardano-node run \\
  --config /opt/cardano/config/mainnet-config.json \\
  --topology /opt/cardano/config/mainnet-topology.json \\
  --socket-path /opt/cardano/proc/socket \\
  --database-path /opt/cardano/data \\
  --host-addr 0.0.0.0 \\
  --port 3001

Type=simple
User=core
Group=core
WorkingDirectory=/home/core
SyslogIdentifier=cardano-node

# https://www.freedesktop.org/software/systemd/man/systemd.exec.html#StandardOutput=
# StandardOutput=file:/home/core/cardano-node.log
# StandardError=file:/home/core/cardano-node.log

# https://www.freedesktop.org/software/systemd/man/systemd.kill.html#KillSignal=
# NOTE these options are apparently ignored (systemd, after a slow kill, says "sending sigterm")
# Leaving in there because provided by Cardano specifications, so they may work someday...
KillSignal=SIGINT

# ... in the meantime, relying on reduced timeout (default = 90 seconds, set by systemd itself)
# https://www.freedesktop.org/software/systemd/man/systemd.service.html#TimeoutStopSec=
# (choosing this value relevant to the 21 seconds it takes for a full reboot)
TimeoutStopSec=2

# default = 4096 (may not be enough open files for state directory):
LimitNOFILE=32768
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

  sudo systemctl enable cardano.service
  sudo systemctl start cardano.service
fi

journalctl -f -u cardano
