# Build the cardano node image
#
# Several examples for pkgs.dockerTools are here
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix
#
{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required image architecture
  imageArch,

  # Required version args
  cardanoVersion,
  cardanoRev,
  debianVersion,
  cabalVersion,
  ghcVersion,

  # https://hydra.iohk.io/build/6782523/download/1/index.html
  hydraBuild ? "6782523",

  baseImage ? import ../baseImage { inherit debianVersion; },
  cardano ? import ../../cardano { inherit cardanoVersion cardanoRev cabalVersion ghcVersion; },
  gLiveView ? import ../../gLiveView {},
  libsodium ? import ../../libsodium {},
}:

let

  imageName = "nessusio/cardano-node";

  # curl -o "./nix/docker/node/context/config/mainnet-config.json" "https://hydra.iohk.io/build/${hydraBuild}/download/1/mainnet-config.json"
  # curl -o "./nix/docker/node/context/config/testnet-config.json" "https://hydra.iohk.io/build/${hydraBuild}/download/1/testnet-config.json"

  # The mainet configs for the cardano-node
  mainnet-config = ./context/config/mainnet-config.json;
  mainnet-topology = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/mainnet-topology.json";
  mainnet-byron-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/mainnet-byron-genesis.json";
  mainnet-shelley-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/mainnet-shelley-genesis.json";
  mainnet-alonzo-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/mainnet-alonzo-genesis.json";

  # The testnet configs for the cardano-node
  testnet-config = ./context/config/testnet-config.json;
  testnet-topology = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/testnet-topology.json";
  testnet-byron-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/testnet-byron-genesis.json";
  testnet-shelley-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/testnet-shelley-genesis.json";
  testnet-alonzo-genesis = builtins.fetchurl "https://hydra.iohk.io/build/${hydraBuild}/download/1/testnet-alonzo-genesis.json";

  # Custom mainnet-config.json

  # The Docker context with static content
  context = ./context;

in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${cardanoVersion}${cardanoRev}-${imageArch}";

    fromImage = "${baseImage.out}/nessusio-debian.tgz";

    contents = [

      # Base packages needed by cardano
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.coreutils         # Basic utilities expected in GNU OS's
      pkgs.curl              # CLI tool for transferring files via URLs
      pkgs.glibcLocales      # Locale information for the GNU C Library
      pkgs.iana-etc          # IANA protocol and port number assignments
      pkgs.iproute           # Utilities for controlling TCP/IP networking
      pkgs.iputils           # Useful utilities for Linux networking
      pkgs.socat             # Utility for bidirectional data transfer
      pkgs.utillinux         # System utilities for Linux
      libsodium              # Cardano crypto library fork

      # Packages needed on RaspberryPi
      pkgs.numactl           # Tools for non-uniform memory access

      # Packages needed by gLiveView
      pkgs.bc                # An arbitrary precision calculator
      pkgs.gawk              # GNU implementation of the Awk programming language
      pkgs.gnugrep           # GNU implementation of the Unix grep command
      pkgs.jq                # Utility for JSON processing
      pkgs.ncurses           # Free software emulation of curses
      pkgs.netcat            # Networking utility for reading from and writing to network connections
      pkgs.procps            # Utilities that give information about processes using the /proc filesystem
      pkgs.tuptime           # Total uptime & downtime statistics utility
    ];

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    # Requires 'system-features = kvm' in /etc/nix/nix.conf
    # https://discourse.nixos.org/t/cannot-build-docker-image/7445
    # runAsRoot = '' do root stuff '';

    extraCommands = ''

      mkdir -p usr/local/bin
      mkdir -p opt/cardano/config
      mkdir -p opt/cardano/data
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs

      # Entrypoint and helper scripts
      cp ${context}/bin/* usr/local/bin

      # Node configurations
      cp ${mainnet-config} opt/cardano/config/mainnet-config.json
      cp ${mainnet-topology} opt/cardano/config/mainnet-topology.json
      cp ${mainnet-byron-genesis} opt/cardano/config/mainnet-byron-genesis.json
      cp ${mainnet-shelley-genesis} opt/cardano/config/mainnet-shelley-genesis.json
      cp ${mainnet-alonzo-genesis} opt/cardano/config/mainnet-alonzo-genesis.json

      cp ${testnet-config} opt/cardano/config/testnet-config.json
      cp ${testnet-topology} opt/cardano/config/testnet-topology.json
      cp ${testnet-byron-genesis} opt/cardano/config/testnet-byron-genesis.json
      cp ${testnet-shelley-genesis} opt/cardano/config/testnet-shelley-genesis.json
      cp ${testnet-alonzo-genesis} opt/cardano/config/testnet-alonzo-genesis.json

      # gLiveView scripts
      cp -r ${gLiveView}/cnode-helper-scripts cnode-helper-scripts

      # Create links for executables
      ln -s ${cardano}/bin/cardano-cli usr/local/bin/cardano-cli
      ln -s ${cardano}/bin/cardano-node usr/local/bin/cardano-node
    '';

    config = {
      Env = [
        # Export the default socket path for use by the cli
        "CARDANO_NODE_SOCKET_PATH=/opt/cardano/ipc/node.socket"
      ];
      Entrypoint = [ "entrypoint" ];
      StopSignal = "SIGINT";
    };
  }
