# Build the cardano node Docker image
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
  nessusRevision,
  cabalVersion,
  ghcVersion,

  baseImage ? import ../debian {},
  cardano ? import ../../cardano { inherit cardanoVersion nessusRevision cabalVersion ghcVersion; },
  gLiveView ? import ../../gLiveView { inherit cardanoVersion nessusRevision; },
  libsodium ? import ../../libsodium {},
}:

let

  imageName = "nessusio/cardano-node";

  # The configs for the given cardano-node version
  # mainnet-config = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-config.json";
  mainnet-topology = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-topology.json";
  byron-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-byron-genesis.json";
  shelley-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-shelley-genesis.json";

  # Custom mainnet-config.json
  mainnet-config = ./context/config/mainnet-config.json;

  # The Docker context with static content
  context = ./context;

in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${cardanoVersion}-${nessusRevision}-${imageArch}";

    fromImage = baseImage;

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
      cp ${byron-genesis} opt/cardano/config/mainnet-byron-genesis.json
      cp ${shelley-genesis} opt/cardano/config/mainnet-shelley-genesis.json

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
    };
  }
